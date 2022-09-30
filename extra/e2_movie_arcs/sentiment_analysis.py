# Lifelines
# Sentiment Analysis

# Import packages
import numpy as np
import os
from tqdm import trange
import pandas as pd
import codecs
import re
import seaborn as sns
from numpy import floor, zeros, array
import numpy as np
import shutil
import subprocess
import datetime
import glob
from pdb import set_trace
import json
import matplotlib.pyplot as plt
import random
import tensorflow_datasets as tfds

# Set seed
random.seed(2)

# Set working directory
root_path = os.path.dirname(os.path.realpath(__file__))

# -------------------------------------------------------------------------------------------------------------------- #
                                                # Neural Network #
# -------------------------------------------------------------------------------------------------------------------- #

# Using matplotlib, create a helper function to plot graphs
def plot_graphs(history, metric):
  plt.plot(history.history[metric])
  plt.plot(history.history['val_'+metric], '')
  plt.xlabel("Epochs")
  plt.ylabel(metric)
  plt.legend([metric, 'val_'+metric])

# Setup input pipeline: Download the dataset using [TFDS](https://www.tensorflow.org/datasets).
# See the [loading text tutorial](../load_data/text.ipynb) for details on how to load this sort of data manually.
def prepare_dataset():
  dataset, info = tfds.load('imdb_reviews', with_info=True,
                          as_supervised=True)
  train_dataset, test_dataset = dataset['train'], dataset['test']
  return train_dataset, test_dataset

train_dataset, test_dataset = prepare_dataset()
train_dataset.element_spec

# Initially this returns a dataset of (text, label pairs)
for example, label in train_dataset.take(1):
  print('text: ', example.numpy())
  print('label: ', label.numpy())

# Next shuffle the data for training and create batches of these (text, label) pairs
BUFFER_SIZE = 10000
BATCH_SIZE = 64

train_dataset = train_dataset.shuffle(BUFFER_SIZE).batch(BATCH_SIZE).prefetch(tf.data.AUTOTUNE)
test_dataset = test_dataset.batch(BATCH_SIZE).prefetch(tf.data.AUTOTUNE)

for example, label in train_dataset.take(1):
  print('texts: ', example.numpy()[:3])
  print()
  print('labels: ', label.numpy()[:3])

# -------------------------------------------------------------------------------------------------------------------- #
# Create the text encoder
VOCAB_SIZE = 1000

# Create the layer, and pass the dataset's text to the layer's .adapt method
encoder = tf.keras.layers.experimental.preprocessing.TextVectorization(
    max_tokens=VOCAB_SIZE)
encoder.adapt(train_dataset.map(lambda text, label: text))

# After the padding and unknown tokens, they're sorted by frequency
vocab = np.array(encoder.get_vocabulary())
vocab[:20]

# Once the vocabulary is set, the layer can encode text into indices
encoded_example = encoder(example)[:3].numpy()
encoded_example

for n in range(3):
  print("Original: ", example[n].numpy())
  print("Round-trip: ", " ".join(vocab[encoded_example[n]]))
  print()

# -------------------------------------------------------------------------------------------------------------------- #
# Create the model

# Define a simple sequential model
def create_model():
    model = tf.keras.Sequential([
        encoder,
        tf.keras.layers.Embedding(
            input_dim=len(encoder.get_vocabulary()),
            output_dim=64,
            # Use masking to handle the variable sequence lengths
            mask_zero=True),
        tf.keras.layers.Bidirectional(tf.keras.layers.LSTM(64)),
        tf.keras.layers.Dense(64, activation='relu'),
        tf.keras.layers.Dense(1)
    ])

    model.compile(loss=tf.keras.losses.BinaryCrossentropy(from_logits=True),
                  optimizer=tf.keras.optimizers.Adam(1e-4),
                  metrics=['accuracy'])

    return model

# Create a basic model instance
model = create_model()

# predict on a sample text without padding.
sample_text = ('The movie was terrible. The animation and the graphics '
               'were awful. I hated this movie.')
predictions = model.predict(np.array([sample_text]))
print(predictions[0])

# predict on a sample text with padding
padding = "the " * 2000
predictions = model.predict(np.array([sample_text, padding]))
print(predictions[0])

# Display the model's architecture
model.summary()

# All the layers after the Embedding support masking
print([layer.supports_masking for layer in model.layers])

# -------------------------------------------------------------------------------------------------------------------- #
# Train the model
history = model.fit(train_dataset, epochs=10,
                    validation_data=test_dataset,
                    validation_steps=30)

model.save('root_path')
model = keras.models.load_model('root_path')

test_loss, test_acc = model.evaluate(test_dataset)

print('Test Loss: {}'.format(test_loss))
print('Test Accuracy: {}'.format(test_acc))

# plt.figure(figsize=(16,8))
# plt.subplot(1,2,1)
# plot_graphs(history, 'accuracy')
# plt.ylim(None,1)
# plt.subplot(1,2,2)
# plot_graphs(history, 'loss')
# plt.ylim(0,None)

# Run a prediction on a new sentence: if the prediction is >= 0.0, it is positive; else, it is negative.
sample_text = ('The movie was cool. The animation and the graphics '
               'were out of this world. I would recommend this movie.')
predictions = model.predict(np.array([sample_text]))
predictions

# -------------------------------------------------------------------------------------------------------------------- #
                                                # Define Functions #
# -------------------------------------------------------------------------------------------------------------------- #

def process_words(raw_text_clean, window_size):
    ''' Chops up wikipedia summary (words) based on bin size (words_per_win),
    then gives the valence score for each bin'''
    '''Inputs: movie text, window size'''
    '''Outputs: a list of valence scores (valence_list)'''

    # raw_text = raw_text_clean.lower()
    # clean_text = re.sub(r'[^A-Za-z0-9]+', ' ', raw_text)
    # words = list(clean_text.split(" "))
    clean_words = [x.lower() for x in
                   re.findall(r"[\w\@\#\'\&\]\*\-\/\[\=\;]+", raw_text_clean, flags=re.UNICODE)]  # clean words
    join_words = re.sub(r'(\s-\s)|(\s&\s)|( +)', ' ',
                        ' '.join(clean_words))  # get rid of space-dash-space, space-ampersand-space, etc. in text
    words = list(join_words.split(" "))  # split words by space
    # print(words)

    print("now splitting the text into chunks of size", len(words) // window_size)
    words_per_win = len(words) // window_size

    valence_list = []
    for i in range(int(floor(window_size))):
        chunk_list = []
        if i == window_size - 1:
            # For the last window number, use whatever number of words remain
            for j in range(i * words_per_win, len(words) - 1):
                chunk_list.append(words[j])
        else:
            # For every window number except the last one...
            for j in range(i * words_per_win, (i + 1) * words_per_win):
                chunk_list.append(words[j])

        chunk_list = ' '.join(chunk_list)
        # print(chunk_list)

        predict_chunk = model.predict(np.array([chunk_list]))
        valence_list.append(predict_chunk[0][0])

    return (valence_list)

# -------------------------------------------------------------------------------------------------------------------- #
                                                # Analyze Sentiment #
# -------------------------------------------------------------------------------------------------------------------- #

scripts = glob.glob(root_path + '/wiki_summaries/*.txt')  # read in wiki_summaries
final_data = pd.DataFrame()
window_size = 9  # 81 #change window size here

for script in scripts:
    with open(script, encoding="utf-8") as f:
        raw_text_clean = f.read()

    # Get rid of movie summaries that are too short for our window_size size
    check_words = raw_text_clean.split()
    if len(check_words) <= window_size:
        continue

    # For the remaining movie summaries
    words_per_window = len(raw_text_clean) // window_size
    file_name = os.path.splitext(script)[0]
    movie_name = file_name.split("/")[-1]
    print(movie_name)
    try:
        valence_list = process_words(raw_text_clean, window_size)
        final_data[movie_name] = valence_list
    except Exception:
        continue

# Print final data frame
print(final_data)

# -------------------------------------------------------------------------------------------------------------------- #
                                                # Analyze Sentiment #
# -------------------------------------------------------------------------------------------------------------------- #
#
# # Download Scores CSV
# final_data.to_csv("python_9_bins_scores.csv")  # , index = False, encoding = "utf-8-sig") #"utf-8-sig" makes sure that the movie names are properly encoded
# files.download("python_9_bins_scores.csv")
# # Download onto local computer and manually move to Lifelines/e2_movie_arcs folder



























































































































