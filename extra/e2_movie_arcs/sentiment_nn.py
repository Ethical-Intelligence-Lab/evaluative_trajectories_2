import numpy as np

import tensorflow_datasets as tfds
import tensorflow as tf

tfds.disable_progress_bar()


import matplotlib.pyplot as plt

def plot_graphs(history, metric):
  plt.plot(history.history[metric])
  plt.plot(history.history['val_'+metric], '')
  plt.xlabel("Epochs")
  plt.ylabel(metric)
  plt.legend([metric, 'val_'+metric])

dataset, info = tfds.load('imdb_reviews', with_info=True,
                          as_supervised=True)
train_dataset, test_dataset = dataset['train'], dataset['test']

train_dataset.element_spec

for example, label in train_dataset.take(1):
  print('text: ', example.numpy())
  print('label: ', label.numpy())

VOCAB_SIZE=1000
encoder = tf.keras.layers.experimental.preprocessing.TextVectorization(
    max_tokens=VOCAB_SIZE)
encoder.adapt(train_dataset.map(lambda text, label: text))

vocab = np.array(encoder.get_vocabulary())
vocab[:20]

encoded_example = encoder(example)[:3].numpy()
encoded_example

for n in range(3):
  print("Original: ", example[n].numpy())
  print("Round-trip: ", " ".join(vocab[encoded_example[n]]))
  print()

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

print([layer.supports_masking for layer in model.layers])

# predict on a sample text without padding.

sample_text = ('The movie was cool. The animation and the graphics '
               'were out of this world. I would recommend this movie.')
predictions = model.predict(np.array([sample_text]))
print(predictions[0])

# predict on a sample text with padding

padding = "the " * 2000
predictions = model.predict(np.array([sample_text, padding]))
print(predictions[0])

# predict on a sample text with padding

padding = "the " * 2000
predictions = model.predict(np.array([sample_text, padding]))
print(predictions[0])

model.compile(loss=tf.keras.losses.BinaryCrossentropy(from_logits=True),
              optimizer=tf.keras.optimizers.Adam(1e-4),
              metrics=['accuracy'])

history = model.fit(train_dataset, epochs=10,
                    validation_data=test_dataset, 
                    validation_steps=30)

test_loss, test_acc = model.evaluate(test_dataset)

print('Test Loss: {}'.format(test_loss))
print('Test Accuracy: {}'.format(test_acc))

plt.figure(figsize=(16,8))
plt.subplot(1,2,1)
plot_graphs(history, 'accuracy')
plt.ylim(None,1)
plt.subplot(1,2,2)
plot_graphs(history, 'loss')
plt.ylim(0,None)

sample_text = ('The movie was cool. The animation and the graphics '
               'were out of this world. I would recommend this movie.')
predictions = model.predict(np.array([sample_text]))

model = tf.keras.Sequential([
    encoder,
    tf.keras.layers.Embedding(len(encoder.get_vocabulary()), 64, mask_zero=True),
    tf.keras.layers.Bidirectional(tf.keras.layers.LSTM(64,  return_sequences=True)),
    tf.keras.layers.Bidirectional(tf.keras.layers.LSTM(32)),
    tf.keras.layers.Dense(64, activation='relu'),
    tf.keras.layers.Dropout(0.5),
    tf.keras.layers.Dense(1)
])

model.compile(loss=tf.keras.losses.BinaryCrossentropy(from_logits=True),
              optimizer=tf.keras.optimizers.Adam(1e-4),
              metrics=['accuracy'])

history = model.fit(train_dataset, epochs=10,
                    validation_data=test_dataset,
                    validation_steps=30)

test_loss, test_acc = model.evaluate(test_dataset)

print('Test Loss: {}'.format(test_loss))
print('Test Accuracy: {}'.format(test_acc))

# predict on a sample text without padding.

sample_text = ('The movie was not good. The animation and the graphics '
                    'were terrible. I would not recommend this movie.')
predictions = model.predict(np.array([sample_text]))
print(predictions)

plt.figure(figsize=(16,6))
plt.subplot(1,2,1)
plot_graphs(history, 'accuracy')
plt.subplot(1,2,2)
plot_graphs(history, 'loss')

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

from google.colab import drive
from google.colab import files
drive.mount('/content/drive', force_remount = True)

root_path = '/content/drive/My Drive/Lab Stuff/wiki_summaries/'

def process_words(raw_text_clean, window_size): 
    ''' Chops up wikipedia summary (words) based on bin size (words_per_win), 
    then gives the valence score for each bin'''
    '''Inputs: movie text, window size'''
    '''Outputs: a list of valence scores (valence_list)'''

    # raw_text = raw_text_clean.lower()
    # clean_text = re.sub(r'[^A-Za-z0-9]+', ' ', raw_text)
    # words = list(clean_text.split(" "))
    words = [x.lower() for x in re.findall(r"[\w\@\#\'\&\]\*\-\/\[\=\;]+", raw_text_clean, flags = re.UNICODE)] #clean words 
    
    print("now splitting the text into chunks of size", len(words)//window_size)
    words_per_win = len(words)//window_size

    valence_list = []
    for i in range(int(floor(window_size))):
        chunk_list = []
        if i == window_size-1:
            # For the last window number, use whatever number of words remain
            for j in range(i*words_per_win,len(words)-1):
                chunk_list.append(words[j])
        else:
            # For every window number except the last one...
            for j in range(i*words_per_win,(i+1)*words_per_win):
                chunk_list.append(words[j])
        
        chunk_list = ' '.join(chunk_list)
        # print(chunk_list)
        
        predict_chunk = model.predict(np.array([chunk_list]))
        valence_list.append(predict_chunk[0][0])

    return(valence_list) 

scripts = glob.glob(root_path + '/*.txt') #wiki_summaries
final_data = pd.DataFrame()
window_size = 81 #change window size here

for script in scripts:
        with open(script, encoding = "utf-8") as f: 
            raw_text_clean = f.read()
        
        # Get rid of movie summaries that are too short for our window_size size 
        check_words = raw_text_clean.split()
        if len(check_words) <= window_size: 
            continue 
    
        # For the remaining movie summaries 
        words_per_window = len(raw_text_clean)//window_size
        file_name = os.path.splitext(script)[0]
        movie_name = file_name.split("/")[-1] 
        print(movie_name)        
        try:
            valence_list = process_words(raw_text_clean, window_size)
            final_data[movie_name] = valence_list
        except (IndexError, ZeroDivisionError):
            continue

# Download Scores CSV 
final_data.to_csv("python_81_bins_scores.csv") 
files.download("python_81_bins_scores.csv") 
# Manually move to Lifelines/e2_movie_arcs folder

