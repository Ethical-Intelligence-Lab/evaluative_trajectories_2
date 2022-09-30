import labMTsimple.storyLab 
from labMTsimple.storyLab import emotionFileReader
from labMTsimple.speedy import LabMT
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

def process(raw_text_clean, words_per_window, movie_name, labMT, labMTvector):
    ''''splits words by line and cleans them, then calls the chopper() function'''
    '''Inputs: raw movie text, window size (in main), movie name (in main), labMT dictionary, labMT valence column'''
    '''Outputs: movie text (kwords), a list of valence scores (valence_list)'''
    
    #words = [x.lower() for x in re.findall(r"[\w\@\#\'\&\]\*\-\/\[\=\;]+",raw_text_clean,flags=re.UNICODE)] #create list of single word entries; only used to print number of original words

    # Get lines of text (aka paragraphs of wiki entry), and clean them up.
    # Also, save what paragraph each word came from (do we need this? )
    lines = raw_text_clean.split("\n") 
    kwords = []
    klines = []

    for i in range(len(lines)):
        if lines[i][0:3] != "<b>":
            tmpwords = [x.lower() for x in re.findall(r"[\w\@\#\'\&\]\*\-\/\[\=\;]+",lines[i],flags=re.UNICODE)]
            kwords.extend(tmpwords)
            klines.extend([i for j in range(len(tmpwords))])

    #print("length of the original parse: ", len(words))
    print("length of the new parse: ", len(kwords))

    valence_list = chopper(kwords, labMT, labMTvector, window_size) 

    return kwords, valence_list


def chopper(words, labMT, labMTvector, window_size, verbose=False): 
    ''' Chops up wikipedia summary (words) based on bin size (words_per_win),
    then gives the valence score for each bin'''
    '''Inputs: movie text, labMT dictionary, labMT valence column, window size, and verbose=TRUE/FALSE'''
    '''Outputs: a list of valence scores (valence_list)'''
    
    print("now splitting the text into chunks of size ", len(words)//window_size)
    words_per_win = len(words)//window_size
    
    allFvec = []
    valence_list = []
    for i in range(int(floor(window_size))):
        chunk = str('')
        chunk_list = []
        if i == window_size-1:
            # For the last window number, use whatever number of words remain
            for j in range(i*words_per_win,len(words)-1):
                chunk_list.append(words[j])
        else:
            # For every window number except the last one...
            for j in range(i*words_per_win,(i+1)*words_per_win):
                chunk_list.append(words[j])
        
        #textValence, textFvec = labMTsimple.storyLab.emotion(chunk, labMT, scoreIndex=3, shift=True, happsList=labMTvector)
        #! Probably want to use a more robust sentiment score package here. 
        cur_valence_list = []
        for word in chunk_list:
            if word in labMT:
                cur_valence_list.append(labMT[word][3])
            else: 
                if verbose:
                    print('missing word: ', word)

        if verbose:
            print('custom mean valence: ', np.mean(cur_valence_list))

        valence_list.append(np.mean(cur_valence_list)) #append valence of current chunk

    return valence_list

if __name__ == "__main__":
    scripts = glob.glob('./wiki_summaries/*.txt') #wiki_summaries

    # Read in dictionary of valence scores
    lexicon = pd.read_csv("NRC-VAD-Lexicon.txt", sep = "\t")
    lexicon['Ranking'] = np.arange(1, len(lexicon)+1) #add ranking
    columnsTitles = ["Word", "Ranking", "Arousal", "Valence", "Dominance"]
    lexicon = lexicon.reindex(columns = columnsTitles)
    avd_dict = lexicon.set_index('Word').T.to_dict('list')

    # get valence scores for words, and rename with labMTsimple-compatible names
    nlist = lexicon['Valence'].tolist() 
    labMTvector = nlist
    wlist = lexicon['Word'].tolist() 
    labMTwordList = wlist 
    labMT = avd_dict
    final_data = pd.DataFrame()

    for idx, script in enumerate(scripts):
        file = codecs.open(script, "r", "utf8")
        raw_text_clean = file.read()
        file.close()
        window_size = 80
        words_per_window = len(raw_text_clean)//window_size
        #potentially need to add logic that splits based on PC/mac
        # movie_name = script.split("/")[2].replace('.txt', '') #pc
        movie_name = script.split("\\",1)[1].replace('-', '').replace('.txt', '') #mac
        try:
            idx, valence_list = process(raw_text_clean, window_size, movie_name, labMT, labMTvector)
            final_data[movie_name] = valence_list
        except IndexError:
            pass
        except ZeroDivisionError: 
            continue
        print(movie_name)
    
    final_data.to_csv('movie_valence_scores.csv')