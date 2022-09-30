# Lifelines
# Scrape Wikipedia (from https://kaijento.github.io/2017/05/13/web-scraping-wikipedia.com/)
# 1/2/2021

# Import packages
import requests
from bs4 import BeautifulSoup
import os
import csv
import pandas as pd
import re

# Create a folder in the working directory called "wiki_summaries" to store plot summaries
wiki_dir = "wiki_summaries"

# Read in Rotten Tomatoes CSV for reviews of 17k+ movies
data = pd.read_csv("rotten_tomatoes_movies.csv")

# Define movie names, minimum summary length, and general Wikipedia url
movie_names = data.movie_title.tolist()
# print(movie_names)
min_summary = 80
url = 'http://en.wikipedia.org/wiki/'

# Get plot summaries; takes about two hours
# The movie names from Rotten Tomatoes don't always match up to those from Wikipedia, so we will skip those titles.
with requests.session() as s:
    s.headers['user-agent'] = 'Mozilla/5.0' # set web browser

    for movie in movie_names: # loop through all movies
        try:
            r = s.get(url + movie) # get each movie url
            soup = BeautifulSoup(r.content, 'html5lib') # get html content
            plot = [] # make empty list for plot summaries
            tag = soup.select_one('#Plot').find_parent('h2').find_next_sibling() # get all paragraphs of plot summary
            if not tag: # exclude movies if there is no plot summary to be found
                continue

            while tag.name == 'p':
                plot.append(tag.text)
                if not plot: # if there are no paragraphs to scrape, skip
                    continue
                tag = tag.find_next_sibling()

            # write out to file
            plot = "".join(plot) # convert plot from list to string
            plot_length = len(re.split(r"\s+", plot)) # count the individual words in each plot summary
            if plot_length < min_summary: # only get summaries that are above 80 words
                continue
            # print(plot) # replace spaces with underscores
            with open(os.path.join(wiki_dir, movie.replace(" ", "_") + '.txt'), 'w', encoding='utf-8') as outfile:
                outfile.write(plot)
        except:
            pass

# Remove blank files (of size 0)
file_directory = os.path.dirname(os.path.abspath(__file__)) # set current file's directory
str_directory = os.path.join(file_directory, "wiki_summaries") # go down to the wiki_summaries folder
list_files = [x for x in os.listdir(str_directory) if x[0] != '.'] # list all files in the wiki_summaries folder
for each_file in list_files: # loop through the list of files
    file_path = '%s/%s' % (str_directory, each_file)
    # check size and delete if 0
    if os.path.getsize(file_path) == 0:
        os.remove(file_path)
    else:
        pass

# Afterward, visually inspect and manually delete files that are still blank or too small.
# (Tip: sort folder contents by size.)