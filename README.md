Data and Code for 'Evaluative Summaries'
Julian De Freitas, Pechthida Kim, Tomer Ullman

## Instructions
* For topic modelling, install 'gsl' (If you use homebrew, execute `brew install gsl`)
* Run `tools/Lifelines_Generate_Plots.R`, since we will use the variables generated here in the other analyses.
* Steps below are only required if you want to run clustering code:
  * Create a virtual Python environment
  * Install requirements: `pip install -r requirements.txt`
  * Run `python -m spacy download en_core_web_sm`
<br><br>
* This step is only needed if you want to re-run sentiment analysis (not required for main analyses): Setup sentiment.ai (For more information: https://benwiseman.github.io/sentiment.ai/#Installation__Setup)

### For Analyses Comparing Different Studies:
* Run `between_experiment_analyses/analysis.R`

### Study 1 - Satisfaction of a Customer Journey:
* Run `satisfaction_of_a_customer_journey/analysis/analysis.R`

### Study 2 - Process Awareness:
* Run `meta_awareness/analysis/analysis.R`

### Study 3 - Hiring Likelihood:
* Run `hiring_likelihood/analysis/analysis.R`

### Study 4 - Directly Experienced Content:
* Run `directly_experienced_content/analysis/analysis.R`

    #### Other Analysis Files
  * For exclusions, converting timeseries data to lines, plotting each participant line, calculating features of each line (e.g., num of peaks, derivative): Run `directly_experienced_content/main.ipynb`
  * For clustering, run `directly_experienced_content/TimeSeriesClustering.ipynb`

### Study S1 - Naturally Evaluative
* Run `naturally_evaluative/analysis/analysis.R`

### Study S2 - Meaningfulness of a Life
* Run `lifelines/analysis/analysis.R`

------------

## Troubleshooting
* sentiment.ai setup probably won't work on an ARM based macbook, I will probably need to find a workaround for that.
* If you run into this error while setting up sentiment.ai: `‘~/.virtualenvs/r-sentiment-ai/bin/python’ was not built with a shared library.
reticulate can only bind to copies of Python built with ‘--enable-shared’.`, install pyenv and run `$ env PYTHON_CONFIGURE_OPTS="--enable-shared" pyenv install 3.8.10`