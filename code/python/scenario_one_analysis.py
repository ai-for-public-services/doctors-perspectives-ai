### Import necessary libraries. 
# Quantized LLama
from ctransformers import AutoModelForCausalLM,AutoTokenizer

# Embedding Model
from sentence_transformers import SentenceTransformer, util
from sentence_transformers.util import cos_sim

# Kmeans and PCA 
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_samples, silhouette_score
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA

# Plotting
import matplotlib.pyplot as plt

# NLP utilities
import re
import nltk
from nltk.corpus import stopwords
from nltk.util import ngrams 
import string

# Python dataframes and arrays
import pandas as pd
import numpy as np

# Misc
from collections import Counter

import requests
import json
import torch
from ctransformers import AutoModelForCausalLM
import random
import pandas as pd

from gensim import corpora
from gensim.models import LdaModel
from gensim.models.coherencemodel import CoherenceModel
from pprint import pprint

##Function definition - run llama on azure
target_url = 'https://Llama-2-70b-chat-dft-serverless.eastus2.inference.ai.azure.com'
api_key = 't8FlgEG09Wlr9MfgXeIDMkJy0kv5eb8R'

def ask_llama(prompt, temp = 1, max_tokens = 10000):
    llm_inputs = {
            "messages":
            [
                { 
                "role": "system", 
                "content": prompt
                }
            ],
            "temperature": temp,
        }
    response = requests.post(f'{target_url}/v1/chat/completions', headers={'Authorization': f'Bearer {api_key}', 'Content-Type': 'application/json'}, data=json.dumps(llm_inputs))
    return(response.json()['choices'][0]['message']['content'])


# Read data
df = pd.read_csv("/Users/sesnaashari/GMC/Data/cleaned_analytic_file_for_viz_090224.csv")

## Seperate responses who mention not_know to make processing faster
question_scenario_one = "You are using an AI clinical decision-support system which recommends \
treatment options for individual patients. You disagree with the recommendation that is given by \
the system. How would you proceed?"

not_know_lists = ["dont know","don't know", "not know", "not sure", "don’t know"]
df = df.dropna(subset=['scenario_one'])
df['not_know'] = df['scenario_one'].apply(lambda x: 1 if any(ss.lower() in x.lower() for ss in not_know_lists) else 0)
df_filtered = df[df['not_know']== 0]
df_filtered.index = range(len(df_filtered)) #reindex the new list to start from 0-len. 

### Using Gensim to extract topics from each response. 
texts = df_filtered['scenario_one'][0:10]
dictionary = corpora.Dictionary([texts])
dictionary.doc2bow()




