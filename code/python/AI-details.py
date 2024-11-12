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


#Read data
df = pd.read_csv("/Users/sesnaashari/Documents/Github/GMC/Data/cleaned_analytic_file_for_viz_290224.csv")
df['details_ai_system'] = df['details_ai_system_1_1']
df.loc[df['details_ai_system'].isnull(), 'details_ai_system'] = df.details_ai_system_1


# Decision support system details.  
dss_ai_systems = df.loc[df['ai_type_use_qs']== 'Diagnostic and decision support system', 'details_ai_system']
used_freq = 100-sum(dss_ai_systems.isnull())/len(dss_ai_systems)*100
dss_ai_systems = dss_ai_systems.dropna()
system_list = [['ray', 'imag','scan','mri', 'radio', 'brain', 'oscopy', 'ultra', 'Lung'], 
               ['ecg', 'CTG'], ['risk','risc'],['predict','prog'],['detect','diag'], 'NELA', 'klinik','nhs','Neuro', 'Triage','Ardens', 'Violet']
names = ['Imaging','ECG/CTG', 'Risk','Predict','Diagnos'] + system_list[5:]
all_index = [False]*len(dss_ai_systems)
system_users = dict()
dss_ai_systems['dss_detail'] = ""
for i in range(len(system_list)):
    system = system_list[i]
    name = names[i]
    if(type(system)!=list):
        system = [system]
    index = dss_ai_systems.apply(lambda x: True if any(ss.lower() in x.lower() for ss in system) else False)
    dss_ai_systems.loc[index,'dss_detail'] = dss_ai_systems.loc[index,'dss_detail'] + "," + name
    all_index = np.logical_or(all_index,index)
    system_users[''.join(list(name))] = dss_ai_systems[index]
