# -*- coding: utf-8 -*-
"""
Created on Sat Dec 16 14:18:28 2023

@author: lijiajun
"""

import pandas as pd
import numpy as np
from sklearn.metrics import average_precision_score, precision_recall_curve,auc

df = pd.read_csv('./data/rank_ptuneos.csv')

def fr100(x):
    if x:
        x = np.nan_to_num(x, nan=101)
        x = [int(i) for i in x]
        return(len([j for j in x if j <= 100])/len(x))
    else:
        return 0

    
def ttif(x):
    if x:
        x = np.nan_to_num(x, nan=101)
        x = [int(i) for i in x]
        return(len([j for j in x if j <= 20])/20)
    else:
        return 0

def fun(x):
    l = [0] * max(x)
    for i in x:
        l[i-1]=1
    return l

def auprc(x):
    
    if x:
        x = np.nan_to_num(x, nan=101)
        x = [int(i) for i in x]
        x = [j for j in x if j <= 100]
        if x:   
            score = list(range(max(x),0,-1))
            lable = fun(x)
            precision, recall, _ = precision_recall_curve(lable,score)
            auprc = auc(recall,precision)
            return auprc
        else:
            return 0
        
    else:
        return 0
        
def r_pod(x):
    if x:
        x = np.nan_to_num(x, nan=101)
        x = [int(i) for i in x]
        return((len([j for j in x if j <= 20])+sum([1/(2**j) for j in x if j <= 20]))/20)
    else:
        return 0
    
    
    
    
def cal(patient, rank):
    df = pd.DataFrame({'patient':patient, 'rank':rank})
    df1 = df.groupby('patient').agg(list)['rank'].to_frame().reset_index()
    df1['FR100'] = df1['rank'].apply(fr100)
    df1['TTIF'] = df1['rank'].apply(ttif)  
    df1['AUPRC'] = df1['rank'].apply(auprc)  
    df1['R_PoD'] = df1['rank'].apply(r_pod) 
    df1 = df1[['patient','FR100','TTIF','R_PoD','AUPRC']]
    return df1

RNN_NB = cal(df['patient'], df['RNN_NB_rank'])
RNN_voting = cal(df['patient'], df['RNN_voting_rank'])
pTuneos = cal(df['patient'], df['pTuneos_rank'])


RNN_NB['method'] = 'RNN_NB'
RNN_voting['method'] = 'RNN_voting' 
pTuneos['method'] = 'pTuneos'

FR_TTIF_AUPRC = pd.concat([RNN_NB,RNN_voting,pTuneos], axis = 0)

FR_TTIF_AUPRC.to_csv('./result/result_pTuneos.csv',index=False)