# -*- coding: utf-8 -*-
"""
Created on Mon Nov 27 10:32:28 2023

@author: lijiajun
"""


import pandas as pd

dat = pd.read_csv('./data/Neopep_data_org.txt',sep='\t')
dat = dat[dat['response_type'] != 'not_tested']
# print(dat.isnull().sum())

dat = dat.fillna(0)
dat['mut_is_binding_pos'].replace([False,True],
                        [0,1], inplace=True)


dat['bestWTMatchType_I'].replace(['NONE','COVER','EXACT','INCLUDED','PARTIAL','PARTIAL_MUT'],
                        [0,1,2,3,4,5], inplace=True)

dat['Clonality'].replace(['clonal','subclonal'],
                        [1,2], inplace=True)

dat['mutation_driver_statement_Intogen'].replace(['KNOWN DRIVER','POLYMORPHISM','PREDICTED DRIVER','PREDICTED PASSENGER'],
                        [1,2,3,4], inplace=True)

dat['gene_driver_Intogen'].replace(['OTHER TUMOR DRIVER','TUMOR DRIVER'],
                        [1,2], inplace=True)

# print(dat.isnull().sum())


train_dat = dat[dat['train_test']=='train']
val_dat = dat[dat['train_test']=='test']

train_dat.to_csv('./data/train.csv',index=False)
val_dat.to_csv('./data/test.csv',index=False)































