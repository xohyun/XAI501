###
# 대로와 구를 나누는 코드 / 
#
###
import numpy as np
import pandas as pd
import tensorflow as tf
#@tf.autograph.experimental.do_not_convert
df = pd.read_csv("C:\\Users\\soso\\Desktop\\mlmlml\\airseoul_day.csv",encoding='CP949')
# gangnam=df.iloc[:3982,:]
# gangbuk=df.iloc[3982:,:]
daero_idx = df['SGG'].str.contains('대로')
df_daero = df.loc[daero_idx]
df_sgg = df.loc[~daero_idx]

df_daero.SGG = '대로'; df_sgg.SGG = '구'

df_daero = df_daero.groupby(['DATE'], as_index = False).mean()
df_sgg = df_sgg.groupby(['DATE'], as_index = False).mean()

df_daero['SGG'] = '대로'; df_sgg['SGG'] = '구'

df_daero = df_daero.drop('Unnamed: 0', axis = 1)
df_sgg = df_sgg.drop('Unnamed: 0', axis = 1)

df = pd.concat([df_daero, df_sgg])
# df.to_csv("C:\\Users\\soso\\Desktop\\mlmlml\\separate_SGG_DG.csv", encoding = 'utf-8-sig')