import os
import pandas as pd
import pickle

from sklearn.ensemble import RandomForestClassifier
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestClassifier


from ftglobals import *


train_file = '/home/tomas/GitRepos/IPC2023/train_models/epm_train_01.csv'


df = pd.read_csv(train_file, index_col=0).reset_index(drop=True)



def matriz_confusion(y_real, y_pred):
    real = pd.Series(y_real, name='Real')
    predicted = pd.Series(y_pred, name="Predicted")
    return pd.crosstab(predicted, real)



target = df.outcome.copy()
target.loc[target != 'solved'] = 'failed'


train_data = df.drop(excled_cols + ['outcome'], axis=1)


train_data['cpt_fact_balance'] = train_data.h_ff_ratio.notnull().astype(int)
train_data['cpt_heuristics'] = train_data.Goal_count.notnull().astype(int)
train_data['cpt_landmarks'] = train_data.n_landmarks.notnull().astype(int)
train_data['cpt_redblack'] = train_data.blackVariables.notnull().astype(int)


raw_features = train_data[ftcols_baseft + ftcols_ftcomputed + ftcols_landmarks + ['planner']]
features = pd.get_dummies(raw_features)


rforest = RandomForestClassifier()
_ = rforest.fit(features, target)


rf_pred_probs = rforest.predict_proba(features)


model_data = {
    'model': rforest,
    'columns': features.columns,
    'planners': list(raw_features.planner.unique()),
    'fillna_avg': features.mean(numeric_only=True)
}


# Make sure pickle protocol is compatible with Python2.7
fmod = '/home/tomas/GitRepos/IPC2023/ibacop2-2018/src/models/iba_model_data_rf.pkl'
fmod = 'src/models/iba_model_data_rf.pkl'
# fmod = '/home/tomas/GitRepos/IPC2023/train_models/iba_model_data_rf_2.pkl'
# print(os.listdir('./'))    

with open(fmod,'wb') as fo:
    pickle.dump(model_data, fo, 2)
print("Model saved in: ", fmod)
