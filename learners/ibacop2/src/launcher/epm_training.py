import os, sys
import pandas as pd
import pickle

from sklearn.ensemble import RandomForestClassifier
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestClassifier


from ftglobals import *
print(len(sys.argv))
if len(sys.argv) != 3:
    print "Usage: $python epm_training.py <trainfile.csv> <model_file.pkl>"
    sys.exit(-1)
else:
    train_file = sys.argv[1]
    fmod = sys.argv[2]

df = pd.read_csv(train_file, index_col=0).reset_index(drop=True)


target = df.outcome.copy()
target.loc[target != 'solved'] = 'failed'


train_data = df.drop(excled_cols + ['outcome'], axis=1)


train_data['cpt_fact_balance'] = train_data.h_ff_ratio.notnull().astype(int)
train_data['cpt_heuristics'] = train_data.Goal_count.notnull().astype(int)
train_data['cpt_landmarks'] = train_data.n_landmarks.notnull().astype(int)
train_data['cpt_redblack'] = train_data.blackVariables.notnull().astype(int)


raw_features = train_data[
    ftcols_baseft + 
    ftcols_ftcomputed + 
    ftcols_fact_balance + 
    ftcols_heuristics + 
    ftcols_redblack +
    ['planner']
]

fill_mean = raw_features.mean(numeric_only=True)
raw_features = raw_features.fillna(fill_mean)

features = pd.get_dummies(raw_features)


rforest = RandomForestClassifier(n_estimators=120, max_depth=17)
_ = rforest.fit(features, target)

model_data = {
    'model': rforest,
    'columns': features.columns,
    'planners': list(raw_features.planner.unique()),
    'fillna_avg': fill_mean
}


# Make sure pickle protocol is compatible with Python2.7

with open(fmod,'wb') as fo:
    pickle.dump(model_data, fo, 2)
print("Model saved in: ", fmod)
