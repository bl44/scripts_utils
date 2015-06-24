import sys, csv
import pandas as pd
import numpy as np
import features

feature_names = ['event_count' ]

class FeatureBuilder:
    def __init__(self, logdata, objectdata, enrollmentdata):
        self.logdata = logdata
        self.objectdata = objectdata
        self.enrollmentdata = enrollmentdata
 
    def event_count(self):
        return self.logdata.groupby('enrollment_id')['event'].count()

    def extract_features(self):
        feat = pd.DataFrame(index=self.enrollmentdata.enrollment_id)

        for fname in feature_names:
            print fname
            d = getattr(self, fname)()
            d.name = fname
            feat = feat.join(d)
        return feat

if __name__ == "__main__":
    log_train = pd.read_csv('../data/log_train.csv')
    object = pd.read_csv('../data/object.csv')
    enrollment_train = pd.read_csv('../data/enrollment_train.csv')
    builder = FeatureBuilder(log_train, object, enrollment_train)
    features = builder.extract_features()
    print features.head()
