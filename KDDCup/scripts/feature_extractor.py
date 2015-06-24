'''
In the training phase, mu and sigma used to normalize the features are added as
attributes to the feature vector that is returned.
feat.mu is a pd.Series as is feat.sig
'''

import argparse
import functools
import pandas as pd
# import numpy as np


FEATURE_NAMES = [
    'event_count',
    'total_event_count',
]


class FeatureBuilder:
    def __init__(self, logdata, objectdata, enrollmentdata, truthdata, is_normalize):
        '''
        is_normalize: set it to True for training and False for predicting/testing
        '''
        self.logdata = logdata
        self.objectdata = objectdata
        self.enrollmentdata = enrollmentdata
        self.truthdata = truthdata
        self.is_normalize = is_normalize

    @property
    def labels(self):
        return self.truthdata['is_dropout']

    def _normalize(self, df):
        '''
        Returns a new normalized df.
        Either use the passed in self.mu & self.sig to normalize or compute from the training set.
        If training set, also add mu and sig as attributes to returned df.
        '''
        if self.is_normalize:
            print 'Normalizing feature'
            mu = df.mean()
            sig = df.std()

        df = (df - mu) / sig

        # add mu and sig attributes to df
        if self.is_normalize:
            df.mu = mu
            df.sig = sig

        return df

    def event_count(self):
        # @todo: normalize
        df1 = pd.DataFrame({'count': self.logdata.groupby(['enrollment_id', 'event']).size()}).reset_index()
        df2 = pd.pivot_table(df1, columns=['event'], values='count', index=['enrollment_id'], fill_value=0)
        return self._normalize(df2) if self.is_normalize else df2

    def total_event_count(self):
        # return self.logdata.groupby('enrollment_id')['event'].count()
        # @todo: normalize
        df = pd.DataFrame({'total_event_count': self.logdata.groupby('enrollment_id').size()})
        return self._normalize(df) if self.is_normalize else df

    def extract_features(self):
        feat = pd.DataFrame(index=self.enrollmentdata.enrollment_id)
        mu_arr = list()
        sig_arr = list()

        for fname in FEATURE_NAMES:
            print 'Processing feature(s): ', fname
            df = getattr(self, fname)()
            df.name = fname
            feat = feat.join(df)
            if hasattr(df, 'mu'):
                # train
                mu_arr.append(df.mu)
                sig_arr.append(df.sig)

        if mu_arr:
            # train
            feat.mu = pd.concat(mu_arr)
            feat.sig = pd.concat(sig_arr)
            print 'Normalization values:'
            print '**** mu:\n{}\n**** sig:\n{}\n'.format(feat.mu, feat.sig)

        return feat


def _myargparse():
    help_formatter = functools.partial(argparse.ArgumentDefaultsHelpFormatter, width=104)
    parser = argparse.ArgumentParser(
        description='Feature Extractor',
        formatter_class=help_formatter)
    parser.add_argument('-l', '--log-csv', help='CSV file of log of events',
                        default='../data/log_train.csv')
    parser.add_argument('-o', '--object-csv', help='CSV file of module/object data',
                        default='../data/object.csv')
    parser.add_argument('-e', '--enrollment-csv', help='CSV file of enrollment data',
                        default='../data/enrollment_train.csv')
    parser.add_argument('-t', '--truth-train-csv', help='CSV file of dropout label',
                        default='../data/truth_train.csv')

    args = parser.parse_args()
    return args


def make_feature_builder(log_csv, object_csv, enrollment_csv, truth_train_csv, is_normalize):
    log_train = pd.read_csv(log_csv)
    object_ = pd.read_csv(object_csv)
    enrollment_train = pd.read_csv(enrollment_csv)
    truth_train = None
    if truth_train_csv:
        truth_train = pd.read_csv(truth_train_csv, names=['enrollment_id', 'is_dropout'])
    builder = FeatureBuilder(log_train, object_, enrollment_train, truth_train, is_normalize)
    return builder


def main():
    args = _myargparse()
    return make_feature_builder(args.log_csv, args.object_csv, args.enrollment_csv, args.truth_train_csv)


if __name__ == "__main__":
    main()
