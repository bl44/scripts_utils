import feature_extractor

import argparse
import csv
import functools
import sklearn
import sklearn.cross_validation
import sklearn.externals.joblib
import sklearn.linear_model
import sklearn.svm


'''
Usage:
$ python $0 -a fit
$ python $0 -a xval
$ python $0 -a predict -p ../data/enrollment_test.csv

$ python $0 -a fit -e ../data/enrollment_train_full.csv -l ../data/log_train_full.csv -t ../data/truth_train_full.csv -m sgdc_log.pkl  # noqa
$ python $0 -a xval -e ../data/enrollment_train_full.csv -l ../data/log_train_full.csv -t ../data/truth_train_full.csv
$ python $0 -a predict -p ../data/enrollment_test.csv -m sgdc_log.pkl
'''


class Model(object):
    def __init__(self, model_filename):
        self.model_filename = model_filename
        # self.clf = sklearn.svm.SVC(gamma=2, C=1, probability=True)
        # self.clf = sklearn.svm.SVC(kernel='linear', C=1)
        # self.clf = sklearn.linear_model.SGDClassifier(loss='log', verbose=1)
        self.clf = sklearn.linear_model.LogisticRegression(solver='liblinear', max_iter=1000, verbose=1)
        self.clf
        print self.clf

    def fit(self, x_train, y_train):
        self.clf.fit(x_train, y_train)
        # Also store normalizing factors
        self.clf.mu = x_train.mu
        self.clf.sig = x_train.sig
        sklearn.externals.joblib.dump(self.clf, self.model_filename)
        predicted = self.clf.predict(x_train)
        print sklearn.metrics.classification_report(y_train, predicted)

    def cross_validate(self, x_train, y_train, num_folds=3):
        skf = sklearn.cross_validation.StratifiedKFold(y_train, n_folds=num_folds)
        scores = sklearn.cross_validation.cross_val_score(
            self.clf, x_train, y_train, cv=skf)
        print("Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))

    def predict(self, x_test, outfile):
        clf = sklearn.externals.joblib.load(self.model_filename)
        x_test_norm = (x_test - clf.mu) / clf.sig
        y_test = clf.predict_proba(x_test_norm)
        with open(outfile, 'w') as fout:
            writer = csv.writer(fout)
            for ii, enrollment_id in enumerate(x_test.index):
                writer.writerow([enrollment_id, y_test[ii][1]])


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

    # Model related args
    parser.add_argument('-a', '--action', help='model action', choices=['fit', 'xval', 'predict'], required=True)
    parser.add_argument('-m', '--model-file', help='filename to store model', default='model.pkl')
    parser.add_argument('-p', '--predict-csv', help='CSV file to store model predictions', default=None)
    args = parser.parse_args()

    if args.action == 'predict':
        assert args.predict_csv, 'Need predict-csv to store predictions'
    return args


def main():
    args = _myargparse()
    is_normalize = args.action in ('fit', 'xval')
    feature_builder = feature_extractor.make_feature_builder(
        args.log_csv, args.object_csv, args.enrollment_csv, args.truth_train_csv, is_normalize)

    features = feature_builder.extract_features()
    print 'Features:'
    print features.head()
    print
    print feature_builder.labels.head()

    model = Model(args.model_file)
    if args.action == 'fit':
        model.fit(features, feature_builder.labels)
    elif args.action == 'xval':
        model.cross_validate(features, feature_builder.labels)
    else:
        model.predict(features, args.predict_csv)


if __name__ == '__main__':
    main()
