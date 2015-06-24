#!/usr/bin/env python
'''
Compare two predictions.
'''

import pandas as pd
import sys


if not sys.argv[2:]:
    print 'Error: Usage: {} <predict1.csv> <predict2.csv>'.format(sys.argv[0])
    sys.exit(1)


model1 = pd.read_csv(sys.argv[1], header=None, names=['enrollment_id', 'prob_dropout'])
model2 = pd.read_csv(sys.argv[2], header=None, names=['enrollment_id', 'prob_dropout'])

diff = model1 - model2

print diff.describe()
