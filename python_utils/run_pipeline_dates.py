#!/usr/local/bin/python
# This is forked from conorc/analysis
# create a tmp sql file and run with bteq
# - input from command line
# python run_pipeline_dates.py <sql_file> <{fill, offset}> [{-d <datekey, ...>, -r <d1 d2>}]

from datetime import date, timedelta
import sys
import argparse
import string
import subprocess
import glob
import os
import tempfile

path = sys.path[0]+"/" # directory from which the script was launched
sql_scripts = glob.glob(path+'*.sql') # gets the directory of all sql files in the directory
if len(sql_scripts) == 0:
    default_script = 'null'
elif len(sql_scripts) > 1:
    print "Warning: multiple sql files found. Script defaulting to %s" % sql_scripts[0]
    default_script = sql_scripts[0]
else:
    default_script = sql_scripts[0] # gets the first sql file in the list

today = date.today()
run_date_list = [] # empty list of date_keys to run 

# function to convert date keys to dates
def sdate(dkey):
    sdkey = str(dkey)
    sdate_out = date(year=int(sdkey[0:4]),month=int(sdkey[4:6]),day=int(sdkey[6:8]))
    return sdate_out

# get optional arguments from command line
# sub-command functions
def offset(args):
    rundates = []
    if args.date_offset: 
        for delta in args.date_offset:
            run = today - timedelta(days=delta)
            rundates.append(run)
    elif args.range: 
        # convert the delta arguments to two seperate variables
        rdelta1 = args.range[0]
        rdelta2 = args.range[1]

        # generate list of dates between two supplied offsets. if statement to accept dates in whatever order
        if rdelta1 > rdelta2:
            rund = [today - timedelta(days=rdelta1) + timedelta(days=x) for x in range(0,(rdelta1-rdelta2) + 1)]
        elif rdelta1 < rdelta2:
            run = [today - timedelta(days=rdelta2) + timedelta(days=x) for x in range(0,(rdelta2-rdelta1) + 1)]
        else:
            sys.exit('ERROR: Enter two different offsets')
        rundates.extend(run)     
    else:
        run = today - timedelta(days=1)
        rundates.append(run)
        print "no arguments supplied, defaulting to %s" % run
    return rundates

# sub-parser: fill
def fill(args):
    rundates = []
    if args.date_key:
        for dkey in args.date_key:
            rund = sdate(dkey)
            rundates.append(rund)
    elif args.range:
        # convert the two date key arguments into dates
        rdate1 = sdate(args.range[0])
        rdate2 = sdate(args.range[1])
        # genrate list of dates between the two supplied dates. if statement to accept dates in whatever order
        if rdate1 > rdate2:
            run = [rdate2 + timedelta(days=x) for x in range((rdate1-rdate2).days + 1)]
        elif rdate1 < rdate2:
            run = [rdate1 + timedelta(days=x) for x in range((rdate2-rdate1).days + 1)]
        else:
            sys.exit('ERROR: Enter two differente dates')
        rundates.extend(run)
    else:
        run = today - timedelta(days=1)
        rundates.append(run)
        print "no arguments supplied, defaulting to %s" % run
    return rundates

# create top level-parser
parser = argparse.ArgumentParser('default')
parser.add_argument('dir', default = default_script,
                    help='Directory to sql file. Optional, defaults to python script directory')
subparsers = parser.add_subparsers(title='subcommands',help='-d: <date_key> or <date_offset> int; -r --range datekey1 datakey2 or int1 int2')
# subparsers.required = False

# create offset subparser
offset_parser = subparsers.add_parser('offset')
offset_group = offset_parser.add_mutually_exclusive_group()
# action defaults to store which simply stores the argument's value
offset_group.add_argument('-d','--date_offset', type=int, nargs='+',
                    help='Argument is what is subracted from current date for the run. Defaults to 1.') 
offset_group.add_argument('-r','--range', type=int, nargs=2,
                    help='Optional argument that will create a range of dates to run. Takes two ints subtracted from current date')
offset_group.set_defaults(func=offset)


# create fill subparser
fill_parser = subparsers.add_parser('fill')
fill_group = fill_parser.add_mutually_exclusive_group()
fill_group.add_argument('-d','--date_key', type=int, nargs='+',
                    help='Argument is the day_key to insert into selected table') 
fill_group.add_argument('-r','--range', type=int, nargs=2,
                    help='Optional argument that will create a range of dates to run. Argument is the start and end date of range to insert')
fill_group.set_defaults(func=fill)

args = parser.parse_args()
rdates = args.func(args) 
query_path = args.dir

# check to see if the query path is valid
if not os.path.exists(query_path):
    sys.exit('ERROR: No valid sql files found')

# create format rdates as date_keys and append them to run_date_list
for d in rdates:
    ds = d.strftime('%Y%m%d') # in python dates are treated as objects. This converts today (date) into a date string to insert into the run_dates_str list
    run_date_list.append(ds)

# get tdsql username and login from bteqlogon
fo = open('/home/bli/.bteqTDWC.txt','r')
logon = fo.read()
fo.close()

# open the sql query for reading (if you change the option to w it will delete the contents)
fo = open(query_path, 'r')
query = fo.read()
fo.close() # close the file

query_logon = logon+query

query_logon_t = string.Template(query_logon)

for d in run_date_list:
    query_logon_s = query_logon_t.substitute(datereplace=d)
    with tempfile.NamedTemporaryFile(suffix="btq") as tmp:
        f = open(tmp.name, 'w')
        f.write(query_logon_s)
        f.close()
        subprocess.check_call("bteq < "+tmp.name, shell = True)
