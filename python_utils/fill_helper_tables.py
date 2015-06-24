#!/usr/local/bin/python
# this script is used to update helper tables:
# bcookie_experiment, funnels
# this script is for running the impressions funnel
# which extracts interaction data directly from NST logs

import os,datetime
from sys import argv
from time import time
import subprocess
user=subprocess.check_output("whoami").strip()

pipeline={}
pipeline['fill_bcookie_experiment']=False
pipeline['fill_funnels']=False

action,country_code,t1,t2=None,None,None,None
if len(argv)>4:
    action,country_code,t1,t2=argv[1],argv[2],argv[3],argv[4]
    year,month,day=t1.split('-')
    t1=datetime.date(int(year),int(month),int(day))
    year,month,day=t2.split('-')
    t2=datetime.date(int(year),int(month),int(day))
elif len(argv)==3:
    action=argv[1]
    country_code=argv[2]
    if action=='EXTRACT_YESTERDAY':
        t1=datetime.date.today()-datetime.timedelta(days=1)
        t2=t1
else:
    print 'argument is not sufficent:action, country_code, [t1,t2]'

if action=="FILL_BCOOKIE_EXPERIMENT":
    pipeline['fill_bcookie_experiment']=True
if action=="FILL_FUNNELS":
    pipeline['fill_funnels']=True
if action=='FILL_ALL':
    pipeline['fill_bcookie_experiment']=True
    pipeline['fill_funnels']=True

if pipeline['fill_bcookie_experiment']:
    ds_list=[(t1+datetime.timedelta(days=i)).strftime('%Y%m%d') for i in range(0,(t2-t1).days+1)]
    ds_list.reverse()
    for ds in ds_list: 
        print("hive -S -f /home/"+user+"/tmp/fill_bcookie_experiment.hql -hiveconf date_replace="+ds+" -hiveconf country_replace='"+country_code+"'")
        #os.system("hive -S -f /home/"+user+"/tmp/fill_bcookie_experiment.hql -hiveconf date_replace="+ds+" -hiveconf country_replace='"+country_code+"'") 

if pipeline['fill_funnels']:
    ds_list=[(t1+datetime.timedelta(days=i)).strftime('%Y%m%d') for i in range(0,(t2-t1).days+1)]
    ds_list.reverse()
    for ds in ds_list:
        print("hive -S -f /home/"+user+"/tmp/fill_all_impression_funnels.hql -hiveconf date_replace="+ds+" -hiveconf country_replace='"+country_code+"'")
       # os.system("hive -S -f /home/"+user+"/tmp/fill_all_impression_funnels.hql -hiveconf date_replace="+ds+" -hiveconf country_replace='"+country_code+"'")
