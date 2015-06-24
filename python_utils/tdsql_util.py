#!/usr/local/bin/python

# usage of the class
# initialization(tdserver, username, password)
# run_sqlfile as a whole
# run_commands: run sql quries independently

class tdsql_util:
    def __init__(self, tdserver, username, password):
        self.username = username
        self.password = password
        self.tdserver = tdserver

    def __read_and_split_file__(self, infile):
        import re
        f = open(infile,'r')
        data = f.read()
        newdata = data.split(';')
        sql = []
        for line in newdata:
            line = re.sub('^--.*\n$','',line)
            line = re.sub('^[\n]*','',line)
            line = re.sub('\n$','',line)
            line = line.replace('\n', ' ')
            line = line.strip()
            if line != '':
                sql.append(line)
        f.close()
        return sql
    
    def run_sqlfile(self, sqlfile, nvp={}, ofile=''):
        import sys, commands as pcmd, re

        commands = self.__read_and_split_file__(sqlfile)
        all_cmd = ';'.join(commands)

        if ofile != '':
            base_cmd = 'tdsql -H '+self.tdserver+' -u '+self.username+' -p '+self.password+' -f csv -o '+ofile+' ';
        else:
            base_cmd = 'tdsql -H '+self.tdserver+' -u '+self.username+' -p '+self.password+' ';

        for (k,v) in nvp.items():
            s = '\$\{'+k+'\}'
            all_cmd = re.sub(s,v, all_cmd)
        print all_cmd
        newcommand = "echo \""+all_cmd+"\" | " + base_cmd
        ret = pcmd.getstatusoutput(newcommand)
        if ret[0]:
            print ret[1]
            raise Exception('tdsql error', all_cmd)

    def run_commands(self, sqlfile, nvp={}, ofile=''):
        import sys, commands as pcmd, re

        commands = self.__read_and_split_file__(sqlfile)

        if ofile != '':
            base_cmd = 'tdsql -H '+self.tdserver+' -u '+self.username+' -p '+self.password+' -f csv -o '+ofile+' ';
        else:
            base_cmd = 'tdsql -H '+self.tdserver+' -u '+self.username+' -p '+self.password+' ';

        for temp_cmd in commands :
            if not temp_cmd: continue
            #print temp_cmd
            for (k,v) in nvp.items():
                s = '\$\{'+k+'\}'
                temp_cmd = re.sub(s,v, temp_cmd)
            print temp_cmd
            newcommand = "echo \""+temp_cmd+"\" | " + base_cmd
            ret = pcmd.getstatusoutput(newcommand)
            if ret[0]:
                print ret[1]
                raise Exception('tdsql error', temp_cmd)

    def append_header(self, hfile, dfile):
        import commands, os, sys
        h = commands.getstatusoutput('cat '+hfile)
        cmd = 'sed -i ' + '\'1i ' +  h[1] + '\' ' + dfile
        os.system(cmd)

    def getinfo(self):
        print  self.username + ' ' + self.password

def main():
    import sys
    foo = tdsql_util('tdwb', 'bli', 'duke2008')
    #foo.run_commands('web_relevance_dash_intl_v1_coldstart.sql',nvp={'dk1':'20150512'},ofile='test.log')
    #foo.run_commands('web_relevance_dash_intl_v1.sql',nvp={'dk1':'20150505'},ofile='test.log')
    #foo.run_commands('web_relevance_dash_intl_v1.sql',nvp={'dk1':'20150506'},ofile='test.log')

if __name__ == "__main__":
    main()
