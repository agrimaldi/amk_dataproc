#!/usr/bin/env python
# -*- coding:utf-8

import sys


class Session:
    def __init__(self, line, nbin):
        self.line = line
        self.nbin = nbin
        self.parseLine(line, nbin)

    def parseLine(self, line, nbin):
        '''Parse and sanitize a line read from a file
        (converting fields to integers if possible)
        '''
        columns = line.strip().split('\t')
        self.project, self.userid, self.protocol, self.session, \
                self.station, self.run, self.subject, self.rundate, \
                self.runtime = map(tryInt, columns[:9])
        session = map(tryInt, columns[9:])
        self.essays = {}
        for i, bin in enumerate(session):
            e = i / nbin
            if e in self.essays:
                self.essays[e].append(bin)
            else:
                self.essays[e] = [bin]

    def meansBin(self):
        means = []
        for i in range(self.nbin):
            means.append(mean([v[i] for v in self.essays.values()]))
        return means

    def meanActivity(self):
        meansBin = self.meansBin()
        return mean(meansBin)





def tryInt(value):
    try:
        return int(value)
    except:
        return value


def mean(values):
    return sum(values) / float(len(values))



def main():

    # Check the number of command line arguments
    if len(sys.argv) == 3:

        # Open the file in readonly mode
        # this returns a file object
        myfile = open(sys.argv[1], 'r')
	
	# Specify bin count / essay as a third argument
        nbin = int(sys.argv[2])
        
        # Iterate over each line of the given file object and create Session objects
        sessions = []
        for line in myfile:
            if not line.strip().startswith('Project'):
                sessions.append(Session(line, nbin))
        

    else:
        print "Nombre d'arguments insatisfaisant : ./parse.py <nom du fichier> <nombre de bins>"


if __name__ == '__main__':
    main()
