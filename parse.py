#!/usr/bin/env python
# -*- coding:utf-8

import sys
import numpy


class Session:
    def __init__(self, line, nbin):
        self.line = line
        self.nbin = nbin
        self.parseLine(line, nbin)

    @property
    def metadata(self):
        return [
            self.project, self.userid, self.protocol, self.session,
            self.station, self.run, self.subject, self.rundate, self.runtime   
        ]

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

    def meansBin(self, factor=1):
        means = []
        for i in range(self.nbin):
            means.append(numpy.mean([v[i] for v in self.essays.values()]) / factor)
        return means

    def meanActivity(self, factor=1):
        return numpy.mean(self.meansBin(factor))




def tryInt(value):
    try:
        return int(value)
    except:
        return value



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
        
        for session in sessions:
            output = session.metadata + session.meansBin + [session.meanActivity]
            print '\t'.join(map(str, output))

    else:
        print "Nombre d'arguments insatisfaisant : ./parse.py <nom du fichier> <nombre de bins>"


if __name__ == '__main__':
    main()
