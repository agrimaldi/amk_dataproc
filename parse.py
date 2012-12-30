#!/usr/bin/env python
# -*- coding:utf-8

import sys


class Session:
    def __init__(self, line, nbin):
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




def tryInt(value):
    try:
        return int(value)
    except:
        return value


def mean(values):
    return sum(values) / float(len(values))


def space(line):
	av = mean(line[9:])
	return av


def meanSession(line, nbin=60):
    '''Computes the mean of the bins of each trials for the mean_session
    '''
    # Recuperation des valeurs "bin"
    session = line[9:]
    # Initialize dont la longueur = nombres d'essais
    essays = {}
    means = []
    #Donne le numero du sujet
    rat = line[6]
    # Assigne un bin à l'essai correspondant (un essai = 60 bins)
    # dans un dictionnaire (structure de donnee 'cle : valeur')
    # A la fin,
    # essays = {
    #    1: [0, 3, 0, 2, 2, 1, 0, ...],
    #    2: [0,02, 2, 1, 1, 0, 3, ...],
    #    3: ...
    # }
    for i, bin in enumerate(session):
        e = i / nbin
        if e in essays:
            essays[e].append(bin)
        else:
            essays[e] = [bin]
    for i in range(nbin):
        means.append(mean([v[i] for v in essays.values()]))
    return [rat,means]


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
