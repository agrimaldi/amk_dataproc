#!/usr/bin/env python
# -*- coding:utf-8

import sys


def tryInt(value):
    try:
        return int(value)
    except:
        return value


def parseLine(line):
    '''Parse and sanitize a line read from a file
    (converting fields to integers if possible)
    '''
    output = []
    columns = line.strip().split('\t')
    for column in columns:
        output.append(tryInt(column))
    return output


def mean(values):
    return sum(values) / float(len(values))


def meanSession(line):
    '''Computes the mean of the bins of each trials for the mean_session
    '''
    # Recuperation des valeurs "bin"
    session = line[9:]
    # Initialize dont la longueur = nombres d'essais
    essays = {}
    means = []
    # Assigne un bin à l'essai correspondant (un essai = 60 bins)
    # dans un dictionnaire (structure de donnee 'cle : valeur')
    # A la fin,
    # essays = {
    #    1: [0, 3, 0, 2, 2, 1, 0, ...],
    #    2: [0,02, 2, 1, 1, 0, 3, ...],
    #    3: ...
    # }
    for i, bin in enumerate(session):
        e = i / 60
        if e in essays:
            essays[e].append(bin)
        else:
            essays[e] = [bin]
    for i in range(60):
        means.append(mean([v[i] for v in essays.values()]))
    return means


def main():

    # Check the number of command line arguments
    if len(sys.argv) == 2:

        # Open the file in readonly mode
        # this returns a file object
        myfile = open(sys.argv[1], 'r')

        # Iterate over each line of the given file object
        for line in myfile:
            columns = parseLine(line)
            if columns[0] != 'Project':
                mean_session = meanSession(columns)
                print mean_session

    else:
        print "Nombre d'arguments insatisfaisant : ./parse.py <nom du fichier>"


if __name__ == '__main__':
    main()
