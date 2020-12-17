#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  5 09:49:48 2020

@author: vxs914
"""

import os

def modify_scenario_file(levels, sandbox, fname_db):    
    # levels refer to the experimental factors for this scenario

    scencols = ['FileName', 'Wind', 'Harvest']
    scenfname_db = fname_db.loc[fname_db['Type'] == 'Scenario', scencols]
    wind_bool = levels[1] > 0
    scenfname = scenfname_db[(scenfname_db.Harvest == levels[0]) & (scenfname_db.Wind == wind_bool)].FileName.values[0]
    climcols = ['FileName', 'Climate', 'GDD']
    climfname_db = fname_db.loc[fname_db['Type'] == 'NECN', climcols]
    climfname = climfname_db[(climfname_db.Climate == levels[2]) & (climfname_db.GDD == levels[3])].FileName.values[0]
    if wind_bool:
        windcols = ['FileName', 'Wind']
        windfname_db = fname_db.loc[fname_db['Type'] == 'Wind', windcols]
        windfname = windfname_db[windfname_db.Wind == levels[1]].FileName.values[0]

    with open(scenfname, 'r') as f:
        with open(os.path.join(sandbox, 'Scenario_ViFF.txt'), 'w') as new_file:
            for line in f:
                llist = [elt.strip() for elt in line.split(' ')]
                if llist[0] == '"NECN':
                    llist[5] = climfname
                if llist[0] == '"Base':
                    llist[3] = windfname
                new_file.write(' '.join(llist) + '\n')
