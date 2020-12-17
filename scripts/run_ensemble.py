#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  5 10:07:28 2020

@author: vxs914
"""

from mpi4py import MPI
import numpy as np
import pandas as pd
import os
import subprocess
import shutil
from manage_config import modify_scenario_file
from setup_experiment import factorial_design


def create_sandbox(name, outpath, workpath):
    # check if directory corresponding to this exists and if not creates
    sandbox = os.path.join(outpath, f'LANDIS-{name}/')
    if not os.path.isdir(sandbox):
        shutil.copytree(workpath, sandbox)

    return sandbox


# start MPI communicator
comm = MPI.COMM_WORLD
size = comm.Get_size()
rank = comm.Get_rank()

# set up experimental design factors
nlevels = [5, 4, 7, 3]

outpath = '/gpfs/group/kzk10/default/private/vxs914/landis-RDM'
workpath = '/storage/work/vxs914/LANDIS/repo'
fname_db = pd.read_csv('Filenames_Viff.csv')

if rank == 0:
    print(size)
    exp_design = factorial_design(nlevels)
    # split factor array for scattering
    exp_split = np.array_split(exp_design, size, axis=0)
    # get sizes of split buffers for sending
    split_sizes = np.zeros(size, dtype=np.int)
    for i in range(0, size, 1):
        split_sizes[i] = len(exp_split[i])
    scatter_sizes = split_sizes * len(nlevels)
    # displacements for each chunk
    displ = np.insert(np.cumsum(scatter_sizes), 0, 0)[0:-1]
else:
    # initialize variables on other workers
    exp_split = None

exp_split = comm.scatter(exp_split, root=0)
for lev in exp_split:
    if len(lev) != len(nlevels):
        raise ValueError('lev is the wrong size...')
    scenName = '-'.join(l for l in lev.astype(str))
    workdir = os.getcwd()
    sandbox = create_sandbox(scenName, outpath, workpath)
    os.chdir(sandbox)
    # modify scenario file in sandbox directory
    modify_scenario_file(lev, sandbox, fname_db)
    subprocess.call(['singularity', 'run', f'-B {sandbox}:/landis', '../landis.simg', 'Scenario_ViFF.txt'])
    os.chdir(workdir)
