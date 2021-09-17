#!/bin/bash
#SBATCH --job-name=mcf_stacks
#SBATCH --account=merlab
#SBATCH --partition=compute-hugemem
#SBATCH --nodes=1
## Walltime (days-hours:minutes:seconds format)
#SBATCH --time=3-12:00:00
## Memory per node
#SBATCH --mem=100G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=mfisher5@uw.edu


## ENVIRONMENT SETUP

DATADIR=/gscratch/merlab/mfisher5/stacks


## CODE FOR JOB

populations -b 7 -P stacks_b8_rerun -M PopMap_L1-5.txt -t 36 -r 0.80 -p 4 -m 10 --write_random_snp --genepop --fasta --vcf