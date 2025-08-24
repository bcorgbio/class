#!/bin/tcsh
#SBATCH --job-name=trout # Job name
#SBATCH --ntasks 1 --cpus-per-task 64 # 1 cpu on single node
#SBATCH --mem-per-cpu=3gb # Job memory request
#SBATCH --time=48:00:00
#SBATCH --error=efin0.err
#SBATCH --output=ofin0.out
#SBATCH --mail-type=ALL # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=kenaley@bc.edu # Where to send mail

module load R/4.2.1gnu9.2.0

cd /mmfs1/data/kenaley

R -e 'setwd("/mmfs1/data/kenaley/trout")'
Rscript trout_kin_analyze_HPC.R

