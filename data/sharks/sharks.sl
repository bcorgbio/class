#!/bin/tcsh
#SBATCH --job-name=shark_hOUwie #name
#SBATCH --nodes=8
#SBATCH --ntasks=50
#SBATCH --ntasks-per-core=1
#SBATCH --tasks-per-node=10
#SBATCH --mem-per-cpu=10mb # Job memory request
#SBATCH --time=120:00:00
#SBATCH --error=efin0.err
#SBATCH --output=ofin0.out
#SBATCH --mail-type=ALL # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=kenaley@bc.edu # Where to send mail
#SBATCH --partition=exclusive #(for heavy job computation)


module load R/4.2.1gnu9.2.0

cd /mmfs1/data/kenaley/sharks

R -e 'setwd("/mmfs1/data/kenaley/sharks")'
Rscript sharks_HPC.R

