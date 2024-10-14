#!/bin/bash

#$ -M netid@nd.edu   # Email address for job notification
#$ -m abe            # Send mail when job begins, ends and aborts
#$ -pe smp 24        # Specify parallel environment and legal core size
#$ -q long           # Specify queue
#$ -N job_name       # Specify job name

export OMP_NUM_THREADS=${NSLOTS}
module load python
module load conda
conda activate acms80870_parallel

cd ../
python ${1}.py
