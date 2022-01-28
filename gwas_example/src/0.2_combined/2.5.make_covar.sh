#!/bin/bash
#SBATCH --job-name=makeCovar
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=00:30:00
#SBATCH --mem=4gb
#SBATCH -o ./log/0.1_make_covar.%J.out
#SBATCH -e ./log/0.1_make_covar.%J.err

# This script will 
# [1] output sex and phenotype to separate files to use with gcta, 
# [2] covert the phenotype "1/2" to "0/1" for control/case, respectively, and 
# [3] write -9 to the sex/pheno columns in the plink.fam file. 

processed=/work/gr-fe/username/case_gwas/data/processed
bfile=$processed/combined/geno/cohort.filt.group

# prepared cohort dataset of case/controls

echo started at `date`
 cd $processed/combined/geno/

# print the $bfile.sex file
printf "Cut sex column from $bfile.fam > $bfile.sex\n"
printf "$ cat $bfile.fam | cut -d \" \" -f1,2,5 > $bfile.sex\n"
cat $bfile.fam | cut -d " " -f1,2,5 > $bfile.sex

# print the $bfile.pheno file
printf "\nCut phenotype column from $bfile.fam > $bfile.pheno\n"
printf "$ cat $bfile.fam | cut -d \" \" -f1,2,6 | sed 's/1$/0/g' | sed 's/2$/1/g' > $bfile.pheno\n"
cat $bfile.fam | cut -d " " -f1,2,6 | sed 's/1$/0/g' | sed 's/2$/1/g' > $bfile.pheno

# update the $bfile.fam to -9, keep an original copy
printf "\nThen update $bfile.fam to sex and pheno = -9 -9\n"
printf "$ mv $bfile.fam $bfile.fam_original\n"
mv $bfile.fam $bfile.fam_original

printf "$ cat $bfile.fam_original | sed 's/0 0 [1-2] [1-2]/0 0 -9 -9/g'  > $bfile.fam\n"
cat $bfile.fam_original | sed 's/0 0 [1-2] [1-2]/0 0 -9 -9/g'  > $bfile.fam

# print 8pcs to be used for qcovar
printf "\nprint 8pcs to be used for qcovar\n"
cat $processed/combined/grm/cohort.filt.group.pca.eigenvec | cut -f 1-10 -d ' ' > cohort.filt.group.pca.eigenvec.8pc
echo ended at `date`
