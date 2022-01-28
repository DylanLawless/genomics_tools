#!/bin/bash
#SBATCH --job-name=kpneuomonia
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=28
#SBATCH --time=01:00:00
#SBATCH --mem=60gb
#SBATCH -o ./log/3.2_kpneuomonia.%J.out
#SBATCH -e ./log/3.2_kpneuomonia.%J.err

echo started at `date`
processed=/work/gr-fe/lawless/spss_gwas/data/processed
kpneuomonia=/work/gr-fe/lawless/spss_gwas/data/processed/misc/kpneuomonia

# The CC study has shown an assocciation with the same locus in mice
# for infection to K.pneuomonia
# The pathogen in too infrequent in our SPSS study to expect sufficient power.
# However, we are interested to see if the OR increases with this pathogen
# compared to sepsis in general.

extract region of interest
9 111463-112074 kb

# checking first the cases only
plink \
	--bfile $processed/combined/geno/cohort.filt.group \
	--chr 9 --from-kb 111463 --to-kb 112074 \
	--make-bed \
	--out $kpneuomonia/chr9.111_112.casecontrol

plink \
	--bfile $processed/geno/SPSS.QC.impute2_plink2.allfiles.king_mind1_maf01_geno2.info8 \
	--chr 9 --from-kb 111463 --to-kb 112074 \
	--make-bed \
	--out $kpneuomonia/chr9.111_112.cases


# make covariates
pheno=/work/gr-fe/lawless/spss_gwas/data/processed/misc/kpneuomonia/phenotype_klebsiellaPn
pheno2=/work/gr-fe/lawless/spss_gwas/data/processed/misc/kpneuomonia/phenotype_klebsiellaPn.case_control
pheno3=/work/gr-fe/lawless/spss_gwas/data/processed/misc/kpneuomonia/phenotype_sepsis.case_control
covar=/work/gr-fe/lawless/spss_gwas/data/processed/grm/SPSS.QC.impute2_plink2.allfiles.king_mind1_maf01_geno2.info8.pca.eigenvec
covar2=$processed/combined/grm/cohort.filt.group.pca.eigenvec

# Kpneuomonia in sepsis cases only
plink \
	--bfile $kpneuomonia/chr9.111_112.cases \
	--logistic sex hide-covar \
	--ci 0.95 \
	--pheno $pheno \
	--allow-no-sex \
	--covar $covar --covar-number 3-8 \
	--out $kpneuomonia/chr9.111_112.kpneuomonia.cases

# Kpneuomonia including non-sepsis controls
plink \
	--bfile $kpneuomonia/chr9.111_112.casecontrol \
	--logistic sex hide-covar \
	--ci 0.95 \
	--pheno $pheno2 \
	--allow-no-sex \
	--covar $covar2 --covar-number 3-8 \
	--out $kpneuomonia/chr9.111_112.kpneuomonia.casecontrol

# MAIN
# Sepsis yes/no
plink \
	--bfile $kpneuomonia/chr9.111_112.casecontrol \
	--logistic sex hide-covar \
	--ci 0.95 \
	--pheno $pheno3 \
	--allow-no-sex \
	--covar $covar2 --covar-number 3-8 \
	--out $kpneuomonia/chr9.111_112.sepsis.casecontrol


# Kpneuomonia including non-sepsis controls
cd /work/gr-fe/lawless/spss_gwas/data/processed/misc/kpneuomonia
grep " 1$" phenotype_sepsis.case_control > phenotype_sepsis_control_only
grep " 2$" phenotype_klebsiellaPn > phenotype_klebsiellaPn_only
grep " 1$" phenotype_klebsiellaPn > phenotype_non_klebsiellaPn_only
sed 's/ 1$/ 2/g' phenotype_non_klebsiellaPn_only > phenotype_non_klebsiellaPn_only2

cat phenotype_sepsis_control_only phenotype_klebsiellaPn_only \
	> phenotype_klebsiellaPn_vs_control

cat phenotype_sepsis_control_only phenotype_non_klebsiellaPn_only2 \
	> phenotype_non_klebsiellaPn_vs_control

rm phenotype_sepsis_control_only phenotype_klebsiellaPn_only
rm phenotype_non_klebsiellaPn_only phenotype_non_klebsiellaPn_only2

pheno4=/work/gr-fe/lawless/spss_gwas/data/processed/misc/kpneuomonia/phenotype_klebsiellaPn_vs_control
pheno5=/work/gr-fe/lawless/spss_gwas/data/processed/misc/kpneuomonia/phenotype_non_klebsiellaPn_vs_control

# MAIN
# Kp versus healthy
plink \
	--bfile $kpneuomonia/chr9.111_112.casecontrol \
	--assoc \
	--ci 0.95 \
	--pheno $pheno4 \
	--allow-no-sex \
	--covar $covar2 --covar-number 3-8 \
	--out $kpneuomonia/chr9.111_112.kpneuomonia_vs_control

# non-Kp versus healthy
plink \
	--bfile $kpneuomonia/chr9.111_112.casecontrol \
	--assoc \
	--ci 0.95 \
	--pheno $pheno5 \
	--allow-no-sex \
	--covar $covar2 --covar-number 3-8 \
	--out $kpneuomonia/chr9.111_112.non_kpneuomonia_vs_control
echo ended at `date`



# MAIN redo
# Sepsis yes/no
plink \
	--bfile $kpneuomonia/chr9.111_112.casecontrol \
	--assoc \
	--ci 0.95 \
	--pheno $pheno3 \
	--allow-no-sex \
	--covar $covar2 --covar-number 3-8 \
	--out $kpneuomonia/chr9.111_112.sepsis.casecontrol

