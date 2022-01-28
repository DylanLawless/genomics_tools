
# Print P-values to file, skip header line
# cat dataset_1_2_3_merge_inspire.mind.filt_update.case_control.all_varaibles.loco.mlma \
# | awk '{print $9}' \
# | tail -n +2  > pvalues

# Test p-values 
# pvalue <- runif(1000000, min=0, max=1)

# Import pvalues
pvlaue <- read.table(
  file="./pvalues",
  na.strings=c("", "NA"), sep=" ", header=T)


chisq <- qchisq(1 - pvalue, 1)
lambda <- median(chisq) / qchisq(0.5, 1)
cat(lambda)

qchisq( 1-median(pvalue), df=1 )/0.4549

