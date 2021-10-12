# Variant testing
Example datasets for association testing.
There are many association tests that can be used.
Here, we want to think about the genetic data that we are likely to test.

Plink format is the most likely testing scenario since most genet datasets can be converted to plink format and used with other tools.
However, to understand the data we will hand code some example and "tidy" the data into formats which are likely to also mirror the plink genotype style. 
E.g. converting a multiple alignment sequence (MSA) into a genotype matrix.
Then we can run an association test on individual variants (SNP1 case vs control), or collapse variants to run one test on all variants (gene burden, case vs control).

Think about things like linear regression versus ANOVA.

## Case/control enrichment of variant
The toy_examlple:
* dataset - multiple alignment sequence of amino acids
* Logistic regression - on single variant or whole gene
* Fisher exact test
* ODDs ratio and confidence interval

