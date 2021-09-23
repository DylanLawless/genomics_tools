## Filtering files based on VEP consequences.
<https://www.ensembl.org/info/docs/tools/vep/index.html>
Besides the use of "vep_filter" tool, I often like to filter in R.
During variant annotation, VEP supplies a "consequence" column. 
Consequences are general and based on translation of genetic code in humans.
The Loss-of-function (LoF) consequence is the simplest example (splice, stop mutations)
The variant consequence may be one of the defining criteria by which variants can be included in analysis since they are interpretable or of ostensibly known significance. Note: Using this alone could introduce spurious results so it is best to have a solid criteria for selecting consequences of interest. 
The consequences provided by VEP are too long to discuss in detail here. 
The table from the ensembl website is worth reading; the HIGH impact variants might be a simple method for selecting candidates: Ensembl Variation - Calculated variant consequences.
<https://grch37.ensembl.org/info/genome/variation/prediction/predicted_data.html#consequences>

The terms in the table linked above are shown in order of severity (more severe to less severe) as estimated by Ensembl.
In single case studies, we often find rare disease due to variants of "IMPACT" that is "HIGH or MODERATE". 
However, there may be a bias in that these variants are often most likely to be functionally interpretable.
Variants that are more difficult to predict function might only be detected in large-scale association studies where a is statistically valid explanation for correlation/cause is possible.
Other tools may provide alternative estimates that the ones shown here from VEP.
Missense variants may have further annotation on their effect on the protein function, using a number of algorithms (see VEP Ensembl Variation - Calculated variant consequences
<https://m.ensembl.org/info/genome/variation/prediction/predicted_data.html>).
