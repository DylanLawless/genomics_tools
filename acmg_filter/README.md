# To do 
In the project I will have:
1. input: annotated vcf
2. table of annotation databases: tool_used, database_name, URL, application (reference, population, prediction, etc.) - and then add a column for linking to acmg criteria.
3. table of acmg criteria
4. R function: for annotation


# Collabl testing
1. pick a random sequence containing ~100 coding variants in 1 sample. Use only the default columns (<https://gatk.broadinstitute.org/hc/en-us/articles/360035531692-VCF-Variant-Call-Format>) the first 7 fields (up to FILTER) are required by the VCF format and must be present, although they can be empty (in practice, there has to be a dot, ie . to serve as a placeholder).
* CHROM  POS ID  REF ALT     QUAL    FILTER  INFO    FORMAT  NA12878 [other samples...]
2. pick an annotation tool (VEP, dbNSFP)
3. list of all columns (databases that are added).
4. list of ACMG annotation rules and their respective column + R code



<https://clinicalgenome.org/working-groups/sequence-variant-interpretation/>
<h3>SVI General Recommendations for Using ACMG/AMP Criteria</h3>
<p>SVI provides general recommendations for using the ACMG/AMP criteria to improve consistency in usage and transparency in classification rationale.</p>
<div>
<ul>
	<li><a href="/docs/clingen-sequence-variant-interpretation-working-group-recommendations-for-acmg-amp-guideline-criteria-code-modifications/" target="_blank" rel="noreferrer noopener">Guidance on how to rename criteria codes when strength of evidence is modified</a></li>
	<li><a href="/docs/updated-recommendation-for-the-benign-stand-alone-acmg-amp-criterion/" target="_blank" rel="noreferrer noopener">BA1: Updated Recommendation for the ACMG/AMP Stand Alone Pathogenicity Criterion for Variant Classification</a>
	<ul>
		<li><a href="/docs/ba1-exception-list/" target="_blank" rel="noreferrer noopener">BA1 Exception List (July 2018)</a></li>
		<li><a href="https://tinyurl.com/ClinGen-BA1" target="_blank" rel="noreferrer noopener">BA1 Exception List Nomination Form</a></li>
	</ul>
	</li>
	<li><a href="/docs/recommendations-for-interpreting-the-loss-of-function-pvs1-acmg-amp-variant-criterion/" target="_blank" rel="noreferrer noopener">PVS1: Recommendations for Interpreting the Loss of Function PVS1 ACMG/AMP Variant Criteria</a> </li>
	<li><a href="/docs/ps2-pm6-recommendation-for-de-novo-ps2-and-pm6-acmg-amp-criteria-version-1.0/" target="_blank" rel="noreferrer noopener">PS2/PM6: Recommendation for de novo PS2 and PM6 ACMG/AMP criteria (Version 1.1)</a></li>
	<li><a href="/docs/recommendations-for-application-of-the-functional-evidence-ps3-bs3-criterion-using-the-acmg-amp-sequence-variant-interpretation/" target="_blank" rel="noreferrer noopener">PS3/BS3: Recommendations for application of the functional evidence PS3/BS3 criterion using the ACMG/AMP sequence variant interpretation framework</a></li>
	<li><a href="/docs/pm2-recommendation-for-absence-rarity/" target="_blank" rel="noreferrer noopener">PM2: Recommendation for Absence/Rarity Criterion PM2 (Version 1.0)</a></li>
	<li><a href="/docs/pm3-recommendation-for-in-trans-criterion-pm3-version-1.0/" target="_blank" rel="noreferrer noopener">PM3: Recommendation for in trans Criterion PM3 (Version 1.0)</a></li>
	<li><a href="/docs/the-acmg-amp-reputable-source-criteria-for-the-interpretation-of-sequence-variants/" target="_blank" rel="noreferrer noopener">PP5/BP6: Recommendation for reputable source PP5 and BP6 ACMG/AMP criteria</a></li>
</ul>
</div>

<https://www.genome.gov/sites/default/files/media/files/2019-03/ClinGen%20and%20ClinVar.pdf>

