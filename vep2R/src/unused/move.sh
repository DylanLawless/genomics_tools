
VEPdb=/work/gr-fe/databases/vep
base=/scratch/lawless

$base/ensembl-vep/vep 
$base/VEP_plugins
$base/VEP_cache/
$base/gnomAD_files/
$VEPdb/cache_b37


rsync -avz -P

