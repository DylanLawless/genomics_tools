#!/bin/bash
for i in {1..10}; do 
   echo ">lcl|$i.5 matrix protein [Human respiratory syncytial virus]
METYVNKLHEGSTYTAAVQYNVLEKDDDPASLTIWVPMFQSSMPADLLIKELANVNILVK
QISTPKGPSLRVMINSRSAVLAQMPSKFTICANVSLDERSKLAYDVTTPCEIKACSLTCL
KSKNMLTTVKDLTMKTLNPTHDIIALCEFENIVTSKKVIIPTYLRSISVRNKDLNTLENI
TTTEFKNAITNAKIIPYSGLLLVITVTDNKGAFKYIKPQSQFIVDLGAYLEKESIYYVTT
NWKHTATRFAIKPMED"
done > ../data/rsv.msa

# some deletions
for i in {11..20}; do 
   echo ">lcl|$i.5 matrix protein [Human respiratory syncytial virus]
METYVNKLH---TYTAAVQYNVLEKDDDPASLTIWVPMFQSSMPADLLIKELANVNILVK
QISTPKGPSLRVMINSRSAVLAQMPSKFTICANVSLDERSKLAYDVTTPCEIKACSLTCL
KSKNMLTTVKDLTMKTLNPTHDIIALCEFENIVTSKKVIIPTYLRSISVRNKDLNTLENI
TTTEFKNAITNAKIIPYSGLLLVITVTDNKGAFKYIKPQSQFIVDLGAYLEKESIYYVTT
NWKHTATRFAIKPMED"
done >> ../data/rsv.msa

# some substitutions in positions 1-20
for i in {21..30}; do 
   echo ">lcl|$i.5 matrix protein [Human respiratory syncytial virus]
MLTYVAKLH---TYTARVQYKVLEKDDDPASLTIWVPMFQSSMPADLLIKELANVNILVK
QISTPKGPSLRVMINSRSAVLAQMPSKFTICANVSLDERSKLAYDVTTPCEIKACSLTCL
KSKNMLTTVKDLTMKTLNPTHDIIALCEFENIVTSKKVIIPTYLRSISVRNKDLNTLENI
TTTEFKNAITNAKIIPYSGLLLVITVTDNKGAFKYIKPQSQFIVDLGAYLEKESIYYVTT
NWKHTATRFAIKPMED"
done >> ../data/rsv.msa
