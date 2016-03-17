##########################
## Filter gene/trait association according to variant origin and clinical status
geneByTrait <- filterGeneByTrait()
head(geneByTrait)
## Get gene/HP association according to gene/trait and trait/HP association
## The filtering options are the same than for filterGeneByTrait().
hpByGene <- getHpByGene()
head(hpByGene)

