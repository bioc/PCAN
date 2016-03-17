rm(list=ls())
gc()

#############################
load("curOptions.rda")
toLoad <- list.files(
  path=tmpDir,
  pattern="[.]rda$",
  full.names=T
)
toSave <- c(
  "geneByTrait", "traitDef", "hpByTrait",
  "cv.rcvaSubmitters"
)
toLoad <- setdiff(
  toLoad,
  file.path(tmpDir, paste(toSave, ".rda", sep=""))
)
for(f in toLoad){
  load(f)
}

#######################
## Gene to disease DB
geneByTrait <- unique(merge(
  cv.varEntrez,
  cv.rcvaVariant,
  by="varId",
  all=F
)[,c("entrez", "rcvaId")])
geneByTrait <- merge(
  geneByTrait,
  cv.rcvaTraits,
  by="rcvaId",
  all=F
)
geneByTrait <- merge(
  geneByTrait,
  cv.traitCref,
  by="t.id",
  all=F
)[, c("t.id", "entrez", "id", "db", "rcvaId")]
dbOi <- c(
  "MedGen",
  "Orphanet",
  "OMIM",
  "Human Phenotype Ontology"
)
geneByTrait <- unique(geneByTrait[which(geneByTrait$db %in% dbOi),])

#######################
## MedGen
## Names
mgid <- unique(geneByTrait$id[which(geneByTrait$db=="MedGen")])
mgDis <- mg.concepts[which(mg.concepts$CUI %in% mgid), c("CUI", "name")]
colnames(mgDis) <- c("id", "name")
toAdd <- setdiff(mgid, mgDis$id)
toAdd <- unique(merge(
  unique(geneByTrait[
    which(geneByTrait$id %in% toAdd & geneByTrait$db=="MedGen"),
    c("t.id", "id")
    ]),
  cv.traits,
  by.x="t.id",
  by.y="id",
  all.x=T, all.y=F
)[, c("id", "name")])
mgDis <- rbind(mgDis, toAdd)
mgDis$db <- "MedGen"
rownames(mgDis) <- c()

## MedGen <- HP
hpoCuiByCui <- unique(mg.omim[
  which(mg.omim$CUI %in% mgDis$id & mg.omim$relationship != "not_manifestation_of"),
  c("CUI", "HPO.CUI")
  ])
hpoCuiByCui <- unstack(hpoCuiByCui, HPO.CUI~CUI)
hpByCui <- lapply(
  hpoCuiByCui,
  function(hpoCui){
    unique(mg.hpo$SDUI[which(mg.hpo$CUI %in% hpoCui)])
  }
)
hpByCui <- lapply(
  hpByCui,
  function(x){
    unique(hp.altid[which(hp.altid$alt %in% x), "id"])
  }
)
hpByMedGen <- stack(hpByCui)
hpByMedGen$values <- as.character(hpByMedGen$values)
hpByMedGen$ind <- as.character(hpByMedGen$ind)
colnames(hpByMedGen) <- c("hp", "id")
hpByMedGen$db <- "MedGen"

#######################
## Orphanet
orphid <- unique(geneByTrait$id[which(geneByTrait$db=="Orphanet")])
orphDis <- on.disorders[which(on.disorders$id %in% orphid), c("id", "name")]
toAdd <- setdiff(orphid, orphDis$id)
toAdd <- unique(merge(
  unique(geneByTrait[
    which(geneByTrait$id %in% toAdd & geneByTrait$db=="Orphanet"),
    c("t.id", "id")
    ]),
  cv.traits,
  by.x="t.id",
  by.y="id",
  all.x=T, all.y=F
)[, c("id", "name")])
orphDis <- rbind(orphDis, toAdd)

## Orphanet <- HP
hpByOrphId <- hp.diseases[
  which(hp.diseases$Source=="ORPHANET"),
  c("HP", "Phenotype ID")
  ]
colnames(hpByOrphId) <- c("hp", "id")
hpByOrphId$db <- "Orphanet"

## Orph dis to add
toAdd <- unique(hp.diseases[
  which(
    hp.diseases$"Phenotype ID" %in% setdiff(hpByOrphId$id, orphDis$id) &
    hp.diseases$Source=="ORPHANET"
  ),
  c("Phenotype ID", "Phenotype")
])
colnames(toAdd) <- c("id", "name")
orphDis <- rbind(orphDis, toAdd)
orphDis$db <- "Orphanet"
rownames(orphDis) <- c()

#######################
## OMIM
omimId <- unique(geneByTrait$id[which(geneByTrait$db=="OMIM")])
omimDis <- unique(cv.diseaseNames[
  which(cv.diseaseNames$MIM %in% omimId),
  c("MIM", "name")
  ])
colnames(omimDis) <- c("id", "name")
omimDis <- omimDis[which(!duplicated(omimDis$id)),]
toAdd <- setdiff(omimId, omimDis$id)
toAdd <- unique(merge(
  unique(geneByTrait[
    which(geneByTrait$id %in% toAdd & geneByTrait$db=="OMIM"),
    c("t.id", "id")
    ]),
  cv.traits,
  by.x="t.id",
  by.y="id",
  all.x=T, all.y=F
)[, c("id", "name")])
toAdd <- toAdd[which(!duplicated(toAdd$id)),]
omimDis <- rbind(omimDis, toAdd)

## OMIM <- HP
hpByOmimId <- hp.diseases[
  which(hp.diseases$Source=="OMIM"),
  c("HP", "Phenotype ID")
  ]
colnames(hpByOmimId) <- c("hp", "id")
hpByOmimId$db <- "OMIM"

## OMIM dis to add
toAdd <- unique(hp.diseases[
  which(
    hp.diseases$"Phenotype ID" %in% setdiff(hpByOmimId$id, omimDis$id) &
    hp.diseases$Source=="OMIM"
  ),
  c("Phenotype ID", "Phenotype")
  ])
colnames(toAdd) <- c("id", "name")
omimDis <- rbind(omimDis, toAdd)
omimDis$db <- "OMIM"
rownames(omimDis) <- c()

#######################
## Human Phenotype Ontology
## Converting id in canonical ids
gToHp <- geneByTrait[which(geneByTrait$db=="Human Phenotype Ontology"),]
gToHpConv <- unique(do.call(rbind, apply(
  gToHp, 1,
  function(x){
    canId <- hp.altid[which(hp.altid$alt==x[3]),"id"]
    return(data.frame(
      t.id=rep(x[1], length(canId)),
      entrez=rep(x[2], length(canId)),
      id=canId,
      db=rep("Human Phenotype Ontology", length(canId)),
      rcvaId=rep(x[5], length(canId)),
      stringsAsFactors=F
    ))
  }
)))
geneByTrait <- rbind(
  geneByTrait[which(geneByTrait$db != "Human Phenotype Ontology"),],
  gToHpConv
)
hpId <- unique(geneByTrait$id[which(geneByTrait$db=="Human Phenotype Ontology")])
hpTraits <- hp.definitions[which(hp.definitions$id %in% hpId), c("id", "name")]
hpTraits$db <- "Human Phenotype Ontology"
hpByHp <- data.frame(
  hp=hpTraits$id,
  id=hpTraits$id,
  db="Human Phenotype Ontology",
  stringsAsFactors=F
)

#######################
## Gathering information from the different DB
traitDef <- rbind(
  mgDis,
  orphDis,
  omimDis,
  hpTraits
)
traitDef <- traitDef[, c("db", "id", "name")]
hpByTrait <- rbind(
  hpByMedGen,
  hpByOrphId,
  hpByOmimId,
  hpByHp
)
hpByTrait <- hpByTrait[, c("hp", "db", "id")]

cv.rcvaSubmitters <- merge(
  cv.ReferenceClinVarAssertion,
  cv.ClinVarAssertions,
  by="cvs",
  all=F
)[, c("id.x", "id.y")]
cv.rcvaSubmitters <- unique(merge(
  cv.rcvaSubmitters,
  cv.cvaSubmitters,
  by.x="id.y",
  by.y="cvaId"
)[, c("id.x", "submitter")])
names(cv.rcvaSubmitters) <- c("rcvaId", "submitter")

geneByTrait <- unique(geneByTrait[, c("entrez", "db", "id", "rcvaId")])

############################
for(f in toSave){
  message(paste("Saving", f))
  ## Ensure unicity
  assign(f, get(f))
  if(length(names(f))==0){
      f <- unique(f)
  }
  ##
  save(
    list=f,
    file=file.path(tmpDir, paste(f, ".rda", sep=""))
  )
}
