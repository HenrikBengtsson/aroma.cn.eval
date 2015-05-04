############################################################################
#
############################################################################
library("aroma.cn.eval");

setOption(aromaSettings, "output/checksum", TRUE);
setOption(aromaSettings, "output/path", FALSE);
setOption(aromaSettings, "output/ram", FALSE);


dataSet <- "GSE13372";
chipType <- "GenomeWideSNP_6";
targetChipType <- chipType;



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Setting up data sets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if (!exists("dsList", mode="list")) {
  dsList <- loadAllDataSets(dataSet, chipType=chipType, pattern=sprintf("^%s,.*,pairs", dataSet));

  # Set the names
  names <- names(dsList);
  names <- gsub("^GSE13372(,HCC1143|),", "", names);
  names <- gsub("ACC,(ra,|)-XY,BPN,-XY,(AVG|RMA),A[+]B,FLN,-XY", "CRMAv2,\\2", names);
  names <- gsub("ACC,-XY,v1,(AVG|RMA),A[+]B,FLN,-XY", "CRMAv1,\\1", names);
  names <- gsub("GTC", "CN5", names);
  names <- sapply(names, FUN=strsplit, split=",", fixed=TRUE);
  commonTags <- Reduce(intersect, names);
  names <- sapply(names, FUN=setdiff, commonTags);
  names <- sapply(names, FUN=paste, collapse=",");
  names(dsList) <- names;
}


############################################################################
# HISTORY:
# 2009-02-23
# o Created.
############################################################################
