############################################################################
#
############################################################################
library("aroma.cn.eval");

setOption(aromaSettings, "output/checksum", TRUE);
setOption(aromaSettings, "output/path", FALSE);
setOption(aromaSettings, "output/ram", FALSE);


targetChipType <- chipType;



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Setting up data sets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if (!exists("dsList", mode="list")) {
  dsList <- loadAllDataSets(dataSet, chipType=chipType, pattern=sprintf("^%s,.*,pairs", dataSet));

  # Set the names
  names <- names(dsList);
  pattern <- sprintf("^%s(,HCC1143|),", dataSet)
  names <- gsub(pattern, "", names);

  # CRMA v1
  pattern <- "ACC,-XY,CRMAv1(,[-0-9]+|),(AVG|RMA),A[+]B,FLN,-XY,CRMAv1";
  gsub(pattern, "CRMAv1,\\2\\1", names);
  names <- gsub(pattern, "CRMAv1,\\2\\1", names);

  # CRMA v2
  pattern <- "ACC,(ra,|)-XY,BPN,-XY(,[-0-9]+|),(AVG|RMA),A[+]B,FLN,-XY";
  gsub(pattern, "CRMAv2,\\3\\2", names);
  names <- gsub(pattern, "CRMAv2,\\3\\2", names);

  pattern <- "ACC,-XY,v1,(AVG|RMA),A[+]B,FLN,-XY";
  names <- gsub(pattern, "CRMAv1,\\1", names);
  names <- gsub("GTC", "CN5", names);

  names <- sapply(names, FUN=strsplit, split=",", fixed=TRUE);
  commonTags <- Reduce(intersect, names);
  names <- lapply(names, FUN=setdiff, commonTags);
  names <- sapply(names, FUN=paste, collapse=",");

  names(dsList) <- names;
  o <- order(names(dsList));
  dsList <- dsList[o];
}


############################################################################
# HISTORY:
# 2009-04-07
# o Generalized to CRMAv1 and CRMAv2.
# 2009-02-23
# o Created.
############################################################################
