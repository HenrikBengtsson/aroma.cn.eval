# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rootPath <- "totalAndFracBData"
dataSet <- "TCGA,OV,testSet,pairs,Broad";
chipType <- "GenomeWideSNP_6";
targetChipType <- chipType;

genTags <- c("Birdseed", "NGC");
genTag <- genTags[1];

methodPattern <- "^(raw|TBN,v4)";
rocCurvesPattern <- "^(raw|TCN),Birdseed$|^TBN";

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Change-points of interest
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
regions <- c(
             "TCGA-04-1335:Chr9@0.0-120.0,cp=55.0+/-18.0,s=0/3",
             "TCGA-04-1336:Chr2@120.0-230.0,cp=176.0+/-2.0,s=0/2",
             "TCGA-23-1027:Chr2@108-140,cp=124+/-0.5,s=0/1",
             "TCGA-23-1027:Chr2@125.0-157.0,cp=141.0+/-0.5,s=1/3",
             "TCGA-23-1027:Chr2@25-75.0,cp=50.0+/-0.5,s=0/1", ## "FALSE BREAKPOINT"
             "TCGA-23-1027:Chr1@165-245.0,cp=201.0+/-0.5,s=0/1",
             "TCGA-23-1027:Chr1@0-120,cp=60+/-0.5,s=0/1",   ## CN LOH within a deletion (+more)
             "TCGA-23-1027:Chr10@80-109,cp=94+/-0.5,s=0/2", ## deletion
             "TCGA-23-1027:Chr10@106.5-113.5,cp=110+/-0.5,s=2/3" ## deletion -> CN LOH
             );

## regions <- regions[1:5]
regions <- regions[9]
## regions <- regions[c(3:4, 8:9)]
