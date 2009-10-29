# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Additional configurations
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
what <- "fracB";
evalSignal <- c("abs(fracB-1/2)", "minorCn", "majorCn")[1];

addLegend <- TRUE;
addSdEst <- FALSE;

addBinTrack <- TRUE;
doRocCurves <- TRUE;
plotAllRocCurves <- c(TRUE, FALSE)[2];
doFracB <- c(TRUE, FALSE)[1];
plotTracks <- c(TRUE, FALSE)[1];

## options for for plotsByState
addTrueBetaLines <- FALSE;
addLinearRegressionLines <- TRUE;
addTrueBetaPoints <- FALSE;
addDiagHorizLines <- TRUE;
kappasPbs <- c(0.55, 1);

## options for main
tumorPurify <- c(FALSE, TRUE)[1];
kappaMain <- 0.5;
if (!tumorPurify) {
   kappaMain <- 1;
};
useFixedNbrOfPoints <- TRUE;
fixedNbrOfPoints <- 100000; ## not used if useFixedNbrOfPoints
useFixedSeed <- FALSE; ## not implemented satisfactorily yet
seed <- 1; ## not used if !useFixedSeed

trackAspect <- 0.22;
trackWidth <- 0.9;

binCounts <- c(1, 1.25, 1.5, 2, 3, 4); rocCols <- 2;
binCounts <- binCounts[c(1, 4:6)];
## binCounts <- binCounts[union(1, length(binCounts))];

byCount <- c(TRUE, FALSE)[1];

fpLim <- c(0,0.5);
## fpLim <- c(0,1);

robust <- c(FALSE, TRUE)[1];
robustStr <- ifelse(robust, "median", "mean");
binFFracB <- ifelse(robust, "median", "mean"); 
confQuantile <- 0.9;

# Infer document tags
if (regexpr("ismpolish", dataSet) != -1) {
  docTags <- "ismpolish";
  genTags <- c("Birdseed", "NGC");
  rocCurvesPattern <- "^(raw|TCN),Birdseed$|^TBN";
} else if (regexpr("(ACC,ra,-XY,BPN,-XY,AVG,FLN,-XY|CRMAv2)", dataSet) != -1) {
  docTags <- "CRMAv2";
  genTags <- c("Birdseed", "NGC");
  rocCurvesPattern <- "^(raw|TCN),Birdseed$|^TBN";
} else if (regexpr("BeadStudio", dataSet) != -1) {
  docTags <- "BeadStudio";
  genTags <- c("NGC");
  rocCurvesPattern <- "^(raw|TCN),NGC$|^TBN";
} else {
  throw("Cannot infer ('docTags', 'genTags') from 'dataSet': ", dataSet);
}

genTag <- genTags[2];


docPath <- sprintf("doc-%s", docTags);
docPath <- Arguments$getWritablePath(docPath);
docName <- sprintf("main,%s", docTags);

pdfName <- sprintf("%s.pdf", docName);
pdfPathname <- filePath(docPath, pdfName);

figPath <- file.path(docPath, "figures", "col");
figForce <- 3;
figDev <- function(..., force=(figForce > 0)) { epsDev(..., path=figPath, force=force) }
figDev <- function(..., force=(figForce > 0)) { pngDev(..., device=png, path=figPath, force=force) }
