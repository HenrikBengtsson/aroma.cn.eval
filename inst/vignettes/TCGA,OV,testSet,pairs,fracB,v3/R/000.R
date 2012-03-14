library("aroma.cn.eval");
library("xtable");
library("MASS");
library("RColorBrewer");

# Requires R.utils (>= 1.11.0) [for generating figures]
# source("http://www.braju.com/R/hbLite.R")
# installPackages("http://www.braju.com/R/repos/R.utils_1.11.0.tar.gz") 
stopifnot(!isOlderThan(R.utils, "1.11.0"));

# Get the command line arguments
args <- commandArgs(asValues=TRUE, excludeReserved=TRUE, excludeEnvVars=TRUE);
args <-args[-1];
cat("Command line arguments:\n");
print(args);

# aroma settings
setOption(aromaSettings, "output/checksum", TRUE);
setOption(aromaSettings, "output/path", FALSE);
setOption(aromaSettings, "output/ram", FALSE);
