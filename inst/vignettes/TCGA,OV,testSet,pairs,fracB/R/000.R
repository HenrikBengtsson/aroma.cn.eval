library("aroma.cn.eval");
library("xtable");
library("MASS");
library("RColorBrewer");

setOption(aromaSettings, "output/checksum", TRUE);
setOption(aromaSettings, "output/path", FALSE);
setOption(aromaSettings, "output/ram", FALSE);

pd <- packageDescription("aroma.core");
stopifnot(compareVersion(pd$Version, "1.1.2") >= 0);

# Get the command line arguments
args <- commandArgs(asValues=TRUE, excludeReserved=TRUE, excludeEnvVars=TRUE);
args <-args[-1];
cat("Command line arguments:\n");
print(args);
