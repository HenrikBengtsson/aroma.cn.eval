library("aroma.cn.eval");
library("xtable");
library("MASS");
library("RColorBrewer");

# Get the command line arguments
args <- commandArgs(asValues=TRUE, excludeReserved=TRUE, excludeEnvVars=TRUE);
args <-args[-1];
cat("Command line arguments:\n");
print(args);

# aroma settings
setOption(aromaSettings, "output/checksum", TRUE);
setOption(aromaSettings, "output/path", FALSE);
setOption(aromaSettings, "output/ram", FALSE);
