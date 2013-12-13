reg <- parseRegion("TCGA-23-1027:Chr2@108-140,cp=124+/-0.5,s=0/1")
str(reg)

stateFcn <- makeTruth(reg)
print(reg)

data <- data.frame(chromosome=2, x=seq(from=122e6, to=126e6, by=0.2e6))
data$state <- stateFcn(data)
print(data)
