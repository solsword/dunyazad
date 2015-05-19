#!/usr/bin/env Rscript
responses <- read.csv(file="pilot-results.csv",head=TRUE,sep=",")
cat("\nData output:\n\n")
print(responses)
cat("\nMann-Whitney-Wilcoxon:\n\n")
wilcox.test(Answer.consequences ~ Answer.clearbest, data=responses)
