#context("Benchmark local linear regression function")
source("llr_functions.R")

library(microbenchmark)
library(reshape2)

data(french_fries)
french_fries = french_fries[complete.cases(french_fries),]
z = seq(0,15,length.out = 100)
x = french_fries$potato
y = french_fries$buttery
omega = 2
#fits = llr(z = z,x = french_fries$potato,y = french_fries$buttery,omega = 2)

microbenchmark(llr(x,y,z,omega))