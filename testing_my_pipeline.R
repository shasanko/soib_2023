load("01_analyses_full/dataforanalyses.RData")
dataforanaly <- data
rm(data)
load("01_analyses_full/dataforanalyses.RData-data_opt")
dataforanaly_opt <- data
all.equal(dataforanaly_opt,
          dataforanaly)
names(dataforanaly_opt)==names(dataforanaly)
