ipak <- function(pkg){
  new.pkg<- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg,require, character.only=TRUE)
}

packages<- c( "plyr","reshape2","RColorBrewer","scales","grid",
             "zoo","googleVis","data.table","outliers",
             "RRF","FactoMineR", "car",
             "Caret","CBA","forecast", "survival",
             "LSMeans", "Comparison","ACD",
             "ClustEval","TimeROC","shiny","devtools",
             "ggord")

packages2<- c("regtest","comparison","BaSTA","ltsa","Rankcluster","cba",
              "caret","CORElearn","rminer","features","ISLR","evir","rvest")
ipak(packages2)
