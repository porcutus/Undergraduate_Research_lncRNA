file_location <-  "C:\\Users\\Porcutus\\Desktop\\R Programming\\Stroke"
setwd(file_location)
data_ognl <- read.csv(file.choose(), sep="\t")
#fil_df <- read.csv(file.choose(), sep="\t")

lncProbe <- read.csv(file.choose(), sep="\t")

data_ognl$isKeep <- c(data_ognl$ID_REF %in% lncProbe$Probe)
nrow(data_ognl[data_ognl$isKeep == "TRUE", ])
fil_df <- data_ognl[data_ognl$isKeep == "TRUE", ]
fil_df$isKeep <- NULL

#categorize samples into corresponding dataframes.
probes <- fil_df[,c(1)]
control <- fil_df[,2:24]
patient <- fil_df[,25:93]
bigmatrix <- cbind(control, patient)
row.names(bigmatrix) <- probes
colnames(bigmatrix) <- c(rep('control',23),rep('patientfalala',69))
z=vector()

for(i in 1:3052){
  z[i] <- t.test(control[i,],patient[i,],paired=FALSE)$p.value
}

bigmatrix_fil <- bigmatrix[which(z<0.005), ] 
jpeg(file="col_725_0.005.jpg")
library(gplots)
heatmap.2(as.matrix(bigmatrix_fil), Colv = TRUE, col=redgreen(75), scale="row",
          key=TRUE, symkey=FALSE, density.info="none", 
          trace="none", cexRow=0.5)
dev.off()