#uncomment if readxl is not installed
#install.packages('readxl')
library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
        # I prefer straight data.frames
        # but if you like tidyverse tibbles (the default with read_excel)
        # then just pass tibble = TRUE
        sheets <- readxl::excel_sheets(filename)
        x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
        if(!tibble) x <- lapply(x, as.data.frame)
        names(x) <- sheets
        x
}
#the path to working directory, ideally you should keep CPDataJPSPLC.xlsx there
setwd("/Users/a66as/Desktop/SPLC21/")
data <- read_excel_allsheets("CPDataJPSPLC.xlsx") # all the sheets
#1TFWith
data$TFSnotRemoved$sim->reqSim
data$TFSnotRemoved$JPLag->softSim
reqSim<-as.numeric(reqSim)
softSim<-as.numeric(softSim)
softSim<-softSim/100
shapiro.test(reqSim)
shapiro.test(softSim)
res <- cor.test(reqSim, softSim,  method="pearson", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="kendall", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="spearman", conf.level = 0.95)
res
plot(reqSim, softSim,
     xlab = "TF", ylab = "SS",
     pch = 19, frame = FALSE, cex.axis=1.5, cex.lab=1.5)
abline(lm(softSim ~ reqSim), col = "blue")
#2DWWith
data$DWSnotRemoved$sim->reqSim
data$DWSnotRemoved$JPLag->softSim
reqSim<-as.numeric(reqSim)
softSim<-as.numeric(softSim)
softSim<-softSim/100
res <- cor.test(reqSim, softSim,  method="pearson", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="kendall", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="spearman", conf.level = 0.95)
res
plot(reqSim, softSim, 
     xlab = "DW", ylab = "SS",
     pch = 19, frame = FALSE, cex.axis=1.5, cex.lab=1.5)
abline(lm(softSim ~ reqSim), col = "blue")
#3FTWith
data$FTSNotRemoved$sim->reqSim
data$FTSNotRemoved$JPLag->softSim
reqSim<-as.numeric(reqSim)
softSim<-as.numeric(softSim)
softSim<-softSim/100
res <- cor.test(reqSim, softSim,  method="pearson", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="kendall", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="spearman", conf.level = 0.95)
res
plot(reqSim, softSim, 
     xlab = "FT", ylab = "SS",
     pch = 19, frame = FALSE, cex.axis=1.5, cex.lab=1.5)
abline(lm(softSim ~ reqSim), col = "blue")
#4BERTWith
data$BERTSnotRemoved$sim->reqSim
data$BERTSnotRemoved$JPLag->softSim
reqSim<-as.numeric(reqSim)
softSim<-as.numeric(softSim)
softSim<-softSim/100
res <- cor.test(reqSim, softSim,  method="pearson", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="kendall", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="spearman", conf.level = 0.95)
res
plot(reqSim, softSim,
     xlab = "BERT", ylab = "SS",
     pch = 19, frame = FALSE, cex.axis=1.5, cex.lab=1.5)
abline(lm(softSim ~ reqSim), col = "blue")
#5TFWithout
data$TFSRemoved$sim->reqSim
data$TFSRemoved$JPLag->softSim
reqSim<-as.numeric(reqSim)
softSim<-as.numeric(softSim)
softSim<-softSim/100
res <- cor.test(reqSim, softSim,  method="pearson", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="kendall", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="spearman", conf.level = 0.95)
res
plot(reqSim, softSim, 
     xlab = "pTF", ylab = "SS",
     pch = 19, frame = FALSE, cex.axis=1.5, cex.lab=1.5)
abline(lm(softSim ~ reqSim), col = "blue")
#6DWWithout
data$DWSRemoved$sim->reqSim
data$DWSRemoved$JPLag->softSim
reqSim<-as.numeric(reqSim)
softSim<-as.numeric(softSim)
softSim<-softSim/100
res <- cor.test(reqSim, softSim,  method="pearson", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="kendall", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="spearman", conf.level = 0.95)
res
plot(reqSim, softSim, 
     xlab = "pDW", ylab = "SS",
     pch = 19, frame = FALSE, cex.axis=1.5, cex.lab=1.5)
abline(lm(softSim ~ reqSim), col = "blue")
#7FTWithout
data$FTSRemoved$sim->reqSim
data$FTSRemoved$JPLag->softSim
reqSim<-as.numeric(reqSim)
softSim<-as.numeric(softSim)
softSim<-softSim/100
res <- cor.test(reqSim, softSim,  method="pearson", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="kendall", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="spearman", conf.level = 0.95)
res
plot(reqSim, softSim,
     xlab = "pFT", ylab = "SS",
     pch = 19, frame = FALSE, cex.axis=1.5, cex.lab=1.5)
abline(lm(softSim ~ reqSim), col = "blue")
#8BERTWithout
data$BERTSRemoved$sim->reqSim
data$BERTSRemoved$JPLag->softSim
reqSim<-as.numeric(reqSim)
softSim<-as.numeric(softSim)
softSim<-softSim/100
res <- cor.test(reqSim, softSim,  method="pearson", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="kendall", conf.level = 0.95)
res
res <- cor.test(reqSim, softSim,  method="spearman", conf.level = 0.95)
res
plot(reqSim, softSim,
     xlab = "pBERT", ylab = "SS",
     pch = 19, frame = FALSE,cex.axis=1.5, cex.lab=1.5)
abline(lm(softSim ~ reqSim), col = "blue")
boxplot(as.numeric(data$TFSnotRemoved$sim), as.numeric(data$TFSnotRemoved$JPLag)/100,
        as.numeric(data$DWSnotRemoved$sim), as.numeric(data$TFSRemoved$JPLag)/100,
        as.numeric(data$FTSNotRemoved$sim), as.numeric(data$DWSnotRemoved$JPLag)/100,
        as.numeric(data$BERTSnotRemoved$sim), as.numeric(data$DWSRemoved$JPLag)/100,
        as.numeric(data$TFSRemoved$sim), as.numeric(data$FTSNotRemoved$JPLag)/100,
        as.numeric(data$DWSRemoved$sim), as.numeric(data$FTSRemoved$JPLag)/100,
        as.numeric(data$FTSRemoved$sim), as.numeric(data$BERTSnotRemoved$JPLag)/100,
        as.numeric(data$BERTSRemoved$sim), as.numeric(data$BERTSRemoved$JPLag)/100,
        main = "Requirements Similarity Vs Software Similarity",
        at = c(1,2,3,4,5,6,7,8,9,10, 11, 12, 13, 14, 15, 16),
        names = c("TF", "SS", "pTF", "SS", "DW", "SS", "pDW", "SS", "FT", "SS", "pFT", "SS", "BERT", "SS", "pBERT", "SS"),
        las =3,
        col = c("lightsteelblue2","plum3"),
        border = "gray34",
        horizontal = FALSE,
        notch = FALSE
)
