library(tidyr)
library(fastmatch)
library(cellWise)
library(FuzzyHRT)

## BEGIN USER CUSTOMIZED SETTING
DATA_FOLDER <- "~"
##### Data file
FILENAME <- "abcnew.csv"
##### Fraction of Data contamination (number in [0, 1])
CONTAMINATION <- 0.08
##### Name of data file in output
OUTPUT_FILE <- sub(".csv$", "_results.csv", FILENAME)
## END USER CUSTOMIZED SETTING

setwd(DATA_FOLDER)
a <- read.csv(FILENAME) # Reading the data
b <- a
overa <- matrix(NA, nrow = length(unique(b$STATE)),ncol = 5)
overn <- matrix(NA, nrow = length(unique(b$STATE)),ncol = 5)
timen <- c()
timea <- c()
for(i in seq_along(unique(b$STATE))) {
  tryCatch({
    a <- b[b$STATE==unique(b$STATE)[i],]
    
    ptm <- proc.time()
    ## Historical and zero check
    hScore <- .C("history_check", double(nrow(a)), double(nrow(a)),
                 as.double(a$current_value_num),
                 as.double(a$prev_value_1), nrow(a),
                 NAOK = TRUE, DUP = TRUE)[1L:2L]
    zScore <- hScore[[2L]]
    hScore <- hScore[[1L]]
    
    ## Tail-check
    dtac <- a[, c("STATE", "stratay", "id",
                  "master_varname", "current_value_num")] %>%
      pivot_wider(names_from = master_varname,
                  values_from = current_value_num)
    dtac[dtac <= 0] <- NA
    dtal <- as.matrix(log(dtac[, -1L:-3L]))
    
    #gr <- factor(sprintf("%02d_%d", dtac$STATE, dtac$stratay))
    gr <- factor(sprintf("%02d", dtac$STATE))
    
    tScore <- .C("tail_check", as.double(dtal), dim(dtal),
                 gr, nlevels(gr), res = double(prod(dim(dtal))),
                 NAOK = TRUE, DUP = FALSE, PACKAGE = "FuzzyHRT")$res
    

    ## Relational-check
    rScore <- 1
    dtae <- .C("normalize", as.double(dtal), dim(dtal),
               gr, nlevels(gr), res = double(prod(dim(dtal))),
               NAOK = TRUE, DUP = FALSE, PACKAGE = "FuzzyHRT")$res
    dtae[is.na(dtae)] <- 0
    rScore <- .C("relat_check", dtae = as.double(dtae),
                 dim(dtal), DUP = FALSE, PACKAGE = "FuzzyHRT")$dtae
    rScore <- array(rScore, dim = dim(dtal))
    
    ## Putting things together using a Fuzzy-Logic-Inspired procedure
    dtac[, -1L:-3L] <- array(rScore * tScore, dim=dim(dtal))
    dtar <- dtac %>% pivot_longer(3 + seq_len(ncol(dtal)), values_drop_na = TRUE)
    
    dtar <- dtac %>% pivot_longer(cols = 4:dim(dtac)[2], names_to = "master_varname", values_to = "rScore") #dtac %>% pivot_longer(5 + seq_len(ncol(dtal)), values_drop_na=TRUE)
    dtar <- left_join(a, dtar)
    
    a$score <- zScore * hScore*dtar$rScore
    th <- quantile(a$score[a$score!=0], CONTAMINATION, na.rm = TRUE)
    a$outlier <- a$score < th & a$score != 0
    timen[i] <- (proc.time() - ptm)[3]
    
    dtac2 <- a[, c("STATE", "stratay", "id",
                   "master_varname", "current_value_num")] %>%
      pivot_wider(names_from = master_varname,
                  values_from = current_value_num)
    
    
    D <- dtac2[ , colSums(is.na(dtac2))!=dim(dtac2)[1]]
    
    ptm <- proc.time()
    # Default options for DDC:
    DDCpars = list(fracNA = 1, numDiscrete = 3, precScale = 1e-12,
                   cleanNAfirst = "automatic", tolProb = 0.99, 
                   corrlim = 0.5, combinRule = "wmedian",
                   returnBigXimp = FALSE, silent = FALSE,
                   nLocScale = 25000, fastDDC = FALSE,
                   standType = "1stepM", corrType = "gkwls",
                   transFun = "wrap", nbngbrs = 100)
    DDCmortality = DDC(D[,-c(1:3)])#,DDCpars)
    remX = DDCmortality$remX
    timea[i] <- (proc.time() - ptm)[3]
    
    #dim(remX)
    #n = nrow(remX)
    #nrowsinblock = ncol(remX)
    #rowlabels = rownames(remX)
    
    #d = ncol(remX)
    #ncolumnsinblock = ncol(remX)
    #columnlabels = 1:d
    #cellMap(D=remX,  R=DDCmortality$stdResid ,rowlabels = rownames(remX), columnlabels = colnames(D[,-c(1:3)]))
    
    D <- D[rownames(remX), c(1:3, which(colnames(D)%in%colnames(remX)))]
    ##flag of the outliers
    flag <- rep("", nrow(remX) * ncol(remX))
    flag[DDCmortality$indcells] <- "h"
    #flag <- matrix(flag,nrow(remX),ncol(remX))
    
    dtac2 <- a[, c("STATE", "stratay", "id",
                   "master_varname", "Anomaly")] %>%
      pivot_wider(names_from = master_varname,
                  values_from = Anomaly)
    
    Dflag <- left_join(D[, c(1:3)], dtac2) 
    Dflag <- Dflag[,colnames(D)]
    Dflag <- c(as.matrix(Dflag[, -c(1:3)]))
    Dflag[is.na(Dflag)] <- ""
    Dflag[which(Dflag!="")] <- "h"
	
    ## Checking results on test datasets
    pr <- prop.table(table(Dflag,flag))
    overa[i,1] <-sum(diag(pr)) 
    cat("Overall accuracy:\n", sum(diag(pr)), "\n")
    cat("User accuracy:\n")
    acc.user <- diag(pr) / colSums(pr) # USER
    overa[i,2] <- acc.user[1]
    overa[i,3] <- acc.user[2]
    names(acc.user) <- c("normal", "outlier")
    print(acc.user)
    cat("Producer accuracy:\n")
    acc.prod <- diag(pr) / rowSums(pr) # PRODUCER (This is important in this context)
    names(acc.prod) <- c("normal", "outlier")
    overa[i,4] <- acc.prod[1]
    overa[i,5] <- acc.prod[2]
    print(acc.prod)
    #trp <- rowSums(pr)
    #cat("Test efficiency from random choice:\n")
    #print(pr/outer(trp, trp))
    
    
    
    dtac3 <- a[, c("STATE", "stratay", "id",
                   "master_varname", "outlier")] %>%
      pivot_wider(names_from = master_varname,
                  values_from = outlier)
    dtac3 <- dtac3[, names(D)]
    Nflag <- left_join(D[, c(1:3)], dtac3) 
   
    Nflag <- c(as.matrix(Nflag[, -c(1:3)]))
    Nflag[Nflag==FALSE] <- ""
    Nflag[is.na(Nflag)] <- ""
    Nflag[Nflag==TRUE] <- "h"
    
    
    ## Checking results on test datasets
    pr <- prop.table(table(Dflag,Nflag))
    overn[i,1] <-sum(diag(pr)) 
    cat("Overall accuracy:\n", sum(diag(pr)), "\n")
    cat("User accuracy:\n")
    acc.user <- diag(pr) / colSums(pr) # USER
    overn[i,2] <- acc.user[1]
    overn[i,3] <- acc.user[2]
    names(acc.user) <- c("normal", "outlier")
    print(acc.user)
    cat("Producer accuracy:\n")
    acc.prod <- diag(pr) / rowSums(pr) # PRODUCER (This is important in this context)
    names(acc.prod) <- c("normal", "outlier")
    overn[i,4] <- acc.prod[1]
    overn[i,5] <- acc.prod[2]
    print(acc.prod)
    #trp <- rowSums(pr)
    #cat("Test efficiency from random choice:\n")
    #print(pr/outer(trp, trp))
  }, error=function(e){})
}

overa <- data.frame(data = FILENAME, method = "cellWise", overa)
overn <- data.frame(data = FILENAME, method = "our", overn)
accuracy1 <- rbind(overa, overn)

accuracy1 <- accuracy1[c(order(accuracy1$X1[accuracy1$method=="cellWise"]),50+order(accuracy1$X1[accuracy1$method=="cellWise"])),]0
# accuracy2 <- accuracy2[c(order(accuracy2$X1[accuracy2$method=="cellWise"]),50+order(accuracy2$X1[accuracy2$method=="cellWise"])),]
# accuracy3 <- accuracy3[c(order(accuracy3$X1[accuracy3$method=="cellWise"]),43+order(accuracy3$X1[accuracy3$method=="cellWise"])),]
# accuracy4 <- accuracy4[c(order(accuracy4$X1[accuracy4$method=="cellWise"]),43+order(accuracy4$X1[accuracy4$method=="cellWise"])),]

accuracy1 <- data.frame(state=1:50,accuracy1)
accuracy1$method[accuracy1$method=="cellWise"] <- "DDC"
accuracy1$method[accuracy1$method=="our"] <- "Proposed Method"

library(ggplot2)
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)
ggplot(accuracy1, aes(x = state, y = X1, group = method, color =m ethod)) +
  geom_point(aes(shape = method), size = 3) +
  labs(title = "ABC - 2022") + 
  xlab("State") + ylab("Overall Accuracy") +
  scale_color_manual(values = c("#b45f06", "#45818e")) + 
  scale_y_continuous(limits = c(min(accuracy1[, -c(1:3)]) + 0.5, max(accuracy1[, -c(1:3)]))) +
  theme(text = element_text(size = 30), legend.position = "bottom", axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  scale_size(guide="none")
