# Program
library(rstudioapi)
library(tidyr)
library(dplyr)
library(fastmatch)
library(stringr)
library(FuzzyHRT)

## BEGIN USER CUSTOMIZED SETTING
fldr <- (dirname(getActiveDocumentContext()$path))
accuracy_poids <- data.frame("Srvy" = 0, "Dte" = 0, "levl" = 0,"total_poids" = 0, "naggle" = 0, 
                             "True Anom" = 0, "True NonAnom" = 0,"Total True" = 0, 
                             "False Anom" = 0, "False NonAnom" = 0, "Total False" = 0, 
                             "User accuracy Normal" = 0, "User accuracy Outlier" = 0, 
                             "Producer accuracy Normal" = 0, "Producer accuracy outlier" = 0, 
                             "True Anom TEFRC" = 0, "True NonAnom TEFRC" = 0, 
                             "False Anom TEFRC" = 0, "False NonAnom TEFRC" = 0, "total time")
accuracy_vars <- data.frame("Srvy" = 0, "Dte" = 0, "levl" = 0, "total_poids" = 0, "naggle" = 0,
                            "True Anom"=0,"True NonAnom" = 0,"Total True"=0, 
                            "False Anom" = 0, "False NonAnom" = 0, "Total False" = 0,
                            "User accuracy Normal" = 0, "User accuracy Outlier" = 0,
                            "Producer accuracy Normal" = 0, "Producer accuracy outlier" = 0,
                            "True Anom TEFRC" = 0, "True NonAnom TEFRC" = 0, 
                            "False Anom TEFRC" = 0, "False NonAnom TEFRC" = 0, "total time")
srvys <- c("A1", "A2", "A3", "A4")
dtes <- c("2021-02-01", "2022-01-01", "2021-07-01", "2021-01-01")
levls <- c('low', 'high')
for(srvy in srvys){
  for(levl in levls){
    dte <- dtes[grep(srvy,srvys)]
    print(paste0(srvy, "_", dte,"_", levl))

    ## BEGIN USER CUSTOMIZED SETTING
    DATA_FOLDER <- paste0("~/")
        
    ##### Data file
    FILENAME <- paste0(srvy, "_", dte, '_', levl, '.csv')

    ##### Fraction of Data contamination (number in [0, 1])
    CONTAMINATION <- 0.08
    ##### Name of data file in output
    OUTPUT_FILE <- sub(".csv$", "_results.csv", FILENAME)
    ## END USER CUSTOMIZED SETTING

    setwd(DATA_FOLDER)
    a <- read.csv(paste0(DATA_FOLDER, FILENAME)) # Reading the data
    start_time = Sys.time()

    ## Historical and zero check
    hScore <- .C("history_check", double(nrow(a)), double(nrow(a)),
                as.double(a$current_value_num),
                as.double(a$prev_value_1), nrow(a),
                NAOK = TRUE, DUP = TRUE, PACKAGE = "FuzzyHRT")[1L:2L]
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
    dtar <- dtac %>% pivot_longer(3 + seq_len(ncol(dtal)), values_drop_na=TRUE)
    a$score <- zScore * hScore
    wh <- fmatch(paste(dtar$id, dtar$name),
                paste(a$id, a$master_varname))
    nna <- !is.na(wh)
    a$score[wh[nna]] <- a$score[wh[nna]] * dtar$value[nna]
    th <- quantile(a$score, CONTAMINATION)
    a$outlier <- a$score < th

    tot_poids<-NROW(unique(a$id))
    end_time = Sys.time()
    total_time<-end_time - start_time


    ctime <- Sys.time()                                             # get time for timestamp on output folder
    ct <- str_replace_all(ctime, "[^[:alnum:]]", "")
    ct<-paste0(srvy,"_",dte,"_",levl,"_",ct)

    naggle_fldr<-paste0("~/")
    dir.create(paste0(naggle_fldr, file.path(ct)))                                       # create output directory file in Run Folder



    naggle_full<-a
    anom_full<-read.csv(paste0("~/", srvy, "_", dte, '_', levl, '.csv'))
    full_compare<-anom_full %>% full_join(naggle_full, by = c("id" = "id", "master_varname" = "master_varname"))
    write.csv(full_compare, paste0(naggle_fldr, file.path(ct), "full_Compare.csv"))

    b <- data.frame('id' = unique(a$id)) %>%  
        mutate(Anomaly = ifelse(id %in% a$id[a$Anomaly != ""], TRUE, FALSE),
                outlier = ifelse(id %in% a$id[a$outlier == TRUE], TRUE, FALSE))
    pr <- prop.table(table(truth = b$Anomaly, model = b$outlier))
    pr_eff <- pr / outer(rowSums(pr), rowSums(pr))

    new_row_poid <- c(srvy, dte, levl, tot_poids, "LNL", round(pr[2, 2], 4), #true anom
                    round(pr[1, 1], 4), #true nonAnom
                    round(pr[2, 2], 4) + round(pr[1, 1], 4), #total true
                    round(pr[1, 2], 4), #False Anom
                    round(pr[2, 1], 4), #False nonAnom
                    round(pr[1, 2], 4) + round(pr[2, 1], 4), #total false
                    round(as.numeric((diag(pr) / colSums(pr))[1]), 4), #user acc. normal
                    round(as.numeric((diag(pr) / colSums(pr))[2]), 4), #user acc. outlier
                    round(as.numeric((diag(pr) / rowSums(pr))[1]), 4), #prod acc. normal
                    round(as.numeric((diag(pr) / rowSums(pr))[2]), 4), #prod acc. outlier
                    round(pr_eff[2, 2], 4), #True Anom TEFRC
                    round(pr_eff[1, 1], 4), #True NonAnom TEFRC
                    round(pr_eff[1, 2], 4), #False Anom TEFRC
                    round(pr_eff[2, 1], 4), #False NonAnom TEFRC
                    total_time)

    if(accuracy_poids$Srvy[1] == 0) {
      accuracy_poids[1, ] <- new_row_poid
    } else {
      accuracy_poids[nrow(accuracy_poids) + 1, ] <- new_row_poid
    }

    pr <- prop.table(table(truth = a$Anomaly != "", model = a$outlier))
    pr_eff<-pr / outer(rowSums(pr), rowSums(pr))


    new_row<-c(srvy, dte, levl, tot_poids, "LNL",
              round(pr[2, 2], 4), #true anom
              round(pr[1, 1], 4), #true nonAnom
              round(pr[2, 2], 4) + round(pr[1, 1], 4), #total true
              round(pr[1, 2], 4), #False Anom
              round(pr[2, 1], 4), #False nonAnom
              round(pr[1, 2], 4) + round(pr[2, 1], 4), #total false
              round(as.numeric((diag(pr) / colSums(pr))[1]), 4), #user acc. normal
              round(as.numeric((diag(pr) / colSums(pr))[2]), 4), #user acc. outlier
              round(as.numeric((diag(pr) / rowSums(pr))[1]), 4), #prod acc. normal
              round(as.numeric((diag(pr) / rowSums(pr))[2]), 4), #prod acc. outlier
              round(pr_eff[2, 2], 4), #True Anom TEFRC
              round(pr_eff[1, 1], 4), #True NonAnom TEFRC
              round(pr_eff[1, 2], 4), #False Anom TEFRC
              round(pr_eff[2, 1], 4), #False NonAnom TEFRC
              total_time) 

    if(accuracy_vars$Srvy[1] == 0) {
      accuracy_vars[1, ] <- new_row
    } else {
      accuracy_vars[nrow(accuracy_vars) + 1, ] <- new_row
    }
    write.csv(accuracy_poids,paste0("~/LNL_ID_results.csv"))
    write.csv(accuracy_vars,paste0("~/LNL_VARS_results.csv"))
    rm(list = setdiff(ls(), c("srvys", "dtes", "levls", "accuracy_ids", "accuracy_vars", "srvy", "dte", "levl", 'fldr')))
  }
}

# 
# ## Checking results on test datasets
# if ("Anomaly" %fin% names(a)) {
#   invisible(pr <- prop.table(table(truth = a$Anomaly != "", model = a$outlier)))
#   cat("Overall accuracy:\n", sum(diag(pr)), "\n")
#   cat("User accuracy:\n")
#   acc.user <- diag(pr) / colSums(pr) # USER
#   names(acc.user) <- c("normal", "outlier")
#   print(acc.user)
#   cat("Producer accuracy:\n")
#   acc.prod <- diag(pr) / rowSums(pr) # PRODUCER (This is important in this context)
#   names(acc.prod) <- c("normal", "outlier")
#   print(acc.prod)
#   trp <- rowSums(pr)
#   cat("Test efficiency from random choice:\n")
#   print(pr / outer(rowSums(pr), rowSums(pr)))
# }
# 
# 
# 
# ## Writing/Viewing the output
# if (askYesNo("Do you want to save the results?"))
# 	write.csv(a, file = OUTPUT_FILE, row.names = FALSE)
# if (askYesNo("Do you want to view the results?"))
# 	View(a)
