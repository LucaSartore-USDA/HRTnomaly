library(tidyverse)
data <- read.csv("~/abc.csv")
check <- read.csv("~/abc22.csv")
prev <- read.csv("~/abc21.csv")

#####################
data <- data %>% 
        filter(!duplicated(cbind(stateid, id, item_name, item_code, status, current_value_timestamp, master_varname)))
data2 <- data %>% 
         select(id,master_varname,current_value_num) %>%
		 pivot_wider(names_from = master_varname, values_from = current_value_num, values_fill = NA)

data1 <- data2[, c(1L, which(substr(colnames(data2), 1L, 2L) == 'LC'),
                   which(colnames(data2) %in% c("STATE", "STRATAY")))]
data1[data1 < 0] <- NA
data1 <- data1[-which(is.na(data1$STRATAY)), ]

#####################
prev <- prev %>% 
        filter(!duplicated(cbind(id, STATUS, current_value_timestamp, master_varname)))
data2 <- prev %>% 
         select(id,master_varname,current_value_num) %>%
		 pivot_wider(names_from = master_varname, values_from = current_value_num, values_fill = NA)
datap <- data2[, colnames(data1)]
datap[datap < 0] <- NA

###################
check <- check %>%
         filter(!duplicated(cbind(id, STATUS, current_value_timestamp, master_varname)))
data2 <- check %>% 
         select(id, master_varname, current_value_num) %>%
		 pivot_wider(names_from = master_varname, values_from = current_value_num, values_fill = NA)
datac <- data2[, colnames(data1)]
datac[datac < 0] <- NA

###################
data1l <- data1 %>% pivot_longer(!c(id, STATE, STRATAY), names_to = "varname", values_to = "current_value_num")
datapl <- datap %>% pivot_longer(!c(id, STATE, STRATAY), names_to = "varname", values_to = "prev_value_1")
datacl <- datac %>% pivot_longer(!c(id, STATE, STRATAY), names_to = "varname", values_to = "check_value")
cdata <- left_join(data1l,datapl)
cdata$diff <- cdata$current_value_num - cdata$prev_value_1
cdata <- left_join(cdata,datacl)
names(cdata) <- c("id", "STATE", "stratay", "master_varname", 
			      "current_value_num", "prev_value_1",
				  "original_value_num"," Anomaly")
cdata$X <- seq_len(dim(cdata)[1L])
cdata <- cdata[, names(a)]
write.csv(cdata, file = "~/abcnew.csv", row.names = FALSE)
