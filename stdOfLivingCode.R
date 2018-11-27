regions <- read.csv("~/regions.csv", stringsAsFactors = FALSE)
regions <- rbind(regions, c("India",NULL))

###########
##ECONOMY##
###########

data1 <- read.csv("~/datagov/Economy/state-wise-net-domestic-product-ndp-constant-price.csv", header=FALSE)

data1 <- t(data1)
rownames(data1) <- NULL
data1w <- data1[-(1:2),]
data1w <- data1w[,1:7]
colnames(data1w) <- c("States.and.Union.Territories","nsdp_const_11-12","nsdp_const_12-13","nsdp_const_13-14","nsdp_const_14-15","nsdp_const_15-16","nsdp_const_16-17")

#check for naming discrepencies of states/UTs and their removal
namesReg <- as.vector(regions$States.and.Union.Territories)
namesData <- data1w[,1]
namesUp <- namesReg[!(namesReg %in% namesData)]
namesData[!(namesData %in% namesReg)]
data1w[c(1,29,30,32,34),1] <- namesUp[c(1,2,3,7,8)]

namesData <- data1w[,1]
namesData[!(namesData %in% namesReg)] ##final check for naming discrepencies

dataPool <- merge(data1w, regions, by="States.and.Union.Territories", all.y=TRUE)

####################################################
data1 <- read.csv("~/datagov/Economy/state-wise-net-domestic-product-ndp-current-price.csv", header=FALSE)

data1 <- t(data1)
rownames(data1) <- NULL
data1w <- data1[-(1:2),1:7]
colnames(data1w) <- c("States.and.Union.Territories","nsdp_cur_11-12","nsdp_cur_12-13","nsdp_cur_13-14","nsdp_cur_14-15","nsdp_cur_15-16","nsdp_cur_16-17")
data1w[c(1,29,30,32,34),1] <- namesUp[c(1,2,3,7,8)]

dataPool <- merge(data1w, dataPool, by="States.and.Union.Territories", all.y=TRUE)

#####################################################
data1 <- read.csv("~/datagov/Economy/gross-domestic-product-gdp-constant-price.csv", header=FALSE)

data1 <- t(data1)
rownames(data1) <- NULL
data1w <- data1[-(1:2),1:7]
colnames(data1w) <- c("States.and.Union.Territories","gdp_const_11-12","gdp_const_12-13","gdp_const_13-14","gdp_const_14-15","gdp_const_15-16","gdp_const_16-17")
data1w[c(1,29,30,32,34),1] <- namesUp[c(1,2,3,7,8)]

dataPool <- merge(data1w, dataPool, by="States.and.Union.Territories", all.y=TRUE)

#####################################################
data1 <- read.csv("~/datagov/Economy/gross-domestic-product-gdp-current-price.csv", header=FALSE)

data1 <- t(data1)
rownames(data1) <- NULL
data1w <- data1[-(1:2),1:7]
colnames(data1w) <- c("States.and.Union.Territories","gdp_cur_11-12","gdp_cur_12-13","gdp_cur_13-14","gdp_cur_14-15","gdp_cur_15-16","gdp_cur_16-17")
data1w[c(1,29,30,32,34),1] <- namesUp[c(1,2,3,7,8)]

dataPool <- merge(data1w, dataPool, by="States.and.Union.Territories", all.y=TRUE)

#####################################################

##############
##DEMOGRAPHY##
##############

data1 <- read.csv("~/datagov/Demography/decadal-growth-rate.csv", header=TRUE, stringsAsFactors = FALSE)

colnames(data1)[-1] <- c("States.and.Union.Territories","gr_tot_91-01","gr_tot_01-11","gr_rur_91-01","gr_rur_01-11","gr_urb_91-01","gr_urb_01-11")

#check for naming discrepencies of states/UTs and their removal
namesReg <- as.vector(regions$States.and.Union.Territories)
namesData <- data1[,2]
namesUp <- namesReg[!(namesReg %in% namesData)]
namesData[!(namesData %in% namesReg)]
data1[1,2] <- namesUp[2]

dataPool <- merge(data1, dataPool, by="States.and.Union.Territories", all.y = TRUE)
dataPool$Category[(dataPool$States.and.Union.Territories=='Telangana')] <- 'State'

######################################################
data1 <- read.csv("~/datagov/Demography/sex-ratio.csv", header=TRUE, stringsAsFactors = FALSE)
data1 <- data1[,-1]
data1[1,1] <- namesUp[2]

colnames(data1) <-c("States.and.Union.Territories","sr_tot_01","sr_tot_11","sr_rur_01","sr_rur_11","sr_urb_01","sr_urb_11")
dataPool <- merge(data1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

######################################################
data1 <- read.csv("~/datagov/Demography/child-sex-ratio-0-6-years.csv", header=TRUE, stringsAsFactors = FALSE)
data1 <- data1[,-1]
data1[1,1] <- namesUp[2]

colnames(data1) <-c("States.and.Union.Territories","csr_tot_01","csr_tot_11","csr_rur_01","csr_rur_11","csr_urb_01","csr_urb_11")
dataPool <- merge(data1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

##########################################################

#############
##EDUCATION##
#############

data1 <- read.csv("~/datagov/Education/drop-out-rate.csv", stringsAsFactors = F)
data1[data1 == "NR"] <- NA
data1[data1 == "Uppe_r_Primary"] <- NA
data1.1 <- data1[(as.character(data1$year) == "2012-13"),-2]

namesReg <- as.vector(regions$States.and.Union.Territories)
namesData <- data1.1[,1]
namesUp <- namesReg[!(namesReg %in% namesData)]
namesData[!(namesData %in% namesReg)]

colnames(data1.1) <- c("States.and.Union.Territories","DR_prm_boy_12-13","DR_prm_girl_12-13","DR_prm_tot_12-13","DR_uPrm_boy_12-13","DR_uPrm_girl_12-13","DR_uPrm_tot_12-13","DR_sec_boy_12-13","DR_sec_girl_12-13","DR_sec_tot_12-13","DR_hSec_boy_12-13","DR_hSec_girl_12-13","DR_hSec_tot_12-13")
data1.1[c(8,10,20,31,36),1] <- namesUp[c(4,5,1,2,6)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.2 <- data1[(as.character(data1$year) == "2013-14"),-2]
colnames(data1.2) <- c("States.and.Union.Territories","DR_prm_boy_13-14","DR_prm_girl_13-14","DR_prm_tot_13-14","DR_uPrm_boy_13-14","DR_uPrm_girl_13-14","DR_uPrm_tot_13-14","DR_sec_boy_13-14","DR_sec_girl_13-14","DR_sec_tot_13-14","DR_hSec_boy_13-14","DR_hSec_girl_13-14","DR_hSec_tot_13-14")
data1.1[c(8,10,20,31,36),1] <- namesUp[c(4,5,1,2,6)]

dataPool <- merge(data1.2, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.3 <- data1[(as.character(data1$year) == "2014-15"),-2]
colnames(data1.3) <- c("States.and.Union.Territories","DR_prm_boy_14-15","DR_prm_girl_14-15","DR_prm_tot_14-15","DR_uPrm_boy_14-15","DR_uPrm_girl_14-15","DR_uPrm_tot_14-15","DR_sec_boy_14-15","DR_sec_girl_14-15","DR_sec_tot_14-15","DR_hSec_boy_14-15","DR_hSec_girl_14-15","DR_hSec_tot_14-15")
data1.1[c(8,10,20,31,36),1] <- namesUp[c(4,5,1,2,6)]

dataPool <- merge(data1.3, dataPool, by="States.and.Union.Territories", all.y = TRUE)

##################################################################

data1 <- read.csv("~/datagov/Education/gross-enrolment-ratio-higher-education.csv", stringsAsFactors = F)

data1.1 <- data1[(as.character(data1$Year) == "2010-11"),-1]

namesReg <- as.vector(regions$States.and.Union.Territories)
namesData <- data1.1[,1]
namesUp <- namesReg[!(namesReg %in% namesData)]
namesData[!(namesData %in% namesReg)]

colnames(data1.1) <- c("States.and.Union.Territories","ERH_male_10-11","ERH_fmale_10-11","ERH_tot_10-11","ERH_maleSC_10-11","ERH_fmaleSC_10-11","ERH_totSC_10-11","ERH_maleST_10-11","ERH_fmaleST_10-11","ERH_totST_10-11")
data1.1[c(1,7,8,10,15,34,36),1] <- namesUp[c(5,1,6,7,2,4,8)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$Year) == "2011-12"),-1]
colnames(data1.1) <- c("States.and.Union.Territories","ERH_male_11-12","ERH_fmale_11-12","ERH_tot_11-12","ERH_maleSC_11-12","ERH_fmaleSC_11-12","ERH_totSC_11-12","ERH_maleST_11-12","ERH_fmaleST_11-12","ERH_totST_11-12")
data1.1[c(1,7,8,10,15,34,36),1] <- namesUp[c(5,1,6,7,2,4,8)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$Year) == "2012-13"),-1]
colnames(data1.1) <- c("States.and.Union.Territories","ERH_male_12-13","ERH_fmale_12-13","ERH_tot_12-13","ERH_maleSC_12-13","ERH_fmaleSC_12-13","ERH_totSC_12-13","ERH_maleST_12-13","ERH_fmaleST_12-13","ERH_totST_12-13")
data1.1[c(1,7,8,10,15,34,36),1] <- namesUp[c(5,1,6,7,2,4,8)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$Year) == "2013-14"),-1]
colnames(data1.1) <- c("States.and.Union.Territories","ERH_male_13-14","ERH_fmale_13-14","ERH_tot_13-14","ERH_maleSC_13-14","ERH_fmaleSC_13-14","ERH_totSC_13-14","ERH_maleST_13-14","ERH_fmaleST_13-14","ERH_totST_13-14")
data1.1[c(1,7,8,10,15,34,36),1] <- namesUp[c(5,1,6,7,2,4,8)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$Year) == "2014-15"),-1]
colnames(data1.1) <- c("States.and.Union.Territories","ERH_male_14-15","ERH_fmale_14-15","ERH_tot_14-15","ERH_maleSC_14-15","ERH_fmaleSC_14-15","ERH_totSC_14-15","ERH_maleST_14-15","ERH_fmaleST_14-15","ERH_totST_14-15")
data1.1[c(1,7,8,10,15,34,36),1] <- namesUp[c(5,1,6,7,2,4,8)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$Year) == "2015-16"),-1]
colnames(data1.1) <- c("States.and.Union.Territories","ERH_male_15-16","ERH_fmale_15-16","ERH_tot_15-16","ERH_maleSC_15-16","ERH_fmaleSC_15-16","ERH_totSC_15-16","ERH_maleST_15-16","ERH_fmaleST_15-16","ERH_totST_15-16")
data1.1[c(1,7,8,10,15,34,36),1] <- namesUp[c(5,1,6,7,2,4,8)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

###################################################

data1 <- read.csv("~/datagov/Education/gross-enrolment-ratio-schools.csv", stringsAsFactors = F)
data1[data1 == "NR"] <- NA
data1.1 <- data1[(as.character(data1$Year) == "2013-14"),-2]

namesReg <- as.vector(regions$States.and.Union.Territories)
namesData <- data1.1[,1]
namesUp <- namesReg[!(namesReg %in% namesData)]
namesData[!(namesData %in% namesReg)]

colnames(data1.1) <- c("States.and.Union.Territories","ER_prm_boy_13-14","ER_prm_girl_13-14","ER_prm_tot_13-14","ER_uPrm_boy_13-14","ER_uPrm_girl_13-14","ER_uPrm_tot_13-14","ER_sec_boy_13-14","ER_sec_girl_13-14","ER_sec_tot_13-14","ER_hSec_boy_13-14","ER_hSec_girl_13-14","ER_hSec_tot_13-14")
data1.1[c(1,8,10,15,27,34,36),1] <- namesUp[c(4,5,6,1,7,3,8)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$Year) == "2014-15"),-2]
colnames(data1.1) <- c("States.and.Union.Territories","ER_prm_boy_14-15","ER_prm_girl_14-15","ER_prm_tot_14-15","ER_uPrm_boy_14-15","ER_uPrm_girl_14-15","ER_uPrm_tot_14-15","ER_sec_boy_14-15","ER_sec_girl_14-15","ER_sec_tot_14-15","ER_hSec_boy_14-15","ER_hSec_girl_14-15","ER_hSec_tot_14-15")
data1.1[c(1,8,10,15,27,34,37),1] <- namesUp[c(4,5,6,1,7,3,8)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$Year) == "2015-16"),-2]
colnames(data1.1) <- c("States.and.Union.Territories","ER_prm_boy_15-16","ER_prm_girl_15-16","ER_prm_tot_15-16","ER_uPrm_boy_15-16","ER_uPrm_girl_15-16","ER_uPrm_tot_15-16","ER_sec_boy_15-16","ER_sec_girl_15-16","ER_sec_tot_15-16","ER_hSec_boy_15-16","ER_hSec_girl_15-16","ER_hSec_tot_15-16")
data1.1[c(1,8,10,15,27,35,37),1] <- namesUp[c(4,5,6,1,7,3,8)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

#######################################################

data1 <- read.csv("~/datagov/Education/literacy-rate-7-years.csv", stringsAsFactors = F)[-1]
colnames(data1) <- c("States.and.Union.Territories","LR7_tot-01","LR7_tot-11","LR7_rur-01","LR7_rur-11","LR7_urb-01","LR7_urb-11")

namesReg <- as.vector(regions$States.and.Union.Territories)
namesData <- data1[,1]
namesUp <- namesReg[!(namesReg %in% namesData)]
namesData[!(namesData %in% namesReg)]
data1[1,1] <- namesUp[2]

dataPool <- merge(data1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

#########################################################

data1 <- read.csv("~/datagov/Education/percentage-schools-boys-toilet.csv", stringsAsFactors = F)
data1.1 <- data1[(as.character(data1$year) == "2013-14"),-2]

namesReg <- as.vector(regions$States.and.Union.Territories)
namesData <- data1.1[,1]
namesUp <- namesReg[!(namesReg %in% namesData)]
namesData[!(namesData %in% namesReg)]

colnames(data1.1) <- c("States.and.Union.Territories","T_prm_boy_13-14","T_prm-uPr_boy_13-14","T_prm-hSc_boy_13-14","T_uPr_boy_13-14","T_uPr-hSec_boy_13-14","T_prm-sec_boy_13-14","T_uPr-sec_boy_13-14","T_sec_boy_13-14","T_sec-hSc_boy_13-14","T_hSc_boy_13-14","T_all_boy_13-14")
data1.1[c(1,8,10,15,36),1] <- namesUp[c(3,4,5,1,6)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$year) == "2014-15"),-2]
colnames(data1.1) <- c("States.and.Union.Territories","T_prm_boy_14-15","T_prm-uPr_boy_14-15","T_prm-hSc_boy_14-15","T_uPr_boy_14-15","T_uPr-hSec_boy_14-15","T_prm-sec_boy_14-15","T_uPr-sec_boy_14-15","T_sec_boy_14-15","T_sec-hSc_boy_14-15","T_hSc_boy_14-15","T_all_boy_14-15")
data1.1[c(1,8,10,15,36),1] <- namesUp[c(3,4,5,1,6)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$year) == "2015-16"),-2]
colnames(data1.1) <- c("States.and.Union.Territories","T_prm_boy_15-16","T_prm-uPr_boy_15-16","T_prm-hSc_boy_15-16","T_uPr_boy_15-16","T_uPr-hSec_boy_15-16","T_prm-sec_boy_15-16","T_uPr-sec_boy_15-16","T_sec_boy_15-16","T_sec-hSc_boy_15-16","T_hSc_boy_15-16","T_all_boy_15-16")
data1.1[c(1,8,10,15,36),1] <- namesUp[c(3,4,5,1,6)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

#############################################################

data1 <- read.csv("~/datagov/Education/percentage-schools-girls-toilet.csv", stringsAsFactors = F)
data1.1 <- data1[(as.character(data1$year) == "2013-14"),-2]

namesReg <- as.vector(regions$States.and.Union.Territories)
namesData <- data1.1[,1]
namesUp <- namesReg[!(namesReg %in% namesData)]
namesData[!(namesData %in% namesReg)]

colnames(data1.1) <- c("States.and.Union.Territories","T_prm_girl_13-14","T_prm-uPr_girl_13-14","T_prm-hSc_girl_13-14","T_uPr_girl_13-14","T_uPr-hSec_girl_13-14","T_prm-sec_girl_13-14","T_uPr-sec_girl_13-14","T_sec_girl_13-14","T_sec-hSc_girl_13-14","T_hSc_girl_13-14","T_all_girl_13-14")
data1.1[c(1,2,9,11,16),1] <- namesUp[c(6,3,4,5,1)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$year) == "2014-15"),-2]
colnames(data1.1) <- c("States.and.Union.Territories","T_prm_girl_14-15","T_prm-uPr_girl_14-15","T_prm-hSc_girl_14-15","T_uPr_girl_14-15","T_uPr-hSec_girl_14-15","T_prm-sec_girl_14-15","T_uPr-sec_girl_14-15","T_sec_girl_14-15","T_sec-hSc_girl_14-15","T_hSc_girl_14-15","T_all_girl_14-15")
data1.1[c(1,2,9,11,16),1] <- namesUp[c(6,3,4,5,1)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$year) == "2015-16"),-2]
colnames(data1.1) <- c("States.and.Union.Territories","T_prm_girl_15-16","T_prm-uPr_girl_15-16","T_prm-hSc_girl_15-16","T_uPr_girl_15-16","T_uPr-hSec_girl_15-16","T_prm-sec_girl_15-16","T_uPr-sec_girl_15-16","T_sec_girl_15-16","T_sec-hSc_girl_15-16","T_hSc_girl_15-16","T_all_girl_15-16")
data1.1[c(1,2,9,11,16),1] <- namesUp[c(6,3,4,5,1)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

########################################################

data1 <- read.csv("~/datagov/Education/percentage-schools-drinking-water.csv", stringsAsFactors = F)
data1.1 <- data1[(as.character(data1$Year) == "2013-14"),-2]

namesReg <- as.vector(regions$States.and.Union.Territories)
namesData <- data1.1[,1]
namesUp <- namesReg[!(namesReg %in% namesData)]
namesData[!(namesData %in% namesReg)]

colnames(data1.1) <- c("States.and.Union.Territories","DW_prm_13-14","DW_prm-uPr_13-14","DW_prm-hSc_13-14","DW_uPr_13-14","DW_uPr-hSec_13-14","DW_prm-sec_13-14","DW_uPr-sec_13-14","DW_sec_13-14","DW_sec-hSc_13-14","DW_hSc_13-14","DW_all_13-14")
data1.1[c(1,8,10,15,36),1] <- namesUp[c(3,4,5,1,6)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$Year) == "2014-15"),-2]
colnames(data1.1) <- c("States.and.Union.Territories","DW_prm_14-15","DW_prm-uPr_14-15","DW_prm-hSc_14-15","DW_uPr_14-15","DW_uPr-hSec_14-15","DW_prm-sec_14-15","DW_uPr-sec_14-15","DW_sec_14-15","DW_sec-hSc_14-15","DW_hSc_14-15","DW_all_14-15")
data1.1[c(1,8,10,15,36),1] <- namesUp[c(3,4,5,1,6)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$Year) == "2015-16"),-2]
colnames(data1.1) <- c("States.and.Union.Territories","DW_prm_15-16","DW_prm-uPr_15-16","DW_prm-hSc_15-16","DW_uPr_15-16","DW_uPr-hSec_15-16","DW_prm-sec_15-16","DW_uPr-sec_15-16","DW_sec_15-16","DW_sec-hSc_15-16","DW_hSc_15-16","DW_all_15-16")
data1.1[c(1,8,10,15,36),1] <- namesUp[c(3,4,5,1,6)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

###############################################################################

data1 <- read.csv("~/datagov/Education/percentage-schools-electricity.csv", stringsAsFactors = F)
data1.1 <- data1[(as.character(data1$year) == "2013-14"),-2]

namesReg <- as.vector(regions$States.and.Union.Territories)
namesData <- data1.1[,1]
namesUp <- namesReg[!(namesReg %in% namesData)]
namesData[!(namesData %in% namesReg)]

colnames(data1.1) <- c("States.and.Union.Territories","elec_prm_13-14","elec_prm-uPr_13-14","elec_prm-hSc_13-14","elec_uPr_13-14","elec_uPr-hSec_13-14","elec_prm-sec_13-14","elec_uPr-sec_13-14","elec_sec_13-14","elec_sec-hSc_13-14","elec_hSc_13-14","elec_all_13-14")
data1.1[c(1,8,10,15,36),1] <- namesUp[c(3,4,5,1,6)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$year) == "2014-15"),-2]
colnames(data1.1) <- c("States.and.Union.Territories","elec_prm_14-15","elec_prm-uPr_14-15","elec_prm-hSc_14-15","elec_uPr_14-15","elec_uPr-hSec_14-15","elec_prm-sec_14-15","elec_uPr-sec_14-15","elec_sec_14-15","elec_sec-hSc_14-15","elec_hSc_14-15","elec_all_14-15")
data1.1[c(1,8,10,15,36),1] <- namesUp[c(3,4,5,1,6)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$year) == "2015-16"),-2]
colnames(data1.1) <- c("States.and.Union.Territories","elec_prm_15-16","elec_prm-uPr_15-16","elec_prm-hSc_15-16","elec_uPr_15-16","elec_uPr-hSec_15-16","elec_prm-sec_15-16","elec_uPr-sec_15-16","elec_sec_15-16","elec_sec-hSc_15-16","elec_hSc_15-16","elec_all_15-16")
data1.1[c(1,8,10,15,36),1] <- namesUp[c(3,4,5,1,6)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

#######################################################################################

data1 <- read.csv("~/datagov/Education/percentage-schools-computers.csv", stringsAsFactors = F)
data1.1 <- data1[(as.character(data1$year) == "2013-14"),-2]

namesReg <- as.vector(regions$States.and.Union.Territories)
namesData <- data1.1[,1]
namesUp <- namesReg[!(namesReg %in% namesData)]
namesData[!(namesData %in% namesReg)]

colnames(data1.1) <- c("States.and.Union.Territories","comp_prm_13-14","comp_prm-uPr_13-14","comp_prm-hSc_13-14","comp_uPr_13-14","comp_uPr-hSec_13-14","comp_prm-sec_13-14","comp_uPr-sec_13-14","comp_sec_13-14","comp_sec-hSc_13-14","comp_hSc_13-14","comp_all_13-14")
data1.1[c(1,8,10,15,36),1] <- namesUp[c(3,4,5,1,6)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$year) == "2014-15"),-2]
colnames(data1.1) <- c("States.and.Union.Territories","comp_prm_14-15","comp_prm-uPr_14-15","comp_prm-hSc_14-15","comp_uPr_14-15","comp_uPr-hSec_14-15","comp_prm-sec_14-15","comp_uPr-sec_14-15","comp_sec_14-15","comp_sec-hSc_14-15","comp_hSc_14-15","comp_all_14-15")
data1.1[c(1,8,10,15,36),1] <- namesUp[c(3,4,5,1,6)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

data1.1 <- data1[(as.character(data1$year) == "2015-16"),-2]
colnames(data1.1) <- c("States.and.Union.Territories","comp_prm_15-16","comp_prm-uPr_15-16","comp_prm-hSc_15-16","comp_uPr_15-16","comp_uPr-hSec_15-16","comp_prm-sec_15-16","comp_uPr-sec_15-16","comp_sec_15-16","comp_sec-hSc_15-16","comp_hSc_15-16","comp_all_15-16")
data1.1[c(1,8,10,15,36),1] <- namesUp[c(3,4,5,1,6)]

dataPool <- merge(data1.1, dataPool, by="States.and.Union.Territories", all.y = TRUE)

#############################################################

colIdx <- grep("Category", names(dataPool))
dataPool <- dataPool[,c(colIdx, (1:ncol(dataPool))[-colIdx])]
dataPool[(dataPool=="@")] <- NA

ind <- dataPool[which(dataPool$States.and.Union.Territories=="India"),]
dataPool <- dataPool[-which(dataPool$States.and.Union.Territories=="India"),]

regionNames <- unique(regions$Region)
for(reg in regionNames){
  for(i in 3:341){
    dataPool[,i] <- as.numeric(as.character(dataPool[,i]))
    dataPool[(dataPool$Region==reg & is.na(dataPool[,i])),i] <- mean(dataPool[dataPool$Region==reg,i], na.rm = T)
  }
}

for(i in 1:36){
  print(paste(dataPool[i,2],which(dataPool[i,]=="NaN"),dataPool[i,342],sep = ":"))
}



dataPool[c(7,19),297] <- mean(dataPool[,297][!dataPool[,297]=="NaN"])
dataPool[c(7,19),298] <- mean(dataPool[,298][!dataPool[,298]=="NaN"])
dataPool[c(7,19),299] <- mean(dataPool[,299][!dataPool[,299]=="NaN"])
dataPool[c(8:11,18:20,29),323] <- mean(dataPool[,323][!dataPool[,323]=="NaN"])
dataPool[c(8:11,18:20,29),329] <- mean(dataPool[,329][!dataPool[,329]=="NaN"])
dataPool[c(8:11,18:20,29),335] <- mean(dataPool[,335][!dataPool[,335]=="NaN"])
dataPool[c(8:11,18:20,29),341] <- mean(dataPool[,341][!dataPool[,341]=="NaN"])

which(dataPool=="NaN")

rm(data1,data1.1,data1.2,data1.3,data1w)

dataPool.pooled <- as.matrix(apply(dataPool[,336:341],MARGIN = 1,mean),nrow=36)
colnames(dataPool.pooled) <- "nsdp_const"

dataPool.pooled <- cbind(dataPool.pooled, nsdp_cur=apply(dataPool[,330:335],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, gdp_const=apply(dataPool[,324:329],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, gdp_cur=apply(dataPool[,318:323],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, growth_rate=apply(dataPool[,312:317],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, sex_ratio=apply(dataPool[,306:311],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, child_sex_ratio=apply(dataPool[,300:305],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, dropout_rate=apply(dataPool[,264:299],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, enrolment_ratio=apply(dataPool[,174:209],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, enrolment_ratio_h=apply(dataPool[,210:263],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, literacy_rate_7=apply(dataPool[,168:173],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, toilet_boy=apply(dataPool[,135:167],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, toilet_girl=apply(dataPool[,102:134],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, drinking_water=apply(dataPool[,69:101],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, electricity=apply(dataPool[,36:68],MARGIN = 1,mean) )
dataPool.pooled <- cbind(dataPool.pooled, computer=apply(dataPool[,3:35],MARGIN = 1,mean) )

###########################################################
###########################################################
###########################################################
###########################################################
###########################################################



distVec=array(dim=36)
for(x in 1:36){
  a=as.numeric(as.character(dataPool[x,3:341]))-as.numeric(as.character(ind[3:341]))
  distVec[x]=sqrt(sum((a[which(a!="NaN")])^2))
}

dataPool<-cbind(rank(-distVec),dataPool)

for(i in 1:5){
  print(dataPool[which(dataPool[,1] == i),3])
}

dataPool <- dataPool[,-1]

dataPool.normalised <- scale(dataPool[,3:341])
dataPool.normalised <- as.data.frame(cbind(dataPool[,1:2],dataPool.normalised,dataPool[,342]))


ind.normalised <- (as.numeric(ind[3:341]) - apply(dataPool[3:341], 2, mean))/apply(dataPool[3:341], 2, sd)
ind.normalised <- c(1,2,ind.normalised,342)

distVec=array(dim=36)
for(x in 1:36){
  a=as.numeric(as.character(dataPool.normalised[x,3:341]))-as.numeric(as.character(ind.normalised[3:341]))
  distVec[x]=sqrt(sum((a[which(a!="NaN")])^2))
}

dataPool.normalised<-cbind(rank(-distVec),dataPool.normalised)

for(i in 1:5){
  print(dataPool.normalised[which(dataPool.normalised[,1] == i),3])
}

dataPool.normalised <- dataPool.normalised[,-1]

###########################################################
###########################################################
###########################################################
###########################################################
###########################################################

dataPool.normalised <- as.data.frame(scale(dataPool.pooled))
corMat <- cor(dataPool.normalised)
econ <- corMat[1:4,1:4] #corr b/w gdp_cur and nsdp_cur or gdp_const and nsdp_const
demog <- corMat[5:7,5:7] #corr b/w sex_ratio and child_sex_ratio
edu <- corMat[8:16,8:16] #corr b/w toilet_boy and toilet_girl or toilet_* and drinking_water
View(corMat[1:4,5:7]) #slightly negative corr b/w child_sex_ratio and economy as a whole, no correlation between other attributes
View(corMat[1:4,8:16]) #moderate positive correlation between electricity and economy
View(corMat[5:7,8:16]) #moderate correlation b/w dropout rate and child sex ratio

############################################################
############################################################
############################################################
############################################################
############################################################

dataPool.normalised <- as.data.frame(cbind(region=dataPool$Region, dataPool.normalised))
feature.eval1 <- attrEval(region~., dataPool.normalised[,c(1,2:5)], estimator = "ReliefFequalK", ReliefIterations = 20)
feature.eval1
feature.eval2 <- attrEval(region~., dataPool.normalised[,c(1,6:8)], estimator = "ReliefFequalK", ReliefIterations = 20)
feature.eval2
feature.eval3 <- attrEval(region~., dataPool.normalised[,c(1,9:17)], estimator = "Relief", ReliefIterations = 10)
feature.eval3
feature.eval4 <- attrEval(region~., dataPool.normalised[,c(1,5,6,8:10,16)], estimator = "ReliefFequalK", ReliefIterations = 30)
feature.eval4

############################################################
############################################################
############################################################
############################################################
############################################################

plot(dataPool.normalised$nsdp_cur, dataPool.normalised$gdp_cur, 
     xlab= "NSDP (Current)",
     ylab= "GDP (Current)",
     col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)

posvec <- rep(c(1,2,3,4),36)
text(dataPool.normalised$nsdp_cur, dataPool.normalised$gdp_cur, labels=dataPool$States.and.Union.Territories, cex= 0.4, pos = posvec)

plot(dataPool.normalised$growth_rate, dataPool.normalised$child_sex_ratio, 
     xlab= "Growth Rate",
     ylab= "Child Sex Ratio",
     col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)

posvec <- rep(c(1,2,3,4),36)
text(dataPool.normalised$growth_rate, dataPool.normalised$child_sex_ratio, labels=dataPool$States.and.Union.Territories, cex= 0.4, pos = posvec)

plot(dataPool.normalised$enrolment_ratio, dataPool.normalised$computer, 
     xlab= "Enrolment Ratio",
     ylab= "Computer",
     col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)

posvec <- rep(c(1,2,3,4),36)
text(dataPool.normalised$enrolment_ratio, dataPool.normalised$computer, labels=dataPool$States.and.Union.Territories, cex= 0.4, pos = posvec)

plot(dataPool.normalised$gdp_cur, dataPool.normalised$child_sex_ratio, 
     xlab= "GDP (Current)",
     ylab= "Child Sex Ratio",
     col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)

posvec <- rep(c(1,2,3,4),36)
text(dataPool.normalised$gdp_cur, dataPool.normalised$child_sex_ratio, labels=dataPool$States.and.Union.Territories, cex= 0.4, pos = posvec)




qqplot(dataPool.normalised$nsdp_cur, dataPool.normalised$gdp_cur, 
       xlab= "NSDP (Current)",
       ylab= "GDP (Current)",
       col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)

qqplot(dataPool.normalised$growth_rate, dataPool.normalised$child_sex_ratio, 
       xlab= "Growth Rate",
       ylab= "Child Sex Ratio",
       col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)

qqplot(dataPool.normalised$enrolment_ratio, dataPool.normalised$computer, 
       xlab= "Enrolment Ratio",
       ylab= "Computer",
       col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)

qqplot(dataPool.normalised$gdp_cur, dataPool.normalised$child_sex_ratio, 
       xlab= "GDP (Current)",
       ylab= "Child Sex Ratio",
       col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)

############################################################
############################################################
############################################################
############################################################
############################################################

dataPool.pooled <- as.data.frame(dataPool.pooled)
quantile(dataPool.pooled$nsdp_cur, c(0.05,0.95))
min(dataPool.pooled$nsdp_cur)
max(dataPool.pooled$nsdp_cur)

quantile(dataPool.pooled$child_sex_ratio, c(0.05,0.95))
min(dataPool.pooled$child_sex_ratio)
max(dataPool.pooled$child_sex_ratio)
