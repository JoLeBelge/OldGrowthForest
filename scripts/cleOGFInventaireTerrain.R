setwd("/home/jo/Documents/OGF/fromLeaProtocoleOpe/surveyOperationnel")

require(dplyr)
require(tibble)

d <- read.csv2("ogf_terrain_ope.csv")
d$surf <- as.numeric(d$surf)

# une ligne pour la moyenne par peuplement
#d <- d %>% add_row(tibble_row(MAT_nGB = mean(d$MAT_nGB),MAT_nMB = mean(d$MAT_nMB),Bmd_nG = mean(d$Bmd_nG), Bmd_nM = mean(d$Bmd_nM),Bms_nG = mean(d$Bms_nG),Bms_nM = mean(d$Bms_nM)))
vars <- c("MAT_nGB","MAT_nMB", "Bmd_nG", "Bmd_nM", "Bms_nG", "Bms_nM")
d[!is.na(d$surf),colnames(d) %in% vars] <- d[!is.na(d$surf),colnames(d) %in% vars] / (3*d$surf[!is.na(d$surf)])

rel_stand_level <- d %>% group_by(id_ogf) %>% summarise(MAT_nGB = mean(MAT_nGB),MAT_nMB = mean(MAT_nMB),Bmd_nG = mean(Bmd_nG), Bmd_nM = mean(Bmd_nM),Bms_nG = mean(Bms_nG),Bms_nM = mean(Bms_nM))
rel_stand_level$id_ogf <- paste0("stand_",rel_stand_level$id_ogf)
d <- d %>% add_row(rel_stand_level)
# pour l'inventaire ne plein , division par la surfacex3 pour faire comme si c'était à l'échelle de la placette de 1/3 hectare


# init score puis compute score
d$sMAT <- 0 
d$sBMd <- 0
d$sBMs <- 0
d$sBM <- 0
d$OGF <- "NON OGF"



d$sMAT[d$MAT_nGB==0 & d$MAT_nMB>=3] <- 1
d$sMAT[d$MAT_nGB==1] <- 2
d$sMAT[d$MAT_nGB>1 & d$MAT_nGB<5] <- 3
d$sMAT[d$MAT_nGB>=5] <- 5

d$sBMd[d$Bmd_nG==0 & d$Bmd_nM>0] <- 1
d$sBMd[d$Bmd_nG==1] <- 2
d$sBMd[d$Bmd_nG>1] <- 5

d$sBMs[d$Bms_nG==0 & d$Bms_nM>0] <- 1
d$sBMs[d$Bms_nG==1] <- 2
d$sBMs[d$Bms_nG>1 & d$Bms_nG<4] <- 3
d$sBMs[d$Bms_nG>=4 ] <- 5

d$sBM <- d$sBMd+d$sBMs

# ready for the OGF key

d$OGF[d$sMAT < 2 & d$sBM <2] <- "NON OGF"
d$OGF[d$sMAT ==2  & d$sBM ==2 ] <- "OGF en installation" #& d$sBM <=3
d$OGF[d$sMAT >= 3 & d$sBM >=3] <- "OGF mature"

write.csv2(d,"ogf_inv_ope_cleOGFresult.csv")
