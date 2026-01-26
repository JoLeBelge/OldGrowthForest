# old growth forest characterization and mapping in Wallonia, 2025 - Gembloux Agro-Bio Tech
# This script process field plot inventory data to compute dendrometric parameters with a focus on dead wood and large trees
require(RSQLite)

library(tools)
library("readxl")
library(dplyr)

basedir <- "/home/jo/Documents/OGF/OGF_Wallonia_forest_plots"
setwd(basedir)

# script qui contient la ou les fonctions utilisées , ici le tarif de cubage.
source(paste0(basedir,"/scripts/utils_OGF.R"))

# les dictionnaires on les ajoute à priori une seule fois
path.dico.tables <- "/home/jo/Documents/OGF/collect/dico"

# Open foris collect is used as form in a mobile application to collect all the dendrometric measurements, exept the one related to the GNSS
# We export open foris data from the server in csv format and then we integrate them in the sqlite database
# un dossier pour les entry, un pour les cleansing
path.entry.collect.list <- paste0("/home/jo/Documents/OGF/collect/collect-C-2026-01-06")
# quand on reçoit un nouvel encodage via le formulaire web, on l'ajoute à la bd OGF_all
path.ogf.encodage.toadd <- "/home/jo/Documents/OGF/data/OGF20260108.db"

db.path <- paste0(basedir,"/data/OGF_all.db")
db <- dbConnect(SQLite(),dbname=db.path)

# encodage via formulaire web
if (file.exists(path.ogf.encodage.toadd)){
  dbNewOGF <- dbConnect(SQLite(),dbname=path.ogf.encodage.toadd)
  new.ogf<- dbReadTable(dbNewOGF ,"ogf")
  dbDisconnect(dbNewOGF) 
  dbWriteTable(db,"ogf",new.ogf, overwrite=T)
}

# check que tout les dictionnaires sont dans la BD. Si pas, je les ajoute
for (dico.file in list.files(path.dico.tables)){
  if (!file_path_sans_ext(dico.file) %in% dbListTables(db)){
  if ( file_ext(dico.file)=="xlsx"){
    dico_table <- read_excel(paste0(path.dico.tables,"/",dico.file))
    cat(paste("ajout table ", file_path_sans_ext(dico.file),"\n"))
    dbWriteTable(db,file_path_sans_ext(dico.file),dico_table)
  }
  if ( file_ext(dico.file)=="csv"){
    dico_table <- read.csv2(paste0(path.dico.tables,"/",dico.file))
    cat(paste("ajout table ", file_path_sans_ext(dico.file),"\n"))
    dbWriteTable(db,file_path_sans_ext(dico.file),dico_table)
  }
  }
}
# check que toutes les données terrain encodées avec collect sont dans la BD. 
for (path.entry.collect in path.entry.collect.list){
if (file.exists(path.entry.collect)){
for (entry.file in list.files(path.entry.collect)){
  # si la table n'existe pas, on l'ajoute.
  entry_table <- read.csv2(paste0(path.entry.collect,"/",entry.file), sep=",", dec=".")
  if (file_ext(entry.file)=="csv" & !file_path_sans_ext(entry.file) %in% dbListTables(db)){
      dbWriteTable(db,file_path_sans_ext(entry.file),entry_table)
    cat(paste("ajout de la table ", file_path_sans_ext(entry.file),"\n"))
  } else {
    # si la table existe, on vérifie que toute les observations y sont déjà - sinon ajout
    entry.db <- dbReadTable(db ,file_path_sans_ext(entry.file))
    new.record <- setdiff(entry_table,entry.db[colnames(entry.db) %in% colnames(entry_table)])
    if (nrow(new.record)>0){
    # ajout des colonnes qui ne sont pas présentent dans "collect"
    for (newcol in colnames(entry.db)[!colnames(entry.db) %in% colnames(entry_table)]){
        new.record$newcol <- 0
        if (newcol=="A5"){new.record$newcol <- 1}
        colnames(new.record)[ncol(new.record)] <- newcol
    }
    cat(paste("ajout ",nrow(new.record), " observations dans table ", file_path_sans_ext(entry.file),"\n"))
    dbWriteTable(db,file_path_sans_ext(entry.file),new.record,append=T)
    }
  }
}
} else {
  cat(paste0("directory ", path.entry.collect, " does not exist\n"))
}
}

# lecture du contenu des tables
ogf<- dbReadTable(db ,"ogf")
dico_cubage<- dbReadTable(db ,"dico_cubage")
dico_cubage_branche<- dbReadTable(db ,"dico_cubage_branche")
dico_ess_cub<- dbReadTable(db ,"dico_essence_cubage")
dico_ess<- dbReadTable(db ,"essence_list")
arbre<- dbReadTable(db ,"arbre")
cohorte <- dbReadTable(db ,"cohorte_arbre")
ues <- dbReadTable(db ,"ues")
# check qu'il y ai bien une seule ligne par UE
if(nrow(unique(ues[,c("id_ue", "id_ogf")]))!=nrow(ues)){
  cat("vérifier la table ues, certaines placettes apparaissent en doublons")
}

# FAS is realized on the 18m plot. logs > 90 cm
dt_FAS <- dbReadTable(db ,"bois_mort_placette")
# LIS is realized for 3 transects of 27 meters. 30 cm < logs < 90 cm
dt_LIS <- dbReadTable(db ,"bois_mort_transect")

dendro <- dbReadTable(db ,"dendro_plot")


ue_date <- as.Date(paste(ues$date_year,ues$date_month, ues$date_day,sep="-"))
ues$lLIS <- 27
ues$lLIS[ue_date < as.Date("2025-12-1")] <- 15

arbre$A5bool <- arbre$a5=="true"

# calcul du volume des arbres sur pied
arbre$v <- 0
arbre$v_branches <- 0
for (i in 1:nrow(arbre)){
  arbre$v_branches[i] <- vBranches(arbre$circ[i],dico_ess_cub$numtarif[dico_ess_cub$abreviation == arbre$ess[i]])
  # arbre vivant
if (arbre$statut[i]==1){
  arbre$v_tc1[i] <- tc( arbre$circ[i],dico_ess_cub$numtarif[dico_ess_cub$abreviation == arbre$ess[i]])
  
} else {
  # arbre mort sur pied: soit c'est une chandelle et j'ai sa hauteur pour le cubage, soit j'ai encore le houppier et je cube comme pour un arbre vivant.
  arbre$v_branches[i] <- vBranches(arbre$circ[i],dico_ess_cub$numtarif[dico_ess_cub$abreviation == arbre$ess[i]])
  # identification des chandelles pour la première partie de l'inventaire ; je devrais comparer la hauteur prédite par allométrie avec celle mesurée.
  if (!is.na(arbre$h[i])){
  if (arbre$h[i]>15){
  arbre$v_tc1[i] <- tc( arbre$circ[i],dico_ess_cub$numtarif[dico_ess_cub$abreviation == arbre$ess[i]]) 
  # at the beginning of the survey we have not measured the crown volume reduction and we have estimated a tree height even for dead tree that still have a crown. I have to deal with that
  if (is.na(arbre$cvr[i])){
  arbre$v_branches[i] <- 0
  }
  
  } else {
    # cylindre : pi r2 * h. Je devrais plutôt utiliser un défilement car un gros bois de 300 de tour ça ne fait pas un cylindre de 15 mètres de hauteur..
    # tree taper equation from IPRFW - hardwood only
    taper <- 5
    # ça c'est les hypothèses de JL
    if (arbre$circ[i]>200){taper <- 15}
    if (arbre$circ[i]<100){taper <- 2.5}
    
    # demi-hauteur là ou se situe la circ du milieu
    hmil = arbre$h[i] * 0.5 - 1.5 # attention, 1.5 c'est la hauteur de mesure
    cmil = arbre$circ[i] - hmil * taper # donc l'arbre perd 5 cm de circonférence par mètre de hauteur. Rondeux : pour la circonférence, le défilement varie généralement entre 0,5 et 5 cm/m
    if (cmil>arbre$circ[i]){cmil<-arbre$circ[i]}
    arbre$v[i] <- ((cmil/100)^2)/(4*pi) * arbre$h[i]
    cat(paste("chandelle num ", i, " de hauteur", arbre$h[i], " et de circ ", arbre$circ[i]," ratio vol cylindre sans défilement sur cylindre avec défilement : ", round(100*(cmil/100)^2/((arbre$circ[i]/100)^2),0 ),"\n"))
      arbre$v_branches[i] <-0
  }
  } else {
    cat(paste("Pas de hauteur pour arbre mort numéro", i," de ", arbre$circ[i] , " de tour, ogf ",arbre$ues_id_ogf[i], " ue ", arbre$ues_id_ue[i], " \n"))
    arbre$v_tc1[i] <- tc( arbre$circ[i],dico_ess_cub$numtarif[dico_ess_cub$abreviation == arbre$ess[i]])   
  }
  
  # si j'ai un crownVolumeReduction renseigné, je l'applique
  if (!is.na(arbre$cvr[i]) & arbre$cvr[i]>0){
  arbre$v_branches[i] <- arbre$v_branches[i]*(1-arbre$cvr[i]/100)
  }
  
}
}

arbre$v[arbre$v==0 & !is.na(arbre$v_tc1)] <- arbre$v_tc1[arbre$v==0 & !is.na(arbre$v_tc1)] + arbre$v_branches[arbre$v==0 & !is.na(arbre$v_tc1)]

# facteur d'extension pour les 3 parcelles concentriques - arbres vivants
feA3 <- 10000/(pi*9^2)
feA4 <- 10000/(pi*18^2)
feA5 <- 10000/(pi*30^2)
arbre$fe[arbre$statut==1  & arbre$circ>=240] <- feA5 # A5
arbre$fe[arbre$statut==1  & arbre$circ>=120 & arbre$circ<240] <- feA4   # A4
arbre$fe[arbre$statut==1  & arbre$circ<120] <- feA3 # A3
arbre$fe[arbre$A5bool==0] <- 0 # pour repérer les arbres dominants de moins de 240 de tour qu'on a mesuré à une distance de plus de 18 mètres du centre de placette

# facteur d'extension pour les arbres mort sur pied : il sont pris dans la placette de 30 mètres de rayon - et ce n'est pas le même seuil d'inventaire que pour les vivants : 30 cm de circ plutôt que 40
arbre$fe[arbre$statut==2  & arbre$circ>=40] <- feA5
arbre$fe[arbre$statut==2  & arbre$circ<40] <- 0

for (i in 1:nrow(cohorte)){
  # je n'ai pas l'essence, donc il me faut une essence par défaut pour le tarif de cubage. Feuillus divers
  if (cohorte$grosseur[i]==1){
    cohorte$circ[i] <- 30
    cohorte$v[i] <- 0 # car sous le seuil d'inventaire des IFA "classique"
  } else {
    cohorte$circ[i] <- cohorte$grosseurs[i]*40-20
    cohorte$v[i] <- cohorte$nombre[i] * tc(cohorte$circ[i],11) 
  }
}

# tronc de cone pour bois mort FAS 
#dt_FAS$v <- (pi*(dt_FAS$longueur)/12)*((dt_FAS$circ1/(100*pi))^2 +(dt_FAS$circ2/(100*pi))^2 +(dt_FAS$circ1/(100*pi))*(dt_FAS$circ2/(100*pi)))
# Smalian’s formula for truncated cones
dt_FAS$v <- (pi*(dt_FAS$longueur)/2)*((dt_FAS$circ1/(pi*200))^2 +(dt_FAS$circ2/(pi*200))^2)

# calcul du volume des arbres mort au sol LIS, Van Wagner 1968
# en m3/m2 :  v <- pi^2/8*(Longeur) diameter^2
key_ue_cols <- c("ues_id_ogf", "ues_id_ue")
key_ue_cols2 <- c("id_ogf", "id_ue")
lLIS <- merge(dt_LIS,ues[,c(key_ue_cols2,"lLIS")],by.x=key_ue_cols, by.y=key_ue_cols2 , all=F)
# une branche de 76 de tour compte pour 16 m2 / ha si les transects font 15m... ça monte vite donc.
# dans metadata de vandeKerkhove -> volume of lying deadwood per ha (m³/ha);  small fragments excluded (diameter < 10 cm or length < 1m; if diameter > 20 cm, length < 0,5m)
dt_LIS$v <- 10000 * ((pi^2)/(8*(3*lLIS$lLIS)))*(dt_LIS$circ/(pi*100))^2

# les noms des variables dendro se rapprochent autant que ce peux de celles utilisées par Vandekerkhove
dendro_arbre_vivant <- arbre[arbre$statut==1,] %>% group_by(ues_id_ogf,ues_id_ue) %>% summarise(number_of_trees_thres120=sum(fe),vol_alive_thres120 = sum(v_tc1*fe), basal_area_alive_thres120=sum((circ/100)^2*fe/(4*pi)))
dendro_arbre_mort <- arbre[arbre$statut==2,] %>% group_by(ues_id_ogf,ues_id_ue) %>% summarise(vol_dead_standing = sum((v)*fe), basal_area_dead=sum((circ/100)^2*fe/(4*pi)))
dendro_cohorte <- cohorte %>% group_by(ues_id_ogf,ues_id_ue) %>% summarise(number_of_trees_co=sum(nombre*feA3), vol_alive_co = sum(v*feA3), basal_area_alive_co=sum(nombre*(circ/100)^2*feA3/(4*pi)))
dendro_FAS <- dt_FAS %>% group_by(ues_id_ogf,ues_id_ue) %>% summarise(vol_wood_debris_FAS = sum(v*feA4))
dendro_LIS <- dt_LIS %>% group_by(ues_id_ogf,ues_id_ue) %>% summarise(vol_wood_debris_LIS = sum(v))

#création matrice ogf x essence pour proportion de chaque essence dans l'UE
e <- unique(arbre$ess)
# check que les code essences sont dans le dictionnaire
#e[!e %in% dico_ess$essence_code]

gha_rel <- data.frame(matrix(0,ncol=2+length(e),nrow=nrow(ues), dimnames=list(NULL,c(key_ue_cols, e))))
gha_rel[,key_ue_cols] <- ues[, c("id_ogf", "id_ue")]
# un gha par essence
gha_ess <- arbre[arbre$statut==1,] %>% group_by(ues_id_ogf,ues_id_ue, ess) %>% summarise(gha_rel = sum((circ/100)^2*fe/(4*pi)))
# on divise par le gha (presque) total (celui des arbres de plus de 120)
for (i in 1:nrow(gha_ess)){
  ogf <- gha_ess$ues_id_ogf[i]
  ue <- gha_ess$ues_id_ue[i]
  gha_ess$ess[i]
  gha_rel[gha_rel$ues_id_ogf==ogf & gha_rel$ues_id_ue==ue, gha_ess$ess[i]] <-  100*gha_ess$gha_rel[i]/dendro_arbre_vivant$basal_area_alive_thres120[dendro_arbre_vivant$ues_id_ogf==ogf & dendro_arbre_vivant$ues_id_ue==ue]
}
gha_rel$essmaj <- apply(gha_rel[,!colnames(gha_rel) %in% c(key_ue_cols,"essmaj")], 1, twoEssMaj)

# fusion de toutes ces données en une table dendro - group by UE
dendro1 <- merge(dendro_arbre_vivant,dendro_cohorte,by=key_ue_cols, all=T)
dendro2 <-  merge(dendro1,dendro_arbre_mort,by=key_ue_cols, all=T)
dendro3 <-  merge(dendro2,gha_rel[,c(key_ue_cols,"essmaj")],by=key_ue_cols, all=T)
dendro4 <-  merge(dendro3,dendro_FAS,by=key_ue_cols, all=T)
dendro <-  merge(dendro4,dendro_LIS,by=key_ue_cols, all=T)


dendro[is.na(dendro)] <- 0
#, _co pour cohorte = arbres sous le seuil d'inventaire de 120 de circ. _thres120 pour les arbres de la table arbres
dendro$number_of_trees <- dendro$number_of_trees_thres120 + dendro$number_of_trees_co
dendro$vol_alive <- dendro$vol_alive_thres120 + dendro$vol_alive_co
dendro$vol_deadw <- dendro$vol_dead_standing + dendro$vol_wood_debris_FAS + dendro$vol_wood_debris_LIS
dendro$basal_area_alive <- dendro$basal_area_alive_thres120 + dendro$basal_area_alive_co
# circonférence dominante: je prends les 5 plus gros arbres
nDomTree <- 5
for (i in 1:nrow(dendro)){
  cond <- which(arbre$ues_id_ogf==dendro$ues_id_ogf[i] & arbre$ues_id_ue==dendro$ues_id_ue[i] & arbre$statut==1)
  c <-  sort(arbre[cond,"circ"], decreasing=T)
  dendro$cdom[i] <- mean(c[1:min(nDomTree,length(c))])
}

dendro$cdom_1 <- 
  arbre[arbre$statut==1,] %>% group_by(ues_id_ogf,ues_id_ue) %>% summarise(cdom1=cdom_1(pick()))

require("purrr")
arbre[arbre$statut==1,] %>% group_by(ues_id_ogf,ues_id_ue) %>% mutate(temp = map(data, cdom_1)) 

dendro$vol_dead_standing_ratio <- 100*dendro$vol_dead_standing/(dendro$vol_alive+dendro$vol_dead_standing)

# réordonner les colonnes pour plus de logique et de lisibilité
colOrder <- c("ues_id_ogf","ues_id_ue","essmaj","number_of_trees_co","number_of_trees_thres120","number_of_trees","basal_area_alive_thres120","basal_area_alive_co","basal_area_alive","basal_area_dead","vol_alive_co","vol_alive_thres120","vol_alive","vol_dead_standing","vol_dead_standing_ratio","vol_wood_debris_FAS","vol_wood_debris_LIS","vol_deadw","cdom")
dendro <- dendro[,colOrder]
# ajout mesure gha par relascope pour comparaison avec gha calculé
dendro <- merge(dendro,ues[,c(key_ue_cols2,"gha_relascope")],by.x=key_ue_cols, by.y=key_ue_cols2 , all=F)

# STAND LEVEL
var <- c("number_of_trees","basal_area_alive","basal_area_dead","vol_alive","vol_dead_standing","vol_wood_debris_FAS","vol_wood_debris_LIS","vol_deadw")

standard_error <- function(x) {
  sd(x)/sqrt(length(x))
}
dendro_stand <- dendro %>% group_by(ues_id_ogf) %>% summarise(across(any_of(var), list(mean = mean, standard_error= standard_error), .names = "{.col}_{.fn}"))

# writing results in the database
dbWriteTable(db,"arbre",arbre, overwrite=T)
dbWriteTable(db,"bois_mort_transect",dt_LIS, overwrite=T)
dbWriteTable(db,"bois_mort_placette",dt_FAS, overwrite=T)
dbWriteTable(db,"dendro_plot",dendro, overwrite=T)
dbWriteTable(db,"dendro_stand",dendro_stand, overwrite=T)
dbDisconnect(db) 
rm(db)

# data flandre
plot <- read.csv2("/home/jo/Documents/OGF/FLANDERS_stat_per_UE/plotinfo.csv")
plot_dendro <- read.csv2("/home/jo/Documents/OGF/FLANDERS_stat_per_UE/plotlevel_data/dendro_by_plot.csv")
# sub_dendro <- plot_dendro[plot_dendro$plot_id %in% plot$plot_id[plot$forest_reserve=="Kersselaerspleyn"] & plot_dendro$year==2020,]
sub_plot <- plot[plot$forest_reserve=="Kersselaerspleyn" & plot$period==3,]

fl1 <- cbind(sub_plot,sub_dendro)
write.csv2(fl1,"/home/jo/Documents/OGF/FLANDERS_stat_per_UE/Kersselaerspleyn_dendro.csv",row.names =FALSE)
sub_dendro <- plot_dendro[plot_dendro$year==2020,]

# the 3 plots we have surveyed in Soignes forest with INBO team
plot_dendro[plot_dendro$plot_id %in% c(2014,2015,2047) & plot_dendro$period==3,]

library(reshape2)
dw <- dendro[,c("vol_dead_standing","vol_wood_debris_FAS", "vol_wood_debris_LIS", "vol_deadw")]
melted <- melt(dw)
boxplot(data=melted, value~variable, ylim=c(0,200))
summary(dw)

./carteApt --outils 1 --gpkg_layer "centre_placettes" --gpkg "/home/jo/Téléchargements/ogf.gpkg" --layerCode dendro_cdom --pathBD "/home/jo/Documents/carteApt/GIS/dendro202601/aptitudeEssDB.db"
