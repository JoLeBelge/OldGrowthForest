# comparison of field inventory with remote sensing derived information
require(RSQLite)
library(terra)
require(sf)
library("dplyr")
library(magrittr)

basedir <- "/home/jo/Documents/OGF/OGF_Wallonia_forest_plots"
setwd(basedir)
db.path <- paste0(basedir,"/data/OGF_all.db")
db <- dbConnect(SQLite(),dbname=db.path)
key_ue_cols <- c("ues_id_ogf", "ues_id_ue")
key_ue_cols2 <- c("id_ogf", "id_ue")

arbre<- dbReadTable(db ,"arbre")
ues <- dbReadTable(db ,"ues") %>% mutate(date=as.Date(paste(date_year,date_month, date_day,sep="-")))

# jointure de la couche gnss arbres avec la table des relevés terrains arbres - il s'agit uniquement des arbres de plus de 240 de circ
path.arbres.gnss <- paste0(basedir,"/data/ogf_gnss.gpkg")
a.gnss=st_read(path.arbres.gnss,layer="placette_arbre") %>% st_transform(31370) %>% mutate(id_arbre=as.integer(id_arbre),id_ue=as.integer(id_ue),id_ogf=as.integer(id_ogf)) %>% select(id_arbre,id_ue,id_ogf)
a.gnss <- left_join(a.gnss,arbre,by=join_by("id_arbre"=="X_arbre_position", "id_ue"=="ues_id_ue", "id_ogf"=="ues_id_ogf"))
# je vais ajouter la date pour pouvoir faire rajeunir mes arbres jusqu'en 2021
a.gnss %<>% left_join(ues,by=join_by("id_ue"=="id_ue", "id_ogf"=="id_ogf"))

#### altitude (m) pour modèle de croissance
path_dtm <- "//home/jo/Documents/Carto/MNT/MNT_10m_WALLONIA.tif"
dtm <- read_stars(path_dtm)
alts <- st_extract(dtm, a.gnss %>% st_transform(st_crs(dtm)))
a.gnss$dtm_altitude <- alts[, 1, drop = T] %>% as.numeric()
st_write(a.gnss,"/home/jo/Documents/OGF/comac20260310/a.gnss.gpkg",layer="placette_arbre")

# je sélectionne les gros arbres qui sont assez éloignés les uns des autres pour pouvoir les utiliser pour le modèle Dendro_Lidar CDOM
a.gnss %<>% filter(circ>140, statut==1)
a.gnss$keep <- 1
for (i in 1:nrow(a.gnss)){
  if (a.gnss$keep[i]){
inter <- st_intersection(st_buffer(a.gnss[i,],10),st_buffer(a.gnss[-i,],10)) %>% filter(keep.1==1)
if (nrow(inter)>0){
  # supprimer l'arbre i si ce n'est pas le plus gros.
  if (a.gnss$circ[i]<max(inter$circ.1)){
    a.gnss$keep[i] <- 0
  }
}
  }
}
# harmonisation et synchronisation pour usage dans dendro_lidar de NL
trees <- a.gnss %>% filter(keep==1) %>% mutate(
  C150_ = circ,
  CDOM = circ,
  groupess_JP = case_when(ess =="HE" ~ "B_Bee", ess =="FR" ~ "D_Ash", T ~ "A_Oak"),
  fext_ha = 10,
  FERE = "FE",
  plot_radius = 18,
  id_plot = str_c(id_arbre, "_",id_ue, "_", id_ogf),
  source = "ogf",
  prop_FE=1.0,
  prop_RE=0.0,
  inventory_date=date
)
simreg_dico_ess <- read_csv2("/mnt/gf009pc057_data3/NICOLAS/RemoteSensing/IFA/dico_ess_simreg.csv", locale = readr::locale(encoding = "LATIN1"))
simreg_params <- read.csv2("/mnt/gf009pc057_data3/NICOLAS/RemoteSensing/IFA/simreg.csv", sep = ";", dec = ".")
ModIg <- function(C150, A, P, m) {
  P * (C150 - m * A + ((m * A + C150)^2 - 4 * A * C150)^0.5) / 2
}
ModIc <- function(C150, A, P, m) {
  ((C150^2 + 4 * pi * ModIg(C150, A, P, m))^0.5 - C150)
}
ModIc_corr <- function(C150, A, P, m, dCcorr) {
  dCcorr * ModIc(C150, A, P, m)
}

trees %<>% left_join(simreg_params, by = join_by(groupess_JP == Sp))
trees %<>% mutate(G = !!pi * (C150_ / (100 * 2 * !!pi))^2 * fext_ha)
trees %<>% mutate(c150_growth = ModIc_corr(C150_, P = Pa * exp(1 - Pb * dtm_altitude), A = Aa * CDOM^Ab, m = 1 + exp(ma - mb * G), dCcorr))
trees %<>% mutate(diff_date_years = (difftime(date(inventory_date), date("2021-06-01"), units = "days") %>% as.numeric()) / 365.25)
trees %<>% mutate(diff_circonf = diff_date_years * c150_growth, new_c150 = C150_ - diff_circonf)
trees %<>% mutate(new_cdom = new_c150, new_hdom=30,new_gha_FE=10,new_gha_RE=0,new_vha_FE=500,new_vha_RE=0,new_nha_FE=1000,new_nha_RE=0)
trees %<>% st_transform(st_crs(3812)) %>% rename(geom_plot = geom) 
trees %>% saveRDS(str_c("plots_trees_coppices_ogf.RData"))


# comparaison donnée terrain et donnée télédétection
# merge donnée sig et terrain (SetT pour SigEtTerrain) pour une comparaison. C'est pas beau à voir.
#ue_gnss <- dbReadTable(db ,"ue_gnss")

ue_gnss <- st_read("data/ogf_gnss_bl72.gpkg")

dendro <- dbReadTable(db ,"dendro_plot")

sEtT <- merge(ue_gnss,dendro,by.x=key_ue_cols2,by.y=key_ue_cols, all=F)

pngOut <- "/home/jo/Documents/OGF/out/validCarteDendro_"
lim <- c(100,310)
png(paste0(pngOut,"cdom202602.png"), width = 7.75, height = 5.75, res = 300, units = "in")
plot(sEtT$cdom, sEtT$dendro_cdom_202602, xlim=lim, ylim=lim,main="validation carte cdom 2026 02. red= 202601", ylab="cdom neural network [cm]", xlab="cdom terrain [cm]")
# dendro_cdom : la couche de 202601 ()
points(sEtT$cdom, sEtT$dendro_cdom, xlim=lim, ylim=lim,col="red")

lines(lim,lim, type = "l",lwd=2, col="red")
dev.off()

lim <- c(200,600)
png(paste0(pngOut,"vha.png"), width = 7.75, height = 5.75, res = 300, units = "in")
plot(sEtT$vol_alive, sEtT$dendro_vha, xlim=lim, ylim=lim,main="validation carte dendro - vha", ylab="vha Net [m3/ha]", xlab="vha terrain [m3/ha]")
lines(lim,lim, type = "l",lwd=2, col="red")
dev.off()
lim <- c(10,60)
png(paste0(pngOut,"gha.png"), width = 7.75, height = 5.75, res = 300, units = "in")
plot(sEtT$basal_area_alive, sEtT$dendro_gha, xlim=lim, ylim=lim,main="validation carte dendro - gha", ylab="gha Net [m2/ha]", xlab="gha terrain arbre vivant [m2/ha]")
lines(lim,lim, type = "l",lwd=2, col="red")
dev.off()


associate_segment_to_tree=function(i){
  require(sf)
  id_ogf <- a.gnss$id_ogf[i]
  file_segm <- paste0("/home/jo/Documents/OGF/output/bigtrees_in_parcels/bigtrees_in_parcels/parc_",id_ogf,".gpkg")
  if(!file.exists(file_segm)){
    return()
  }
  segms=st_read(file_segm,layer=paste0("parc_",id_ogf))
  apices=sf::st_as_sf(sf::st_drop_geometry(segms), coords = c("x","y"), crs = 31370)
  apices=st_intersection(apices,st_buffer(a.gnss[i,],30))
  apices$d2tr=as.numeric(st_distance(apices,a.gnss[i,]))
  apices=apices[order(apices$d2tr),]
  ap0=apices[1,]
  return(ap0)
}

library(future)
a.gnns.segm = NULL
plan(multisession, workers = 4)
a.gnns.segm <- 1:nrow(a.gnss) %>%
  furrr::future_map(.f = associate_segment_to_tree, .progress = T) %>% bind_rows()
nrow(a.gnns.segm)

# comparaison circ Terrain avec c150 predit (model PL 2025-12)
a.match <- a.gnns.segm[!is.na(a.gnns.segm$circ) & a.gnns.segm$statut==1,]

# 180 arbres
sqrt(mean((a.match$circ - a.match$c150pred)^2))
# [1] 79.82258
mean(abs(a.match$circ - a.match$c150pred))
# [1] 69.98889
cor(a.match$circ, a.match$c150pred)
# [1] 0.2537671
plot(a.match$circ,a.match$c150pred)

st_write(a.match[,colnames(a.match) !=("FID")],"/home/jo/Documents/OGF/output/bigtrees_in_parcels/matchTer.gpkg",delete_layer=T)
