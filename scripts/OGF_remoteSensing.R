# comparison of field inventory with remote sensing derived information
library(sf)
library(terra)

# comparaison donnée terrain et donnée télédétection
# merge donnée sig et terrain (SetT pour SigEtTerrain) pour une comparaison. C'est pas beau à voir.

sEtT <- merge(ue_gnss,dendro,by.x=key_ue_cols2,by.y=key_ue_cols, all=F)

pngOut <- "/home/jo/Documents/OGF/out/validCarteDendro_"
lim <- c(100,310)
png(paste0(pngOut,"cdom.png"), width = 7.75, height = 5.75, res = 300, units = "in")
plot(sEtT$cdom, sEtT$dendro_cdom, xlim=lim, ylim=lim,main="validation carte dendro - cdom", ylab="cdom CNN [cm]", xlab="cdom terrain [cm]")
lines(lim,lim, type = "l",lwd=2, col="red")
dev.off()
lim <- c(200,600)
png(paste0(pngOut,"vha.png"), width = 7.75, height = 5.75, res = 300, units = "in")
plot(sEtT$vol_alive, sEtT$dendro_vha, xlim=lim, ylim=lim,main="validation carte dendro - vha", ylab="vha CNN [m3/ha]", xlab="vha terrain [m3/ha]")
lines(lim,lim, type = "l",lwd=2, col="red")
dev.off()
lim <- c(10,60)
png(paste0(pngOut,"gha.png"), width = 7.75, height = 5.75, res = 300, units = "in")
plot(sEtT$basal_area_alive, sEtT$dendro_gha, xlim=lim, ylim=lim,main="validation carte dendro - gha", ylab="gha CNN [m2/ha]", xlab="gha terrain arbre vivant [m2/ha]")
lines(lim,lim, type = "l",lwd=2, col="red")
dev.off()

# jointure de la couche gnss arbres avec la table des relevés terrains arbres - il s'agit uniquement des arbres de plus de 240 de circ
path.arbres.gnss <- "/home/jo/Documents/OGF/data/ogf_terrain_v3.0.gpkg"
a.gnss=st_read(path.arbres.gnss,layer="placette_arbres")
a.gnss=st_transform(a.gnss,31370)
a.gnss <- left_join(a.gnss,arbre,by=join_by("id_arbre"=="X_arbre_position", "id_ue"=="ues_id_ue", "id_ogf"=="ues_id_ogf"))

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
