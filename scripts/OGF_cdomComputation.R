# NLatte, détermination du CDOM sur base de 100 arbres
library(magrittr)
require(sf)

n_tree_cdom <- 10
plots_trees_coppices$C150_ <- plots_trees_coppices$circonf

plots_trees_coppices %<>%
  group_by(id_plot) %>%
  arrange(desc(C150_), .by_group = T) %>%
  mutate(fexthacumsum = cumsum(fext_ha))
plots_trees_coppices %<>% mutate(
  idd = 1:n(),
  whup = fexthacumsum > n_tree_cdom,
  whup2 = which(whup)[1],
  fexthacumsum = ifelse(whup, n_tree_cdom, fexthacumsum),
  diff = n_tree_cdom - c(0, fexthacumsum[-length(fexthacumsum)]),
  fext_ha2 = case_when(
    idd < whup2 ~ fext_ha,
    idd == whup2 ~ diff,
    is.na(whup2) ~ fext_ha,
    T ~ 0
  ),
  Cha = C150_ * fext_ha2,
  CDOM = sum(Cha, na.rm = T) / sum(fext_ha2, na.rm = T)
) %>% ungroup()

# adaptation JL

# attention, retirer les arbres morts
plots_trees_coppices <- arbre[arbre$statut==1,]
plots_trees_coppices %<>% group_by(ues_id_ogf,ues_id_ue) %>% arrange(desc(circ), .by_group = T) %>% mutate(fexthacumsum = cumsum(fe))

sample <-plots_trees_coppices[plots_trees_coppices$ues_id_ogf==10 & plots_trees_coppices$ues_id_ue==1,c("circ", "fe", "fexthacumsum")]  %>% arrange(desc(circ))

n_tree_cdom <- 10

drops <- c("whup", "whup2", "diff", "fext_ha2", "Cha")
for (n_tree_cdom in c(10:100)){

plots_trees_coppices <- plots_trees_coppices[ , !(names(plots_trees_coppices) %in% drops)]

plots_trees_coppices %<>% group_by(ues_id_ogf,ues_id_ue) %>% arrange(desc(circ), .by_group = T) %>% mutate(fexthacumsum = cumsum(fe))
plots_trees_coppices %<>% mutate(
  idd = 1:n(),
  whup = fexthacumsum > n_tree_cdom ,
  whup2 = which(whup)[1],
  fexthacumsum = ifelse(whup, n_tree_cdom , fexthacumsum),
  diff = n_tree_cdom - c(0, fexthacumsum[-length(fexthacumsum)]),
  fext_ha2 = case_when(
    idd < whup2 ~ fe,
    idd == whup2 ~ diff,
    is.na(whup2) ~ fe,
    T ~ 0
  ),
  Cha = circ * fext_ha2,
  "CDOM_{n_tree_cdom}" :=  sum(Cha, na.rm = T) / sum(fext_ha2, na.rm = T)
) %>% ungroup()
}

# maintenant je fait un graph avec une courbe par UE, en x le nombre d'arbre, en y le cdom calculé :-)
cdoms <- plots_trees_coppices %>% group_by(ues_id_ogf,ues_id_ue) %>% summarise(across(starts_with("CDOM"), mean, .names = "{.col}"))

cdoms = cbind("plot"=as.factor(1:nrow(cdoms)),cdoms)
df2 = tidyr::pivot_longer(cdoms, cols=paste0("CDOM_",c(10:100)),
                          names_to="nb_tree_cdom", values_to="cdom")

df2$ntree= rep(c(10:100),nrow(cdoms))
require("ggplot2")
ggplot(df2, aes(x=ntree, y=cdom, line=plot)) + geom_line()  + theme(legend.position="none") +  ggtitle("influence du nombre d'arbre \n sur la détermination du CDOM en feuillus (76 placettes) ") +
  xlab("nombre d'arbre") + ylab("Circ Dominante [cm]")


# j'ai la nouvelle carte dendro de Nicolas, c'est tellement mauvais que je ne sais pas quoi faire.
# je vais comparer avec ses propres données d'entrainement, en tout cas celle que je sais avoir facilement : pc 60, ifa_ifw
plots_trees_coppices1 <- read.csv2("/home/jo/Documents/OGF/data/IFA_IFW_Carto/one_big_csv.csv")

# selection ; uniquement les peuplements feuillus presque pur

res <- plots_trees_coppices1 %>% group_by(id_plot) %>% summarise(gha_re s=sum(g*fext_ha))

res <- plots_trees_coppices1[plots_trees_coppices1$essence>40,] %>% group_by(id_plot) %>% summarise(gha_res=sum(g*fext_ha))
length(res$id_plot[res$gha_res>3])    # 3 m2 de gha résineux, c'est le premier quantile
length(unique(plots_trees_coppices1$id_plot))
plots_trees_coppices <- plots_trees_coppices1[!plots_trees_coppices1$id_plot %in% res$id_plot[res$gha_res>2] & plots_trees_coppices1$path_ifa_rel!="IFA_NL/IFA Dir Bruxelles/IFA_IFRBC.mdb" & !plots_trees_coppices1$statut_arb %in% c(4,5,11,12,141,142),]
# test aussi sur les résineux, j'espère que ça donne un peu mieux...
plots_trees_coppices <- plots_trees_coppices1[plots_trees_coppices1$id_plot %in% res$id_plot[res$gha_res>15] &  !plots_trees_coppices1$statut_arb %in% c(4,5,11,12,141,142),]


length(unique(plots_trees_coppices$id_plot)) # 4078

# calcul du Cdom feuillus (10 arbres) puis sélection des UE qui dépasse un cdom seuil
cdoms <- plots_trees_coppices1 %>% group_by(id_plot) %>% summarise(CDOM=mean(CDOM),annee=mean(annee), fe=mean(fext_ha))
cdoms <- plots_trees_coppices1 %>% group_by(id_plot) %>% summarise(CDOM=mean(CDOM),annee=mean(annee), fe=mean(fext_ha), hdom=mean(htot,na.rm =T))
cdoms <-  plots_trees_coppices1 %>% select(id_plot, CDOM, annee, rayon) %>% unique()


mature <- cdoms[cdoms$CDOM>200 & cdoms$annee>2020,]
mature <- cdoms[cdoms$annee>2017,]
nrow(mature) # 245
hist(mature$CDOM)

plots_trees_coppices[plots_trees_coppices$id_plot==mature$id_plot[2], c("C150_", "fexthacumsum", "fext_ha","fext_ha2")]
# avec 10 arbres par ha pour le calcul de cdom, c'est quazi toujours la circonférence du plus gros vu que sont facteur d'extension est au dessus de 10
# je n'ai pas fait de simulation mais il faut faire grossir ces arbres pour actualiser leur circonférence en 2021. Bon je vais plutôt garder uniquement les année pas trop éloignée.
# jointure avec position mesurée du plot


plot_center <- read_sf("/home/jo/Documents/OGF/data/IFA_IFW_Carto/one_big_gpkg.gpkg", layer="plot_centers")

#plot_mature <-st_as_sf( merge(mature,plot_center,by.x=c("id_plot"),by.y=c("id_plot"), all=F))
plot_mature <-st_as_sf( merge(cdoms,plot_center,by.x=c("id_plot"),by.y=c("id_plot"), all=F))
st_crs(plot_mature) <- st_crs(31370)


filename <- "/home/jo/Documents/OGF/data/IFA_IFW_Carto/plot_all.gpkg"
filename <- "/home/jo/Documents/OGF/data/IFA_IFW_Carto/plot_mature.gpkg"
filename <- "/home/jo/Documents/OGF/data/IFA_IFW_Carto/plot_notSo_mature.gpkg"
filename <- "/home/jo/Documents/OGF/data/IFA_IFW_Carto/plot_resineux.gpkg"

st_write(plot_mature,filename,delete_layer=T)


./carteApt --outils 1 --gpkg_layer "plot_mature" --gpkg "/home/jo/Documents/OGF/data/IFA_IFW_Carto/plot_mature.gpkg" --layerCode dendro_cdom --pathBD "/home/jo/Documents/carteApt/GIS/dendro202601/aptitudeEssDB.db" --buffer 18
./carteApt --outils 1 --gpkg_layer "plot_resineux" --gpkg "/home/jo/Documents/OGF/data/IFA_IFW_Carto/plot_resineux.gpkg" --layerCode dendro_cdom --pathBD "/home/jo/Documents/carteApt/GIS/dendro202601/aptitudeEssDB.db" --buffer 18


plot_mature <- read_sf(filename, layer="plot_mature")
plot_mature <- read_sf(filename, layer="plot_notSo_mature")
plot_mature <- read_sf(filename, layer="plot_resineux")
pngOut <- "/home/jo/Documents/OGF/out/validCarteDendro_"
lim <- c(0,310)
png(paste0(pngOut,"cdom202601_ifa_ifwMature.png"), width = 7.75, height = 5.75, res = 300, units = "in")
plot(plot_mature$CDOM, plot_mature$dendro_cdom, xlim=lim, ylim=lim,main="validation carte dendro - cdom", ylab="cdom CNN [cm]", xlab="cdom terrain [cm]")
points(plot_mature$CDOM, plot_mature$dendro_cdom2025, xlim=lim, ylim=lim,col="red")

lines(lim,lim, type = "l",lwd=2, col="red")
dev.off()

cor(plot_mature$CDOM, plot_mature$dendro_cdom2025)

# 2026 01 21 - NL me partage ses données d'entrainements, avec la prédiction. uniquement les postérieurs à 2017. Je vérifie
setwd("/home/jo/Documents/OGF/data/IFA_IFW_Carto/")
# il faut effectuer un transform à nouveau car NL à préparé ces données pour ggplot2 avec tidyr::pivot_longer
plot2 <- read_sf("/home/jo/Documents/OGF/data/IFA_IFW_Carto/plots2.gpkg", layer="plots2")

# avec une nouvelle pondération pour la détermination de cdom : plus de poid au feuillus et classe de grosseur par 50 cm 
plot2b <- read_sf("/home/jo/Documents/OGF/Dendro_LIDAR/Wal/plots2.gpkg", layer="plots2")

require("tidyr")
select <- dplyr::select

vars <- c("PROP_FE", "CDOM", "HDOM", "NHA", "GHA", "VHA")
dt <- pivot_wider(
  plot2,
  id_cols = NULL,
  id_expand = FALSE,
  names_from = var,
  values_from = c(true,pred,residual)
)  %>% select(all_of(c("id_plot", "train_test",paste0("true_", vars), paste0("pred_", vars)))) %>% filter(true_PROP_FE>0.8)

plot(dt %>% filter(train_test=="train") %>% pull(true_VHA), dt %>% filter(train_test=="train") %>% pull(pred_VHA))
points(dt %>% filter(train_test=="test") %>% pull(true_VHA), dt %>% filter(train_test=="test") %>% pull(pred_VHA), col="red")

abline(a=0,b=1, col="blue")

plot(dt %>% filter(train_test=="train") %>% pull(true_GHA), dt %>% filter(train_test=="train") %>% pull(pred_GHA))
points(dt %>% filter(train_test=="test") %>% pull(true_GHA), dt %>% filter(train_test=="test") %>% pull(pred_GHA), col="red")

plot(dt %>% filter(train_test=="train") %>% pull(true_HDOM), dt %>% filter(train_test=="train") %>% pull(pred_HDOM))
points(dt %>% filter(train_test=="test") %>% pull(true_HDOM), dt %>% filter(train_test=="test") %>% pull(pred_HDOM), col="red")


mtrcs <- plot2 %>% filter(id_plot %in% dt$id_plot) %>%
  group_by(var, train_test) %>%
  yardstick::metrics(truth = true, estimate = pred)
mtrcs_ <- plot2 %>% filter(id_plot %in% dt$id_plot) %>%
  mutate(true2 = ifelse(true == 0, NA, true)) %>%
  group_by(var, train_test) %>%
  summarise(
    biais = mean(true - pred),
    ecart_type_residuel = sqrt(var(true - pred)),
    erreur_relative = mean(abs(true - pred) / true) * 100,
    erreur_relative2 = mean(abs(true - pred)) / mean(true) * 100,
    erreur_relative3 = mean(abs(true2 - pred) / true2, na.rm = T) * 100,
    erreur_pourcentage = ecart_type_residuel * 100 / mean(true),
    intervalle_de_confiance_95pc = ecart_type_residuel * 1.959 / sqrt(10), # 10 hexagons thus 1 ha
    mean_true = mean(true)
  )
mtrcs %<>% pivot_wider(names_from = .metric, values_from = .estimate) %>%
  select(-.estimator) %>%
  inner_join(mtrcs_) %T>% write_csv("dendro_metrics.csv") %>%

require(ggplot2)
library(ggrepel)

hist(compa$CDOM-compa$true_CDOM) #-> ok ça semble être la même valeur, exception de l'actualisation temporelle que je n'ai pas réalisée. et pour les peuplements avec 20% de résineux, le cdom est un peu plus bas
hist(compa$dendro_cdom-compa$pred_CDOM)
plot(compa$diff, compa$residual_CDOM)

hist(compa$residual_CDOM)

# je vois juste que les 80 parcelles de Haut fays ne sont pas utilisées dans le jeu de donnée de Nicolas. Je ne sais pas pourquoi..
# dernière question ; quelle est la proportion feuillus/résineux dans son jeu de donnée?

sum(wider$true_PROP_FE>0.5)
sum(wider$true_PROP_FE<0.5)
sum(wider$true_PROP_FE>0.8)
hist(wider$residual_CDOM[wider$true_PROP_FE>0.8 & wider$true_CDOM> 180])
voila tout est dit
# beaucoup de parcelles en feuillus de plus de 150 de tour donc..

#write.csv2(mature,"/home/jo/Documents/OGF/data/IFA_IFW_Carto/mature_2020_cdom200.csv")
# 
filename <- "/home/jo/Documents/OGF/data/IFA_IFW_Carto/plot_mature_NL.gpkg"
st_write(feMature,filename,delete_layer=T)

# je calcul un hdom avec mon approche, une approche qui utilise uniquement les hauteurs mesurées sur le terrain et non pas celles inférées de la circonférence et de l'essence
plots_trees_coppices <- readRDS("plots_trees_coppices.RData")
n_tree_cdom <- 10

# en sélectionnant les arbres pour lesquels le htot est différent de Htotest, je me rend compte que les arbres des inventaires ifa contiennent dans la colonne htot la valeur de Htotest, et cela ne me semble pas être voulu. Ce n'est pas le cas dans le fichiers ifa_trees_coppices.gpkg
# voila l'opération coupable :  ifa_trees_coppices %<>% mutate(HTOT_ = Htotest,
# alors que pour ifw c'est bien :  ifw_plots_trees_coppices %<>% mutate(HTOT_ = HTot,
# je suis sur que ce n'est pas voulu..

plots <- plots_trees_coppices %>% select(id_plot, source, new_hdom_B,new_hdom_C, gha, gha_B,gha_C, new_cdom_B, FERE) %>% unique()

plots_trees_coppices$Htotest[is.na(plots_trees_coppices$Htotest)] <- 0
plots_trees_coppices %<>%
  #filter(is.na(htot) == F & (is.na(Htotest) | Htotest!=htot ))%>%# & FERE=="FE") %>% #-> cela retire tout simplement les IFA...
  filter(is.na(htot) == F & (Htotest!=htot ))%>%
  group_by(id_plot) %>%
  arrange(desc(htot), .by_group = T) %>%
  mutate(fexthacumsum = cumsum(fext_ha))
# visuellement je dois déjà voir que je n'ai plus de modèle entre hdom et circ
plot(plots_trees_coppices$C150_, plots_trees_coppices$htot)

plots_trees_coppices %<>% mutate(
  idd = 1:n(),
  whup = fexthacumsum > n_tree_cdom,
  whup2 = which(whup)[1],
  fexthacumsum = ifelse(whup, n_tree_cdom, fexthacumsum),
  diff = n_tree_cdom - c(0, fexthacumsum[-length(fexthacumsum)]),
  fext_ha2 = case_when(
    idd < whup2 ~ fext_ha,
    idd == whup2 ~ diff,
    is.na(whup2) ~ fext_ha,
    T ~ 0
  ),
  Hha2 = htot * fext_ha2,
  hdom_terrain = sum(Hha2, na.rm = T) / sum(fext_ha2, na.rm = T),
) %>% ungroup()


