
library(tidyverse)
library(ape)
library(treeio)
library(rgbif)
library(data.table)
library(phytools)
library(fishtree)
library(rgbif)
library(doParallel)
library(foreach)
library(parallel)
library(dataRetrieval)
library(rfishbase)
library(rvest)
library(geomorph)
library(abind)#install
library(ape)
library(nlme)
library(worms)

### sharks!!

## https://openknowledge.fao.org/server/api/core/bitstreams/1b650fd0-3675-43cc-adcf-876cdc5a001f/content

#https://www.iucnssg.org/uploads/5/4/1/2/54120303/fao_species_catalogue_for_fishery_purposes_-_2001_-_sharks_of_the_world_-_an_annotated_and_illustrated_catalogue_of_shark_species_known_to_date_-_volume_2_-_bullhead_mackerel_and_carpet_sharks.pdf#page=10.07

#https://vertlife.org/sharktree/)

# ? https://cran.r-project.org/web/packages/evolvability/index.html

phy <- readNexus("data/shark_mol_trees.nex")

phy$tree_1$tip.label

mrca <- lapply(phy,function(x) getMRCA(x,c("Chlamydoselachus_anguineus","Carcharhinus_leucas")))

all(mrca==929)

phy2 <- lapply(phy,function(x) extract.clade(x,929))

labs <-  lapply(phy2,function(x) gsub("_"," ",x$tip.label))

for(i in 1:length(phy2)){
  phy2[[i]]$tip.label <- labs[[i]]
}

class(phy2) <- "multiPhylo"
phytools::writeNexus(phy2,"data/shark_trees.nex")


phy <- readNexus("data/shark_trees.nex")

sp <- phy[[1]]$tip.label

gbif_res <- list()
for(i in sp) {
  gbif_res[[i]] <-  occ_search(scientificName=i,limit = 0)
  message('\r', i, appendLF = FALSE)
}

saveRDS(gbif_res,"data/shark_gbif.RDS")

#no more than 10 species
shark_n <- tibble(sp=names(gbif_res),
                  n=lapply(gbif_res, function(x) x$meta$count) %>% unlist()
) %>% mutate(genus=gsub("(\\w*) \\w*","\\1",sp)) %>% 
  group_by(genus) %>% 
  arrange(genus,-n) %>% 
  mutate(rank=1:n()) %>% 
  filter(rank<11)



fb_no <- species(shark_n$sp)$SpecCode



fb_ln_l <- list()

for(i in fb_no[last_i:length(fb_no)]){

  fb_i <-  fb_img_data(i)

  
 fb_ln_l[[i]] <- fb_i
  last_i <- which(i==fb_no)
}

shark_fb_res <- do.call(rbind,fb_ln_l) %>% 
  filter(!duplicated(file_name))
all(fb_no%in%shark_fb_res$id)



#shark taxonomy

so <- rbind(tibble(superorder="Galeomorphii",order=c("Carcharhiniformes","Heterodontiformes","Orectolobiformes","Lamniformes")),
tibble(superorder="Squalomorphii",order=c("Hexanchiformes","Pristiophoriformes","Squaliformes","Squatiniformes","Echinorhiniformes")))


tax <- wormsbynames((shark_fb_res$sp %>% unique)) %>% 
  left_join(so) %>% 
  dplyr::rename(sp=valid_name) %>% 
  select(sp,genus,family,order,superorder)




### SHAPE

shark_fb_res %>% filter(sp=="Cephaloscyllium variegatum")

f <- list.files("data/shark_points",full.names = T)
ldk <- read_csv(f,id="file") %>% select(file,X,Y)

ldk_l <- ldk %>% 
  mutate(file_name=gsub("*_.csv","",basename(file))) %>% 
  left_join(shark_fb_res) %>% 
  select(sp,X,Y) %>% 
  group_split(sp,.keep = F) 

ldk_l <- lapply(ldk_l,as.data.frame)

ldk <-  abind(ldk_l, along=3)

names(ldk) <-gsub("*_.csv","",basename(f))



#ldk <- define.sliders(teeth_sl[,,1],nsliders = 1)

ldk_al <- gpagen(ldk,ProcD = TRUE)
plot(ldk_al)


pca <- gm.prcomp(ldk_al$coords)


PCA <- pca$x %>% 
  data.frame %>% 
  mutate(file_name=gsub("*_.csv","",basename(f))) %>% 
  left_join(shark_fb_res %>% select(sp,file_name)) %>% 
  left_join(tax)


PCA %>% 
  select(sp) %>% 
  mutate(depth=NA,habitat=NA) %>% 
  write_csv("data/shark_hab.csv")

#habitat (combine w filename and sp data)
hab <- read_csv("data/shark_hab_added.csv") %>% 
  mutate(depth=ifelse(depth=="shallow-reef","0-100",depth),
         depth_max=str_split_i(depth,"-",2) %>% as.numeric,
         habitat=ifelse(habitat=="demersal","benthic",habitat))

# hab %>% add_row(sp=PCA %>% filter(!sp%in% hab$sp) %>% pull(sp)) %>%
# write_csv("data/shark_hab_added.csv")

PCA <- PCA %>% left_join(hab)
PCA %>% 
  ggplot(aes(Comp1,Comp2))+geom_point()

PCA %>% 
  ggplot(aes(habitat,size,col=habitat))+geom_beeswarm()

library(ggbeeswarm)
PCA %>% 
  ggplot(aes(habitat,Comp1,col=superorder))+geom_beeswarm()

PCA %>% 
  ggplot(aes(habitat,Comp2))+geom_beeswarm()

phy2 <- keep.tip(phy,PCA$sp)
X=PCA[,1:2]
rownames(X) <- PCA$sp
phylomorphospace(phy2[[1]],X,cex=0.2)

dimnames(ldk_al$coords)[[3]] <- PCA$sp

gdf <- geomorph.data.frame(ldk_al,
                           species = PCA$sp,
                            habitat=PCA$habitat,
                           depth=PCA$depth_max,
                           length=PCA$size,
                           phy=phy2[[1]]
                            )

fit1 <- procD.pgls(coords ~ habitat*depth, 
                 data = gdf, phy=phy,iter = 999, turbo = TRUE,
               print.progress = FALSE)
pc.plot <- plot(fit1, type = "PC", pch = 19)

pc.plot$PC.points
anova(fit1)
M <- ldk_al$consensus
plotRefToTarget(M, fit1$GM$fitted[,,1], mag = 1)
plotRefToTarget(M, fit1$GM$fitted[,,20], mag = 1)



#evo rates

gp.so <- as.numeric(as.factor(PCA$superorder))-1
names(gp.so) <- PCA$sp

PCA$superorder[which(gp.so==0)]
PCA$superorder[which(gp.so==1)]

PCA$habit[which(gp.hab==0)]
PCA$habitat[which(gp.hab==1)]
dimnames(ldk_al$coords)[[3]] <- PCA$sp

gp.hab <- as.numeric(as.factor(PCA$habitat))-1
names(gp.hab) <- PCA$sp

so_er <- list()

for(i in 1:length(phy2)){
  so_er[[i]] <-compare.evol.rates(A=ldk_al$coords, phy=phy2[[i]],
                       method="simulation",gp=gp.so,iter=999)
}

so_er_fit <- lapply(so_er,summary)
funcion(x) x$sigma.d.gp

ER<-compare.evol.rates(A=ldk_al$coords, phy=phy2[[1]],
                       method="simulation",gp=gp.hab,iter=999)
summary(ER)
plot(ER)

fb_img_data <- function(x,type=c("museum","collaborator")){

  url<- paste0("https://fishbase.mnhn.fr/photos/ThumbnailsSummary.php?ID=",x)
  
  sp <- fb_tbl(tbl="species",server="fishbase") %>% filter(SpecCode==x) %>% select(Genus,Species)
  sp <- paste0(sp[,1]," ",sp[,2])
  
  url <-  url(url, "rb")
  try_e <- T
  
    
    url_ <-  url %>%  read_html()
    
    
  
    links <-url_ %>% 
      html_nodes("img") %>%
      html_attr("src") %>% 
      unlist
    
    close(url)
    
    links <- links[!grepl("thumbnails|Upload|mediaphoto",links)]
    if(length(links)>0){
    link_f <- paste0("https://fishbase.mnhn.fr/images/species/",basename(links[grepl(".jpg|.gif",links)]))
 
 link_f <- link_f[grepl("species",link_f)]
 link_f <- link_f[!grepl("&w",link_f)]
 
 
 if(!dir.exists(paste0("data/shark_photos/",sp))) dir.create(paste0("data/shark_photos/",sp))
 dest_f <- paste0("data/shark_photos/",sp,"/",basename(link_f))
 
 res_l <- lapply(1:length(link_f),function(x) try(curl::curl_download(link_f[x],dest_f[x])) )  
 
 res_err <- lapply(res_l,class) %>% unlist
 
 res_f <- res_l[!grepl('try-error',res_err)] %>% unlist %>% basename
 res <- tibble(sp=sp,id=x,file_name=res_f)
    }else{
      res <- tibble(sp=sp,id=x,file_name=NA)
    }


  return(res)
}
