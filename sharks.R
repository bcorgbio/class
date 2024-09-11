
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
library(Momocs)
library(ggthemes)
library(cowplot)
library(OUwie)

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

#saveRDS(gbif_res,"data/shark_gbif.RDS")

gbif_res <- readRDS("data/shark_gbif.RDS")
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
last_i=1
for(i in fb_no[last_i:length(fb_no)]){
  
  fb_i <-  fb_img_data(i)
  
  
  fb_ln_l[[i]] <- fb_i
  last_i <- which(i==fb_no)
  message('\r', round(last_i/length(fb_no),2), appendLF = FALSE)
}



shark_fb_res <- do.call(rbind,fb_ln_l) %>% 
  filter(!duplicated(file_name))
all(fb_no%in%shark_fb_res$id)

shark_fb_res

saveRDS(shark_fb_res,"data/shark_fb_res.RDS")


#shark taxonomy

so <- rbind(tibble(superorder="Galeomorphii",order=c("Carcharhiniformes","Heterodontiformes","Orectolobiformes","Lamniformes")),
            tibble(superorder="Squalomorphii",order=c("Hexanchiformes","Pristiophoriformes","Squaliformes","Squatiniformes","Echinorhiniformes")))



tax <- wormsbynames((shark_fb_res$sp %>% unique)) %>% 
  left_join(so) %>% 
  dplyr::rename(sp=valid_name) %>% 
  select(sp,genus,family,order,superorder)




### SHAPE

shark_fb_res %>% filter(sp=="Cephaloscyllium variegatum")

f <- list.files("data/shark_points",full.names = T,pattern=".csv")
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
  ggplot(aes(superorder,Comp1,col=habitat))+geom_beeswarm()

PCA %>% 
  ggplot(aes(habitat,Comp2))+geom_beeswarm()

PCA %>% 
  ggplot(aes(habitat,size))+geom_beeswarm()

phy2 <- keep.tip(phy,PCA$sp)
X=PCA[,1:2]
rownames(X) <- PCA$sp
phylomorphospace(phy2[[1]],X,cex=0.1)

dimnames(ldk_al$coords)[[3]] <- PCA$sp

fit_l <- list()
for(i in 1:length(phy2)){
  gdf <- geomorph.data.frame(ldk_al,
                             species = PCA$sp,
                             habitat=PCA$habitat,
                             depth=log(PCA$depth_max),
                             length=PCA$size,
                             phy=phy2[[i]]
  )
  
  fit_l[[i]] <- procD.pgls(coords ~ length, 
                           data = gdf, phy=phy,iter = 999, turbo = TRUE,
                           print.progress = FALSE)
}

fit_l[[1]]$pgls.residuals

pc.plot <- plot(fit1, type = "PC", pch = 19)

pc.plot$PC.points
anova(fit1)

## wraps
max_PC1 <- which.max(PCA$Comp1)
min_PC1 <- which.min(PCA$Comp1)
M <- ldk_al$consensus
max_PC1_warp <- plotRefToTarget(M, ldk_al$coords[,,max_PC1], mag = 1)
plotRefToTarget(M, ldk_al$coords[,,min_PC1], mag = 1)

PCA$sp[max_PC1]
PCA$sp[min_PC1]

tps_raw(M,ldk_al$coords[,,max_PC1]) %>% 
  
  gg_tps(fr=M,to=ldk_al$coords[,,max_PC1],geom = "path",grid = T,linewidth=0.5,alpha=0.5,amp = 0.1)+geom_polygon(data=max_PC1_xy_,aes(X1,X2+0.25),fill="grey",alpha=0.3)+scale_y_reverse()
gg_tps(fr=M,to=ldk_al$coords[,,min_PC1],geom = "path",grid = T,linewidth=0.5,alpha=0.5,amp=0.01)+scale_y_reverse()
max_PC1_xy <- read_tsv("data/sharks/XY_Cesil.txt")
sc=coo_length(as.matrix(ldk_al$coords[,,max_PC1]))/coo_length(as.matrix(max_PC1_xy))
max_PC1_xy_ <- coo_center(as.matrix(max_PC1_xy)) %>%  coo_scale(scale = 1/sc)  %>%  data.frame() 

#evo rates

gp.so <- as.numeric(as.factor(PCA$superorder))-1
names(gp.so) <- PCA$sp

so_names <- c(PCA$superorder[which(gp.so==0)] %>% unique,
              PCA$superorder[which(gp.so==1)]%>% unique)

PCA$habit[which(gp.hab==0)]
PCA$habitat[which(gp.hab==1)]
dimnames(ldk_al$coords)[[3]] <- PCA$sp

gp.hab <- as.numeric(as.factor(PCA$habitat))-1
names(gp.hab) <- PCA$sp

hab_names <- c(PCA$habit[which(gp.hab==0)] %>% unique,
               PCA$habit[which(gp.hab==1)]%>% unique)


so_er_fit <- list()

for(i in 1:length(phy2)){
  so_er_fit[[i]] <-compare.evol.rates(A=ldk_al$coords, phy=phy2[[i]],
                                      method="simulation",gp=gp.so,iter=999,print.progress = F)
}

so_er <- lapply(so_er_fit,summary)

so_er <- lapply(so_er,function(x) {
  er <- c(x$sigma.d.gp,x$sigma.d.ratio)
  names(er) <- c(so_names,"ratio")
  return(er)
}) %>% do.call(rbind,.) %>% data.frame


so_er %>% 
  pivot_longer(Galeomorphii:Squalomorphii) %>% 
  ggplot(aes(value,fill=name)) + geom_histogram()


hab_er_fit <- list()

for(i in 1:length(phy2)){
  hab_er_fit[[i]] <-compare.evol.rates(A=ldk_al$coords, phy=phy2[[i]],
                                       method="simulation",gp=gp.hab,iter=999,print.progress = F)
}

hab_er <- lapply(hab_er_fit,summary)

hab_er <- lapply(hab_er,function(x) {
  er <- c(x$sigma.d.gp,x$sigma.d.ratio)
  names(er) <- c(hab_names,"ratio")
  return(er)
}) %>% do.call(rbind,.) %>% data.frame


hab_er %>% 
  pivot_longer(benthic:pelagic) %>% 
  ggplot(aes(value,fill=name)) + geom_histogram()


### distances
dist_df <- data.frame(
  start=c(1,1,1,12,1,1),
  end=c(2,12,10,10,5,8),
  var=c("predorsal","prepectoral","prepelvic","interpaired","precaudal_1","precaudal_2")
  
)
dist_ldk <- geomorph::interlmkdist(ldk_al$coords,lmks = dist_df[,1:2]) %>% data.frame()

colnames(dist_ldk) <- dist_df$var
dist_ldk <- dist_ldk %>% 
  mutate(sp=rownames(dist_ldk)) %>% 
  left_join(hab %>% select(sp,habitat,depth)) %>% 
  mutate(predorsal=predorsal/(mean(c(precaudal_1,precaudal_2))),
         prepectoral=prepectoral/(mean(c(precaudal_1,precaudal_2))),
         prepelvic=prepelvic/(mean(c(precaudal_1,precaudal_2))),
         interpaired=interpaired/(mean(c(precaudal_1,precaudal_2)))
  )
dist_ldk_long <- dist_ldk %>% 
  pivot_longer(predorsal:interpaired)
dist_ldk_long %>% 
  ggplot(aes(habitat,value))+geom_beeswarm()+geom_boxplot(alpha=0.3)+facet_wrap(.~name)

fit_dist <- lm(value~habitat,data=dist_ldk_long %>% filter(name=="interpaired"))
anova(fit_dist)

fit_dist <- lm(value~habitat,data=dist_ldk_long %>% filter(name=="predorsal"))
anova(fit_dist)

fit_dist <- lm(value~habitat,data=dist_ldk_long %>% filter(name=="prepectoral"))
anova(fit_dist)

fit_dist <- lm(value~habitat,data=dist_ldk_long %>% filter(name=="prepelvic"))
anova(fit_dist)


##houwei
args_df <- expand_grid(discrete_model=c("ER","ARD"),
                       continuous_model=c("BM1", "BMV", "OU1", "OUM", "OUA", "OUV", "OUMV", "OUMA", "OUVA", "OUMVA"),
                       var=unique(dist_ldk_long$name)) %>% 
  mutate(n=1:n(),
         nSim=25,
         n_starts=5,
         rate.cat=1) %>% 
  arrange(var)


   

args <- args_df %>% 
  group_by(n) %>% 
  group_split(.keep=F)

data_l <- dist_ldk_long %>% 
  select(sp,habitat,value,name) %>% 
  group_by(name) %>% 
  group_split(.keep=F)

args<-lapply(args, FUN=append,
          list(phy=phy2[[1]]),
          )

for(i in 1:length(args)){
  args[[i]]$data <- dist_ldk_long %>% filter(name==args[[i]]$var) %>% select(sp,habitat,value) %>% data.frame()
  args[[i]]$var <- NULL
}
               
run_hOUwie <- function(x) do.call(hOUwie,args[[x]])

hOUwie_res<- mclapply(X=1:length(args),FUN = run_hOUwie,mc.cores = detectCores()-2)

hOUwie_res_eval <- getModelTable(hOUwie_res) %>% tibble() %>% 
  tibble(args_df %>% select(var,discrete_model,continuous_model))

hOUwie_res_eval %>% 
  group_by(var) %>% 
  filter(BIC==min(BIC))


sapply(X=q, `[[`, "root.p")
sapply(X=q, `[[`, "node.states")
sapply(X=q, `[[`, "get.tip.states")
hOUwie_res_inter <- mclapply(X=1:15,FUN = run_hOUwie,mc.cores = detectCores()-2)
hOUwie_res_preD <- mclapply(X=16:30,FUN = run_hOUwie,mc.cores = detectCores()-2)
hOUwie_res_preP <- mclapply(X=31:45,FUN = run_hOUwie,mc.cores = detectCores()-2)

hOUwie_res_inter_eval <- getModelTable(hOUwie_res_inter)
hOUwie_res_preD_eval <- getModelTable(hOUwie_res_preD)
hOUwie_res_preP_eval <- getModelTable(hOUwie_res_preP)

args[1:15][which.min(hOUwie_res_inter_eval$BIC)]
args[16:30][which.min(hOUwie_res_preD_eval$BIC)]
args[31:45][which.min(hOUwie_res_preP_eval$BIC)]

hOUwie_res_preD[[which.min(hOUwie_res_preD_eval$BIC)]]
hOUwie_res_preP[[which.min(hOUwie_res_preP_eval$BIC)]]
trait_dat <- dist_ldk %>% select(sp,habitat,predorsal)
pp_oum <- hOUwie(phy2[[1]], trait_dat, rate.cat = 1, discrete_model = "ER", 
                 continuous_model = "OUM", nSim = 25,ncores = detectCores()-2,n_starts = 5)


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


### ggplotify tps plots
gg_tps <- function (fr, to, amp = 1, over = 1.2,grid=F, grid.size = 7,
                    geom="path",plot_shape="to",diff_line=0.5,diff_alpha=0.5,facet=F,rot=F,flip=NULL,add_opts=NULL,...) 
{
  args <- list(...)
  fr.n <- substitute(fr)
  to.n <- substitute(to)
  if (!missing(amp)) {
    to <- to + (to - fr) * amp
  }
  
  if(!is.null(rot)){ 
    fr <- fr  %>% coo_rotate(rot*pi/180)
    to <- to  %>% coo_rotate(rot*pi/180)
    colnames(fr) <- colnames(to) <- c("X","Y")
  }
  
  if(!is.null(flip)){ 
    
    if(flip=="flip_x"){
      fr <- fr  %>% coo_flipx()
      to <- to  %>% coo_flipx()
    }
    
    if(flip=="flip_y"){
      fr <- fr  %>% coo_flipy()
      to <- to  %>% coo_flipy()
    }
    
    colnames(fr) <- colnames(to) <- c("X","Y")
  }
  
  grid0 <- Momocs:::.grid.sample(fr, to, nside = round(grid.size), over = over)
  grid1 <- tps2d(grid0, fr, to)
  dim.grid <- c(length(unique(grid0[, 1])), length(unique(grid0[, 
                                                                2])))
  
  if(all(grepl("[.]", rownames(to)))) {
    fr_df <-
      fr %>% coo_close %>% data.frame %>% mutate(grp = gsub("(.*)[.].*", "\\1", rownames(fr)))
    to_df <-
      to %>% oo_close %>% data.frame %>% mutate(grp = gsub("(.*)[.].*", "\\1", rownames(to)))
    
    grp <- gsub("(.*)[.].*", "\\1", rownames(fr)) %>% unique
    
    
    if (geom == "path") {
      to_p <-  geom_polygon(data = to_df, aes(X, Y, col = grp), ...)
      fr_p <-  geom_polygon(data = fr_df, aes(X, Y, col = grp), ...)
    }
    if (geom == "point") {
      to_p <-  geom_point(data = to_df, aes(X, Y, col = grp), ...)
      fr_p <-  geom_point(data = fr_df, aes(X, Y, col = grp), ...)
    }
    
    
  }else{
    
    fr_df <- fr %>% data.frame
    to_df <- to %>% data.frame
    
    
    
    
    if(geom=="path") {
      to_p <-  geom_polygon(data=to_df,aes(X,Y),...) 
      fr_p <-  geom_polygon(data=fr_df,aes(X,Y),...)
    }
    if(geom=="point") {to_p <-  geom_point(data=to_df,aes(X,Y),...)
    fr_p <-  geom_point(data=fr_df,aes(X,Y),...)
    }
  }
  
  if(plot_shape=="to") p_shape <- to_p
  if(plot_shape=="from") p_shape <- fr_p
  
  
  if(plot_shape=="both"){
    both_df <- rbind(
      fr_df %>% mutate(shape="from"),
      to_df %>% mutate(shape="to")
    )
    if(geom=="point") p_shape <- geom_point(data=both_df,aes(X,Y,col=shape),...)
    if(geom=="path") p_shape <- geom_path(data=both_df,aes(X,Y,col=shape),...)
  }
  
  
  p <- ggplot()
  
  
  seg_1 <- list()
  for (i in 1:dim.grid[2]) {
    xy_i <- grid1[(1:dim.grid[1]) + (i - 1) * dim.grid[1], ] %>% data.frame()
    seg_1[[i]] <- geom_path(data=xy_i,aes(X1,X2),...)
    seg_1[[i]]$aes_params$alpha <-  p_shape$aes_params$alpha*diff_alpha
    seg_1[[i]]$aes_params$linewidth <-  p_shape$aes_params$linewidth*diff_line
  }
  
  seg_2 <- list()
  for (i in 1:dim.grid[1]) {
    xy_i <- grid1[(1:dim.grid[2]) * dim.grid[1] - i + 1, ] %>% data.frame
    seg_2[[i]] <- geom_path(data=xy_i,aes(X1,X2),...)
    seg_2[[i]]$aes_params$alpha <-  p_shape$aes_params$alpha*diff_alpha
    seg_2[[i]]$aes_params$linewidth <-  p_shape$aes_params$linewidth*diff_line
  }
  
  if(grid) p <- p+seg_1+seg_2+p_shape+theme_nothing()+coord_equal()
  if(!grid) p <- p+p_shape+theme_nothing()+coord_equal()
  
  if(facet==T) {
    p <- p+facet_wrap(.~grp,nrow=1)
  }
  
  
  
  if(!is.null(add_opts)) {
    p <- p+add_opts
  }
  
  return(p)
}
