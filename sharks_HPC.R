
library(tidyverse)
library(ape)
library(tidyverse)
library(phytools)
library(parallel)
library(ape)
library(OUwie)

### sharks!!


shark_dat <- readRDS("shark_data.RDS")

set.seed(1234)
tree_n <- sample(1:length(shark_dat$phy),50)
phys <- shark_dat$phy[tree_n]

##houwei
args_df <- expand_grid(discrete_model=c("ER","ARD"),
                       continuous_model=c("BM1", "BMV", "OU1", "OUM", "OUA", "OUV", "OUMV", "OUMA", "OUVA", "OUMVA"),
                       var=unique(shark_dat$dat$name)) %>% 
  mutate(nSim=100,
         n_starts=5,
         rate.cat=1) %>% 
  arrange(var) %>% 
  mutate(n=1:n())



args <- args_df %>% 
  group_by(n) %>% 
  group_split(.keep=F)


args<-lapply(args, FUN=append,
             list(phy=phys),
)

args_HMM <- args

for(i in 1:length(args_HMM)){
  args_HMM[[i]]$rate.cat <- 2
  args_HMM[[i]]$null.model <- F
}

args <- c(args,args_HMM)

names(args) <- paste(sapply(X=args, `[[`, "var"),sapply(X=args, `[[`, "discrete_model"),sapply(X=args, `[[`, "continuous_model"),sapply(X=args, `[[`, "rate.cat"),sep = "_") 

#add data
for(i in 1:length(args)){
  args[[i]]$data <- shark_dat$dat %>% filter(name==args[[i]]$var) %>% select(sp,habitat,value) %>% data.frame()
  args[[i]]$var <- NULL
}








run_hOUwie <- function(x) {
  phy_tmp <- args[[x]]$phy
  args_tmp <- rep(args[x],length(phy_tmp))
  
  for(i in 1:length(args_tmp)){
    args_tmp[[i]]$phy <- phy_tmp[[i]]
  }
  
  run_hOUwie_tmp <- function(x) do.call(hOUwie,args_tmp[[x]])
  tmp <- mclapply(X=1:length(args_tmp),run_hOUwie_tmp,mc.cores = detectCores()-2)
  if(!dir.exists("hOUwie_output")) dir.create("hOUwie_output")
  saveRDS(tmp,paste0("hOUwie_output/",names(args)[x],".RDS"))
  return(tmp)                                 
  }


hOUwie_res<- mclapply(X=1:length(args),FUN = run_hOUwie,mc.cores =40)

saveRDS(hOUwie_res,"hOUwie_res.RDS")


#hOUwie_res <- readRDS("data/sharks/hOUwie_res.RDS")

getModelTable(hOUwie_res[[1]])

 lapply(hOUwie_res[[1]], getModelTable)   
