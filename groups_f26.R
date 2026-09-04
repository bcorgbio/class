setwd("~/Dropbox/Documents/bc (1)/3140.f26/ignore")

library(tidyverse)
library(kableExtra)




group_mat <- function(x) {
  pw_mat <- matrix(0, ncol = nrow(x), nrow = nrow(x))
  colnames(pw_mat) <- row.names(pw_mat) <- x$email
  
  for (i in 1:length(x$email)) {
    e <- x$email[i]
    g <- x %>% dplyr::filter(email == e) %>% dplyr::pull(group)
    g_n <- x %>% dplyr::filter(group == g) %>% dplyr::pull(email)
    for (n in g_n) {
      pw_mat[e, g_n] <- 1
    }
  }
  
  
  return(pw_mat)
}

r <- read_csv("roster.txt")%>%mutate(email=paste0(username,"@bc.edu"))

handles <- read_csv("handles.csv") %>% 
  select(handle,email)
#for peer review form
r %>% mutate(name=paste0(Last,",",First)) %>% select(name) %>% kable %>% kable_minimal()


handles %>% 
  filter(!email%in%r$email)

groups <- function(total_n = 30, group_n = 4) {
  
  small_n <- group_n - 1
  
  # Find a valid number of smaller groups
  n_small <- which(
    (total_n - small_n * 0:group_n) >= 0 &
      (total_n - small_n * 0:group_n) %% group_n == 0
  )[1] - 1
  
  if (is.na(n_small)) {
    stop("Cannot divide total_n into groups of ", 
         group_n, " and ", small_n)
  }
  
  n_full <- (total_n - small_n * n_small) / group_n
  
  sizes <- c(
    rep(group_n, n_full),
    rep(small_n, n_small)
  )
  
  rep(seq_along(sizes), sizes)
}

#for first module

if(F){

  group <- groups(nrow(r),4)
  group <- sample(group,length(group))
  
  

groups <- r%>%
  mutate(group=group) %>% 
  left_join(handles) %>% 
  arrange(group)%>%select(Last,First,email,handle,group) 
  
  




groups%>%
  kable
write_csv(groups,"mod1_groups.csv")


r%>%
  arrange(group) %>% 
  kable()
}


# Modules > 1
mod_1 <- read_csv("mod1_groups.csv") %>% rename(email=email)
# mod_2 <- read_csv("mod2_groups.csv") %>% rename(email=email)
# mod_3 <- read_csv("mod3_groups.csv") %>% rename(email=email)
# mod_4 <- read_csv("mod4_groups.csv") %>% rename(email=email)
# mod_5 <- read_csv("mod5_groups.csv") %>% rename(email=email)
# mod_6 <- read_csv("mod6_groups.csv") %>% rename(email=email)
# mod_7 <- read_csv("mod7_groups.csv") %>% rename(email=email)
set.seed(1234)

rep_n <- 10
n_p=1
while(rep_n>2){
mod_7<- r %>% 
  mutate(group=rep(1:7,3)[sample(1:n(),n())],
         email=paste0(username,"@bc.edu")) %>% 
  arrange(group)


mat_1 <- group_mat(mod_1)
mat_2 <- group_mat(mod_2)
mat_3 <- group_mat(mod_3)
mat_4 <- group_mat(mod_4)
mat_5 <- group_mat(mod_5)
mat_6 <- group_mat(mod_6)
mat_7 <- group_mat(mod_7)
# mat_8 <- group_mat(mod_8)


l <- list()
for(i in mod_6$email){
  s <- names(mat_7[i,][mat_7[i,]==1])%in%names(mat_6[i,][mat_6[i,]==1])
 l[[i]] <- length(which(s))>1
}

rep_n <- length(which(l==T))
print(n_p)
n_p <- n_p+1
}





mod_7%>%
  select(First,Last,email,group) %>% 
  left_join(handles) %>% 
  kable

write_csv(mod_7,"mod7_groups.csv")

#### Create local dirs

dirs <- paste0("submissions/reports/module1/Module1_Team",mod_1 %>% dplyr::pull(group) %>% unique)

sapply(dirs,dir.create)

dat <- mod_1%>%
  select(First,Last,email,group) %>% 
  left_join(handles) %>% 
  mutate(repo= paste0("bcorgbio/Module1_Team",group),
         dir=paste0("submissions/reports/module1/Module1_Team",group))

repos <-  paste0("bcorgbio/Module1_Team",mod_1 %>% dplyr::pull(group) %>% unique)

### Create repos
cd <- getwd()

team=1
repo_ <- repos[team]
dir_ <- dirs[team]







git_create <- function(team,d=dat){
file.create("cd_rep.sh")
  repo_ <- d %>% filter(group==team) %>% dplyr::pull(repo) %>% unique
  dir_ <- d %>% filter(group==team) %>% dplyr::pull(dir) %>% unique
  
cat("export GH_TOKEN=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",file="cd_rep.sh",sep="\n")

cat(paste0("cd /Users/Chris/Dropbox/Documents/bc\\ \\(1\\)/3140.f26/ignore/",dir_,"\n"),file = "cd_rep.sh",sep="\n",append=T)

cat("git init",file = "cd_rep.sh",sep="\n",append=T)

cr <- paste0("gh repo create ",repo_," --private")

cat(cr,file = "cd_rep.sh","\n",append=T)

us <- d %>% dplyr::filter(group==team)
for(i in c(us$handle,"ckenaley")){
perm_ <- paste0("gh api --method PUT repos/bcorgbio/",basename(dir_),'/collaborators/',i,' -f permission="push"')
cat(perm_,file="cd_rep.sh","\n",append=T)
}

system("sh cd_rep.sh")

cat("cd /Users/Chris/Dropbox/Documents/bc\\ \\(1\\)/3140.f26",file = "cd_rep.sh",sep="\n",append=T)
setwd(cd)
}


sapply(dat$group %>% unique,git_create)

######



dat <- mod_1%>%
  select(First,Last,email,group) %>% 
  left_join(handles) %>% 
  mutate(repo= paste0("bcorgbio/Module1_Team",group),
         dir=paste0("submissions/reports/module1/Module1_Team",group))

git_pull <- function(team,d=dat){
  file.create("pull_rep.sh")
  
  
  repo_ <- d %>% filter(group==team) %>% dplyr::pull(repo) %>% unique
  dir_ <- d %>% filter(group==team) %>% dplyr::pull(dir) %>% unique
  
  cat("export GH_TOKEN=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",file="pull_rep.sh",sep="\n")
  
  cat(paste0("cd /Users/Chris/Dropbox/Documents/bc\\ \\(1\\)/3140.f26/ignore/",dirname(dir_),"\n"),file = "pull_rep.sh",sep="\n",append=T)
  
  unlink(dir_,recursive = T)
  cl <- paste0("gh repo clone https://github.com/bcorgbio/",basename(dir_),".git")

  cat(cl,file = "pull_rep.sh",sep="\n",append=T)
  
  system("sh pull_rep.sh")
  
  cat("cd /Users/Chris/Dropbox/Documents/bc\\ \\(1\\)/3140.f26",file = "cd_rep.sh",sep="\n",append=T)
  setwd(cd)
}

sapply(dat$group %>% unique,git_pull)


last="Dougherty"

repo=subs$git_url[4]


file=subs$file[4] 

rep_path=subs$rep_path[4]



git_push_student <- function(repo,file,message){
  
  f <- list.files(getwd(),pattern=file,full.names = T,recursive = T)
  f2 <- paste0(rep_path,"/",basename(f))
  file.copy(f,f2)
  
  # Example:
  #   ./clone_and_push.sh https://github.com/user/repo.git repo ~/Desktop/notes.txt "add notes" main
  
 # ./clone_and_push.sh https://github.com/user/repo.git repo ~/Desktop/notes.txt "add notes" main

 f2 <- paste0("/Users/Chris/Dropbox/Documents/bc (1)/3140.f25/ignore/personal_repos/",dirname(f))
  # Usage: ./git_push.sh <repo_url> <file_path> <commit_message>
 unlink(dirname(f2),recursive = T)
 
# ./clone_and_push.sh https://github.com/user/repo.git repo ~/Desktop/notes.txt "add notes" main
 #   
  repo2 <- gsub("https://github.com/|.git","",repo)
  args <- paste("./push.sh", repo,basename(repo2), make_safe_path(f), basename(f), "main", sep=" ")
  system("chmod +x push.sh")
  system(args,intern = F)
}

subs
last=subs$Last[3]
file_path=subs$file_path[3]
repo=subs$rep_path[3]
git=subs$git_url[3]


push_student("Dougherty",file_path,repo,git)

}

         