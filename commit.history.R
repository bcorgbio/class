library(tidyverse)
#run in terminal: git log --stat > history.txt
# source("/Users/biology/Dropbox/Documents/bc (1)/3140.f21/class/commit.history.R")

system("git log --stat > history.txt")
h<- readLines("history.txt")

a <- h[grep("Author",h)]
a <- unique(gsub("Author: (.+) <.+>","\\1",a))
com.st <- grep("commit",h)
com.end <- c(com.st[-1]-1,length(h))
com.n <- tibble(start=com.st,end=com.end) %>%
  rowid_to_column()

com.l <- list()
for(i in com.n$rowid){
  s <- com.n[i,]$start
  e <- com.n[i,]$end
  com.l[[i]] <- h[s:e]
}

stats <- lapply(com.l, function(x) x[grep("Author:|file changed",x)])

commits <-  sapply(a, function(x) length(grep(x,stats)))
commits <- tibble(auth=names(commits),commits=commits)
stats <- stats[grep("changed",stats)]

stats <- lapply(stats, function(x) data.frame(auth=x[1],change=x[2]))

stats2 <- do.call(rbind,stats)%>%tibble%>%
  mutate(auth=gsub("Author: (.+) <.+>","\\1",auth), files=gsub("^ (\\d+) file changed, .*","\\1",change),ins=gsub(".+ (\\d+) insertion.+","\\1",change),
         del=gsub(".+ (\\d+) deletion.+","\\1",change,))%>%
  mutate(files=as.numeric(files),ins=as.numeric(ins),del=as.numeric(del))%>%
  group_by(auth)%>%
  filter(auth%in%a)%>%
  dplyr::summarise(files=sum(files,na.rm = T),ins=sum(ins,na.rm = T),del=sum(del,na.rm=T))%>%
  left_join(commits)%>%print
  