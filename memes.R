
library(tidyverse) 


f <- list.files("ignore/submissions/WCRs/WCR12",pattern=".Rmd",full.names = T)

rmds <- sapply(f,read_lines)



chunks_l <- list()

for(n in 1:length(rmds)){
  
  i <- rmds[[n]]
  i <- gsub('\\{r.*\\}','\\{r,echo=F}',i)
  ch <-  matrix(grep('\`',i),ncol=2,byrow = T)
  
  chunks <- lapply(1:nrow(ch),function(y) i[ch[y,1]:ch[y,2]])
  chunks_l[n] <- chunks[last(grep("meme_get",chunks))]
}


chunks <- chunks_l[sapply(chunks_l,length)<10]

chunks <- chunks[!grepl("mee",chunks)]

rmd <- "memes_24.Rmd"
unlink(rmd)

su <- '
\`\`\`{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,out.width = \'70%\')
library(memer)
library(tidyverse)
\`\`\`

Every once in a while, we break for fun. Here are some memes from our last WCR.

'

cat(file=rmd,c('---\ntitle: "Class Memes"\n---\n',su),append=T,sep="\n")
lapply(chunks,function(x) cat(c(x,"\n","\n","<br>"), file = rmd,append = T,sep = "\n"))
rmarkdown::render(rmd, output_format = "html_document", output_dir = getwd(),output_file = "memes.html",quiet = T)

