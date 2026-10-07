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

r <- read_csv("roster.csv")%>%mutate(email=paste0(username,"@bc.edu"))

handles <- read_csv("handles.csv") %>% 
  dplyr::select(handle,email)
#for peer review form
r %>% mutate(name=paste0(Last,",",First)) %>% dplyr::select(name) %>% kable %>% kable_minimal()


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
mod_2 <- read_csv("mod2_groups.csv") %>% rename(email=email)
mod_3 <- read_csv("mod3_groups.csv") %>% rename(email=email)
mod_4 <- read_csv("mod4_groups.csv") %>% rename(email=email)
# mod_5 <- read_csv("mod5_groups.csv") %>% rename(email=email)
# mod_6 <- read_csv("mod6_groups.csv") %>% rename(email=email)
# mod_7 <- read_csv("mod7_groups.csv") %>% rename(email=email)
set.seed(1234)

rep_n <- 10
n_p=1
while(rep_n>2){
mod_5<- r %>% 
  mutate(group=rep(1:7,4)[sample(1:n(),n())],
         email=paste0(username,"@bc.edu")) %>% 
  arrange(group)


mat_1 <- group_mat(mod_1)
mat_2 <- group_mat(mod_2)
mat_3 <- group_mat(mod_3)
mat_4 <- group_mat(mod_4)
mat_5 <- group_mat(mod_5)
# mat_6 <- group_mat(mod_6)
# mat_7 <- group_mat(mod_7)
# mat_8 <- group_mat(mod_8)


l <- list()
for(i in mod_5$email){
  s <- names(mat_5[i,][mat_5[i,]==1])%in%names(mat_4[i,][mat_4[i,]==1])
 l[[i]] <- length(which(s))>1
}

rep_n <- length(which(l==T))
print(n_p)
n_p <- n_p+1
}





mod_5%>%
  dplyr::select(First,Last,email,group) %>% 
  left_join(handles) %>% 
  kable

write_csv(mod_5,"mod5_groups.csv")

#### Create local dirs

dirs <- paste0("submissions/reports/module1/Module1_Team",mod_1 %>% dplyr::pull(group) %>% unique)

sapply(dirs,dir.create)

dat <- mod_5%>%
  dplyr::select(First,Last,email,group) %>% 
  left_join(handles) %>% 
  mutate(repo= paste0("bcorgbio/Module5_Team",group),
         dir=paste0("submissions/reports/module5/Module5_Team",group)) %>% 
  rename(team=group)

repos <-  paste0("bcorgbio/Module5_Team",mod_5 %>% dplyr::pull(group) %>% unique)

### Create repos
cd <- getwd()

team=1
repo_ <- repos[team]
dir_ <- dirs[team]


sapply(dat$dir %>% unique, function(x) if(!dir.exists(x)) dir.create(x))

git_create <- function(team, d = dat, base_dir = "/Users/Chris/Dropbox/Documents/bc (1)/3140.f26/ignore") {
  
  required <- c("team", "handle", "repo", "dir")
  if (!all(required %in% names(d))) {
    stop("Data must contain: ", paste(required, collapse = ", "))
  }
  
  # Select students belonging to this team
  x <- d[!is.na(d$team) & d$team == team, , drop = FALSE]
  
  if (nrow(x) == 0) {
    stop("No students found for team ", team)
  }
  
  repo_ <- unique(trimws(as.character(x$repo)))
  dir_  <- unique(as.character(x$dir))
  
  if (length(repo_) != 1L || length(dir_) != 1L ||
      anyNA(repo_) || anyNA(dir_)) {
    stop("Each team must have exactly one repository and directory.")
  }
  
  # Repository identifier already includes the organization
  repo_ <- sub("\\.git$", "", repo_)
  
  if (!grepl("^bcorgbio/[^/[:space:]]+$", repo_)) {
    stop("Expected repository format: bcorgbio/Module5_Team1")
  }
  
  local_dir <- normalizePath(
    file.path(base_dir, dir_),
    mustWork = TRUE
  )
  
  repo_url <- paste0("https://github.com/", repo_, ".git")
  
  users <- unique(trimws(c(as.character(x$handle), "ckenaley")))
  users <- users[!is.na(users) & nzchar(users)]
  
  # Quote arguments for bash
  q <- function(z) shQuote(z, type = "sh")
  
  commands <- c(
    "set -e",
    paste("cd", q(local_dir)),
    
    # Initialize locally if needed
    "if [ ! -d .git ]; then git init; fi",
    
    # Create the private GitHub repository if needed
    paste0(
      "if ! gh repo view ", q(repo_),
      " >/dev/null 2>&1; then gh repo create ",
      q(repo_), " --private; fi"
    ),
    
    # Add or repair the remote URL
    paste0(
      "if git remote get-url origin >/dev/null 2>&1; then ",
      "git remote set-url origin ", q(repo_url),
      "; else git remote add origin ", q(repo_url), "; fi"
    ),
    
  
    
    # Grant individual collaborators push access
    vapply(users, function(user) {
      paste(
        "gh api --method PUT",
        q(paste0("repos/", repo_, "/collaborators/", user)),
        "-f permission=push"
      )
    }, character(1))
  )
  
  # Run a temporary script without changing R's working directory
  script <- tempfile(pattern = "git_create_", fileext = ".sh")
  on.exit(unlink(script), add = TRUE)
  writeLines(commands, script)
  
  status <- system2("bash", args = shQuote(script))
  
  if (status != 0L) {
    stop("Repository setup failed for ", repo_, "; see output above.")
  }
  
  message("Repository ready: ", repo_)
  invisible(repo_)
}


git_create(
  1
)

sapply(dat$team[-1] %>% unique,git_create)

######




         