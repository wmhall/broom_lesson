library(broom)
library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(pryr)

##slightly modified version of code from: https://github.com/dgrtwo/broom_paper

# load all required packages so the objects in them can be discovered
desc_file <- system.file("DESCRIPTION", package = "broom")
suggested_pkgs <- devtools:::read_dcf(desc_file)$Suggests %>%
  str_split(",\\s+") %>% .[[1]]
suggested_pkgs

for (l in suggested_pkgs) {
  library(l, character.only = TRUE)
}


tab <- data.frame(method = c("tidy", "augment", "glance")) %>%
  group_by(method) %>%
  do(data.frame(class = as.character(methods(as.character(.$method))))) %>%
  mutate(class = str_sub(str_extract(class, "\\..*"), 2))


agg <- function(x) ifelse(length(x) > 0, "X", "")
tab <- dcast(tab, class ~ method, fun.aggregate = agg)
# for each object, determine its package

pkgs <- sapply(tab$class, failwith(NA, where, quiet = TRUE))
pkgs <- unlist(sapply(pkgs, function(p) attr(p, "name")))
pkgs <- lapply(pkgs,function(p) str_sub(p,9))
pkgs <- unlist(pkgs)
 
# add some manually
pkgs <- c(pkgs, Arima = "stats", confint.glht = "multcomp", data.frame = "base",
          htest = "stats", mer = "lme4", merMod = "lme4",
          pairwise.htest = "stats", ridgelm = "MASS", spec = "stats",
          summary.glht = "multcomp", table = "base")
pkgs["anova"] <- "stats"


tab$package <- pkgs[tab$class]

pkgs_order <- c("base", "stats", "")
pkgs_order <- c(pkgs_order, sort(setdiff(tab$package, pkgs_order)))
tab$package <- factor(tab$package, pkgs_order)

tab <- tab %>% na.omit() %>%
  group_by(package, tidy, augment, glance) %>%
  summarise(class = paste(class, collapse = ", ")) %>%
  dplyr::select(package, class, tidy, augment, glance) %>%
  ungroup() %>%
  arrange(package, -nchar(class)) 

readr::write_csv(tab, "broom_supported_objects.csv")
