#' ---
#' title: Examples
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---
#' <!-- rmarkdown::render("supervised_play/nice_code.R") -->
#' <!-- [See here.](http://brooksandrew.github.io/simpleblog/articles/render-reports-directly-from-R-scripts/) -->
#' Set global options 
#+ setup, warning=FALSE, message=FALSE
# knitr::opts_chunk$set(echo = FALSE)

set.seed(123)
library(here)
library(tidyverse) 
library(Biobase) 

walk(dir(path = here("R"), full.names = TRUE), source) 

############################################################
# LOAD DATA, DEFINE VARIABLES, RECODE VARIABLES
############################################################

load_data(reconciled = FALSE, remove_inflam = FALSE)
 

############################################################
# CECILIA
############################################################

sigs = readRDS(file = "/home/share/projects/aging/completetables.rds")

sigs = map(1:length(sigs), ~ sigs[[.x]]$userId %>%  set_names(sigs[[.x]]$description) %>% map(str_split, ";") %>% flatten())  

get_intersection  = 
  function(intersection_order, op = intersect) {
    # GET ALL INTERSECTIONS OF A GIVEN ORDER
    # y = combinations of sets
    # nm = names of such sets
    # z = intersection of such sets
    
    tibble(y = combn(sigs, intersection_order, simplify = FALSE),
           nm = map(y, names),
           z =  map(y, reduce, op)) %>%
      arrange(-lengths(z)) %>% 
      filter(lengths(z) > 0)
  } 

map(sigs, 
    function(x){
      sigs = x  
      # overlap between the  sets and the focal signature
      a = sigs 
      b = list(signatures$outcome_set$aging_mRNA)
      crossing(a,b) %>% 
        pmap_dbl(function(a,b) length(intersect(a,b))/length(a)) %>% 
        enframe(name = "signature", value = "proportion which overlaps with aging")
    }
)

 