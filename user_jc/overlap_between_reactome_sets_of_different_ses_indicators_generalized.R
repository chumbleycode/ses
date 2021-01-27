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

library(tidyverse)
library(here)

gsea_genesetnames <- readRDS(here("gsea_genesetnames.rds"))
complete_tables <- readRDS(here("gsea_removefig1A.rds"))
complete_tables <- complete_tables %>% reduce(rbind) %>% select(geneSet, description) %>% unique()
gsea_genesetnames_complement <- map(gsea_genesetnames, ~setdiff(complete_tables$geneSet, .x)) # the complement of these reactome terms

# the universe of reactome terms considered here is "all reactome sets deemed significantly related to at least one ses predictor"
# there are then 2^5 intersections in the venn diagram (i.e. intersections over the 5 ses predictors, with each being the complement or not)

universe= 
  list(gsea_genesetnames_complement,
       gsea_genesetnames) %>% 
  transpose() 

get_venn_cell = 
  function(P){ 
    
    # Logic: each venn cell has form ABCDE (interpreted as set intersection or
    # product of set characteristic functions). The collection of 2^D (here 2^5)
    # such set intersections -  where every factor of ABCDE or it's a compliment
    # are permitted independently - fill the 2^D cells of the Ven diagram.
    
    # Now, get the intersection for each collection of "literals" of ABCDE (a "literal"
    # of A is either A or the complement of A, i.e. A') for each row in matrix
    # of indexes P.
    map(1:dim(P)[1], 
        # pluck the corresponding subset 
        ~ map2(names(universe),
               P[.x, ], 
               ~ pluck(universe, .x, .y + 1)
        ) 
    ) %>% 
      # interestion over all
      map(reduce, intersect)
  }

crossing(!!!set_names(rerun(length(universe), 0:1), 
                      names(universe))) %>% 
  mutate(geneSet = get_venn_cell(.)) %>% 
  unnest(geneSet) %>% 
  left_join(complete_tables, by = "geneSet")  %>% 
  knitr::kable()
