library(stringr)
library(dplyr)
library(skimr)
library(readxl)
library(glue)
library(ggplot2)
library(tidyr)
library(readr)
library(skimr)
source('helpers.R')

df <- readr::read_csv("data/combined_dataset_nov_15.csv")
head(df)

no_orient <- list(
    c(1, 0, 1, 0),
    c(0, 1, 0, 1),
    c(0, 0, 0, 0),
    c(1, 1, 1, 1),
    c(1, 0, 0, 1),
    c(0, 1, 1, 0)
)
ns_dom <- list(
    c(1, 1, 0, 1),
    c(0, 1, 0, 0),
    c(1, 0, 0, 0),
    c(1, 1, 1, 0),
    c(1, 1, 0, 0)
)
ew_dom <- list(
    c(0, 0, 1, 1),
    c(1, 0, 1, 1),
    c(0, 1, 1, 1),
    c(0, 0, 0, 1),
    c(0, 0, 1, 0)
)  

df <- 
    df %>%
    apply_comparison("no_orient", no_orient) %>%
    apply_comparison("ns_dom", ns_dom) %>%
    apply_comparison("ew_dom", ew_dom) 

df %>%
    select(walkway_N, walkway_S, walkway_E, walkway_W, no_orient, ns_dom, ew_dom)

readr::write_csv(df, "data/combined_dataset_nov_15_v2.csv")
