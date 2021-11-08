## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(stoRy)

## -----------------------------------------------------------------------------
# install.packages("devtools")
# devtools::install_github("theme-ontology/stoRy")

## -----------------------------------------------------------------------------
library(stoRy)

## -----------------------------------------------------------------------------
help(package = "stoRy")

## -----------------------------------------------------------------------------
?get_similar_stories

## -----------------------------------------------------------------------------
citation("stoRy")

## -----------------------------------------------------------------------------
which_lto()

## -----------------------------------------------------------------------------
set_lto(version = "demo")

## -----------------------------------------------------------------------------
print_lto()

## -----------------------------------------------------------------------------
?`lto-demo`

## -----------------------------------------------------------------------------
demo_metadata_tbl <- clone_active_metadata_tbl()
demo_themes_tbl <- clone_active_themes_tbl()
demo_stories_tbl <- clone_active_stories_tbl()
demo_collections_tbl <- clone_active_collections_tbl()

## -----------------------------------------------------------------------------
theme <- Theme$new(theme_name = "mass hysteria")

## -----------------------------------------------------------------------------
# Print stylized text:
theme

# Print in plain text .th.txt file format:
theme$print(canonical = TRUE)

## -----------------------------------------------------------------------------
theme$annotations()

## -----------------------------------------------------------------------------
# install.packages("dplyr")
suppressMessages(library(dplyr))
# install.packages("stringr")
library(stringr)
demo_themes_tbl <- clone_active_themes_tbl()
demo_themes_tbl %>% filter(str_detect(theme_name, "mass"))

## -----------------------------------------------------------------------------
story <- Story$new(story_id = "tz1959e1x22")

## -----------------------------------------------------------------------------
# In stylized text format:
story

# In plain text .st.txt file format:
story$print(canonical = TRUE)

## -----------------------------------------------------------------------------
themes <- story$themes()
themes

## -----------------------------------------------------------------------------
title <- "The Monsters Are Due on Maple Street"
demo_stories_tbl <- clone_active_stories_tbl()
story_id <- demo_stories_tbl %>% filter(title == !!title) %>% pull(story_id)
story_id

## -----------------------------------------------------------------------------
story$collections()

## -----------------------------------------------------------------------------
collection <- Collection$new(collection_id = "Collection: tvseries: The Twilight Zone (1959)")

## -----------------------------------------------------------------------------
# Print stylized text:
collection

# Print in plain text .st.txt file format:
collection$print(canonical = TRUE)

## -----------------------------------------------------------------------------
demo_collections_tbl <- clone_active_collections_tbl()
demo_collections_tbl

## -----------------------------------------------------------------------------
collection <- Collection$new(collection_id = "Collection: tvseries: The Twilight Zone (1959)")
result_tbl <- get_featured_themes(collection)
result_tbl

## -----------------------------------------------------------------------------
result_tbl <- get_featured_themes()
result_tbl

## -----------------------------------------------------------------------------
test_collection <- Collection$new(collection_id = "Collection: tvseries: The Twilight Zone (1959)")
result_tbl <- get_enriched_themes(test_collection)
result_tbl

## -----------------------------------------------------------------------------
result_tbl <- get_enriched_themes(test_collection, weights = list(choice = 1, major = 1, minor = 0))
result_tbl

## -----------------------------------------------------------------------------
query_story <- Story$new(story_id = "tz1959e1x22")
result_tbl <- get_similar_stories(query_story)
result_tbl

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(123)
#  result_tbl <- get_story_clusters()
#  result_tbl

## ---- eval = FALSE------------------------------------------------------------
#  cluster_id <- 3
#  pull(result_tbl, stories)[[cluster_id]]
#  pull(result_tbl, themes)[[cluster_id]]

## ---- eval = FALSE------------------------------------------------------------
#  cluster_id <- 5
#  pull(result_tbl, stories)[[cluster_id]]
#  pull(result_tbl, themes)[[cluster_id]]

## ---- eval = FALSE------------------------------------------------------------
#  cluster_id <- 7
#  pull(result_tbl, stories)[[cluster_id]]
#  pull(result_tbl, themes)[[cluster_id]]

## ---- eval = FALSE------------------------------------------------------------
#  cluster_id <- 10
#  pull(result_tbl, stories)[[cluster_id]]
#  pull(result_tbl, themes)[[cluster_id]]

## ---- eval = FALSE------------------------------------------------------------
#  cluster_id <- 11
#  pull(result_tbl, stories)[[cluster_id]]
#  pull(result_tbl, themes)[[cluster_id]]

## ---- eval = FALSE------------------------------------------------------------
#  cluster_id <- 13
#  pull(result_tbl, stories)[[cluster_id]]
#  pull(result_tbl, themes)[[cluster_id]]

## ---- eval = FALSE------------------------------------------------------------
#  lto_version_statuses()

## ---- eval = FALSE------------------------------------------------------------
#  configure_lto(version = "latest")

## ---- eval = FALSE------------------------------------------------------------
#  set_lto(version = "latest")

## ---- eval = FALSE------------------------------------------------------------
#  which_lto()

