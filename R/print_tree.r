#' @title Theme hierarchy tree view. 
#' 
#' @description \code{print_tree} Print a theme and its descendents in a 
#' tree format.
#' @return Given a \code{theme} object as input, \code{print_tree} prints 
#' a tree structure of its descendent themes.
#' @param mytheme A pkg{stoRy} package \code{theme} object.
#' @param pruneMethod default \code{c("simple", "dist")}. The 
#' \code{"Simple"} option is the default and prints up to \code{limit} 
#' themes as the appear in order in the tree. The \code{"dist"} option
#' prints the more upper level themes in the hierarchy.
#' @param limit The maximum number of themes to be printed. The default 
#' valuse is \code{100}.
#' @importFrom data.tree as.Node
#' @export
#' @examples
#' ########################################################################
#' # Display the "the human condition" theme hierarchy in tree format     #
#' ########################################################################
#' theme_name <- "the human condition"
#' mytheme <- theme$new(theme_name)
#' print_tree(mytheme, pruneMethod = "dist", limit = 50)
print_tree = function(mytheme, pruneMethod = c("simple", "dist"), limit = 100) { 
  pruneMethod <- pruneMethod[1]
  data <- sysdata$theme_dict
  theme_id <-  which(data[, "Theme"] == mytheme$theme)
  child_ids <- which(data[, "ParentTheme"] == mytheme$theme)
  no_of_child_ids <- length(child_ids)
  node_ids <- c(theme_id, child_ids)

  while(no_of_child_ids >= 1) {
    child_id <- child_ids[1]
    child_ids <- child_ids[-1]
    grandchild_ids <- which(data[, "ParentTheme"] == data[child_id, "Theme"])
    no_of_grandchild_ids <- length(grandchild_ids)

    if(no_of_grandchild_ids >= 1) {
      node_ids <- unique(c(node_ids, grandchild_ids))
    }

    child_ids <- unique(c(child_ids, grandchild_ids))
    no_of_child_ids <- length(child_ids)
  }

  no_of_node_ids <- length(node_ids)
  leaf_ids <- numeric()

  for(i in 1:no_of_node_ids) {
    node_id <- node_ids[i]
    node <- data[node_id, "Theme"]
    if(node%in%data[, "ParentTheme"] == FALSE) {
      leaf_ids <- c(leaf_ids, node_id)
    }
  }

  no_of_leaf_ids <- length(leaf_ids)
  path_strings <- character(no_of_leaf_ids)

  for(i in 1:no_of_leaf_ids) {
    path_string <- character()
    leaf_id <- leaf_ids[i]
    thistheme <- data[leaf_id, "Theme"]

    while(thistheme != mytheme$theme) {
      path_string <- c(thistheme, path_string)
      thistheme <- data[which(data[, "Theme"] == thistheme), "ParentTheme"]
    }

    path_string <- c(mytheme$theme, path_string)
    path_strings[i] <- paste(path_string, collapse = "/")
  }

  subdata <- data.frame(Theme = data[leaf_ids, "Theme"], pathString = path_strings)
  tree <- as.Node(subdata)
  print(tree, pruneMethod = pruneMethod, limit = limit)
}