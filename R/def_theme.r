#############################################################
#Reference Class definition
#############################################################
#' Theme objects
#'
#' The \pkg{stoRy} package uses the \code{theme} object to store
#' information about themes in the package theme dataset.
#' @param theme A string representing a theme in the dataset.
#' @docType class
#' @importFrom R6 R6Class
#' @exportClass theme
#' @export theme
#' @examples
#' ########################################################################
#' # Create a theme object for the theme "utopia" and print the result    #
#' ########################################################################
#' theme_name <- "utopia"
#' mytheme <- theme$new(theme_name)
#' mytheme$print()
#' 
#' ########################################################################
#' # Display the "utopia" theme and its descendent themes in tree format  #
#' ########################################################################
#' print_tree(mytheme)
#' 
#' ########################################################################
#' # Display the "society" theme hierarchy in tree format                 #
#' ########################################################################
#' theme_name <- "society"
#' mytheme <- theme$new(theme_name)
#' print_tree(mytheme, pruneMethod = "dist", limit = 50)
theme <- R6Class("theme",
			public = list(
				theme = "character",
				theme_id = "numeric",
				definition = "character",
				example = "character",
				parent_theme = "vector",
				child_themes = "vector",
				story_ids = "vector",
				central_story_ids = "vector",
				peripheral_story_ids = "vector",
				initialize = function(theme) {
					if(!missing(theme) && !is.null(theme)) {
						check_theme(theme)
						self$theme <- theme
						self$theme_id <- as.numeric(sysdata$story_ids_by_theme[which(sysdata$story_ids_by_theme[, "Theme"] == theme), "ThemeID"])
						self$definition <- sysdata$theme_dict[which(sysdata$theme_dict[, "Theme"] == theme), "Definition"]
						self$example <- sysdata$theme_dict[which(sysdata$theme_dict[, "Theme"] == theme), "Example"]
						self$parent_theme <- sysdata$theme_dict[which(sysdata$theme_dict[, "Theme"] == theme), "ParentTheme"]
						self$child_themes <- sysdata$theme_dict[which(sysdata$theme_dict[, "ParentTheme"] == theme), "Theme"]
						self$story_ids <- sysdata$story_ids_by_theme[which(sysdata$story_ids_by_theme[, "Theme"] == theme), "StoryID"]
						self$central_story_ids <- sysdata$story_ids_by_theme[which(sysdata$story_ids_by_theme[, "Theme"] == theme), "StoryIDCentral"]
						self$peripheral_story_ids <- sysdata$story_ids_by_theme[which(sysdata$story_ids_by_theme[, "Theme"] == theme), "StoryIDPeripheral"]
					}
				},
				print = function(...) {
					cat(paste0("Theme:                ", self$theme, "\n"))
					cat(paste0("Theme ID:             ", self$theme_id, "\n"))
					cat(paste0("Definition:           ", self$definition, "\n"))
					cat(paste0("Example:              ", self$example, "\n"))
					cat(paste0("Parent Theme:         ", self$parent_theme, "\n"))
					cat(paste0("Child Themes:         ", paste(self$child_themes, collapse = ", "), "\n"))
					cat(paste0("Central Story IDs:    ", paste(self$central_story_ids, collapse = ", "), "\n"))
					cat(paste0("Peripheral Story IDs: ", paste(self$peripheral_story_ids, collapse = ", "), "\n"))
				}
			)
)


