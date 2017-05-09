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
#' print(mytheme)
theme <- R6Class("theme",
			public = list(
				theme = "character",
				theme_id = "numeric",
				definition = "character",
				example = "character",
				parent_theme = "vector",
				child_themes = "vector",
				story_ids = "vector",
				choice_story_ids = "vector",
				major_story_ids = "vector",
				minor_story_ids = "vector",
				initialize = function(theme) {
					if(!missing(theme) && !is.null(theme)) {
						check_theme(theme)
						self$theme <- theme
						self$theme_id <- as.numeric(sysdata$todata[which(sysdata$todata[, "Theme"] == theme), "ThemeID"])
						self$definition <- sysdata$todata[which(sysdata$todata[, "Theme"] == theme), "Definition"]
						self$example <- sysdata$todata[which(sysdata$todata[, "Theme"] == theme), "Example"]
						self$parent_theme <- sysdata$todata[which(sysdata$todata[, "Theme"] == theme), "ParentTheme"]
						self$child_themes <- sysdata$todata[which(sysdata$todata[, "ParentTheme"] == theme), "Theme"]
						self$story_ids <- sysdata$tsdata[which(sysdata$tsdata[, "Theme"] == theme), "SID"]
						self$choice_story_ids <- sysdata$tsdata[which(sysdata$tsdata[, "Theme"] == theme), "SIDChoice"]
						self$major_story_ids <- sysdata$tsdata[which(sysdata$tsdata[, "Theme"] == theme), "SIDMajor"]
						self$minor_story_ids <- sysdata$tsdata[which(sysdata$tsdata[, "Theme"] == theme), "SIDMinor"]
					}
				},
				print = function(...) {
					cat(paste0("Theme: ", self$theme, "\n"))
					cat(paste0("Theme ID: ", self$theme_id, "\n"))
					cat(paste0("Definition: ", self$definition, "\n"))
					cat(paste0("Example: ", self$example, "\n"))
					cat(paste0("Parent Theme: ", self$parent_theme, "\n"))
					cat(paste0("Child Themes: ", paste(self$child_themes, collapse=", "), "\n"))
					cat(paste0("Story IDs: ", paste(self$story_ids, collapse=", "), "\n"))
					cat(paste0("Choice Story IDs: ", paste(self$choice_story_ids, collapse=", "), "\n"))
					cat(paste0("Major Story IDs: ", paste(self$major_story_ids, collapse=", "), "\n"))
					cat(paste0("Minor Story IDs: ", paste(self$minor_story_ids, collapse=", "), "\n"))
				}
			)
)


