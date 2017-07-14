#############################################################
#Reference Class definition
#############################################################
#' Story objects
#'
#' The \pkg{stoRy} package uses the \code{story} object to store
#' story themes together with other metadata.
#' 
#' @section Fields:
#' 
#' Each story object has the following ten fields
#' \describe{
#' \item{story_id}{A story ID. See examples.}
#' \item{title}{A string giving the story title.}
#' \item{writer}{A string giving the story writer.}
#' \item{director}{A string giving the story director.}
#' \item{original_air_date}{A string giving the story original air date in the forr YYYY-MM-DD}
#' \item{summary}{A string giving a summary of the story.}
#' \item{characters}{A list of story characters. The list has three fields: \code{ObjectCharacters},
#' \code{MajorCharacters}, and \code{MinorCharacters}. Each list entry should be a ", " separated string
#' of character names.}
#' \item{themes}{A data frame of story themes with associated metadata.}
#' \item{settings}{A data frame of story settings with associated metadata.}
#' \item{keywords}{A data frame of story keywords with associated metadata.}}
#' @param story The object is typically created by passing a story ID from any of the 
#' Star Trek TOS/TAS/TNG series episods to construct the object automatically from system data.  
#' A user-defined story ID may also be accepted in which case the fields will be empty,
#' if not supplied by the user.
#' @docType class
#' @importFrom R6 R6Class
#' @exportClass story
#' @export story
#' @examples
#' ########################################################################
#' # Create a story object for the Star Trek The Original Series episode  #
#' # tos1x19 "The Arena" and manipulate it in various ways                #
#' ########################################################################
#' story_id <- "tos1x19"
#' mystory <- story$new(story_id)
#' print(mystory)
#' 
#' ########################################################################
#' # Add "neo-luddist utopia" as a central theme                          #
#' ########################################################################
#' mystory$add_theme(theme = "neo-luddist utopia", level = "central")
#' 
#' ########################################################################
#' # Remove "neo-luddist utopia" as a central theme                       #
#' ########################################################################
#' mystory$remove_theme(theme = "neo-luddist utopia")
#' 
#' ########################################################################
#' # Add "mountain" as a setting                                          #
#' ########################################################################
#' mystory$add_setting(setting = "mountain")
#' 
#' ########################################################################
#' # Add a new keyword                                                    #
#' ######################################################################## 
#' mystory$add_keyword(keyword = "Captain Kirk is climbing a mountain")
story <- R6Class("story",
			public = list( 
				story_id = "character",
				title = "character",
				writer = "character",
				director = "character",
				original_air_date = "character",
				summary = "character",
				characters = "list",
				themes = "data.frame",
				settings = "data.frame",
				keywords = "data.frame",
				initialize = function(story_id, title, writer, director, original_air_date, summary, characters, themes, settings, keywords) {
					IS_RESERVED_STORY_ID <- ifelse(story_id %in% sysdata$RESERVED_STORY_IDS, TRUE, FALSE)
					if(!missing(story_id) && !is.null(story_id)) {
						check_story_id(story_id)
						self$story_id <- story_id

						if(IS_RESERVED_STORY_ID) {
							self$title <- sysdata$story_metadata[story_id, "Title"]
							self$summary <- sysdata$story_metadata[story_id, "Summary"]
							self$writer <- sysdata$story_metadata[story_id, "Writer"]
							self$director <- sysdata$story_metadata[story_id, "Director"]
							self$original_air_date <- sysdata$story_metadata[story_id, "OriginalAirDate"]

							## initialize story characters
							self$characters <- list(
								object_characters = strsplit(sysdata$story_metadata[story_id, "ObjectCharacters"], split = ", ")[[1]],
								main_characters = strsplit(sysdata$story_metadata[story_id, "MainCharacters"], split = ", ")[[1]],
								supporting_cast = strsplit(sysdata$story_metadata[story_id, "SupportingCast"], split = ", ")[[1]]
							)
							
							## initialize story themes
							themes <- sysdata$themed_stories[which(sysdata$themed_stories$StoryID == story_id), -c(1, 2, 3)]
							colnames(themes) <- c("theme", "level", "theme_id", "comment", "related_characters", "character_class", "related_aliens", "related_things")
							rownames(themes) <- NULL
							self$themes <- themes

							## initialize story settings
							settings <- sysdata$story_settings[which(sysdata$story_settings$StoryID == story_id), c(2, 4)]
							colnames(settings) <- c("setting", "capacity")
							rownames(settings) <- NULL
							self$settings <- settings
							
							## initialize story keywords
							keywords <- sysdata$story_keywords[which(sysdata$story_keywords$StoryID == story_id), -1]
							colnames(keywords) <- c("keyword", "comment", "timing", "parent_keyword")
							rownames(keywords) <- NULL
							self$keywords <- keywords
						} else {
							if(!missing(title) && !is.null(title)) {
								check_title(title)
								self$title <- title
							}
							if(!missing(summary) && !is.null(summary)) {
								check_summary(summary)
								self$summary <- summary
							}
							if(!missing(writer) && !is.null(writer)) {
								check_writer(writer)
								self$writer <- writer
							}
							if(!missing(director) && !is.null(director)) {
								check_director(director)
								self$director <- director
							}
							if(!missing(original_air_date) && !is.null(original_air_date)) {
								check_original_air_date(original_air_date)
								self$original_air_date <- original_air_date
							}
							if(!missing(characters) && !is.null(characters)) {
								check_title(characters)
								characters_int <- list(
									ObjectCharacters = ifelse(!is.null(characters[["object_characters"]]), character()),
									MainCharacters = ifelse(!is.null(characters[["main_characters"]]), character()),
									SupportingCast = ifelse(!is.null(characters[["supporting_cast"]]), character())
								)
								self$characters <- characters_int
							}
							if(!missing(themes) && !is.null(themes)) {
								check_themes(themes)
								self$themes <- themes
							}
							if(!missing(settings) && !is.null(settings)) {
								check_settings(settings)
								self$settings <- settings
							}
							if(!missing(keywords) && !is.null(keywords)) {
								check_keywords(keywords)
								self$keywords <- keywords
							}
						}
					}
				},
				add_theme = function(theme, level, comment, related_cahracters, character_class, related_aliens, related_things) {
					check_add_theme(theme, level)
					if(theme %in% mystory$themes[, "theme"]) {
						stop(paste0("Your theme \"", theme, "\" already occurs in this story."))
					} else {
						new_entry <- c(theme, level)
						if(!missing(comment) && !is.null(comment)) {
							new_entry <- c(new_entry, comment)
						} else {
							new_entry <- c(new_entry, "")
						}
						if(!missing(related_cahracters) && !is.null(related_cahracters)) {
							new_entry <- c(new_entry, related_cahracters)
						} else {
							new_entry <- c(new_entry, "")
						}
						if(!missing(character_class) && !is.null(character_class)) {
							new_entry <- c(new_entry, character_class)
						} else {
							new_entry <- c(new_entry, "")
						}
						if(!missing(related_aliens) && !is.null(related_aliens)) {
							new_entry <- c(new_entry, related_aliens)
						} else {
							new_entry <- c(new_entry, "")
						}
						if(!missing(related_things) && !is.null(related_things)) {
							new_entry <- c(new_entry, related_things)
						} else {
							new_entry <- c(new_entry, "")
						}
						self$themes <- rbind(self$themes, new_entry)
						self$themes <- self$themes[with(self$themes, order(level, theme)), ]
					}
				},
				remove_theme = function(theme) {
					check_remove_theme(theme)
					if(!(theme %in% mystory$themes[, "theme"])) {
						stop(paste0("Your theme \"", theme, "\" does not occur in this story."))
					} else {
						remove_row <-which(mystory$themes[,"theme"] == theme)
						self$themes <- self$themes[-remove_row,]
					}
				},
				add_setting = function(setting, capacity) {
					check_add_setting(setting)
					new_entry <- setting
					if(!missing(capacity) && !is.null(capacity)) {
						new_entry <- c(new_entry, capacity)
					} else {
						new_entry <- c(new_entry, "")
					}
					self$settings <- rbind(self$settings, new_entry)
				},
				remove_setting = function(setting) {
					check_remove_setting(setting)
					if(!(setting %in% mystory$settings[, "setting"])) {
						stop(paste0("Your setting \"", setting, "\" does not occur in this story."))
					} else {
						remove_row <-which(mystory$settings[,"setting"] == setting)
						self$settings <- self$settings[-remove_row,]
					}
				},
				add_keyword = function(keyword, comment, timing, parent_keyword) {
					check_add_keyword(keyword)
					new_entry <- keyword
					if(!missing(comment) && !is.null(comment)) {
						new_entry <- c(new_entry, comment)
					} else {
						new_entry <- c(new_entry, "")
					}
					if(!missing(timing) && !is.null(timing)) {
						new_entry <- c(new_entry, timing)
					} else {
						new_entry <- c(new_entry, "")
					}
					if(!missing(parent_keyword) && !is.null(parent_keyword)) {
						new_entry <- c(new_entry, parent_keyword)
					} else {
						new_entry <- c(new_entry, "")
					}
					self$keywords <- rbind(self$keywords, new_entry)
				},
				remove_keyword = function(keyword) {
					check_remove_keyword(keyword)
					if(!(keyword %in% mystory$keywords[, "keyword"])) {
						stop(paste0("Your keyword \"", keyword, "\" does not occur in this story."))
					} else {
						remove_row <-which(mystory$keywords[,"keyword"] == keyword)
						self$keywords <- self$keywords[-remove_row,]
					}
				},
				print = function(...) {
					cat(paste0("Story ID:          ", self$story_id, "\n"))
					cat(paste0("Title:             ", self$title, "\n"))
					cat(paste0("Writer:            ", self$writer, "\n"))
					cat(paste0("Dirctor:           ", self$director, "\n"))
					cat(paste0("Original Air Date: ", self$original_air_date, "\n"))
					cat(paste0("Summary:           ", self$summary, "\n\n"))
					cat(paste0("Object Characters: ", paste(self$characters[["object_characters"]], collapse=", "), "\n"))
					cat(paste0("Main Characters:   ", paste(self$characters[["main_characters"]], collapse=", "), "\n"))
					cat(paste0("Suporting Cast:    ", paste(self$characters[["supporting_cast"]], collapse=", "), "\n\n"))
					cat(paste0("Central Themes:    ", paste(self$themes[which(self$themes[, "level"] == "central"), "theme"], collapse = ", "), "\n\n"))
					cat(paste0("Peripheral Themes: ", paste(self$themes[which(self$themes[, "level"] == "peripheral"), "theme"], collapse = ", "), "\n\n"))
					cat(paste0("Settings:          ", paste(self$settings[, "setting"], collapse = ", "), "\n\n"))
					cat(paste0("Keywords:          ", paste(self$keywords[, "keyword"], collapse = ", "), "\n\n"))
				}
			)
)


