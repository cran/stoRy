#############################################################
#Reference Class definition
#############################################################
#' Storysets objects
#'
#' The \pkg{stoRy} package uses the \code{storysets} object 
#' to store lists of story IDs.
#' 
#' @section Fields:
#' 
#' Each storysets object has the following five fields
#' \describe{
#' \item{file}{A string specifying the name of an .smt format
#' storysets file. See examples.}
#' \item{names}{A vector of storyset names.}
#' \item{comments}{A vector of comments describing the storysets.}
#' \item{no_of_storysets}{The number of storysets in the .smt file.}
#' \item{storysets_story_ids}{A list of vectors containing the storyset story IDs.}}
#' @param storysets The object is created by passing an .smt format
#' storyset file name. See examples.
#' @docType class
#' @importFrom R6 R6Class
#' @exportClass storysets
#' @export storysets
#' @examples
#' #######################################################################
#' # List all storysets files available in the \pkg{stoRy} package       #
#' #######################################################################
#' list.files(system.file("storysets", package = "stoRy"))
#' 
#' #######################################################################
#' # Load the Star Trek aliens storysets smt file and storysets object   #
#' #######################################################################
#' file <- system.file("storysets", "aliens.smt", package = "stoRy")
#' mystorysets <- storysets$new(file)
#' print(mystorysets)
storysets <- R6Class("storysets",
			public = list(
				file = "character",
				names = "vector",
				comments = "vector",
				no_of_storysets = "numeric",
				storysets_story_ids = "list",
				initialize = function(file) {
					check_storysets_file(file)
					self$file <- file
					raw_storysets <- readLines(file)
					no_of_storysets <- length(raw_storysets)
					names <- character(no_of_storysets)
					comments <- character(no_of_storysets)
					storysets_story_ids <- vector("list", length = no_of_storysets)

					for(i in 1:no_of_storysets) {
						raw_storyset_tokens <- strsplit(raw_storysets[i], split = "\t")[[1]]
						names[i] <- raw_storyset_tokens[1]
						comments[i] <- raw_storyset_tokens[2]
						storysets_story_ids[[i]] <- raw_storyset_tokens[-c(1,2)]
						names(storysets_story_ids) <- names
					}

					self$names <- names
					self$comments <- comments
					self$no_of_storysets <- no_of_storysets
					self$storysets_story_ids <- storysets_story_ids
				},
				print = function(...) {
					for(i in 1:self$no_of_storysets) {
						cat(paste0("$", self$names[i], " (", self$comments[i], ")\n"))
						cat(paste0(paste0(self$storysets_story_ids[[i]], collapse = " "), "\n\n"))
					}
				}
			)
		)


