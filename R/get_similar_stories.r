#' @title Recommend stories similar to a user-selected one. 
#' 
#' @description \code{get_similar_stories} returns a list of stories,
#' sorted by thematic similarity with respect to a user-selected story
#' @return Returns a data frame where each row corresponds to a story.
#' The column SimilarityScore contains scores indicating story similarity to the selected story.
#' @param mystory A story class object. 
#' @param background_storyset default \code{c("tos", "tas", "tng", "voy")}. A string indicating the storyset to use as background.
#' Possible values are \code{c("tos", "tas")}, \code{"tng"}, \code{"voy"}, and \code{c("tos", "tas", "tng", "voy")}.
#' @param theme_levels default \code{c("central", "peripheral")}. A string indicating the theme levels to use in the analysis.
#' Possible values are \code{"central"}, \code{"peripheral"}, and \code{c("central", "peripheral")}.
#' @param similarity_function default "cosine". 
#' Possible values are \code{"cosine"}, \code{"cosine tf-idf"}, and \code{c("soft cardinality")}.
#' @param blacklist default \code{""}. A vector of blacklisted themes.
#' @references Sheridan, P., Onsjö, M., and Jiménez, S. (2018): 
#' "A Content-based Filtering Story Recommender System for Theme Lovers with an Application to the Star Trek Television Franchise", 
#' ArXiv.
#' @export
#' @examples
#' #######################################################################
#' # Recommend Star Trek stories similar to the Star Trek: Voyager       #
#' # episode False Profits (voy3x05)                                     #
#' #######################################################################
#' \dontrun{story_id <- "voy3x05"
#' mystory <- story$new(story_id)
#' result <- get_similar_stories(mystory)}
#' 
#' #######################################################################
#' # Output top ten most similar stories                                 #
#' #######################################################################
#' \dontrun{result[1:10,]}
get_similar_stories = function(mystory, background_storyset = c("tos", "tas", "tng", "voy"), theme_levels = c("central", "peripheral"), similarity_function = c("cosine", "cosine tf-idf", "soft cardinality"), blacklist = "") {	
	## set choice of similarity function
	similarity_function <- similarity_function[1]

	## soft cardinality parameter (hard coded)
	p <- 3

	## number of canon themes
	no_of_themes <- length(sysdata$CANON_THEMES)

	## check mystory story id is valid
	mystory_id <- mystory$story_id
	check_story_id(mystory_id)
	check_is_story_id_reserved(mystory_id)

	## load theme pairwise distance matrix
	if(similarity_function == "soft cardinality") {
		pd_mat <- sysdata$"pdmat"
	}

	## gather test story themes
	if(identical(theme_levels, c("central", "peripheral"))) {
		mystory_themes <- sort(setdiff(mystory$themes[, "theme"], blacklist))
	} else if(identical(theme_levels, "central")) {
		mystory_themes <- sort(setdiff(mystory$themes[which(mystory$themes[, "level"] == "central"), "theme"], blacklist))
	} else if(identical(theme_levels, "peripheral")) {
		mystory_themes <- sort(setdiff(mystory$themes[which(mystory$themes[, "level"] == "peripheral"), "theme"], blacklist))
	}
	no_of_mystory_themes <- length(mystory_themes)

	## gather themes of reference stories
	file <- system.file("storysets", "series.smt", package = "stoRy")
	series_storysets <- storysets$new(file)
	if(identical(background_storyset, c("tos", "tas"))) {
		refstory_ids <- c(series_storysets$storysets_story_ids[["TOS_ALL_EPISODES"]], series_storysets$storysets_story_ids[["TAS_ALL_EPISODES"]])
	} else if(identical(background_storyset, "tng")) {
		refstory_ids <- series_storysets$storysets_story_ids[["TNG_ALL_EPISODES"]]
	} else if(identical(background_storyset, "voy")) {
		refstory_ids <- series_storysets$storysets_story_ids[["VOY_ALL_EPISODES"]]
	} else if(identical(background_storyset, c("tos", "tas", "tng", "voy"))) {
		refstory_ids <- unname(unlist(series_storysets$storysets_story_ids))
	}
	no_of_refstories <- length(refstory_ids)
	refstory_theme_list <- vector("list", length = no_of_refstories)
	names(refstory_theme_list) <- refstory_ids

	for(i in 1:no_of_refstories) {
		refstory_id <- refstory_ids[i]
		refstory <- story$new(refstory_id)

		if(identical(theme_levels, c("central", "peripheral"))) {
			refstory_themes <- sort(setdiff(refstory$themes[, "theme"], blacklist))
		} else if(identical(theme_levels, "central")) {
			refstory_themes <- sort(setdiff(refstory$themes[which(refstory$themes[, "level"] == "central"), "theme"], blacklist))
		} else if(identical(theme_levels, "peripheral")) {
			refstory_themes <- sort(setdiff(refstory$themes[which(refstory$themes[, "level"] == "peripheral"), "theme"], blacklist))
		}

		refstory_theme_list[[refstory_id]] <- refstory_themes
	}

	## construct a vector of theme frequencies in the background storyset
	n_t <- numeric(no_of_themes)
	names(n_t) <- sysdata$CANON_THEMES
	for(i in 1 : no_of_themes) {
		theme <- sysdata$CANON_THEMES[i]
		for(j in 1 : no_of_refstories) {
			refstory_id <- refstory_ids[j]
			if(theme %in% refstory_theme_list[[refstory_id]]) {
				n_t[i] <- n_t[i] + 1
			}
		}
	}
	
	## apply similarity function to find similar stories to the test story
	results <- data.frame(
      StoryID = refstory_ids,
      SimilarityRank = numeric(no_of_refstories),
      SimilarityScore = numeric(no_of_refstories),
      ThemeOverlap = character(no_of_refstories),
      ThemeOverlapCount = numeric(no_of_refstories),
      stringsAsFactors = FALSE
    )

	for(i in 1:no_of_refstories) {
		refstory_id <- refstory_ids[i]
		refstory_themes <- refstory_theme_list[[refstory_id]]
		no_of_refstory_themes <- length(refstory_themes)

		if(similarity_function == "cosine") {
			out <- Cosine(mystory_themes, refstory_themes)
			results[i, "SimilarityScore"] <- out$SimilarityScore
			results[i, "ThemeOverlap"] <- out$ThemeOverlap
			results[i, "ThemeOverlapCount"] <- out$ThemeOverlapCount
		} else if(similarity_function == "cosine tf-idf") {
			## query/reference tf-idf weights
			query_tfidf <- log(1 + no_of_refstories / n_t[mystory_themes])
			ref_tfidf <- log(1 + no_of_refstories / n_t[refstory_themes])

			## cosine similarity
			common_themes <- intersect(mystory_themes, refstory_themes)
			results[i, "SimilarityScore"] <- sum(query_tfidf[common_themes] * ref_tfidf[common_themes]) / sqrt(sum(query_tfidf * query_tfidf) * sum(ref_tfidf * ref_tfidf))
			out <- Jaccard(mystory_themes, refstory_themes)
			results[i, "ThemeOverlap"] <- out$ThemeOverlap
			results[i, "ThemeOverlapCount"] <- out$ThemeOverlapCount
		} else if (similarity_function == "soft cardinality") {
			union_themes <- sort(union(mystory_themes, refstory_themes))

			## query/reference soft cardinality
			qsmat <- pd_mat[mystory_themes, mystory_themes]
			query_card <- sum(1 / apply(qsmat ^ p, 1, sum))
			rsmat <- pd_mat[refstory_themes, refstory_themes]
			ref_card <- sum(1 / apply(rsmat ^ p, 1, sum))

			## intersection cardinality
			usmat <- pd_mat[union_themes, union_themes]
			union_card <- sum(1 / apply(usmat ^ p, 1, sum))
			intersection_card <- query_card + ref_card - union_card

			## cosine similarity
			common_themes <- intersect(mystory_themes, refstory_themes)
			results[i, "SimilarityScore"] <- intersection_card / sqrt(query_card * ref_card)
			out <- Jaccard(mystory_themes, refstory_themes)
			results[i, "ThemeOverlap"] <- out$ThemeOverlap
			results[i, "ThemeOverlapCount"] <- out$ThemeOverlapCount
		} 
	}

	rownames(results) <- results$StoryID
	results <- results[-which(rownames(results) == mystory_id), ]
	results <- results[with(results, order(-SimilarityScore)), ]
	results$SimilarityRank <- sort_scores(results$SimilarityScore)
	return(results)
}