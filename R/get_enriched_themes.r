#' @title Find enriched themes in a storyset of interest relative to a background storyset. 
#' 
#' @description \code{get_enriched_themes} calculates enrichment scores for themes in
#' a storset relative to a background set of stories
#' according to the hypergeometric test.
#' @return Returns a data frame where each row corresponds to a theme.
#' The column Pvalue contains raw enrichment scores, i.e., hypergeometric 
#' test P-values.
#' @param mystorysets A storysets class object that contains one or many individual storysets.
#' @param test_storysets The storyset names to be analysed. The default \code{mystorysets$names} is to analyse all storysets. 
#' @param background_storyset default \code{c("tos", "tas", "tng", "voy")}. A string indicating the storyset to use as background.
#' Possible values are \code{c("tos", "tas")}, \code{"tng"}, \code{"voy"}, and \code{c("tos", "tas", "tng", "voy")}.
#' @param theme_levels \code{c("central", "peripheral")}. A string indicating the theme levels to use in the analysis.
#' Possible values are \code{central} and/or \code{peripheral}.
#' @param min_storyset_size default \code{5}. The minimum allowable size for a storyset.
#' For small samples the hypergeometric test may be unreliable.
#' @param min_theme_occurrence default \code{1}. The minimum number of times a theme must occur in a storyset.
#' For small samples the hypergeometric test may be unreliable.
#' @references Onsjö, M., and Sheridan, P. (2017): 
#' "Theme Enrichment Analysis: A Statistical Test for Identifying Significantly Enriched Themes in a List of Stories with an Application to the Star Trek Television Franchise", 
#' ArXiv.
#' @importFrom stats phyper
#' @export
#' @examples
#' #######################################################################
#' # Load the Star Trek series storysets smt file and storysets object   #
#' #######################################################################
#' file <- system.file("storysets", "series.smt", package = "stoRy")
#' mystorysets <- storysets$new(file)
#' print(mystorysets)
#' 
#' #######################################################################
#' # Perform the theme enrichment analysis for each of TOS, TAS, and TNG #
#' # relative to default TOS/TAS/TNG background                          #
#' #######################################################################
#' results <- get_enriched_themes(mystorysets)
#' 
#' #######################################################################
#' # Output top twenty enriched TOS themes                               #
#' #######################################################################
#' results$TOS[1:20,]
#' 
#' #######################################################################
#' # Output top ten enriched TAS themes                                  #
#' #######################################################################
#' results$TAS[1:10,]
#' 
#' #######################################################################
#' # Output top twenty enriched TNG themes                               #
#' #######################################################################
#' results$TNG[1:20,]
get_enriched_themes = function(mystorysets, test_storysets = mystorysets$names, background_storyset = c("tos", "tas", "tng", "voy"), theme_levels = c("central", "peripheral"), min_storyset_size = 5, min_theme_occurrence = 1) {	
	## read background storyset
	if(identical(background_storyset, c("tos", "tas")) && identical(theme_levels, "central")) {
		bgcodes <- c("TOAXXCX", "SOAXXCX")
	} else if(identical(background_storyset, c("tos", "tas")) && identical(theme_levels, c("central", "peripheral"))) {
		bgcodes <- c("TOAXXCP", "SOAXXCP")
	} else if(identical(background_storyset, "tng") && identical(theme_levels, "central")) {
		bgcodes <- c("TXXNXCX", "SXXNXCX")
	} else if(identical(background_storyset, "tng") && identical(theme_levels, c("central", "peripheral"))) {
		bgcodes <- c("TXXNXCP", "SXXNXCP")
	} else if(identical(background_storyset, "voy") && identical(theme_levels, "central")) {
		bgcodes <- c("TXXXVCX", "SXXXVCX")
	} else if(identical(background_storyset, "voy") && identical(theme_levels, c("central", "peripheral"))) {
		bgcodes <- c("TXXXVCP", "SXXXVCP")
	} else if(identical(background_storyset, c("tos", "tas", "tng", "voy")) && identical(theme_levels, "central")) {
		bgcodes <- c("TOANVCX", "SOANVCX")
	} else if(identical(background_storyset, c("tos", "tas", "tng", "voy")) && identical(theme_levels, c("central", "peripheral"))) {
		bgcodes <- c("TOANVCP", "SOANVCP")
	}

	background_theme_table_full <- sysdata[[bgcodes[2]]]
	rownames(background_theme_table_full) <- as.character(background_theme_table_full[, 1])
	void_theme_rows <- which(background_theme_table_full$Frequency == 0)
	no_of_void_theme_rows <- length(void_theme_rows)
	if(no_of_void_theme_rows > 0) {
		background_theme_table <- background_theme_table_full[-void_theme_rows, ]
	} else {
		background_theme_table <- background_theme_table_full
	}
	rownames(background_theme_table) <- as.character(background_theme_table[, 1])
	no_of_background_themes <- nrow(background_theme_table)
	background_story_table <- sysdata[[bgcodes[1]]]
	#rownames(background_story_table) <- as.character(background_story_table[, 1])
	no_of_background_stories <- nrow(background_story_table)
	background_story_ids <- background_story_table$StoryID

	## construct storyset theme frequency tables
	no_of_test_storysets <- length(test_storysets)
	storyset_theme_tables <- vector("list", no_of_test_storysets)
	names(storyset_theme_tables) <- test_storysets

	for(i in 1:no_of_test_storysets) {
		test_storyset_index <- test_storysets[i]
		storyset <- intersect(mystorysets$storysets_story_ids[[test_storyset_index]], background_story_ids)
		storyset_name <- mystorysets$names[test_storyset_index]
		no_of_storyset_stories <- length(storyset)
		mytable <- table(as.numeric(strsplit(paste(background_story_table[storyset, "ThemeIDs"], collapse = " "), split = " ")[[1]]))
		storyset_theme_ids <- as.numeric(names(mytable))
		storyset_theme_counts <- as.numeric(mytable)
		storyset_themes <- background_theme_table_full[storyset_theme_ids, "Theme"]

		if(no_of_storyset_stories >= min_storyset_size) {
			storyset_theme_tables[[i]] <- data.frame(
				Theme = storyset_themes,
				ThemeID = storyset_theme_ids,
				Frequency = storyset_theme_counts,
				Proportion = storyset_theme_counts / no_of_storyset_stories,
				stringsAsFactors = FALSE
			)
			rownames(storyset_theme_tables[[i]]) <- storyset_themes
		}
	}

	## hypergeometric test for theme over-representation in a test storyset versus background
	results <- vector("list", length = mystorysets$no_of_storysets)
	names(results) <- test_storysets
	for(i in 1:no_of_test_storysets) {
		test_storyset_index <- test_storysets[i]
		storyset <- intersect(mystorysets$storysets_story_ids[[test_storyset_index]], background_story_ids)
		storyset_name <- test_storysets[i]
		n <- length(storyset)

		if(n >= min_storyset_size) {
			storyset_theme_table <- storyset_theme_tables[[i]]
			no_of_storyset_themes <- nrow(storyset_theme_table)
			N <- nrow(background_story_table)
			pvalues <- numeric(no_of_storyset_themes)
			k_rec <- numeric(no_of_storyset_themes)
			K_rec <- numeric(no_of_storyset_themes)
				
			for(j in 1 : no_of_storyset_themes) {
				theme <- storyset_theme_table[j , "Theme"]
				theme_id <- storyset_theme_table[j , "ThemeID"]
				k <- storyset_theme_table[j , "Frequency"]
				k_rec[j] <- k
				K <- background_theme_table[theme , "Frequency"]
				K_rec[j] <- K
				pvalues[j] <- phyper( q = k - 1 , m = K ,  n = ( N - K ) , k = n , lower.tail = FALSE )
			}

			#qvalues <- p.adjust(pvalues, method = "BH")
			out <- data.frame(
				"Theme" = storyset_theme_table[, "Theme"], 
				"ThemeID" = storyset_theme_table[, "ThemeID"], 
				"k" = k_rec,
				"n" = rep(n, no_of_storyset_themes),
				"K" = K_rec,
				"N" = rep(N, no_of_storyset_themes),
				"Pvalue" = pvalues
			)
			#colnames(out) <- c("Theme", "ThemeID", "k" , "p", "K", "P", "Pvalue")
			out <- out[ with( out , order( Pvalue , decreasing = FALSE ) ) , ]
			keeper_rows <- which(out[, "k"] >= min_theme_occurrence)
			no_of_keeper_rows <- length(keeper_rows)
			if(no_of_keeper_rows >= 1) {
				out <- out[keeper_rows, ]
				rownames(out) <- NULL
				results[[i]] <- out
			}
		}
	}

	return(results)
}