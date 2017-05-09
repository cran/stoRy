#' @title Theme enrichment analysis (TEA). 
#' 
#' @description \code{TEA} calculates enrichment scores for themes in
#' a storset relative to a background set of stories
#' according to the hypergeometric test.
#' @return Returns a data frame where each row corresponds to a theme.
#' The column Pvalue contains raw enrichment scores, i.e., hypergeometric 
#' test P-values. The column Padj contains the corresponding Benjamini-Hotchberg
#' adjusted P-values.
#' @param mystorysets A storysets class object that contains one or many individual storysets.
#' @param background_storyset default \code{c("TOS", "TAS", "TNG")}. A string indicating the storyset to use as background.
#' Possible values are \code{TOS}, \code{TAS}, \code{TNG} and their combinations.
#' @param theme_levels \code{c("choice", "major", "minor")}. A string indicating the theme levels to use in the analysis.
#' Possible values are \code{choice}, \code{major}, \code{minor} and their combinations.
#' @param min_storyset_size default \code{5}. The minimum allowable size for a storyset.
#' For small samples the hypergeometric test may be unreliable.
#' @param min_theme_occurrence default \code{1}. The minimum number of times a theme must occur in a storyset.
#' For small samples the hypergeometric test may be unreliable.
#' @importFrom stats p.adjust
#' @importFrom stats phyper
#' @export
#' @examples
#' #######################################################################
#' # Load the Star Trek series storysets smt file and storysets object   #
#' #######################################################################
#' file <- system.file("storysets", "series.smt", package = "stoRy")
#' mystorysets <- storysets$new(file)
#' print(mystorysets)
#' #######################################################################
#' # Perform the theme enrichment analysis for each of TOS, TAS, and TNG #
#' # relative to default TOS/TAS/TNG background                          #
#' #######################################################################
#' results <- TEA(mystorysets)
#' #######################################################################
#' # Output top ten enriched TOS themes                                  #
#' #######################################################################
#' results$TOS[1:10,c("Theme", "NSample", "NOmega", "Pvalue", "Padj")]
#' #######################################################################
#' # Output top ten enriched TAS themes                                  #
#' #######################################################################
#' results$TOS[1:10,c("Theme", "NSample", "NOmega", "Pvalue", "Padj")]
#' #######################################################################
#' # Output top ten enriched TNG themes                                  #
#' #######################################################################
#' results$TOS[1:10,c("Theme", "NSample", "NOmega", "Pvalue", "Padj")]
TEA = function(mystorysets, background_storyset = c("TOS", "TAS", "TNG"), theme_levels = c("choice", "major", "minor"), min_storyset_size = 5, min_theme_occurrence = 1) {	
	## read background storyset
	if(identical(background_storyset, c("TOS", "TAS", "TNG")) && identical(theme_levels, c("choice", "major", "minor"))) {
		bgcodes <- c("TOANCMM", "EOANCMM")
	} else if(identical(background_storyset, c("TOS", "TAS", "TNG")) && identical(theme_levels, c("choice", "major"))) {
		bgcodes <- c("TOANCMX", "EOANCMX")
	} else if(identical(background_storyset, c("TOS", "TAS", "TNG")) && identical(theme_levels, "choice")) {
		bgcodes <- c("TOANCXX", "EOANCXX")
	} else if(identical(background_storyset, c("TOS", "TAS")) && identical(theme_levels, c("choice", "major", "minor"))) {
		bgcodes <- c("TOAXCMM", "EOAXCMM")
	} else if(identical(background_storyset, c("TOS", "TAS")) && identical(theme_levels, c("choice", "major"))) {
		bgcodes <- c("TOAXCMX", "EOAXCMX")
	} else if(identical(background_storyset, c("TOS", "TAS")) && identical(theme_levels, "choice")) {
		bgcodes <- c("TOAXCXX", "EOAXCXX")
	} else if(identical(background_storyset, "TNG") && identical(theme_levels, c("choice", "major", "minor"))) {
		bgcodes <- c("TXXNCMM", "EXXNCMM")
	} else if(identical(background_storyset, "TNG") && identical(theme_levels, c("choice", "major"))) {
		bgcodes <- c("TXXNCMX", "EXXNCMX")
	} else if(identical(background_storyset, "TNG") && identical(theme_levels, "choice")) {
		bgcodes <- c("TXXNCXX", "EXXNCXX")
	}
	background_theme_table_full <- sysdata[[bgcodes[1]]]
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
	background_story_table <- sysdata[[bgcodes[2]]]
	rownames(background_story_table) <- as.character(background_story_table[, 1])
	no_of_background_stories <- nrow(background_story_table)
	background_story_ids <- background_story_table$StoryID

	## construct storyset theme frequency tables
	storyset_theme_tables <- vector("list", mystorysets$no_of_storysets)
	names(storyset_theme_tables) <- names(mystorysets$names)

	for(i in 1:mystorysets$no_of_storysets) {
		storyset <- intersect(mystorysets$storysets_story_ids[[i]], background_story_ids)
		storyset_name <- mystorysets$names[i]
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

	## hypergeometric test for theme over-representation in storyset list versus background
	results <- vector("list", length = mystorysets$no_of_storysets)
	names(results) <- mystorysets$names
	for(i in 1:mystorysets$no_of_storysets) {
		storyset <- intersect(mystorysets$storysets_story_ids[[i]], background_story_ids)
		storyset_name <- mystorysets$names[i]
		no_of_storyset_stories <- length(storyset)
		n_sample <- no_of_storyset_stories

		if(no_of_storyset_stories >= min_storyset_size) {
			storyset_theme_table <- storyset_theme_tables[[i]]
			no_of_storyset_themes <- nrow(storyset_theme_table)
			no_of_background_stories <- nrow(background_story_table)
			n_omega <- no_of_background_stories
			pvalues <- numeric(no_of_storyset_themes)
			XOMEGA <- numeric(no_of_storyset_themes)
			POMEGA <- numeric(no_of_storyset_themes)
			JINDEX1 <- numeric(no_of_storyset_themes)
			NOMEGA2 <- numeric(no_of_storyset_themes)
				
			for(j in 1 : no_of_storyset_themes) {
				theme <- storyset_theme_table[j , "Theme"]
				theme_id <- storyset_theme_table[j , "ThemeID"]
				x_sample <- storyset_theme_table[j , "Frequency"]
				x_omega <- background_theme_table[theme , "Frequency"]
				x_intersection <- length(intersect(storyset, strsplit(background_theme_table[theme, "StoryIDs"], split = " ")[[1]]))
				x_union <- length(union(storyset, strsplit(background_theme_table[theme, "StoryIDs"], split = " ")[[1]]))
				XOMEGA[ j ] <- x_omega
				POMEGA[ j ] <- x_omega / n_omega
				pvalues[ j ] <- phyper( q = x_sample - 1 , m = x_omega ,  n = ( n_omega - x_omega ) , k = n_sample , lower.tail = FALSE )
				JINDEX1[ j ] <- x_intersection / x_union
				NOMEGA2[ j ] <- x_sample / length(strsplit(background_theme_table[theme, "StoryIDs"], split = " ")[[1]])
			}

			pvalues_log <- -log10(pvalues)
			qvalues <- p.adjust(pvalues, method = "BH")
			qvalues_log <- -log10( qvalues )
			out <- cbind(storyset_theme_table, XOMEGA, POMEGA, pvalues, qvalues, pvalues_log, qvalues_log, JINDEX1, NOMEGA2)
			colnames(out) <- c("Theme", "ThemeID", "NSample" , "PSample", "NOmega", "POmega", "Pvalue", "Padj", "log10Pvalue", "log10Qvalue", "Jindex1", "NOmega2")
			out <- out[ with( out , order( Pvalue , decreasing = FALSE ) ) , ]
			keeper_rows <- which(out[, "NSample"] >= min_theme_occurrence)
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