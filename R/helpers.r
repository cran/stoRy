## Non-export functions. 
## Used for calculating similarity scores between pairs of stories

Jaccard = function(mystory_themes, refstory_themes) {
	theme_intersection <- intersect(mystory_themes, refstory_themes)
	no_of_common_themes <- length(theme_intersection)
	theme_union <- union(mystory_themes, refstory_themes)
	score <- no_of_common_themes / length(theme_union)
	return(list(SimilarityScore = score, ThemeOverlap = paste0(theme_intersection, collapse = ", "), ThemeOverlapCount = no_of_common_themes))
}

Cosine = function(mystory_themes, refstory_themes) {
	theme_intersection <- intersect(mystory_themes, refstory_themes)
	no_of_common_themes <- length(theme_intersection)
	no_of_mystory_themes <- length(unique(mystory_themes))
	no_of_refstory_themes <- length(unique(refstory_themes))
	score <- no_of_common_themes / sqrt(no_of_mystory_themes * no_of_refstory_themes)
	return(list(SimilarityScore = score, ThemeOverlap = paste0(theme_intersection, collapse = ", "), ThemeOverlapCount = no_of_common_themes))
}

Cosine_w = function(mystory_themes, mystory_peripheral_themes, refstory_themes, refstory_peripheral_themes, w) {
	# mystory weights
	mystory_themes_unique <- unique(mystory_themes)
	no_of_mystory_themes <- length(mystory_themes_unique)
	mystory_theme_weights <- rep(1, no_of_mystory_themes)
	names(mystory_theme_weights) <- mystory_themes_unique
	mystory_theme_weights[mystory_peripheral_themes] <- w

	# refstory weights
	refstory_themes_unique <- unique(refstory_themes)
	no_of_refstory_themes <- length(refstory_themes_unique)
	refstory_theme_weights <- rep(1, no_of_refstory_themes)
	names(refstory_theme_weights) <- refstory_themes_unique
	refstory_theme_weights[refstory_peripheral_themes] <- w

	## cosine similarity
	common_themes <- intersect(mystory_themes, refstory_themes)
	no_of_common_themes <- length(common_themes)
	score <- sum(mystory_theme_weights[common_themes] * refstory_theme_weights[common_themes]) / sqrt(sum(mystory_theme_weights * mystory_theme_weights) * sum(refstory_theme_weights * refstory_theme_weights))
	return(list(SimilarityScore = score, ThemeOverlap = paste0(common_themes, collapse = ", "), ThemeOverlapCount = no_of_common_themes))
}

sort_scores <- function(x) {
	x_unique <- unique(x)
	no_of_x_unique <- length(x_unique)
	ranks <- numeric(no_of_x_unique)
	current_rank <- 0

	for(i in 1 : no_of_x_unique) {
		y <- which(x == x_unique[i])
		y_len <- length(y)
		current_rank <- current_rank + y_len
		ranks[y] <- current_rank
	}

	return(ranks)
}
 
get_precision_recall_curve <- function(true_story_ids, story_ids, ranks) {
	unique_ranks <- unique(ranks)
	no_of_unique_ranks <- length(unique_ranks)
	PR_data <- mat.or.vec(no_of_unique_ranks, 3)
	colnames(PR_data) <- c("Rank", "Precision", "Recall")

	for(i in 1 : no_of_unique_ranks) {
		rank <- unique_ranks[i]
		positives <- story_ids[which(ranks <= rank)]
		negatives <- setdiff(story_ids, positives)
		TP <- length(intersect(true_story_ids, positives))
		FP <- length(positives) - TP
		FN <- length(true_story_ids) - TP
		PR_data[i, "Rank"] <- rank
		PR_data[i, "Precision"] <- TP / (TP + FP)
		PR_data[i, "Recall"] <- TP / (TP + FN)
	}

	return(PR_data)
}

#get_interpolated_precision_recall_curve <- function(precisions = PR_data[, "Precision"], recalls = PR_data[, "Recall"]) {
get_interpolated_precision_recall_curve <- function(precisions, recalls) {
	interp_recalls <- seq(0, 1, 0.1)
	no_of_interp_recalls <- length(interp_recalls)
	PR_interp_data <- mat.or.vec(no_of_interp_recalls, 2)
	colnames(PR_interp_data) <- c("Recall", "Precision")
	PR_interp_data[, "Recall"] <- interp_recalls <- seq(0, 1, 0.1)

	for(i in 1 : no_of_interp_recalls) {
		recall <- interp_recalls[i]
		PR_interp_data[i, "Precision"] <- max(precisions[which(recalls >= recall)])
	}

	return(PR_interp_data)
}

permutation_test = function(score_table, gs_storyset_ids, cond = c("score", "rank"), n = 100000) {
	cond <- cond[1]
	pvalue <- NA
	refstory_ids <- score_table$StoryID
	no_of_refstory_ids <- length(refstory_ids)
	refstory_scores <- score_table$SimilarityScore
	refstory_ranks <- score_table$SimilarityRank

	no_of_gs_story_ids <- length(gs_storyset_ids)
	gs_storyset_ranks <- score_table[gs_storyset_ids, "SimilarityRank"]
	gs_storyset_rank_sum <- sum(gs_storyset_ranks)
	ssranks <- numeric(n)
	gs_storyset_scores <- score_table[gs_storyset_ids, "SimilarityScore"]
	gs_storyset_score <- sum(gs_storyset_scores)
	test_scores <- numeric(n)

	if(cond == "score") {
		for(i in 1:n) {
			test_scores[i] <- sum(refstory_scores[sample(1:no_of_refstory_ids, no_of_gs_story_ids)])
		}

		test_scores <- sort(test_scores)
		pvalue <- length(which(test_scores >= gs_storyset_score)) / n
	} else if(cond == "rank") {
		for(i in 1:n) {
			ssranks[i] <- sum(sample(1:no_of_refstory_ids, no_of_gs_story_ids))
		}

		ssranks <- sort(ssranks)
		pvalue <- length(which(ssranks <= gs_storyset_rank_sum)) / n
	}

	return(pvalue)
}

grand_permutation_test = function(score_tables, gs_storyset_ids, cond = c("score", "rank"), n = 100000) {
	cond <- cond[1]
	pvalue <- NA
	score_table <- do.call("rbind", score_tables)
	refstory_ids <- unique(score_table$StoryID)
	no_of_refstory_ids <- length(refstory_ids)
	refstory_scores <- score_table$SimilarityScore
	refstory_ranks <- score_table$SimilarityRank
	no_of_gs_story_ids <- length(gs_storyset_ids)
	grand_no_of_gs_story_ids <- nrow(score_table[score_table$StoryID %in% gs_storyset_ids, ])
	grand_no_of_refstory_ids <- nrow(score_table)

	if(cond == "score") {
		gs_storyset_scores <- score_table[score_table$StoryID %in% gs_storyset_ids, "SimilarityScore"]
		gs_storyset_score <- sum(gs_storyset_scores)
		test_scores <- numeric(n)

		for(i in 1:n) {
			test_scores[i] <- sum(refstory_scores[sample(1:grand_no_of_refstory_ids, grand_no_of_gs_story_ids)])
		}

		test_scores <- sort(test_scores)
		pvalue <- length(which(test_scores >= gs_storyset_score)) / n
	} else if(cond == "rank") {
		gs_storyset_ranks <- score_table[score_table$StoryID %in% gs_storyset_ids, "SimilarityRank"]
		gs_storyset_rank_sum <- sum(gs_storyset_ranks)
		ssranks <- numeric(n)

		for(i in 1:n) {
			ssranks[i] <- sum(refstory_ranks[sample(1:grand_no_of_refstory_ids, grand_no_of_gs_story_ids)])
		}

		ssranks <- sort(ssranks)
		pvalue <- length(which(ssranks <= gs_storyset_rank_sum)) / n
	}

	return(pvalue)
}


note_to_smt <- function(infile, outfile) {
	# infile <- "notes-gold-standard-storysets.txt"
	# outfile <- "gold_standard_v1.0.smt"
	infile_path <- paste0("/Users/paul/Desktop/tstpdm/notes/", infile)
	outfile_path <- paste0("/Users/paul/Dropbox/stoRy_package/story/inst/storysets/", outfile)

	inlines <- readLines(infile_path)
	no_of_lines <- length(inlines)
	no_of_storysets <- length(which(inlines == ""))
	ends <- which(inlines == "") - 1
	starts <- c(1, ends[-length(ends)] + 2)
	names <- character(no_of_storysets)
	descriptions <- character(no_of_storysets)
	storyset_story_ids <- character(no_of_storysets)

	for(i in 1 : no_of_storysets) {
		if(i <= 9) {
			name <- paste0("GS0", i)
		} else {
			name <- paste0("GS", i)
		}

		description <- inlines[starts[i]]
		story_ids <- substring(inlines[(starts[i] + 1) : ends[i]], 4, 10)
		outline <- paste0(c(name, description, story_ids), collapse = "\t")
		write(x = outline, file = outfile_path, ncolumns = 1, append = TRUE)
	}
}


symdiff <- function(x, y) { 
	return(setdiff(union(x, y), intersect(x, y)))
}

get_domain <- function(theme) {
	data <- sysdata$theme_dict
	domain <- data[which(data[, "Theme"] == theme), "Domain"]
	return(domain)
}

get_dist_matrix <- function() {
	data <- sysdata$theme_dict
	themes <- data[, "Theme"]
	no_of_themes <- length(themes)
	dist_matrix <- mat.or.vec(no_of_themes, no_of_themes)
	rownames(dist_matrix) <- themes
	colnames(dist_matrix) <- themes

	for(i in 1:(no_of_themes - 1)) {
		theme1 <- themes[i]

		for(j in (i + 1):(no_of_themes)) {
			theme2 <- themes[j]
			dist_matrix[i, j] <- get_pairwise_dist(theme1, theme2)
		}
	}

	return(dist_matrix)
}

get_max_dist <- function(domain) {
	max_dist <- 0
	if(domain == "the human condition") {
		max_dist <- 11/2
	} else if(domain == "society") {
		max_dist <- 7/2
	} else if(domain == "the pursuit of knowledge") {
		max_dist <- 7/2
	} else if(domain == "alternate reality") {
		max_dist <-	7/2
	}

	return(max_dist)
}

get_pairwise_dist <- function(theme1, theme2) {
	if(is_same_domain(theme1, theme2) == FALSE) {
		dist <- 0
	} else {
		data <- sysdata$theme_dict
		mytheme <- theme1
		mytheme_parent <- data[which(data[, "Theme"] == mytheme), "ParentTheme"]
		theme1_ancestors <- c(mytheme, mytheme_parent)
		while(mytheme_parent != "") {
			mytheme_parent <- data[which(data[, "Theme"] == mytheme_parent), "ParentTheme"]
			theme1_ancestors <- c(theme1_ancestors, mytheme_parent)
		}
		if("" %in% theme1_ancestors) {
			theme1_ancestors <- theme1_ancestors[-which(theme1_ancestors == "")]
		}

		mytheme <- theme2
		mytheme_parent <- data[which(data[, "Theme"] == mytheme), "ParentTheme"]
		theme2_ancestors <- c(mytheme, mytheme_parent)
		while(mytheme_parent != "") {
			mytheme_parent <- data[which(data[, "Theme"] == mytheme_parent), "ParentTheme"]
			theme2_ancestors <- c(theme2_ancestors, mytheme_parent)
		}
		if("" %in% theme2_ancestors) {
			theme2_ancestors <- theme2_ancestors[-which(theme2_ancestors == "")]
		}

		common_ancestors <- intersect(theme1_ancestors, theme2_ancestors)
		least_common_ancestor <- common_ancestors[1]
		d1 <- length(setdiff(theme1_ancestors, common_ancestors))
		d2 <- length(setdiff(theme2_ancestors, common_ancestors))

		#dist <- 1 / (d1 + d2 + 1)
		dist <- 1 / (max(d1, d2) + 1)
	}

	return(dist)
}

is_same_domain = function(theme1, theme2) {
	data <- sysdata$theme_dict
	domain1 <- data[which(data[, "Theme"] == theme1), "Domain"]
	domain2 <- data[which(data[, "Theme"] == theme2), "Domain"]

	if(identical(domain1, domain2)) {
		return(TRUE)
	}
	
	return(FALSE)
}

get_all_theme_frequency_tables = function() {
	story_ids <- sysdata$RESERVED_STORY_IDS
	no_of_stories <- length(story_ids)
	story_theme_list <- vector("list", length = 6)
	names(story_theme_list) <- c("recorded_c", "recorded_cp", "all_binary_c", "all_binary_cp", "all_c", "all_cp")
	no_of_themes <- length(sysdata$CANON_THEMES)
	
	theme_levels <- "central"
	theme_record <- "recorded"
	print(theme_levels)
	for(i in 1 : no_of_stories) {
		story_id <- story_ids[i]
		story_theme_list[[story_id]] <- get_story_themes(story_id, theme_levels, theme_record)
	}
	n_t <- numeric(no_of_themes)
	names(n_t) <- sysdata$CANON_THEMES
	for(j in 1 : no_of_themes) {
		theme <- sysdata$CANON_THEMES[j]
		for(k in 1 : no_of_stories) {
			id <- story_ids[k]
			if(theme %in% story_theme_list[[id]]) {
				n_t[j] <- n_t[j] + 1
			}
		}
	}
	story_theme_list[["recorded_c"]] <- n_t

	theme_levels <- c("central", "peripheral")
	theme_record <- "recorded"
	print(theme_levels)
	for(i in 1:no_of_stories) {
		story_id <- story_ids[i]
		story_theme_list[[story_id]] <- get_story_themes(story_id, theme_levels, theme_record)
	}
	n_t <- numeric(no_of_themes)
	names(n_t) <- sysdata$CANON_THEMES
	for(j in 1 : no_of_themes) {
		theme <- sysdata$CANON_THEMES[j]
		for(k in 1 : no_of_stories) {
			id <- story_ids[k]
			if(theme %in% story_theme_list[[id]]) {
				n_t[j] <- n_t[j] + 1
			}
		}
	}
	story_theme_list[["recorded_cp"]] <- n_t

	theme_levels <- "central"
	theme_record <- "all_binary"
	print(theme_levels)
	for(i in 1:no_of_stories) {
		story_id <- story_ids[i]
		story_theme_list[[story_id]] <- get_story_themes(story_id, theme_levels, theme_record)
	}
	n_t <- numeric(no_of_themes)
	names(n_t) <- sysdata$CANON_THEMES
	for(j in 1 : no_of_themes) {
		theme <- sysdata$CANON_THEMES[j]
		for(k in 1 : no_of_stories) {
			id <- story_ids[k]
			if(theme %in% story_theme_list[[id]]) {
				n_t[j] <- n_t[j] + 1
			}
		}
	}
	story_theme_list[["all_binary_c"]] <- n_t

	theme_levels <- c("central", "peripheral")
	theme_record <- "all_binary"
	print(theme_levels)
	for(i in 1:no_of_stories) {
		story_id <- story_ids[i]
		story_theme_list[[story_id]] <- get_story_themes(story_id, theme_levels, theme_record)
	}
	n_t <- numeric(no_of_themes)
	names(n_t) <- sysdata$CANON_THEMES
	for(j in 1 : no_of_themes) {
		theme <- sysdata$CANON_THEMES[j]
		for(k in 1 : no_of_stories) {
			id <- story_ids[k]
			if(theme %in% story_theme_list[[id]]) {
				n_t[j] <- n_t[j] + 1
			}
		}
	}
	story_theme_list[["all_binary_cp"]] <- n_t

	theme_levels <- "central"
	theme_record <- "all"
	print(theme_levels)
	for(i in 1:no_of_stories) {
		story_id <- story_ids[i]
		story_theme_list[[story_id]] <- get_story_themes(story_id, theme_levels, theme_record)
	}
	n_t <- numeric(no_of_themes)
	names(n_t) <- sysdata$CANON_THEMES
	for(j in 1 : no_of_themes) {
		theme <- sysdata$CANON_THEMES[j]
		for(k in 1 : no_of_stories) {
			id <- story_ids[k]
			if(theme %in% story_theme_list[[id]]) {
				n_t[j] <- n_t[j] + length(which(story_theme_list[[id]] == theme))
			}
		}
	}
	story_theme_list[["all_c"]] <- n_t

	theme_levels <- c("central", "peripheral")
	theme_record <- "all"
	print(theme_levels)
	for(i in 1:no_of_stories) {
		story_id <- story_ids[i]
		story_theme_list[[story_id]] <- get_story_themes(story_id, theme_levels, theme_record)
	}
	n_t <- numeric(no_of_themes)
	names(n_t) <- sysdata$CANON_THEMES
	for(j in 1 : no_of_themes) {
		theme <- sysdata$CANON_THEMES[j]
		for(k in 1 : no_of_stories) {
			id <- story_ids[k]
			if(theme %in% story_theme_list[[id]]) {
				n_t[j] <- n_t[j] + length(which(story_theme_list[[id]] == theme))
			}
		}
	}
	story_theme_list[["all_cp"]] <- n_t
	
	return(story_theme_list)
}

get_all_theme_levels = function() {
	story_ids <- sysdata$RESERVED_STORY_IDS
	no_of_stories <- length(story_ids)
	story_list <- vector("list", length = no_of_stories)
	names(story_list) <- story_ids

	for(i in 1 : no_of_stories) {
		story_id <- story_ids[i]
		story_theme_list <- vector("list", length = 6)
		names(story_theme_list) <- c("recorded_c", "recorded_cp", "all_binary_c", "all_binary_cp", "all_c", "all_cp")
		story_theme_list[["recorded_c"]] <- get_story_themes(mystory_id = story_id, theme_levels = "central", theme_record = "recorded")
		story_theme_list[["recorded_cp"]] <- get_story_themes(mystory_id = story_id, theme_levels = c("central", "peripheral"), theme_record = "recorded")
		story_theme_list[["all_binary_c"]] <- get_story_themes(mystory_id = story_id, theme_levels = "central", theme_record = "all_binary")
		story_theme_list[["all_binary_cp"]] <- get_story_themes(mystory_id = story_id, theme_levels = c("central", "peripheral"), theme_record = "all_binary")
		story_theme_list[["all_c"]] <- get_story_themes(mystory_id = story_id, theme_levels = "central", theme_record = "all")
		story_theme_list[["all_cp"]] <- get_story_themes(mystory_id = story_id, theme_levels = c("central", "peripheral"), theme_record = "all")
		story_list[[i]] <- story_theme_list
	}

	return(story_list)
}

get_story_themes = function(mystory_id, theme_levels = c("central", "peripheral"), theme_record = c("recorded", "all_binary", "all")) {
	mystory <- story$new(mystory_id)

	if(identical(theme_record, "recorded") && identical(theme_levels, "central")) {
		themes <- sort(mystory$themes[which(mystory$themes[, "level"] == "central"), "theme"])
	} else if(identical(theme_record, "recorded") && identical(theme_levels, c("central", "peripheral"))) {
		themes <- sort(mystory$themes[, "theme"])
	} else if(identical(theme_record, "recorded") && identical(theme_levels, "peripheral")) {
		themes <- sort(mystory$themes[which(mystory$themes[, "level"] == "peripheral"), "theme"])
	} else if(identical(theme_record, "all_binary") && identical(theme_levels, "central")) {
		theme_data <- sysdata$theme_dict[, c(1, 3)]
		rownames(theme_data) <- theme_data[, 1]
		recorded_themes <- sort(mystory$themes[which(mystory$themes[, "level"] == "central"), "theme"])
		no_of_recorded_themes <- length(recorded_themes)
		themes <- NULL
		for(i in 1 : no_of_recorded_themes) {
			recorded_theme <- recorded_themes[i]
			themes <- c(themes, recorded_theme)
			parent_theme <- theme_data[recorded_theme, "ParentTheme"]

			while(parent_theme != "") {
				themes <- c(themes, parent_theme)
				parent_theme <- theme_data[parent_theme, "ParentTheme"]
			}
		}

		themes <- sort(unique(themes))
	} else if(identical(theme_record, "all_binary") && identical(theme_levels, c("central", "peripheral"))) {
		theme_data <- sysdata$theme_dict[, c(1, 3)]
		rownames(theme_data) <- theme_data[, 1]
		recorded_themes <- sort(mystory$themes[, "theme"])
		no_of_recorded_themes <- length(recorded_themes)
		themes <- NULL
		for(i in 1 : no_of_recorded_themes) {
			recorded_theme <- recorded_themes[i]
			themes <- c(themes, recorded_theme)
			parent_theme <- theme_data[recorded_theme, "ParentTheme"]

			while(parent_theme != "") {
				themes <- c(themes, parent_theme)
				parent_theme <- theme_data[parent_theme, "ParentTheme"]
			}
		}

		themes <- sort(unique(themes))
	} else if(identical(theme_record, "all_binary") && identical(theme_levels, "peripheral")) {
		theme_data <- sysdata$theme_dict[, c(1, 3)]
		rownames(theme_data) <- theme_data[, 1]
		recorded_themes <- sort(mystory$themes[which(mystory$themes[, "level"] == "peripheral"), "theme"])
		no_of_recorded_themes <- length(recorded_themes)
		themes <- NULL
		if(no_of_recorded_themes > 0) {
			for(i in 1 : no_of_recorded_themes) {
				recorded_theme <- recorded_themes[i]
				themes <- c(themes, recorded_theme)
				parent_theme <- theme_data[recorded_theme, "ParentTheme"]

				while(parent_theme != "") {
					themes <- c(themes, parent_theme)
					parent_theme <- theme_data[parent_theme, "ParentTheme"]
				}
			}

			themes <- sort(unique(themes))
		}
	} else if(identical(theme_record, "all") && identical(theme_levels, "central")) {
		theme_data <- sysdata$theme_dict[, c(1, 3)]
		rownames(theme_data) <- theme_data[, 1]
		recorded_themes <- sort(mystory$themes[which(mystory$themes[, "level"] == "central"), "theme"])
		no_of_recorded_themes <- length(recorded_themes)
		themes <- NULL
		for(i in 1 : no_of_recorded_themes) {
			recorded_theme <- recorded_themes[i]
			themes <- c(themes, recorded_theme)
			parent_theme <- theme_data[recorded_theme, "ParentTheme"]

			while(parent_theme != "") {
				themes <- c(themes, parent_theme)
				parent_theme <- theme_data[parent_theme, "ParentTheme"]
			}
		}
	} else if(identical(theme_record, "all") && identical(theme_levels, c("central", "peripheral"))) { 
		theme_data <- sysdata$theme_dict[, c(1, 3)]
		rownames(theme_data) <- theme_data[, 1]
		recorded_themes <- sort(mystory$themes[, "theme"])
		no_of_recorded_themes <- length(recorded_themes)
		themes <- NULL
		for(i in 1 : no_of_recorded_themes) {
			recorded_theme <- recorded_themes[i]
			themes <- c(themes, recorded_theme)
			parent_theme <- theme_data[recorded_theme, "ParentTheme"]

			while(parent_theme != "") {
				themes <- c(themes, parent_theme)
				parent_theme <- theme_data[parent_theme, "ParentTheme"]
			}
		}
	}

	return(themes)
}

## set plot colours 
# gg_color_hue = function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1 : n]
# }
