## Non-export functions. 
## Used for input checking

check_story_id = function(story_id) {
	if(!is.character(story_id)) {
		stop("Your story ID should be a character string.")
	}
}

check_title = function(title) {
	if(!is.character(title)) {
		stop("Your story title should be a character string.")
	}
}

check_writer = function(writer) {
	if(!is.character(writer)) {
		stop("Your story writer should be a character string.")
	}
}

check_director = function(director) {
	if(!is.character(director)) {
		stop("Your story director should be a character string.")
	}
}

check_original_air_date = function(original_air_date) {
	if(!is.character(original_air_date)) {
		stop("Your story air date should be a character string.")
	}
}

check_summary = function(summary) {
	if(!is.character(summary)) {
		stop("Your story summary should be a character string.")
	}
}

check_characters = function(characters) {
	if(!is.list(characters)) {
		stop("Your story character information should take the form of a list.")
	}

	PERMITTED_LIST_ELEMENT_NAMES <- c("object_characters", "main_characters", "supporting_cast")
	forbidden_list_element_names <- setdiff(names(characters), PERMITTED_LIST_ELEMENT_NAMES)
	no_of_forbidden_list_element_names <- length(forbidden_list_element_names)
	if(no_of_forbidden_list_element_names > 0) {
		stop("Your characters list contains forbidden element names.
				Character list names are limited to \"object_characters\", \"main_characters\", \"supporting_cast\".")
	}
}

check_themes = function(themes) {
	if(!is.data.frame(themes)) {
		stop("Your story theme information should take the form of a data frame.")
	}

	PERMITTED_DATAFRAME_COULMN_NAMES <- c("theme", "level", "theme_id", "comment", "related_characters", "character_class", "related_aliens", "named_thing")
	forbidden_dataframe_column_names <- setdiff(colnames(themes), PERMITTED_DATAFRAME_COULMN_NAMES)
	no_of_forbidden_dataframe_column_names <- length(forbidden_dataframe_column_names)
	if(no_of_forbidden_dataframe_column_names > 0) {
		stop("Your themes data frame contains forbidden column names.
				Character list names are limited to \"theme\", \"level\", \"theme_id\", \"comment\", \"related_characters\", \"character_class\", \"related_aliens\", \"named_thing\".")
	}
}

check_settings = function(settings) {
	if(!is.data.frame(settings)) {
		stop("Your story setting information should take the form of a data frame.")
	}

	PERMITTED_DATAFRAME_COULMN_NAMES <- c("setting", "capacity")
	forbidden_dataframe_column_names <- setdiff(colnames(settings), PERMITTED_DATAFRAME_COULMN_NAMES)
	no_of_forbidden_dataframe_column_names <- length(forbidden_dataframe_column_names)
	if(no_of_forbidden_dataframe_column_names > 0) {
		stop("Your settings data frame contains forbidden column names.
				Character list names are limited to \"setting\" and \"comment\".")
	}
}

check_keywords = function(keywords) {
	if(!is.data.frame(keywords)) {
		stop("Your story keyword information should take the form of a data frame.")
	}

	PERMITTED_DATAFRAME_COULMN_NAMES <- c("keyword", "comment", "timing", "implication")
	forbidden_dataframe_column_names <- setdiff(colnames(keywords), PERMITTED_DATAFRAME_COULMN_NAMES)
	no_of_forbidden_dataframe_column_names <- length(forbidden_dataframe_column_names)
	if(no_of_forbidden_dataframe_column_names > 0) {
		stop("Your keywords data frame contains forbidden column names.
				Character list names are limited to \"keyword\", \"comment\", \"timing\", \"implication\".")
	}
}

check_add_theme = function(theme, level) {
	if(!is.character(theme)) {
		stop("Your theme should be a character string.")
	}

	if(!(theme %in% sysdata$CANON_THEMES)) {
		stop(paste0("Your theme \"", theme, "\" is not the official theme cannon.\n",
			"  Try adding a cannon theme, such as \"male bonding\"."))
	}

	if(!(level %in% c("choice", "major", "minor"))) {
		stop("Your theme level should be one of \"choice\", \"major\", \"minor\".")
	}
}

check_remove_theme = function(theme) {
	if(!is.character(theme)) {
		stop("Your theme should be a character string.")
	}
}

check_add_setting = function(setting) {
	if(!is.character(setting)) {
		stop("Your setting should be a character string.")
	}
}

check_remove_setting = function(setting) {
	if(!is.character(setting)) {
		stop("Your setting should be a character string.")
	}
}

check_add_keyword = function(keyword) {
	if(!is.character(keyword)) {
		stop("Your keyword should be a character string.")
	}
}

check_remove_keyword = function(keyword) {
	if(!is.character(keyword)) {
		stop("Your keyword should be a character string.")
	}
}

check_theme = function(theme) {
	if(!is.character(theme)) {
		stop("Your theme should be a character string.")
	}

	if(!(theme %in% sysdata$CANON_THEMES)) {
		stop(paste0("Your theme \"", theme, "\" is not the official theme cannon.\n",
			"  Try a cannon theme, such as \"male bonding\"."))
	}
}

check_name = function(name) {
	if(!is.character(name)) {
		stop("Your storyset name should be a character string.")
	}

	if(grepl("\t", name)) {
		stop("Your storyset name should not contain a tab character.")
	}
}

check_comment = function(comment) {
	if(!is.character(comment)) {
		stop("Your storyset comment should be a character string.")
	}

	if(grepl("\t", comment)) {
		stop("Your storyset comment should not contain a tab character.")
	}
}

check_story_ids = function(story_ids) {
	if(!is.vector(story_ids)) {
		stop("Your story IDs should be in vector form.")
	}

	no_of_story_ids <- length(story_ids)
	if(no_of_story_ids == 0) {
		stop("Your story ID list is empty.")
	} else {
		for(i in 1 : no_of_story_ids) {
			story_id <- story_ids[i]
			if(!ifelse(story_id %in% sysdata$RESERVED_STORY_IDS, TRUE, FALSE)) {
				stop(paste0("Your story ID \"", story_id, "\" is not recognized.\n",
					"  Try a recognized story ID, such as \"TOS1X19\"."))
			}
		}
	}
}

check_file = function(file) {
	if(!is.character(file)) {
		stop("Your storyset file should be a character string.")
	}
}

check_add_story_id = function(story_id) {
	if(!is.character(story_id)) {
		stop("Your story ID should be a character string.")
	}

	if(!ifelse(story_id %in% sysdata$RESERVED_STORY_IDS, TRUE, FALSE)) {
		stop(paste0("Your story ID \"", story_id, "\" is not recognized.\n",
			"  Try a recognized story ID, such as \"TOS1X19\"."))
	}
}

check_remove_story_id = function(story_id) {
	if(!is.character(story_id)) {
		stop("Your story ID should be a character string.")
	}
}

check_storysets_file = function(file) {
	if(!is.character(file)) {
		stop("Your storysets file should be a character string.")
	}

	tok <- strsplit(file, split = "\\.")[[1]]
	file_extension <- tok[length(tok)]
	if(!(file_extension == "smt")) {
		stop("Your storysets file should be a \\.smt file format.")
	}

	if(!file.exists(file)) {
		stop("Your storysets file does not exist.")
	}
}


