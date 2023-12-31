library(dplyr)
library(skimr)
library(tibble)
library(tidyr)


fix_variable_names <- function(dataframe) {
    # Replaces spaces with underscores and the % symbol with the word percent.
    # Returns dataframe with new column names
    
    colnames(dataframe) <- gsub(" ", "_", colnames(dataframe))
    colnames(dataframe) <- gsub("%", "percent", colnames(dataframe))
    
    return(dataframe)
}


create_unique_id <- function(dataframe) {
    dataframe <- 
        unite(dataframe, col='id', c('number', 'name'), sep='_', remove=FALSE) %>%
        group_by(id) %>%
        mutate(row_number = row_number()) %>%
        ungroup() %>%
        mutate(id = ifelse(row_number > 1, paste0(id, "_", row_number), id)) %>%
        select(-row_number)
    
    return(dataframe)
}


completeness_by_var <- function(dataframe) {
    # Calculates the ratio of non-NA rows to total row count per column
    # Returns tibble with two variables, var_name and completeness
    # var_name: string, the name of the variable
    # completeness: decimal between 0 and 1.
    
    return_df <-
        # colMeans gives the proportion we're looking for. 
        colMeans(!is.na(dataframe)) %>%
        # The rest of this function is to convert this into a format that's easy to use
        data.frame() %>%
        tibble::rownames_to_column() %>%
        dplyr::tibble() %>%
        dplyr::rename(., completeness=., var_name=rowname) %>%
        dplyr::arrange(by_group=desc(completeness))
        
    return(return_df)
}


filter_by_completeness <- function(dataframe, threshold=.5, operator='>') {
    # Returns dataframe with only variables that meet the given threshold based on the condition provided
    
    # This creates a list of variables that meet our threshold requirement.
    vars_to_keep <- 
        dataframe %>%
        completeness_by_var() %>%
        dplyr::filter(
            # match.fun allows us to use a selected operator without a bunch of if else clutter
            # This, using the default operator, is equivalent to `completeness > threshold`
            match.fun(operator)(completeness, threshold)
        ) %>%
        dplyr::pull(var_name)
    
    return_df <- 
        dataframe %>%
        dplyr::select(
            dplyr::all_of(c('id', vars_to_keep))
        )
    
    return(return_df)
}


count_particular_string <- function(dataframe, string) {
    
    return_df <- colSums(dataframe == string, na.rm = TRUE) %>%
        data.frame() %>%
        dplyr::arrange(., desc(.)) %>%
        tibble::rownames_to_column() %>%
        dplyr::tibble() %>%
        dplyr::rename(., count=., var_name=rowname) %>%
        dplyr::arrange(by_group=desc(count))
    
    return(return_df)
    
}


pattern_to_bool <- function(string, pattern) {
    # Returns NA if string is NA, 1 if pattern is found, 0 otherwise. 
    return(
        ifelse(
            is.na(string), NA, 
            as.numeric(str_detect(string, pattern))
        )
    )
}


# This function makes what'd be a huge chunk of code way less repetitive.
# Essentially what this does is check if the vector made up by the NSEW walkway columns
# is in the orientation dominance list of vectors. 
# I use mapply here which is the multivariate version of apply to use all 4 columns as variables.
apply_comparison <- function(df, list_name, comparison_list) {
    df %>%
        mutate(
            !!list_name := mapply(
                function(n, s, e, w) {
                    if (any(is.na(c(n, s, e, w)))) {
                        NA
                    } else if (list(c(n, s, e, w)) %in% comparison_list) {
                        1
                    } else {
                        0
                    }
                },
                walkway_N, walkway_S, walkway_E, walkway_W
            )
        )
}



