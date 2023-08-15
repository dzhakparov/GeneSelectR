#' @title Convert and Annotate Gene Lists
#' @description This function converts a list of gene lists to different formats and returns an object of the class AnnotatedGeneLists.
#' @param pipeline_results A PipelineResults object containing a named list of data frames.
#' @param custom_lists (optional) A named list of character vectors containing additional user-defined gene sets.
#' @param annotations_ahb A data.frame object containing gene annotations with columns 'gene_id', 'gene_name', and 'entrezid'.
#' @param format The format of the gene list in 'pipeline_results' and 'custom_lists'. This should be one of "ensembl_gene_name", "ensembl_id", or "gene_name".
#' @param separator (optional) A character indicating the separator between ensembl_id and gene_name when format is "ensembl_gene_name".
#' @return An object of the class AnnotatedGeneLists.
#' @importFrom methods is
#' @importFrom dplyr select inner_join rename coalesce
#' @importFrom stringr str_split_fixed
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
annotate_gene_lists <- function(pipeline_results, custom_lists = NULL, annotations_ahb, format = c("ensembl_gene_name", "ensembl_id", "gene_name"), separator = "__") {
# Check if input object belongs to the PipelineResults class
if (!inherits(pipeline_results, "PipelineResults")) {
  stop("The input object does not belong to the PipelineResults class.")
}

# Validate format
format <- match.arg(format, c("ensembl_gene_name", "ensembl_id", "gene_name"))

# Extract the gene lists from the PipelineResults object
mean_importances_lists <- lapply(pipeline_results@mean_feature_importances, function(x) x$feature)

# Check for permutation_importances in the pipeline_results
if (!is.null(pipeline_results@permutation_importances)) {
  permutation_importances_lists <- lapply(pipeline_results@permutation_importances, function(x) x$feature)
}

# Check and merge custom_lists with mean_importances_lists
if (!is.null(custom_lists)) {
  if (!is.list(custom_lists) || any(names(custom_lists) == "") || any(sapply(custom_lists, class) != "character")) {
    stop("custom_lists must be a named list of character vectors.")
  }
  mean_importances_lists <- c(mean_importances_lists, custom_lists)
  permutation_importances_lists <- c(permutation_importances_lists, custom_lists)
}

# Prepare lists to hold AnnotatedGeneList objects
annotated_mean_lists <- list()
annotated_permutation_lists <- list()

# Function to convert and annotate gene lists
convert_and_annotate <- function(gene_lists, annotated_lists) {
  # Convert each gene list and create AnnotatedGeneList object
  for (method in names(gene_lists)) {
    df <- data.frame(feature = gene_lists[[method]])

    # Check format
    if (format == "ensembl_gene_name") {
      df[,c('ensembl_id', 'gene_name')] = stringr::str_split_fixed(df$feature, separator, n = Inf)
      df <- df %>% dplyr::select(.data$ensembl_id, .data$gene_name)
      df <- dplyr::inner_join(annotations_ahb, df, by = c('gene_id' = 'ensembl_id')) %>%
        dplyr::mutate(gene_name = dplyr::coalesce(.data$gene_name.x, .data$gene_name.y)) %>%
        dplyr::select(-.data$gene_name.x, -.data$gene_name.y)
    } else if (format == "ensembl_id") {
      df$ensembl_id = df$feature
      df <- df %>% dplyr::select(.data$ensembl_id)
      df <- dplyr::inner_join(annotations_ahb, df, by = c('gene_id' = 'ensembl_id')) %>%
        dplyr::mutate(gene_name = dplyr::coalesce(.data$gene_name.x, .data$gene_name.y)) %>%
        dplyr::select(-.data$gene_name.x, -.data$gene_name.y)
    } else if (format == 'gene_name') { # gene_name
      df$gene_name = df$feature
      df <- df %>% dplyr::select(.data$gene_name)
      df <- dplyr::inner_join(annotations_ahb, df, by = c('gene_name' = 'gene_name'))  %>%
        dplyr::mutate(gene_name = dplyr::coalesce(.data$gene_name.x, .data$gene_name.y)) %>%
        dplyr::select(-.data$gene_name.x, -.data$gene_name.y)
    } else if (format == 'entrezid') {
      df$entrezid = df$feature
      df <- df %>% dplyr::select(.data$entrezid)
      df <- dplyr::inner_join(annotations_ahb, df, by = c('entrezid' = 'entrezid'))  %>%
        dplyr::mutate(gene_name = dplyr::coalesce(.data$gene_name.x, .data$gene_name.y)) %>%
        dplyr::select(-.data$gene_name.x, -.data$gene_name.y)
    }

    # Rename the columns as required
    df <- df %>%
      dplyr::rename(ENSEMBL = .data$gene_id, SYMBOL = .data$gene_name, ENTREZID = .data$entrezid)

    # Create AnnotatedGeneList object
    annotated_list <- new("GeneList",
                          SYMBOL = as.character(df$SYMBOL),
                          ENSEMBL = as.character(df$ENSEMBL),
                          ENTREZID = as.character(df$ENTREZID))

    # Add to list of AnnotatedGeneList objects
    annotated_lists[[method]] <- annotated_list
  }

  return(annotated_lists)
}

# Apply the function to mean_feature_importances
annotated_mean_lists <- convert_and_annotate(mean_importances_lists, annotated_mean_lists)

# Apply the function to permutation_importances if they exist
if (!is.null(pipeline_results@permutation_importances)) {
  annotated_permutation_lists <- convert_and_annotate(permutation_importances_lists, annotated_permutation_lists)
}

# Create AnnotatedGeneLists object
annotated_gene_lists <- new("AnnotatedGeneLists")

# Store inbuilt and permutation lists
if (!is.null(pipeline_results@permutation_importances)) {
  annotated_gene_lists@permutation <- annotated_permutation_lists
} else annotated_gene_lists@permutation <- NULL

annotated_gene_lists@inbuilt <- annotated_mean_lists


return(annotated_gene_lists)
}
