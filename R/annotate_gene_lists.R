#' @title Convert and Annotate Gene Lists
#' @description This function converts a list of gene lists to different formats and returns an object of the class AnnotatedGeneLists.
#' @param pipeline_results A PipelineResults object containing a named list of data frames.
#' @param custom_lists (optional) A named list of character vectors containing additional user-defined gene sets.
#' @param annotations_ahb A data.frame object containing gene annotations with columns 'gene_id', 'gene_name', and 'entrezid'.
#' @param format The format of the gene list in 'pipeline_results' and 'custom_lists'. This should be one of "ensembl_gene_name", "ensembl_id", or "gene_name".
#' @param separator (optional) A character indicating the separator between ensembl_id and gene_name when format is "ensembl_gene_name".
#' @return An object of the class AnnotatedGeneLists.
#' @importFrom methods is
#' @importFrom clusterProfiler bitr
#' @export
#'
annotate_gene_lists <- function(pipeline_results, custom_lists = NULL, annotations_ahb, format = c("entrezid", "ensembl_id", "gene_name")) {
# Check if input object belongs to the PipelineResults class
if (!inherits(pipeline_results, "PipelineResults")) {
  stop("The input object does not belong to the PipelineResults class.")
}

# Validate format
format <- match.arg(format, c("ensembl_gene_name", "ensembl_id", "gene_name"))

# Extract the gene lists from the PipelineResults object
mean_importances_lists <- lapply(pipeline_results@inbuilt_feature_importance, function(x) x$feature)

# Check for permutation_importance in the pipeline_results
if (!is.null(pipeline_results@permutation_importance)) {
  permutation_importance_lists <- lapply(pipeline_results@permutation_importance, function(x) x$feature)
}

# Check and merge custom_lists with mean_importances_lists
if (!is.null(custom_lists)) {
  if (!is.list(custom_lists) || any(names(custom_lists) == "") || any(sapply(custom_lists, class) != "character")) {
    stop("custom_lists must be a named list of character vectors.")
  }
  mean_importances_lists <- c(mean_importances_lists, custom_lists)
  permutation_importance_lists <- c(permutation_importance_lists, custom_lists)
}

# Prepare lists to hold AnnotatedGeneList objects
annotated_mean_lists <- list()
annotated_permutation_lists <- list()


convert_and_annotate <- function(gene_lists, annotated_lists) {
  for (method in names(gene_lists)) {
    original_ids <- gene_lists[[method]]
    # original_ids <- as.character(original_ids)

    # Remove version numbers from Ensembl IDs if format is "ensembl_id"
    if (format == "ensembl_id") {
      original_ids <- gsub("\\..*", "", original_ids)
    }

    tryCatch({
      # Convert IDs using bitr
      if (format == "ensembl_id") {
        converted_df <- clusterProfiler::bitr(original_ids, fromType = "ENSEMBL", toType = c("SYMBOL", "ENTREZID"), OrgDb = org.Hs.eg.db::org.Hs.eg.db)
      } else if (format == "gene_name") {
        converted_df <- clusterProfiler::bitr(original_ids, fromType = "SYMBOL", toType = c("ENSEMBL", "ENTREZID"), OrgDb = org.Hs.eg.db::org.Hs.eg.db)
      } else if (format == "entrezid") {
        converted_df <- clusterProfiler::bitr(original_ids, fromType = "ENTREZID", toType = c("SYMBOL", "ENSEMBL"), OrgDb = org.Hs.eg.db::org.Hs.eg.db)
      }

      # Create AnnotatedGeneList object
      annotated_list <- new("GeneList",
                            SYMBOL = as.character(converted_df$SYMBOL),
                            ENSEMBL = as.character(converted_df$ENSEMBL),
                            ENTREZID = as.character(converted_df$ENTREZID))

      # Add to list of AnnotatedGeneList objects
      annotated_lists[[method]] <- annotated_list

    }, error = function(e) {
      warning(paste("Failed to convert IDs for method:", method, "\nError:", e$message))
      annotated_lists[[method]] <- NULL
    })
  }

  return(annotated_lists)
}

# Apply the function to inbuilt_feature_importance
annotated_mean_lists <- convert_and_annotate(mean_importances_lists, annotated_mean_lists)

# Apply the function to permutation_importance if they exist
if (!is.null(pipeline_results@permutation_importance)) {
  annotated_permutation_lists <- convert_and_annotate(permutation_importance_lists, annotated_permutation_lists)
}

# Create AnnotatedGeneLists object
annotated_gene_lists <- new("AnnotatedGeneLists")

# Store inbuilt and permutation lists
if (!is.null(pipeline_results@permutation_importance)) {
  annotated_gene_lists@permutation <- annotated_permutation_lists
} else annotated_gene_lists@permutation <- NULL

annotated_gene_lists@inbuilt <- annotated_mean_lists


return(annotated_gene_lists)
}
