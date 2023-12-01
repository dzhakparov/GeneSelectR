#' @title Convert and Annotate Gene Lists
#' @description This function converts a list of gene lists to different formats and returns an object of the class AnnotatedGeneLists.
#' @param pipeline_results A PipelineResults object containing a named list of data frames.
#' @param custom_lists (optional) A named list of character vectors containing additional user-defined gene sets.
#' @param annotations_ahb A data.frame object containing gene annotations with columns 'gene_id', 'gene_name', and 'entrezid'.
#' @param format The format of the gene list in 'pipeline_results' and 'custom_lists'. This should be one of "ENSEMBL", "ENTREZ", or "SYMBOL".
#' @return An object of the class AnnotatedGeneLists, which contains the following components:
#'     - @field inbuilt: A list of AnnotatedGeneList objects derived from the 'inbuilt_feature_importance' field of the 'pipeline_results' parameter.
#'     - @field permutation: A list of AnnotatedGeneList objects derived from the 'permutation_importance' field of the 'pipeline_results' parameter, if available.
#'     Each AnnotatedGeneList object in these lists includes the gene identifiers in different formats (SYMBOL, ENSEMBL, ENTREZID) and is structured to facilitate further analysis and visualization of gene lists.
#'     If an error occurs during the conversion or annotation process, a warning message is given and the corresponding list entry will be NULL.
#' @importFrom methods is
#' @examples
#' \donttest{
#' # Simple Usage with Mock Data
#' # Create a mock PipelineResults object with minimal data
#' mock_pipeline_results <- new("PipelineResults",
#'                              inbuilt_feature_importance = list(
#'                              "GeneSet1" = data.frame(feature = c("gene1", "gene2"))),
#'                              permutation_importance = NULL)
#'
#' # Mock annotations data frame
#' mock_annotations_ahb <- data.frame(gene_id = c("gene1", "gene2"),
#'                                    gene_name = c("Gene One", "Gene Two"),
#'                                    entrezid = c(101, 102))
#'
#' # Convert and annotate gene lists
#' annotated_lists <- annotate_gene_lists(mock_pipeline_results,
#'                                        NULL,
#'                                        mock_annotations_ahb,
#'                                        "SYMBOL")
#' print(annotated_lists)
#'
#' # Using Custom Gene Lists
#' # Create custom gene lists
#' custom_gene_lists <- list("CustomList1" = c("gene3", "gene4"))
#'
#' # Convert and annotate gene lists with custom gene lists included
#' annotated_lists_custom <- annotate_gene_lists(mock_pipeline_results,
#'                                               custom_gene_lists,
#'                                               mock_annotations_ahb,
#'                                               "SYMBOL")
#' print(annotated_lists_custom)}
#'
#' @export
#'
annotate_gene_lists <- function(pipeline_results, custom_lists = NULL, annotations_ahb, format = c("ENTREZ", "ENSEMBL", "SYMBOL")) {
  if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
    stop("The clusterProfiler package is required but not installed. Please install it first.")
  }
  # Check if input object belongs to the PipelineResults class
if (!inherits(pipeline_results, "PipelineResults")) {
  stop("The input object does not belong to the PipelineResults class.")
}

# Validate format
format <- match.arg(format, c("ENTREZ", "ENSEMBL", "SYMBOL"))

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
    if (format == "ENSEMBL") {
      original_ids <- gsub("\\..*", "", original_ids)
    }

    tryCatch({
      # Convert IDs using bitr
      if (format == "ENSEMBL") {
        converted_df <- clusterProfiler::bitr(original_ids, fromType = "ENSEMBL", toType = c("SYMBOL", "ENTREZID"), OrgDb = org.Hs.eg.db::org.Hs.eg.db)
      } else if (format == "SYMBOL") {
        converted_df <- clusterProfiler::bitr(original_ids, fromType = "SYMBOL", toType = c("ENSEMBL", "ENTREZID"), OrgDb = org.Hs.eg.db::org.Hs.eg.db)
      } else if (format == "ENTREZ") {
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
