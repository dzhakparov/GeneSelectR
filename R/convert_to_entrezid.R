#' @title Convert to ENTREZIDs
#' @description This function converts a list of gene lists to ENTREZIDs.
#' @param pipeline_results A PipelineResults object containing a named list of data frames.
#' @param custom_lists (optional) A named list of character vectors containing additional user-defined gene sets.
#' @return A list of gene sets converted to ENTREZIDs.
#' @importFrom AnnotationHub AnnotationHub query
#' @importFrom ensembldb genes
#' @importFrom dplyr select inner_join
#' @importFrom stringr str_split_fixed



# before everything the lists should be converted to ENTREZIDs
# Define the function to convert a list of gene lists to ENTREZIDs
convert_to_entrezid <- function(pipeline_results, custom_lists = NULL) {
  # Extract the gene lists from the PipelineResults object
  gene_lists <- lapply(pipeline_results$mean_feature_importances, function(x) x$feature)

  # Check and merge custom_lists with gene_lists
  if (!is.null(custom_lists)) {
    if (!is.list(custom_lists) || any(names(custom_lists) == "") || any(sapply(custom_lists, class) != "character")) {
      stop("custom_lists must be a named list of character vectors.")
    }
    gene_lists <- c(gene_lists, custom_lists)
  }

  # Get the annotations
  ah <- AnnotationHub::AnnotationHub()
  human_ens <- AnnotationHub::query(ah, c("Homo sapiens", "EnsDb"))
  human_ens <- human_ens[['AH98047']]
  annotations_ahb <- ensembldb::genes(human_ens, return.type = "data.frame") %>%
    dplyr::select(gene_id, gene_name, entrezid, gene_biotype)

  # Convert each gene list to ENTREZIDs
  entrez_lists <- lapply(gene_lists, function(gene_list) {
    df <- data.frame(feature = gene_list)
    df[,c('ensembl_id', 'gene_name')] = stringr::str_split_fixed(df$feature, '__', n = Inf)
    df <- df %>% dplyr::select(ensembl_id, gene_name)
    df <- dplyr::inner_join(annotations_ahb, df, by = c('gene_id' = 'ensembl_id'))
    return(as.character(df$entrezid))
  })

  # Set the names for the entrez_lists using the names from 'selected_features' and 'custom_lists'
  names(entrez_lists) <- names(gene_lists)

  return(entrez_lists)
}

