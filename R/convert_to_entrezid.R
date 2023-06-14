#' @title Convert to ENTREZIDs
#' @description This function converts a list of gene lists to ENTREZIDs.
#' @param pipeline_results A PipelineResults object containing a named list of data frames.
#' @param custom_lists (optional) A named list of character vectors containing additional user-defined gene sets.
#' @param annotations_ahb A data.frame object containing gene annotations with columns 'gene_id', 'gene_name', and 'entrezid'.
#' @param format The format of the gene list in 'pipeline_results' and 'custom_lists'. This should be one of "ensembl_gene_name", "ensembl_id", or "gene_name".
#' @param separator (optional) A character indicating the separator between ensembl_id and gene_name when format is "ensembl_gene_name".
#' @return A list of gene sets converted to ENTREZIDs.
#' @importFrom methods is
#' @importFrom dplyr select inner_join
#' @importFrom stringr str_split_fixed
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
convert_to_entrezid <- function(pipeline_results, custom_lists = NULL, annotations_ahb, format = c("ensembl_gene_name", "ensembl_id", "gene_name"), separator = "__") {
  # Check if input object belongs to the PipelineResults class
  if (!inherits(pipeline_results, "PipelineResults")) {
    stop("The input object does not belong to the PipelineResults class.")
  }

  # Validate format
  format <- match.arg(format, c("ensembl_gene_name", "ensembl_id", "gene_name"))

  # Extract the gene lists from the PipelineResults object
  gene_lists <- lapply(pipeline_results@mean_feature_importances, function(x) x$feature)

  #print(gene_lists)
  # Check and merge custom_lists with gene_lists
  if (!is.null(custom_lists)) {
    if (!is.list(custom_lists) || any(names(custom_lists) == "") || any(sapply(custom_lists, class) != "character")) {
      stop("custom_lists must be a named list of character vectors.")
    }
    gene_lists <- c(gene_lists, custom_lists)
  }

  # Convert each gene list to ENTREZIDs
  entrez_lists <- lapply(gene_lists, function(gene_list) {
    df <- data.frame(feature = gene_list)
    #print(df)
    # Check format
    if (format == "ensembl_gene_name") {
      df[,c('ensembl_id', 'gene_name')] = stringr::str_split_fixed(df$feature, separator, n = Inf)
      #print(df)
      df <- df %>% dplyr::select(.data$ensembl_id, .data$gene_name)
      df <- dplyr::inner_join(annotations_ahb, df, by = c('gene_id' = 'ensembl_id'))
      print(df)
    } else if (format == "ensembl_id") {
      df$ensembl_id = df$feature
      df <- df %>% dplyr::select(.data$ensembl_id)
      df <- dplyr::inner_join(annotations_ahb, df, by = c('gene_id' = 'ensembl_id'))
    } else { # gene_name
      df$gene_name = df$feature
      df <- df %>% dplyr::select(.data$gene_name)
      df <- dplyr::inner_join(annotations_ahb, df, by = c('gene_name' = 'gene_name'))
      #print(df)
    }
    return(as.character(df$entrezid))
  })

  # Set the names for the entrez_lists using the names from 'selected_features' and 'custom_lists'
  names(entrez_lists) <- names(gene_lists)

  return(entrez_lists)
}

# # Get the annotations
# ah <- AnnotationHub::AnnotationHub()
# human_ens <- AnnotationHub::query(ah, c("Homo sapiens", "EnsDb"))
# human_ens <- human_ens[['AH98047']]
# annotations_ahb <- ensembldb::genes(human_ens, return.type = "data.frame") %>%
#   dplyr::select(.data$gene_id, .data$gene_name, .data$entrezid, .data$gene_biotype)
