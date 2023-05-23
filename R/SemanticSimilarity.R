#' @title Convert Gene Lists to ENTREZIDs
#' @description This function converts a list of gene lists to ENTREZIDs.
#' @param pipeline_results A PipelineResults object containing selected_features.
#' @param custom_lists (optional) A named list of character vectors containing additional user-defined gene sets.
#' @return A named list of character vectors containing ENTREZIDs.
#' @importFrom AnnotationHub AnnotationHub query
#' @importFrom ensembldb genes
#' @importFrom dplyr select inner_join
#' @importFrom stringr str_split_fixed
#' @importFrom purrr map
#' @importFrom rlang .data
convert_to_entrezid <- function(pipeline_results, custom_lists = NULL) {
  # Extract the gene lists from the PipelineResults object
  gene_lists <- map(pipeline_results$mean_feature_importances, "feature")

  # Check and merge custom_lists with gene_lists
  if (!is.null(custom_lists)) {
    if (!is.list(custom_lists) || any(names(custom_lists) == "") || any(map_chr(custom_lists, class) != "character")) {
      stop("custom_lists must be a named list of character vectors.")
    }
    gene_lists <- c(gene_lists, custom_lists)
  }

  # Get the annotations
  ah <- AnnotationHub::AnnotationHub()
  human_ens <- AnnotationHub::query(ah, c("Homo sapiens", "EnsDb"))
  human_ens <- human_ens[['AH98047']]
  annotations_ahb <- ensembldb::genes(human_ens, return.type = "data.frame") %>%
    dplyr::select(.data$gene_id, .data$gene_name, .data$entrezid, .data$gene_biotype)

  # Convert each gene list to ENTREZIDs
  entrez_lists <- map(gene_lists, function(gene_list) {
    df <- data.frame(feature = gene_list)
    df[,c('ensembl_id', 'gene_name')] <- stringr::str_split_fixed(df$feature, '__', n = Inf)
    df <- df %>% dplyr::select(.data$ensembl_id, .data$gene_name)
    df <- dplyr::inner_join(annotations_ahb, df, by = c('gene_id' = 'ensembl_id'))
    as.character(df$entrezid)
  })

  # Set the names for the entrez_lists using the names from 'selected_features' and 'custom_lists'
  names(entrez_lists) <- names(gene_lists)

  return(entrez_lists)
}

#' @title Perform Gene Set Enrichment Analysis
#' @description This function performs gene set enrichment analysis for each gene set and returns the merged results.
#' @param gene_sets A named list of character vectors containing gene sets.
#' @param background A character vector representing the background gene set.
#' @return A data frame containing the merged enrichment results for all gene sets.
#' @importFrom ViSEAGO Bioconductor2GO annotate create_topGOdata runTest merge_enrich_terms
#' @importFrom purrr map map_chr
perform_GO_enrichment_analysis <- function(gene_sets, background) {
  # Check if gene_sets is a list of named vectors. If not, raise an error.
  if (!is.list(gene_sets) || any(map_chr(gene_sets, class) != "character") || any(is.na(names(gene_sets)))) {
    stop("gene_sets must be a list of named vectors.")
  }

  # Connect to Bioconductor and retrieve the GO annotations.
  Bioconductor <- ViI'm sorry, but I made a mistake in the previous message. Here is the correct refactoring of your functions:

```r
#' @title Convert Gene Lists to ENTREZIDs
  #' @description This function converts a list of gene lists to ENTREZIDs.
  #' @param pipeline_results A PipelineResults object containing selected_features.
  #' @param custom_lists (optional) A named list of character vectors containing additional user-defined gene sets.
  #' @return A named list of character vectors containing ENTREZIDs.
  #' @importFrom AnnotationHub AnnotationHub query
  #' @importFrom ensembldb genes
  #' @importFrom dplyr select inner_join
  #' @importFrom stringr str_split_fixed
  #' @importFrom purrr map
  #' @importFrom rlang .data
  convert_to_entrezid <- function(pipeline_results, custom_lists = NULL) {
    # Extract the gene lists from the PipelineResults object
    gene_lists <- map(pipeline_results$mean_feature_importances, "feature")

    # Check and merge custom_lists with gene_lists
    if (!is.null(custom_lists)) {
      if (!is.list(custom_lists) || any(names(custom_lists) == "") || any(map_chr(custom_lists, class) != "character")) {
        stop("custom_lists must be a named list of character vectors.")
      }
      gene_lists <- c(gene_lists, custom_lists)
    }

    # Get the annotations
    ah <- AnnotationHub::AnnotationHub()
    human_ens <- AnnotationHub::query(ah, c("Homo sapiens", "EnsDb"))
    human_ens <- human_ens[['AH98047']]
    annotations_ahb <- ensembldb::genes(human_ens, return.type = "data.frame") %>%
      dplyr::select(.data$gene_id, .data$gene_name, .data$entrezid, .data$gene_biotype)

    # Convert each gene list to ENTREZIDs
    entrez_lists <- map(gene_lists, function(gene_list) {
      df <- data.frame(feature = gene_list)
      df[,c('ensembl_id', 'gene_name')] <- stringr::str_split_fixed(df$feature, '__', n = Inf)
      df <- df %>% dplyr::select(.data$ensembl_id, .data$gene_name)
      df <- dplyr::inner_join(annotations_ahb, df, by = c('gene_id' = 'ensembl_id'))
      as.character(df$entrezid)
    })

    # Set the names for the entrez_lists using the names from 'selected_features' and 'custom_lists'
    names(entrez_lists) <- names(gene_lists)

    return(entrez_lists)
  }

  #' @title Perform Gene Set Enrichment Analysis
  #' @description This function performs gene set enrichment analysis for each gene set and returns the merged results.
  #' @param gene_sets A named list of character vectors containing gene sets.
  #' @param background A character vector representing the background gene set.
  #' @return A data frame containing the merged enrichment results for all gene sets.
  #' @importFrom ViSEAGO Bioconductor2GO annotate create_topGOdata runTest merge_enrich_terms
  #' @importFrom purrr map map_chr
  perform_GO_enrichment_analysis <- function(gene_sets, background) {
    # Check if gene_sets is a list of named vectors. If not, raise an error.
    if (!is.list(gene_sets) || any(map_chr(gene_sets, class) != "character") || any(is.na(names(gene_sets)))) {
      stop("gene_sets must be a list of named vectors.")
    }

    # ConnectI apologize for the confusion, but it seems there was a mistake in my previous response. Here's the refactored version of your functions:

    ```r
    # Load necessary libraries
    library(ViSEAGO)

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

    # The perform_GO_enrichment_analysis function takes a list of gene_sets and a background gene set
    # as input and returns the results of a Gene Ontology (GO) enrichment analysis.
    perform_GO_enrichment_analysis <- function(gene_sets, background) {
      # Check if gene_sets is a list of named vectors. If not, raise an error.
      if (!is.list(gene_sets) || any(!sapply(gene_sets, is.vector)) || any(is.na(names(gene_sets)))) {
        stop("gene_sets must be a list of named vectors.")
      }

      # Connect to Bioconductor and retrieve the GO annotations.
      Bioconductor <- ViSEAGO::Bioconductor2GO()
      myGENE2GO <- ViSEAGO::annotate("org.Hs.eg.db", Bioconductor)

      input_list <- list()

      # Iterate through the names of the gene_sets list, excluding the "background" vector.
      gene_set_names <- setdiff(names(gene_sets), "background")
      for (gene_set_name in gene_set_names) {
        # Print the name of the current gene set for debugging purposes.
        print(gene_set_name)

        # Extract the gene set using its name.
        gene_set <- gene_sets[[gene_set_name]]

        # Create BP topGOdata for the current gene set using the ViSEAGO package.
        BP_data <- ViSEAGO::create_topGOdata(
          geneSel = gene_set,       I apologize for the confusion, but it seems there was a mistake in my previous response. Here's the refactored version of your functions:

```r
# Load necessary libraries
library(ViSEAGO)

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

# The perform_GO_enrichment_analysis function takes a list of gene_sets and a background gene set
# as input and returns the results of a Gene Ontology (GO) enrichment analysis.
perform_GO_enrichment_analysis <- function(gene_sets, background) {
  # Check if gene_sets is a list of named vectors. If not, raise an error.
  if (!is.list(gene_sets) || any(!sapply(gene_sets, is.vector)) || any(is.na(names(gene_sets)))) {
    stop("gene_sets must be a list of named vectors.")
  }

  # Connect to Bioconductor and retrieve the GO annotations.
  Bioconductor <- ViSEAGO::Bioconductor2GO()
  myGENE2GO <- ViSEAGO::annotate("org.Hs.eg.db", Bioconductor)

  input_list <- list()

  # Iterate through the names of the gene_sets list, excluding the "background" vector.
  gene_set_names <- setdiff(names(gene_sets), "background")
  for (gene_set_name in gene_set_names) {
    # Print the name of the current gene set for debugging purposes.
    print(gene_set_name)

    # Extract the gene set using its name.
    gene_set <- gene_sets[[gene_set_name]]

    # Create BP topGOdata for the current gene set using the ViSEAGO package.
    BP_data <- ViSEAGO::create_topGOdata(
      geneSel = gene_set,        # Selected gene set
      allGenes = background,     # Background gene set
      gene2GO = myGENE2GO,       # GO annotations
      ont = "BP",                # GO term ontology (Biological ProcessI apologize for the confusion earlier. Here is the continuation of the refactoring of your functions:

```r
      nodeSize = 5               # Minimum number of annotated genes for a GO term
    )

    # Run the enrichment test using the topGO package with the "elim" algorithm and Fisher's test.
      elim_test <- topGO::runTest(
        BP_data,                   # Input topGOdata object
        algorithm = "elim",        # Algorithm to use (Elim)
        statistic = "fisher",      # Statistical test to use (Fisher's test)
        cutOff = 0.01              # Significance cutoff
      )

      # Assign the BP_data and elim_test results to the function's environment
      assign(paste0("BP_", gene_set_name), BP_data, envir = environment())
      assign(paste0("elim_BP_", gene_set_name), elim_test, envir = environment())
      }

    for (method in gene_set_names) {
      input_list[[method]] <- c(
        paste0("BP_", method),
        paste0("elim_BP_", method)
      )
    }

    # Merge the topGO results using the ViSEAGO package.
    BP_sResults <- ViSEAGO::merge_enrich_terms(
      cutoff = 0.05,              # Significance cutoff for merging results
      Input = input_list,
      envir = environment())

    # Return the results of the GO enrichment analysis.
    return(BP_sResults)
    }

  # Custom wrapper functions
  custom_compute_SS_distances <- function(GO_SS, distance, ...) {
    ViSEAGO::compute_SS_distances(GO_SS, distance = distance, ...)
  }

  custom_GOterms_heatmap <- function(GO_SS, showIC, showGOlabels, GO_tree, samples_tree, ...) {
    ViSEAGO::GOterms_heatmap(GO_SS, showIC = showIC, showGOlabels = showGOlabels,
                             GO.tree = GO_tree, samples.tree = samples_tree, ...)
  }

  custom_GOclusters_heatmap <- function(GO_cluster_SS, tree, ...) {
    ViSEAGO::GOclusters_heatmap(GO_cluster_SS, tree = tree, ...)
  }

  visualize_results <- function(results,
                                distance_GO_SS = "Wang",
                                distance_GO_cluster_SS = "BMA",
                                tree_distance = "Wang",
                                tree_aggreg_method = "ward.D2",
                                heatmap_dynamic_deepSplit = 2,
                                heatmap_dynamic_minClusterSize = 10,
                                compute_SS_distances_args = list(),
                                GOterms_heatmap_args = list(),
                                GOclusters_heatmap_args = list()) {
    require('ViSEAGO')

    # Retrieve GO annotations
    Bioconductor <- ViSEAGO::Bioconductor2GO()
    myGENE2GO <- ViSEAGO::annotate("org.Hs.eg.db", Bioconductor)

    # Semantic similarity analysis
    GO_SS <- ViSEAGO::build_GO_SS(gene2GO = myGENE2GO, enrich_GO_terms = results)
    GO_SS <- do.call(custom_compute_SS_distances,
                     c(list(GO_SS = GO_SS, distance = distance_GO_SS), compute_SS_distances_args))

    # heatmap of GO terms
    GO_terms_heatmap <- do.call(custom_GOterms_heatmap,
                                c(list(GO_SS = GO_SS,
                                       showIC = TRUE,
                                       showGOlabels = F,
                                       GO_tree = list(
                                         tree = list(
                                           distance = tree_distance,
                                           aggreg.method = tree_aggreg_method
                                         ),
                                         cut = list(
                                           dynamic = list(
                                             pamStage = TRUE,
                                             pamRespectsDendro = TRUE,
                                             deepSplit = heatmap_dynamic_deepSplit,
                                             I apologize for the confusion earlier. Here is the continuation of the refactoring of your functions:

                                               ```r
                                             minClusterSize = heatmap_dynamic_minClusterSize
                                           )
                                         )
                                       ),
                                       samples_tree = NULL),
                                  GOterms_heatmap_args))

    # Add class attribute
    class(GO_terms_heatmap) <- "GOTermsHeatmap"

    # Semantic similarity between clusters
    GO_cluster_SS <- do.call(custom_compute_SS_distances, c(list(GO_SS = GO_terms_heatmap,
                                                                 distance = distance_GO_cluster_SS), compute_SS_distances_args))

    # Heatmap of GO clusters
    GO_clusters_heatmap <- do.call(custom_GOclusters_heatmap,
                                   c(list(GO_cluster_SS = GO_cluster_SS,
                                          tree = list(
                                            distance = distance_GO_cluster_SS,
                                            aggreg.method = tree_aggreg_method)),
                                     GOclusters_heatmap_args))

    # Add class attribute
    class(GO_clusters_heatmap) <- "GOClustersHeatmap"

    return(list(GO_terms_heatmap = GO_terms_heatmap, GO_clusters_heatmap = GO_clusters_heatmap))
  }

  display_heatmap <- function(heatmap_object, heatmap_type, save_plot=FALSE, output_file=NULL) {
    # Check that the heatmap type is valid
    if (!heatmap_type %in% c("GOterms", "GOclusters")) {
      stop("Invalid heatmap_type. Must be 'GOterms' or 'GOclusters'.")
    }

    # Check that the input object is of the correct class
    if (!inherits(heatmap_object, heatmap_type)) {
      stop(sprintf("heatmap_object must be of class '%s'", heatmap_type))
    }

    # Extract the heatmap data based on the heatmap_type
    heatmap_data <- switch(heatmap_type,
                           GOterms = heatmap_object@heatmap$GOterms,
                           GOclusters = heatmap_object@heatmap$GOclusters)

    if (!save_plot) {
      # Display the interactive heatmap
      heatmap_data
    } else {
      if (is.null(output_file)) {
        stop("output_file must be specified when save_plot is set to TRUE.")
      }

      if (heatmap_type == "GOterms") {
        # Number of rows
        rowlen <- nrow(heatmap_object@enrich_GOs@data)

        # Adjust minimum size
        if (rowlen < 10) { rowlen <- 10 }

        # Compute height
        rowlen <- rowlen^(1.70 + 1.70 * exp(-rowlen / 20))

        # Max height limit
        if (rowlen > 10000) { rowlen <- 10000 }

        # Adjust heatmap size
        heatmap_data <- layout(heatmap_data, height = rowlen)
      }

      # Save the heatmap as a static PNG image
      plotly::plotly_IMAGE(heatmap_data, format = "png", out_file = output_file, width = NULL, height = NULL, scale = 1)
    }
  }

