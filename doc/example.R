## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  eval = FALSE, #this makes so that chunks are not rerun 
  cache = TRUE,
  warning = FALSE)

## -----------------------------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("dzhakparov/GeneSelectR", build_vignettes = FALSE)
#  

## -----------------------------------------------------------------------------
#  GeneSelectR::configure_environment()

## -----------------------------------------------------------------------------
#  GeneSelectR::set_reticulate_python()
#  library(GeneSelectR)
#  # rest of your code

## -----------------------------------------------------------------------------
#  data("UrbanRandomSubset")
#  head(UrbanRandomSubset[,1:10])

## -----------------------------------------------------------------------------
#  GeneSelectR::set_reticulate_python()
#  library(GeneSelectR)
#  library(dplyr)
#  # the rest of the code

## -----------------------------------------------------------------------------
#  X <- UrbanRandomSubset %>% dplyr::select(-treatment) # get the feature matrix
#  y <- UrbanRandomSubset['treatment'] # store the data point label in a separate vector

## -----------------------------------------------------------------------------
#  selection_results <- GeneSelectR::GeneSelectR(X_train = X,
#                                                y_train = y,
#                                                njobs = -1) # all cores will be used
#  selection_results

## -----------------------------------------------------------------------------
#  fs_param_grids <- list(
#    "Lasso" = list(
#      "feature_selector__estimator__C" = c(0.01, 0.1, 1L, 10L),
#      "feature_selector__estimator__solver" = c('liblinear','saga')
#    ),
#    "Univariate" = list(
#      "feature_selector__param" = seq(50L, 200L, by = 50L)
#    ),
#    "boruta" = list(
#      "feature_selector__perc" = seq(80L, 100L, by = 10L),
#      'feature_selector__n_estimators' = c(50L, 100L, 250L, 500L)
#    ),
#    "RandomForest" = list(
#      "feature_selector__estimator__n_estimators" = seq(100L, 500L,by = 50L),
#      "feature_selector__estimator__max_depth" = c(10L, 20L, 30L),
#      "feature_selector__estimator__min_samples_split" = c(2L, 5L, 10L),
#      "feature_selector__estimator__min_samples_leaf" = c(1L, 2L, 4L),
#      "feature_selector__estimator__bootstrap" = c(TRUE, FALSE)
#    )
#  )

## -----------------------------------------------------------------------------
#  selection_results <- GeneSelectR::GeneSelectR(X_train = X,
#                                                  y_train = y,
#                                                  njobs = -1,
#                                                  search_type = 'grid')

## -----------------------------------------------------------------------------
#  # import the sklearn module
#  sklearn <- reticulate::import('sklearn')
#  # define the feature selection submodule and wanted methods with an estimator
#  feature_selection <- sklearn$feature_selection
#  select_from_model <- feature_selection$SelectFromModel
#  RFE <- feature_selection$RFE
#  rf <- sklearn$ensemble$RandomForestClassifier
#  
#  my_methods <- list('RFE' = RFE(estimator = rf(), n_features_to_select = 100L),
#                     'SelectFromModel' = select_from_model(estimator = rf()))

## -----------------------------------------------------------------------------
#  selection_results <- GeneSelectR::GeneSelectR(X_train = X,
#                                                  y_train = y,
#                                                  njobs = -1,
#                                                  feature_selection_methods=my_methods)

## -----------------------------------------------------------------------------
#  plot_feature_importance(selection_results, top_n_features = 10)

## -----------------------------------------------------------------------------
#  plot_metrics(selection_results)

## -----------------------------------------------------------------------------
#  selection_results$test_metrics

## -----------------------------------------------------------------------------
#  overlap <- calculate_overlap_coefficients(selection_results)
#  overlap

## -----------------------------------------------------------------------------
#  plot_overlap_heatmaps(overlap)

## -----------------------------------------------------------------------------
#  custom_list <- list(custom_list = c('char1','char2','char3','char4','char5'),
#                      custom_list2 = c('char1','char2','char3','char4','char5'))
#  overlap1 <- calculate_overlap_coefficients(selection_results, custom_lists = custom_list)
#  plot_overlap_heatmaps(overlap1)

## -----------------------------------------------------------------------------
#  # Get the annotations for the genes in the analysis
#  # The example dataset contains sequencing from humans
#  ah <- AnnotationHub::AnnotationHub()
#  human_ens <- AnnotationHub::query(ah, c("Homo sapiens", "EnsDb"))
#  human_ens <- human_ens[['AH98047']]
#  annotations_ahb <- ensembldb::genes(human_ens, return.type = "data.frame") %>%
#   dplyr::select(gene_id,gene_name,entrezid,gene_biotype)
#  
#  annotations_df <- annotate_gene_lists(pipeline_results = selection_results,
#                                        annotations_ahb = annotations_ahb,
#                                        format = 'ensembl_gene_name')
#  annotations_df

## -----------------------------------------------------------------------------
#  # Formal class 'AnnotatedGeneLists' [package "GeneSelectR"] with 2 slots
#  #   ..@ inbuilt    :List of 4
#  #   .. ..$ Lasso       :Formal class 'GeneList' [package "GeneSelectR"] with 3 slots
#  #   .. .. .. ..@ SYMBOL  : chr [1:54] "MCOLN3" "AGL" "NBPF26" "EXO1" ...
#  #   .. .. .. ..@ ENSEMBL : chr [1:54] "ENSG00000055732" "ENSG00000162688" "ENSG00000273136" "ENSG00000174371" ...
#  #   .. .. .. ..@ ENTREZID: chr [1:54] "55283" "178" "101060684" "9156" ...
#  #   .. ..$ Univariate  :Formal class 'GeneList' [package "GeneSelectR"] with 3 slots
#  #   .. .. .. ..@ SYMBOL  : chr [1:74] "CDA" "BMP8B" "MCOLN3" "AGL" ...
#  #   .. .. .. ..@ ENSEMBL : chr [1:74] "ENSG00000158825" "ENSG00000116985" "ENSG00000055732" "ENSG00000162688" ...
#  #   .. .. .. ..@ ENTREZID: chr [1:74] "978" "656" "55283" "178" ...
#  #   .. ..$ RandomForest:Formal class 'GeneList' [package "GeneSelectR"] with 3 slots
#  #   .. .. .. ..@ SYMBOL  : chr [1:73] "CDA" "BMP8B" "MCOLN3" "NBPF26" ...
#  #   .. .. .. ..@ ENSEMBL : chr [1:73] "ENSG00000158825" "ENSG00000116985" "ENSG00000055732" "ENSG00000273136" ...
#  #   .. .. .. ..@ ENTREZID: chr [1:73] "978" "656" "55283" "101060684" ...
#  #   .. ..$ boruta      :Formal class 'GeneList' [package "GeneSelectR"] with 3 slots
#  #   .. .. .. ..@ SYMBOL  : chr [1:36] "MCOLN3" "AGL" "NBPF26" "BNIPL" ...
#  #   .. .. .. ..@ ENSEMBL : chr [1:36] "ENSG00000055732" "ENSG00000162688" "ENSG00000273136" "ENSG00000163141" ...
#  #   .. .. .. ..@ ENTREZID: chr [1:36] "55283" "178" "101060684" "149428" ...
#  #   ..@ permutation:List of 4
#  #   .. ..$ Lasso       :Formal class 'GeneList' [package "GeneSelectR"] with 3 slots
#  #   .. .. .. ..@ SYMBOL  : chr [1:4] "HBB" "" "ABCC12" "LTF"
#  #   .. .. .. ..@ ENSEMBL : chr [1:4] "ENSG00000244734" "ENSG00000259529" "ENSG00000140798" "ENSG00000012223"
#  #   .. .. .. ..@ ENTREZID: chr [1:4] "3043" "NA" "94160" "4057"
#  #   .. ..$ Univariate  :Formal class 'GeneList' [package "GeneSelectR"] with 3 slots
#  #   .. .. .. ..@ SYMBOL  : chr [1:19] "KAZN" "RGS16" "IGFN1" "CHI3L1" ...
#  #   .. .. .. ..@ ENSEMBL : chr [1:19] "ENSG00000189337" "ENSG00000143333" "ENSG00000163395" "ENSG00000133048" ...
#  #   .. .. .. ..@ ENTREZID: chr [1:19] "23254" "6004" "91156" "1116" ...
#  #   .. ..$ RandomForest:Formal class 'GeneList' [package "GeneSelectR"] with 3 slots
#  #   .. .. .. ..@ SYMBOL  : chr [1:16] "ZBED6" "CYP2E1" "HBB" "OTOGL" ...
#  #   .. .. .. ..@ ENSEMBL : chr [1:16] "ENSG00000257315" "ENSG00000130649" "ENSG00000244734" "ENSG00000165899" ...
#  #   .. .. .. ..@ ENTREZID: chr [1:16] "100381270" "1571" "3043" "283310" ...
#  #   .. ..$ boruta      :Formal class 'GeneList' [package "GeneSelectR"] with 3 slots
#  #   .. .. .. ..@ SYMBOL  : chr [1:8] "CHI3L1" "HBB" "IDH1" "PTPRN" ...
#  #   .. .. .. ..@ ENSEMBL : chr [1:8] "ENSG00000133048" "ENSG00000244734" "ENSG00000138413" "ENSG00000054356" ...
#  #   .. .. .. ..@ ENTREZID: chr [1:8] "1116" "3043" "3417" "5798" ...

## -----------------------------------------------------------------------------
#  print(annotated_df@inbuilt$RandomForest@ENTREZID)

## -----------------------------------------------------------------------------
#  custom_list <- list(background = c('gene1', 'gene2', 'gene3', 'gene4'),
#                      DEGs = c('gene5','gene6','gene7'))
#  annotations_df_with_custom <- annotate_gene_lists(pipeline_results = selection_results,
#                                        annotations_ahb = annotations_ahb,
#                                        format = 'ensembl_gene_name',
#                                        custom_lists = custom_list)
#  annotations_df_with_custom

## -----------------------------------------------------------------------------
#  annotated_GO <- GO_enrichment_analysis(annotated_df)

## -----------------------------------------------------------------------------
#  annotated_GO <- GO_enrichment_analysis(annotated_df,
#                                         list_type = 'permutation', #run GO enrichment on permutation based selected features
#                                         keyType = 'ENSEMBL', # run analysis with ENSEMBLIDs
#                                         ont = 'BP') # run BP ontology
#  annotated_GO

## -----------------------------------------------------------------------------
#  annot_child_fractions <- compute_GO_child_term_metrics(GO_data = annotated_GO,
#                                GO_terms = c("GO:0002376", "GO:0044419"),
#                                plot = TRUE)

