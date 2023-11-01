# Load the PipelineResults object from the fixture
pipeline_results_fixture <- readRDS("fixtures/PipelineResults.rds")

skip_if_not_installed("AnnotationHub")
skip_if_offline()

custom_lists <- list(
  custom1 = c("ENSG00000141510", "ENSG00000223972", "ENSG00000186092", "ENSG00000187634"),
  custom2 = c("ENSG00000167286", "ENSG00000225972", "ENSG00000225630", "ENSG00000237973")
)

#
ah <- AnnotationHub::AnnotationHub()
human_ens <- AnnotationHub::query(ah, c("Homo sapiens", "EnsDb"))
human_ens <- human_ens[['AH98047']]
annotations_ahb <- ensembldb::genes(human_ens, return.type = "data.frame") %>%
  dplyr::select(gene_id,gene_name,entrezid,gene_biotype)

# Test 1: Check if it returns an object of the correct class
test_that("Returns an object of the correct class", {
  result <- annotate_gene_lists(pipeline_results_fixture, annotations_ahb = ah, format = "ENSEMBL")
  expect_s4_class(result, "AnnotatedGeneLists")
})

# Test 2: Validate if it correctly annotates gene lists from PipelineResults
test_that("Correctly annotates gene lists from PipelineResults", {
  result <- annotate_gene_lists(pipeline_results_fixture, annotations_ahb = ah, format = "ENSEMBL")
  expect_true("inbuilt" %in% slotNames(result))
  expect_s4_class(result@inbuilt[[1]], "GeneList")
})

# Test 3: Check if it handles custom lists properly
test_that("Handles custom lists properly", {
  result <- annotate_gene_lists(pipeline_results_fixture, custom_lists, ah, format = "ENSEMBL")
  expect_true("custom1" %in% names(result@inbuilt))
  expect_true("custom2" %in% names(result@inbuilt))
  expect_s4_class(result@inbuilt[["custom1"]], "GeneList")
  expect_s4_class(result@inbuilt[["custom2"]], "GeneList")
})

# Test 4: Validate the format of the gene list in the returned object
test_that("Validates the format of the gene list", {
  result <- annotate_gene_lists(pipeline_results_fixture, annotations_ahb = ah, format = "ENSEMBL")
  expect_true("ENSEMBL" %in% slotNames(result@inbuilt[[1]]))
  expect_true("SYMBOL" %in% slotNames(result@inbuilt[[1]]))
  expect_true("ENTREZID" %in% slotNames(result@inbuilt[[1]]))
})
