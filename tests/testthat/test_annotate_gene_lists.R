# Load the testthat package
library(testthat)

# Load the PipelineResults object from the fixture
pipeline_results_fixture <- readRDS("fixtures/PipelineResults.rds")

# Create custom lists
custom_lists <- list(
  custom1 = c("TP53", "MTND1P23"),
  custom2 = c("BRCA2", "MYC")
)

# Create a mock annotations_ahb data frame with more realistic gene names and IDs
annotations_ahb_mock <- data.frame(
  gene_id = c("ENSG00000141510", "ENSG00000225972", "ENSG00000139618", "ENSG00000198727", "ENSG00000136997"),
  gene_name = c("TP53", "MTND1P23", "BRCA2", "MTND5", "MYC"),
  entrezid = c("7157", "456594", "675", "4539", "4609"),
  gene_biotype = c("protein_coding", "pseudogene", "protein_coding", "protein_coding", "protein_coding")
)

# Test 1: Check if it returns an object of the correct class
test_that("Returns an object of the correct class", {
  result <- annotate_gene_lists(pipeline_results_fixture, annotations_ahb = annotations_ahb_mock, format = "ensembl_gene_name")
  expect_s4_class(result, "AnnotatedGeneLists")
})

# Test 2: Validate if it correctly annotates gene lists from PipelineResults
test_that("Correctly annotates gene lists from PipelineResults", {
  result <- annotate_gene_lists(pipeline_results_fixture, annotations_ahb = annotations_ahb_mock, format = "ensembl_gene_name")
  expect_true("inbuilt" %in% slotNames(result))
  expect_s4_class(result@inbuilt[[1]], "GeneList")
})

# Test 3: Check if it handles custom lists properly
test_that("Handles custom lists properly", {
  result <- annotate_gene_lists(pipeline_results_fixture, custom_lists, annotations_ahb_mock, format = "ensembl_gene_name")
  expect_true("custom1" %in% names(result@inbuilt))
  expect_true("custom2" %in% names(result@inbuilt))
  expect_s4_class(result@inbuilt[["custom1"]], "GeneList")
  expect_s4_class(result@inbuilt[["custom2"]], "GeneList")
})

# Test 4: Validate the format of the gene list in the returned object
test_that("Validates the format of the gene list", {
  result <- annotate_gene_lists(pipeline_results_fixture, annotations_ahb = annotations_ahb_mock, format = "ensembl_gene_name")
  expect_true("ENSEMBL" %in% slotNames(result@inbuilt[[1]]))
  expect_true("SYMBOL" %in% slotNames(result@inbuilt[[1]]))
  expect_true("ENTREZID" %in% slotNames(result@inbuilt[[1]]))
})
