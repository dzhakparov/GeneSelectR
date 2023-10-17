generate_data <- function(n_genes=100, n_samples=100) {
  # Create a list to store the matrices and labels
  data <- list()

  # Generate a matrix where all genes are perfectly correlated
  matrix1 <- matrix(rep(rnorm(n_samples), n_genes), nrow=n_samples, ncol=n_genes)
  colnames(matrix1) <- paste0("Gene", 1:n_genes)
  data$matrix1 <- matrix1

  # Generate a matrix where all genes are uncorrelated
  matrix2 <- matrix(rnorm(n_samples * n_genes), nrow=n_samples, ncol=n_genes)
  colnames(matrix2) <- paste0("Gene", 1:n_genes)
  data$matrix2 <- matrix2

  # Create a vector of labels with even class distribution
  labels <- rep(c(0, 1), each=n_samples/2)
  data$labels <- labels

  return(data)
}
