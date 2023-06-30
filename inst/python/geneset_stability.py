import numpy as np
import pandas as pd

def calculate_correlation_matrix(X):
    print("Calculating correlation matrix...")
    correlation_matrix = X.corr()
    print("Correlation matrix calculated.")
    return correlation_matrix

def jaccard_index(set1, set2):
    print("Calculating Jaccard index...")
    intersection = len(set(set1) & set(set2))
    union = len(set(set1) | set(set2))
    jaccard = intersection / union
    print(f"Jaccard index for the given sets is {jaccard}")
    return jaccard

def correlation_factor(set1, set2, correlation_matrix, tau=0.5):
    correlations = []
    for gene1 in set1:
        for gene2 in set2:
            if gene1 != gene2 and gene1 in correlation_matrix and gene2 in correlation_matrix[gene1]:
                correlation = correlation_matrix[gene1][gene2]
                if abs(correlation) > tau:
                    correlations.append(abs(correlation))
    mean_correlations = np.mean(correlations) if correlations else 0
    print(f"Correlation factor for the given sets is {mean_correlations}") 
    return mean_correlations 

    

def zucknick_stability(gene_sets, correlation_matrix):
    print("Calculating Zucknick stability...")
    print(gene_sets)  # print the gene_sets object
    M = len(gene_sets)
    stability = 0
    for i in range(M):
        for j in range(i+1, M):
            print(f"Calculating stability for sets {i+1} and {j+1}...")
            jaccard = jaccard_index(gene_sets[str(i+1)], gene_sets[str(j+1)])
            correlation = correlation_factor(gene_sets[str(i+1)], gene_sets[str(j+1)], correlation_matrix)
            stability += (jaccard + correlation) / (2 * jaccard)
    stability /= (M * (M - 1) / 2)
    print(f"Zucknick stability is {stability}")
    return stability
