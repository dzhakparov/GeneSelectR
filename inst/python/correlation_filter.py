import numpy as np
import pandas as pd
from sklearn.base import BaseEstimator, TransformerMixin

# class CorrelationFilter(BaseEstimator, TransformerMixin):
#     def __init__(self, threshold=0.9):
#         self.threshold = threshold
# 
#     def fit(self, X, y=None):
#         if not isinstance(X, pd.DataFrame):
#             X = pd.DataFrame(X)
# 
#         corr_matrix = X.corr().abs()
#         upper_triangle = np.triu(np.ones(corr_matrix.shape), k=1).astype(bool)
#         self.to_drop_ = [column for column in X.columns if any(corr_matrix.where(upper_triangle)[column] > self.threshold)]
#         return self
# 
#     def transform(self, X, y=None):
#         if not isinstance(X, pd.DataFrame):
#             X = pd.DataFrame(X)
# 
#         return X.drop(columns=self.to_drop_)
      

class CorrelationFilter(BaseEstimator, TransformerMixin):
    def __init__(self, threshold=0.9):
        self.threshold = threshold

    def fit(self, X, y=None):
        if not isinstance(X, pd.DataFrame):
            X = pd.DataFrame(X)

        corr_matrix = X.corr().abs()
        upper_triangle = np.triu(np.ones(corr_matrix.shape), k=1).astype(bool)
        self.to_drop_ = [column for column in X.columns if any(corr_matrix.where(upper_triangle)[column] > self.threshold)]
        self.remaining_columns_ = list(X.columns.difference(self.to_drop_))
        return self

    def transform(self, X, y=None):
        if not isinstance(X, pd.DataFrame):
            X = pd.DataFrame(X)

        return X.drop(columns=self.to_drop_)
    
    def get_feature_names_out(self):
        return self.remaining_columns_
