from sklearn.model_selection import train_test_split
from sklearn.metrics import precision_score, recall_score, f1_score, accuracy_score
from sklearn.inspection import permutation_importance
from joblib import Parallel, delayed
import pandas as pd

def split_data(X, y, test_size):
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size)
    return X_train, X_test, y_train, y_test

def evaluate_test_metrics(grid_search, X_test, y_test):
    best_model = grid_search.best_estimator_
    y_pred = best_model.predict(X_test)
    precision = precision_score(y_test, y_pred, average="weighted")
    recall = recall_score(y_test, y_pred, average="weighted")
    f1 = f1_score(y_test, y_pred, average="weighted")
    accuracy = accuracy_score(y_test, y_pred)
    return {'precision': precision, 'recall': recall, 'f1': f1, 'accuracy': accuracy}

# def get_feature_importances(pipeline, X_train, pipeline_name, iter):
#     classifier = pipeline.named_steps['classifier']
#     feature_importances = getattr(classifier, 'coef_', None) or getattr(classifier, 'feature_importances_', None)
#     if feature_importances is None:
#         print("Classifier doesn't have coef_ or feature_importances_ attributes")
#         return None
# 
#     feature_selector = pipeline.named_steps['feature_selector']
#     original_feature_names = X_train.columns.tolist()
# 
#     if hasattr(feature_selector, 'get_support'):
#         selected_indices = feature_selector.get_support(indices=True)
#     elif hasattr(feature_selector, 'support_'):
#         selected_indices = feature_selector.support_
#     else:
#         print("Feature selector doesn't have get_support() or support_ attributes")
#         return None
# 
#     selected_feature_names = [original_feature_names[i] for i in selected_indices]
#     importances = pd.DataFrame({'feature': selected_feature_names, 'importance': feature_importances})
#     importances.sort_values(by='importance', ascending=False, inplace=True)
#     importances['rank'] = range(1, len(importances)+1)
#     importances.rename(columns={'rank': f'rank_{pipeline_name}_split_{iter}'}, inplace=True)
#     print(importances)
#     return importances

def get_feature_importances(pipeline, X_train, pipeline_name, iter):
    classifier = pipeline.named_steps['classifier']
    feature_importances = getattr(classifier, 'coef_', None) or getattr(classifier, 'feature_importances_', None)
    
    if feature_importances is None:
        print("Classifier doesn't have coef_ or feature_importances_ attributes")
        return None

    feature_selector = pipeline.named_steps['feature_selector']
    original_feature_names = X_train.columns.tolist()

    if hasattr(feature_selector, 'get_support'):
        selected_indices = feature_selector.get_support(indices=True)
    elif hasattr(feature_selector, 'support_'):
        selected_indices = feature_selector.support_
    else:
        print("Feature selector doesn't have get_support() or support_ attributes")
        return None

    if len(selected_indices) != len(feature_importances):
        print("Length mismatch between selected indices and feature importances.")
        return None

    selected_feature_names = [original_feature_names[i] for i in selected_indices]
    
    importance_dict = {
        'feature': selected_feature_names,
        'importance': list(feature_importances),
        'rank': list(range(1, len(feature_importances) + 1)),
        'pipeline_name': [pipeline_name] * len(feature_importances),
        'iter': [iter] * len(feature_importances)
    }
    
    return importance_dict
  
def calculate_permutation_feature_importance(pipeline, X_train, y_train, pipeline_name, iter, n_repeats=10, random_state=0, n_jobs=-1):
    perm_importance = permutation_importance(pipeline, X_train, y_train, n_repeats=n_repeats, random_state=random_state, n_jobs=n_jobs)
    importances = perm_importance.importances_mean
    feature_names = X_train.columns.tolist()
    importance_df = pd.DataFrame({'feature': feature_names, 'importance': importances})
    importance_df.sort_values(by='importance', ascending=False, inplace=True)
    importance_df['rank'] = range(1, len(importance_df)+1)
    importance_df.rename(columns={'rank': f'rank_{pipeline_name}_split_{iter}'}, inplace=True)
    return importance_df

from joblib import Parallel, delayed
from sklearn.model_selection import GridSearchCV, RandomizedSearchCV

def perform_grid_search(X_train, y_train, pipeline, scoring, params, search_type, n_iter, n_jobs):
    if search_type == 'grid':
        search_cv = GridSearchCV(estimator=pipeline, param_grid=params, scoring=scoring, cv=5, n_jobs=n_jobs, verbose=1)
    elif search_type == 'random':
        search_cv = RandomizedSearchCV(estimator=pipeline, param_distributions=params, n_iter=n_iter, scoring=scoring, cv=5, n_jobs=n_jobs, verbose=1)
    else:
        raise ValueError("Invalid search_type. Choose either 'grid' or 'random'.")

    search_cv.fit(X_train, y_train)
    return search_cv

def single_split(split_idx):
        print(f"Fitting the data split: {split_idx}")

        if perform_test_split:
            X_train_split, X_test_split, y_train_split, y_test_split = split_data(X_train, y_train, testsize)
        else:
            X_train_split, y_train_split = X_train, y_train

        split_fitted_pipelines = {}
        split_cv_results = {}
        split_best_score = {}
        split_selected_features = {}
        split_test_metrics = {}
        split_permutation_importances = {}

        for fs_name, pipeline in selected_pipelines.items():
            print(f"Fitting pipeline for {fs_name} feature selection method")

            X_train_sub_split, X_valid_split, y_train_sub_split, y_valid_split = split_data(X_train_split, y_train_split, validsize)

            fs_params = {}
            if custom_fs_grids is not None:
                fs_params.update(custom_fs_grids.get(fs_name, {}))
            if fs_grids is not None:
                fs_params.update(fs_grids.get(fs_name, {}))

            if not fs_params:
                raise ValueError(f"No parameters found for feature selector {fs_name}")

            params = {**classifier_params, **fs_params}

            search_cv = perform_grid_search(X_train_sub_split, y_train_sub_split, pipeline, scoring, params, search_type, n_iter, n_jobs = n_jobs_grid)

            best_model = search_cv.best_estimator_

            if perform_test_split:
                test_set_metrics = evaluate_test_metrics(search_cv, X_test_split, y_test_split)
                split_test_metrics[fs_name] = test_set_metrics

            split_fitted_pipelines[fs_name] = best_model
            split_cv_results[fs_name] = search_cv.cv_results_
            split_best_score[fs_name] = search_cv.best_score_
            split_selected_features[fs_name] = get_feature_importances(best_model, X_train_sub_split, pipeline_name = fs_name, iter = split_idx)

            if calculate_permutation_importance:
                print('Performing Permutation Importance Calculation')
                split_permutation_importances[fs_name] = calculate_permutation_feature_importance(best_model, X_valid_split, y_valid_split)

        return {
            'cv_best_score': split_best_score,
            'split_results': split_fitted_pipelines,
            'selected_features': split_selected_features,
            'cv_results': split_cv_results,
            'test_metrics': split_test_metrics,
            'permutation_importances': split_permutation_importances
        }
        
def run_pipelines(X_train, y_train, selected_pipelines, custom_fs_grids, fs_grids, classifier_params, n_jobs_grid, n_splits=5, perform_test_split=True, testsize=0.2, validsize=0.2, scoring='accuracy', search_type='grid', n_iter=10, n_jobs=-1, calculate_permutation_importance=False):
    """
    Run a series of machine learning pipelines with different feature selection methods.

    Parameters:
    - X_train, y_train: Training data and labels
    - selected_pipelines: Dictionary of pipelines for different feature selection methods
    - custom_fs_grids, fs_grids: Custom and default parameter grids for feature selection
    - classifier_params: Parameters for the classifier
    - n_splits: Number of train-test splits
    - perform_test_split: Whether to perform a train-test split
    - testsize, validsize: Size of the test and validation sets
    - scoring: Scoring metric
    - search_type: Type of hyperparameter search ('grid' or 'random')
    - n_iter: Number of iterations for random search
    - n_jobs: Number of jobs for parallelization
    - calculate_permutation_importance: Whether to calculate permutation importance

    Returns:
    Dictionary containing fitted pipelines, cross-validation results, selected features, best scores, test metrics, and permutation importances.
    """
# 
#     def single_split(split_idx):
#         print(f"Fitting the data split: {split_idx}")
# 
#         if perform_test_split:
#             X_train_split, X_test_split, y_train_split, y_test_split = split_data(X_train, y_train, testsize)
#         else:
#             X_train_split, y_train_split = X_train, y_train
# 
#         split_fitted_pipelines = {}
#         split_cv_results = {}
#         split_best_score = {}
#         split_selected_features = {}
#         split_test_metrics = {}
#         split_permutation_importances = {}
# 
#         for fs_name, pipeline in selected_pipelines.items():
#             print(f"Fitting pipeline for {fs_name} feature selection method")
# 
#             X_train_sub_split, X_valid_split, y_train_sub_split, y_valid_split = split_data(X_train_split, y_train_split, validsize)
# 
#             fs_params = {}
#             if custom_fs_grids is not None:
#                 fs_params.update(custom_fs_grids.get(fs_name, {}))
#             if fs_grids is not None:
#                 fs_params.update(fs_grids.get(fs_name, {}))
# 
#             if not fs_params:
#                 raise ValueError(f"No parameters found for feature selector {fs_name}")
# 
#             params = {**classifier_params, **fs_params}
# 
#             search_cv = perform_grid_search(X_train_sub_split, y_train_sub_split, pipeline, scoring, params, search_type, n_iter, n_jobs = n_jobs_grid)
# 
#             best_model = search_cv.best_estimator_
# 
#             if perform_test_split:
#                 test_set_metrics = evaluate_test_metrics(search_cv, X_test_split, y_test_split)
#                 split_test_metrics[fs_name] = test_set_metrics
# 
#             split_fitted_pipelines[fs_name] = best_model
#             split_cv_results[fs_name] = search_cv.cv_results_
#             split_best_score[fs_name] = search_cv.best_score_
#             split_selected_features[fs_name] = get_feature_importances(best_model, X_train_sub_split, pipeline_name = fs_name, iter = split_idx)
# 
#             if calculate_permutation_importance:
#                 print('Performing Permutation Importance Calculation')
#                 split_permutation_importances[fs_name] = calculate_permutation_feature_importance(best_model, X_valid_split, y_valid_split)
# 
#         return {
#             'cv_best_score': split_best_score,
#             'split_results': split_fitted_pipelines,
#             'selected_features': split_selected_features,
#             'cv_results': split_cv_results,
#             'test_metrics': split_test_metrics,
#             'permutation_importances': split_permutation_importances
#         }

    #results = [single_split(split_idx) for split_idx in range(1, n_splits)]
    # with ProcessPoolExecutor(max_workers=n_jobs) as executor:
    #     results = list(executor.map(single_split, range(1, n_splits + 1)))
    #   results = list(executor.map(single_split, range(1, n_splits + 1)))
    # from pyspark import SparkContext
    # sc = SparkContext()
    # results = sc.parallelize(range(1, n_splits + 1)).map(single_split).collect()
    from multiprocessing import Pool

    with Pool(processes=n_jobs) as pool:
      results = pool.map(single_split, range(1, n_splits + 1))
  
    

    
    
    # # Combine results from all splits
    cv_best_score = {}
    split_results = {}
    selected_features = {}
    cv_results = {}
    test_metrics = {}
    permutation_importances = {}
    
    for idx, result in enumerate(results, 1):
        cv_best_score[f'split_{idx}'] = result['cv_best_score']
        split_results[f'split_{idx}'] = result['split_results']
        selected_features[f'split_{idx}'] = result['selected_features']
        cv_results[f'split_{idx}'] = result['cv_results']
        test_metrics[f'split_{idx}'] = result['test_metrics']
        permutation_importances[f'split_{idx}'] = result['permutation_importances']

    # print(cv_best_score)
    return {
        'cv_best_score': cv_best_score,
        'split_results': split_results,
        'selected_features': selected_features,
        'cv_results': cv_results,
        'test_metrics': test_metrics,
        'permutation_importances': permutation_importances
    }

