import pandas as pd
from sklearn.metrics import confusion_matrix, cohen_kappa_score
import numpy as np
from scipy.stats import norm


def cohen_kappa_report(csv_path, rater_1, rater_2):
    """
    References:
      - https://en.wikipedia.org/wiki/Cohen's_kappa
      - https://rowannicholls.github.io/python/statistics/agreement/cohens_kappa.html
      - https://real-statistics.com/reliability/interrater-reliability/cohens-kappa/
      - https://numiqo.com/statistics-calculator/reliability-analysis/cohens-kappa-calculator

    :param csv_path: path to CSV file to read in pandas
    :param rater_1: column name of rater_1 in CSV file to read in pandas
    :param rater_2: column name of rater_2 in CSV file to read in pandas
    :return: None, just prints results to console
    """

    print("cohen_kappa_report: ", csv_path)

    df = pd.read_csv(csv_path)
    cm = confusion_matrix(df[rater_1], df[rater_2])
    print("Confusion Matrix:")
    print(cm)

    # Cross check our manual calculations with sklearn
    kappa_sklearn = cohen_kappa_score(df['rater_1'], df['rater_2'])
    print(f'kappa (sklearn) = {kappa_sklearn:.3f}')

    # Sample size
    n = np.sum(cm)

    # Expected matrix
    sum0 = np.sum(cm, axis=0)
    sum1 = np.sum(cm, axis=1)
    expected = np.outer(sum0, sum1) / n

    # Number of classes
    n_classes = cm.shape[0]

    # Calculate p_o (the observed proportionate agreement) and
    # p_e (the probability of random agreement)
    identity = np.identity(n_classes)
    p_o = np.sum((identity * cm) / n)
    p_e = np.sum((identity * expected) / n)

    # Calculate Cohen's kappa
    kappa = (p_o - p_e) / (1 - p_e)

    # Confidence intervals
    se = np.sqrt((p_o * (1 - p_o)) / (n * (1 - p_e)**2))
    ci = 1.96 * se * 2
    lower = kappa - 1.96 * se
    upper = kappa + 1.96 * se

    # z and p-value
    z = kappa / se
    p = 2 * (1 - norm.cdf(abs(z)))

    print(
        f'kappa = {kappa:.3f}',
        f'p_o = {p_o:.3f}, p_e = {p_e:.3f}',
        f'standard error = {se:.3f}',
        f'95% CI = ({lower:.3f}, {upper:.3f})',
        f'z-value = {z:.3f}',
        f'p-value = {p:.3e}', sep='\n'
    )
