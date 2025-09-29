import numpy as np
import pandas as pd
from statsmodels.stats.inter_rater import fleiss_kappa, aggregate_raters
from scipy.stats import norm

def fleiss_kappa_report(csv_path, rater_1, rater_2, rater_3):
    data = pd.read_csv(csv_path)
    counts, cats = aggregate_raters(data[[rater_1, rater_2, rater_3]])

    kappa = fleiss_kappa(counts)

    # Bootstrap over ites
    M = 2000
    rng = np.random.default_rng(42)
    n_items = counts.shape[0]
    idx = np.arange(n_items)

    boot = np.empty(M, dtype=float)
    for b in range(M):
        samp = counts[rng.choice(idx, size=n_items, replace=True)]
        boot[b] = fleiss_kappa(samp)

    se = boot.std(ddof=1)
    z = kappa / se
    p = 2 * (1 - norm.cdf(abs(z)))

    # Optional: 95% bootstrap percentile CI
    ci_low, ci_high = np.quantile(boot, [0.025, 0.975])

    print("Fleiss' kappa:", kappa)
    print("Bootstrap SE:", se)
    print("Z-value:", z)
    print("P-value:", p)
    print("95% bootstrap CI:", tuple(map(float, np.quantile(boot, [0.025, 0.975]))))