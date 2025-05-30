import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestClassifier
import umap
from sklearn.linear_model import LinearRegression
from nonlinear_clock import NonLinearClock
from matplotlib.lines import Line2D


def compute_rf_importances(csv_path,
                           target_col='cluster_louvain',
                           n_estimators=200,
                           random_state=42,
                           n_jobs=-1,
                           top_n=10):
    df = pd.read_csv(csv_path)
    y = df[target_col]
    X = df.drop(columns=[target_col])
    num_cols = X.select_dtypes(include='number').columns
    X[num_cols] = StandardScaler().fit_transform(X[num_cols])

    rf = RandomForestClassifier(n_estimators=n_estimators,
                                random_state=random_state,
                                n_jobs=n_jobs)
    rf.fit(X, y)

    imp = pd.Series(rf.feature_importances_, index=X.columns)
    top_imp = imp.nlargest(top_n)
    print(top_imp)
    top_imp.sort_values().plot.barh(title=f"Top-{top_n} RF importances")
    plt.tight_layout()
    plt.show()
    return top_imp


def feature_clock_pipeline(csv_path,
                           top_features,
                           target_col='cluster_louvain',
                           umap_params=None,
                           figsize=(7,7),
                           save_path='feature_clock.png'):
    df = pd.read_csv(csv_path)
    labels = df[target_col]
    X = df.drop(columns=[target_col])
    num_cols = X.select_dtypes(include='number').columns
    X[num_cols] = StandardScaler().fit_transform(X[num_cols])

    reducer = umap.UMAP(**(umap_params or {'n_components':2, 'random_state':42}))
    emb = reducer.fit_transform(X)
    emb = (emb - emb.mean(0)) / emb.std(0)

    X_top = X[top_features]
    clock = NonLinearClock(
        X=X_top.values,
        obs=top_features,
        embedding=emb,
        cluster_labels=labels,
        method='umap',
        color_scheme='tab20'
    )

    fig, ax = plt.subplots(figsize=figsize)
    sc = ax.scatter(emb[:,0], emb[:,1], c=labels.cat.codes if hasattr(labels, 'cat') else labels,
                    cmap='tab20', alpha=0.2, edgecolors='none')

    lm1 = LinearRegression().fit(X_top, emb[:,0])
    lm2 = LinearRegression().fit(X_top, emb[:,1])
    c1 = (lm1.coef_ - lm1.coef_.mean()) / lm1.coef_.std()
    c2 = (lm2.coef_ - lm2.coef_.mean()) / lm2.coef_.std()
    mags = np.hypot(c1, c2)
    angs = np.arctan2(c2, c1)
    xs = mags * np.cos(angs) * 1.5
    ys = mags * np.sin(angs) * 1.5

    colors = plt.get_cmap('tab10')(range(len(top_features)))
    for i, feat in enumerate(top_features):
        ax.arrow(0, 0, xs[i], ys[i], width=0.02, length_includes_head=True, color=colors[i])
    legend = [Line2D([0],[0], color=colors[i], lw=2, label=feat) for i, feat in enumerate(top_features)]
    ax.legend(handles=legend, title='Features', bbox_to_anchor=(1,1), loc='upper left')
    ax.set_xticks([])
    ax.set_yticks([])
    ax.set_title('Feature Clock')
    plt.tight_layout()
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    plt.show()


if __name__ == '__main__':
    top = compute_rf_importances('sampled_data_louvain.csv')
    feature_clock_pipeline('sampled_data_louvain.csv', top.index.tolist())