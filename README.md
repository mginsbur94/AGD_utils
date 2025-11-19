# Fonctions de visualisation pour l’ACM (MCA) et pour la typologie HCPC

Ce dépôt fournit des fonctions R permettant de produire facilement des graphiques lisibles :

- des **modalités actives** et **supplémentaires** d’une analyse des correspondances multiples (**ACM / MCA**) réalisée avec `{FactoMineR}`,
- de l’**évolution des inerties intra et inter** dans une classification hiérarchique sur composantes principales (**HCPC**).

Les fonctions reposent sur **ggplot2** et **ggrepel** afin de :

- éviter le chevauchement des labels,
- fournir des visualisations propres, reproductibles et directement exploitables dans des rapports ou présentations.

---

## 📦 Fonctions incluses

### 🔹 `plot_quali_act()`

Affiche les **modalités actives** d’un objet MCA (résultats principaux).

Fonctionnalités :

- filtrage selon la **contribution** aux axes (argument `contrib`),
- choix des axes à représenter (`axes = c(1,2)`),
- sélection d’un sous-ensemble de variables actives (`vars`),
- option `legend = FALSE` pour un rendu épuré,
- option `fix_axes = TRUE` (par défaut) imposant des limites d’axes symétriques pour éviter les graphiques « écrasés »,
- suppression optionnelle des modalités `*.NA`.

---

### 🔹 `plot_quali_sup()`

Affiche les **modalités supplémentaires** d’un objet MCA (variables passées via `quali.sup`).

Fonctionnalités :

- sélection de variables (`vars`), simple ou via un vecteur **nommé** pour afficher des labels personnalisés,
- retrait optionnel des modalités `.NA`,
- choix des axes (`axes = c(1,3)`),
- légende automatique avec formes distinctes par variable supplémentaire.

---

### 🔹 `plot_hcpc_inertia_ratios()`

Affiche, pour un objet **HCPC**, les **rapports d’inertie intra et inter** en fonction du nombre de classes :

- **Intra(k) / Intra(k−1)** (axe de gauche),
- **Inter(k) / Inter(k−1)** (axe de droite),
- mise à l’échelle automatique de la courbe inter,
- double axe Y via `sec_axis`,
- légende propre en bas.

Ce graphique permet de **justifier le choix du nombre de classes** retenu par HCPC, en montrant les zones où les gains intra/inter sont les plus importants.

---

## 📥 Installation

Cloner le dépôt puis sourcer les fonctions :

```r
# install.packages(c("FactoMineR", "ggplot2", "ggrepel"))
source("https://raw.githubusercontent.com/mginsbur94/AGD_utils/main/Visualisation_ACM_quali.R")
