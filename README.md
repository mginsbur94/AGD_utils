# Fonctions de visualisation pour l’ACM (MCA)

Ce dépôt contient deux fonctions R destinées à produire facilement des graphiques lisibles des **modalités actives** et **supplémentaires** d’une analyse des correspondances multiples (**ACM / MCA**) réalisée avec `{FactoMineR}`.

Les deux fonctions utilisent **ggplot2** et **ggrepel**, afin d’éviter le chevauchement des labels et de fournir une représentation propre et interprétable.

---

## Fonctions incluses

### 🔹 `plot_quali_act()`

Affiche les **modalités actives** d’un objet MCA, avec :

* filtrage possible selon la **contribution aux axes**,
* choix des axes à représenter,
* option `legend = FALSE` si l’on souhaite un graphique épuré,
* option `fix_axes = TRUE` (par défaut) qui empêche l’écrasement de l’un des axes si seules des modalités très proches de 0 sont affichées.

---

### 🔹 `plot_quali_sup()`

Affiche les **modalités qualitatives supplémentaires** (variables passées dans l’argument `quali.sup` de `MCA()`).

Permet :

* la sélection de variables,
* l’affichage de labels personnalisés dans la légende,
* le retrait optionnel des modalités `*.NA`.

---

## Installation

Cloner le dépôt puis sourcer le fichier :

```r
# install.packages(c("FactoMineR", "ggplot2", "ggrepel"))
source("R/plots_acm.R")   # adapter au chemin réel
```

Les fonctions supposent un objet MCA construit avec `{FactoMineR}` :

```r
library(FactoMineR)
res_mca <- MCA(donnees, quali.sup = c(5, 6))
```

---

## Exemple : `plot_quali_act()`

```r
plot_quali_act(
  res_mca,
  axes = c(1, 3),
  contrib = c(Inf, 1),  # seuil de contribution sur l’axe 3
  legend = FALSE,
  fix_axes = TRUE       # évite l’écrasement des axes
)
```

### Arguments principaux

| Argument             | Description                                                    |
| -------------------- | -------------------------------------------------------------- |
| `res_mca`            | Objet MCA issu de `{FactoMineR}`                               |
| `vars`               | Sélection des variables actives (codes seuls ou vecteur nommé) |
| `axes`               | Axes à représenter (ex : `c(1,2)` )                            |
| `contrib`            | Seuil de contribution (multiplicateur de la moyenne)           |
| `legend`             | Affiche ou non la légende                                      |
| `fix_axes`           | Fixe des limites cohérentes pour éviter les axes compressés    |
| `drop_na_modalities` | Retire les modalités `*.NA`                                    |

---

## Exemple : `plot_quali_sup()`

```r
plot_quali_sup(
  res_mca,
  vars = c("Diplôme"="DIPLOME", "Revenu total"="REVTOT_rec"),
  axes = c(1, 3),
  drop_na_modalities = TRUE
)
```

### Arguments principaux

| Argument             | Description                                                   |
| -------------------- | ------------------------------------------------------------- |
| `res_mca`            | Objet MCA                                                     |
| `vars`               | Variables supplémentaires à afficher (codes ou vecteur nommé) |
| `axes`               | Axes à représenter                                            |
| `drop_na_modalities` | Supprime les modalités `.NA`                                  |
| `legend`             | Gérée automatiquement selon le choix des shapes               |

---

## Exemple complet d’utilisation

```r
library(FactoMineR)
library(ggplot2)
library(ggrepel)

res_mca <- MCA(donnees, quali.sup = c(5, 6))

# Modalités actives contribuant fortement aux axes 1 et 2
plot_quali_act(res_mca, contrib = c(2, 2))

# Modalités supplémentaires (diplôme et revenu) sur les axes 1 et 3
plot_quali_sup(
  res_mca,
  vars = c("Diplôme"="DIPLOME", "Revenu total"="REVTOT_rec"),
  axes = c(1, 3)
)
```

---
