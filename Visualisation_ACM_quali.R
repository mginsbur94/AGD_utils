plot_quali_sup <- function(res_mca,
                           vars = NULL,            # soit c("DIPLOME","REVTOT_rec"), soit c("Diplôme"="DIPLOME", ...)
                           axes = c(1, 2),         # numéros d’axes à afficher
                           drop_na_modalities = TRUE) {
  
  # Packages nécessaires
  stopifnot(requireNamespace("ggplot2", quietly = TRUE),
            requireNamespace("ggrepel", quietly = TRUE))
  library(ggplot2)
  library(ggrepel)
  
  ## 0. Préparation de l'argument vars : codes vs labels d'affichage -----------
  var_codes  <- NULL  # noms des variables dans l'objet MCA (ex: "DIPLOME")
  var_labels <- NULL  # labels pour la légende (ex: "Diplôme")
  
  if (!is.null(vars)) {
    if (!is.null(names(vars)) && any(names(vars) != "")) {
      # Cas nommé : c("Diplôme"="DIPLOME","Revenu"="REVTOT_rec")
      var_codes  <- unname(vars)
      var_labels <- names(vars)
    } else {
      # Cas non nommé : c("DIPLOME","REVTOT_rec")
      var_codes  <- vars
      var_labels <- vars
    }
  }
  
  ## 1. Coordonnées des modalités supplémentaires ------------------------------
  coord_sup <- as.data.frame(res_mca$quali.sup$coord)
  coord_sup$label <- rownames(coord_sup)
  
  ## 2. Récupérer la base d'origine et les variables supplémentaires ----------
  df_all <- as.data.frame(res_mca$call$X)
  quali_sup_idx   <- res_mca$call$quali.sup
  quali_sup_names <- colnames(df_all)[quali_sup_idx]
  
  coord_sup$var <- NA_character_
  for (v in quali_sup_names) {
    levs <- levels(df_all[[v]])
    coord_sup$var[coord_sup$label %in% levs & is.na(coord_sup$var)] <- v
  }
  
  ## 3. Option : enlever les modalités ".NA" -----------------------------------
  if (drop_na_modalities) {
    coord_sup <- coord_sup[!grepl("\\.NA$", coord_sup$label), , drop = FALSE]
  }
  
  ## 4. Filtrer sur les variables demandées ------------------------------------
  if (!is.null(var_codes)) {
    coord_sup <- coord_sup[coord_sup$var %in% var_codes, , drop = FALSE]
  }
  
  if (nrow(coord_sup) == 0) {
    stop("Aucune modalité supplémentaire à afficher (vérifie l’argument 'vars').")
  }
  
  ## 5. Préparer les axes ------------------------------------------------------
  xcol <- paste0("Dim ", axes[1])
  ycol <- paste0("Dim ", axes[2])
  
  if (!xcol %in% colnames(coord_sup) || !ycol %in% colnames(coord_sup)) {
    stop("Les axes demandés ne sont pas disponibles dans l'objet MCA.")
  }
  
  coord_sup$x <- coord_sup[[xcol]]
  coord_sup$y <- coord_sup[[ycol]]
  
  ## 5bis. Enlever les lignes avec NA sur x ou y -------------------------------
  coord_sup <- coord_sup[!is.na(coord_sup$x) & !is.na(coord_sup$y), , drop = FALSE]
  
  ## 6. Labels d’axes avec valeurs propres ------------------------------------
  ev <- res_mca$eig
  lab_x <- paste0("Axe ", axes[1], " (", round(ev[axes[1], 2], 1), " %)") 
  lab_y <- paste0("Axe ", axes[2], " (", round(ev[axes[2], 2], 1), " %)")
  
  ## 7. Construire une variable "var_plot" avec les beaux noms -----------------
  if (!is.null(var_codes)) {
    # mapping code -> label d'affichage
    map_codes_to_labels <- setNames(var_labels, var_codes)
    coord_sup$var_plot <- unname(map_codes_to_labels[coord_sup$var])
  } else {
    coord_sup$var_plot <- coord_sup$var
  }
  
  ## 8. Préparer un vecteur de formes suffisant --------------------------------
  vars_uniques <- unique(coord_sup$var_plot)
  n_vars <- length(vars_uniques)
  shapes_all <- c(0:25)  # 26 formes possibles
  if (n_vars > length(shapes_all)) {
    warning("Plus de variables que de formes disponibles, certaines seront recyclées.")
  }
  shapes_vec <- shapes_all[seq_len(n_vars)]
  names(shapes_vec) <- vars_uniques
  
  ## 9. Graphique ggplot -------------------------------------------------------
  p <- ggplot(coord_sup, aes(x = x, y = y, shape = var_plot)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(colour = "black", size = 2) +
    ggrepel::geom_text_repel(aes(label = label),
                             colour = "black",
                             size = 3,
                             show.legend = FALSE,
                             max.overlaps = Inf) +
    scale_shape_manual(values = shapes_vec, name = "Variable suppl.") +
    coord_equal() +
    labs(x = lab_x, y = lab_y) +
    theme_bw()
  
  return(p)
}

plot_quali_act <- function(res_mca,
                           vars = NULL,          
                           axes = c(1, 2),       
                           contrib = c(0, 0),
                           legend = TRUE,
                           drop_na_modalities = TRUE,
                           fix_axes = TRUE       # <<< NOUVEL ARGUMENT
                           ) {
  
  stopifnot(requireNamespace("ggplot2", quietly = TRUE),
            requireNamespace("ggrepel", quietly = TRUE))
  library(ggplot2)
  library(ggrepel)
  
  ## --- 0. Préparer l’argument vars -------------------------------------------
  var_codes  <- NULL
  var_labels <- NULL
  
  if (!is.null(vars) && length(vars) > 0) {
    if (!is.null(names(vars)) && any(names(vars) != "")) {
      var_codes  <- unname(vars)
      var_labels <- names(vars)
    } else {
      var_codes  <- vars
      var_labels <- vars
    }
  }
  
  ## --- 1. Coordonnées des modalités actives ----------------------------------
  coord_act <- as.data.frame(res_mca$var$coord)
  coord_act$label <- rownames(coord_act)
  
  df_all <- as.data.frame(res_mca$call$X)
  quali_sup_idx <- res_mca$call$quali.sup
  active_idx <- setdiff(seq_len(ncol(df_all)), quali_sup_idx)
  active_names <- colnames(df_all)[active_idx]
  
  coord_act$var <- NA_character_
  for (v in active_names) {
    levs <- levels(df_all[[v]])
    coord_act$var[coord_act$label %in% levs & is.na(coord_act$var)] <- v
  }
  
  ## --- 2. Retirer les modalités .NA ------------------------------------------
  if (drop_na_modalities) {
    coord_act <- coord_act[!grepl("\\.NA$", coord_act$label), , drop = FALSE]
  }
  
  ## --- 3. Ajouter contributions ----------------------------------------------
  contrib_mat <- as.data.frame(res_mca$var$contrib)
  
  ## Colonnes de contributions pour les axes choisis
  xcol_contrib <- paste0("Dim ", axes[1])
  ycol_contrib <- paste0("Dim ", axes[2])
  
  coord_act$contrib_x <- contrib_mat[coord_act$label, xcol_contrib]
  coord_act$contrib_y <- contrib_mat[coord_act$label, ycol_contrib]
  
  ## --- 4. Filtre contribution -------------------------------------------------
  if (!(contrib[1] == 0 && contrib[2] == 0)) {
    
    avg_x <- mean(coord_act$contrib_x, na.rm = TRUE)
    avg_y <- mean(coord_act$contrib_y, na.rm = TRUE)
    
    thr_x <- contrib[1] * avg_x
    thr_y <- contrib[2] * avg_y
    
    if (is.finite(contrib[1]) && is.finite(contrib[2])) {
      keep <- (coord_act$contrib_x > thr_x) | (coord_act$contrib_y > thr_y)
    } else if (is.finite(contrib[1]) && !is.finite(contrib[2])) {
      keep <- (coord_act$contrib_x > thr_x)
    } else if (!is.finite(contrib[1]) && is.finite(contrib[2])) {
      keep <- (coord_act$contrib_y > thr_y)
    } else {
      keep <- TRUE
    }
    
    coord_act <- coord_act[keep, , drop = FALSE]
  }
  
  ## --- 5. Filtre sur les variables demandées ---------------------------------
  if (!is.null(var_codes) && length(var_codes) > 0) {
    coord_act <- coord_act[coord_act$var %in% var_codes, , drop = FALSE]
  }
  
  if (nrow(coord_act) == 0) stop("Aucune modalité active à afficher.")
  
  ## --- 6. Préparer les axes --------------------------------------------------
  xcol <- paste0("Dim ", axes[1])
  ycol <- paste0("Dim ", axes[2])
  
  coord_act$x <- coord_act[[xcol]]
  coord_act$y <- coord_act[[ycol]]
  
  coord_act <- coord_act[!is.na(coord_act$x) & !is.na(coord_act$y), ]
  
  ## --- 6bis. Calcul des limites globales des axes (NOUVEAU) ------------------
  if (fix_axes) {
    coord_all <- as.data.frame(res_mca$var$coord)
    
    all_x <- coord_all[[xcol]]
    all_y <- coord_all[[ycol]]
    
    max_abs_x <- max(abs(all_x), na.rm = TRUE)
    max_abs_y <- max(abs(all_y), na.rm = TRUE)
    
    # limites symétriques autour de 0
    lim_x <- c(-max_abs_x, max_abs_x)
    lim_y <- c(-max_abs_y, max_abs_y)
  } else {
    lim_x <- NULL
    lim_y <- NULL
  }
  
  ## --- 7. Labels d’axes -------------------------------------------------------
  ev <- res_mca$eig
  lab_x <- paste0("Axe ", axes[1], " (", round(ev[axes[1], 2], 1), " %)")
  lab_y <- paste0("Axe ", axes[2], " (", round(ev[axes[2], 2], 1), " %)")
  
  ## --- 8. Gestion des labels de légende --------------------------------------
  if (!is.null(var_codes)) {
    map_codes_to_labels <- setNames(var_labels, var_codes)
    coord_act$var_plot <- unname(map_codes_to_labels[coord_act$var])
  } else {
    coord_act$var_plot <- coord_act$var
  }
  
  ## --- 9. Shapes selon legend -------------------------------------------------
  if (legend) {
    vars_uniques <- unique(coord_act$var_plot)
    shapes_all <- 0:25
    shapes_vec <- shapes_all[seq_len(length(vars_uniques))]
    names(shapes_vec) <- vars_uniques
    
    aes_shape <- aes(shape = var_plot)
    shape_scale <- scale_shape_manual(values = shapes_vec, name = "Variable active")
    
  } else {
    aes_shape <- NULL
    shape_scale <- NULL
    coord_act$var_plot <- NULL
  }
  
  ## --- 10. Plot ---------------------------------------------------------------
  p <- ggplot(coord_act, aes(x = x, y = y)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50")
  
  if (legend) {
    p <- p + geom_point(aes(shape = var_plot), size = 2, colour = "black") +
      shape_scale
  } else {
    p <- p + geom_point(shape = 16, size = 2, colour = "black")
  }
  
  p <- p +
    ggrepel::geom_text_repel(aes(label = label),
                             colour = "black",
                             size = 3,
                             show.legend = FALSE,
                             max.overlaps = Inf)
  
  if (fix_axes) {
    p <- p + coord_equal(xlim = lim_x, ylim = lim_y)
  } else {
    p <- p + coord_equal()
  }
  
  p <- p +
    labs(x = lab_x, y = lab_y) +
    theme_bw()
  
  if (!legend) p <- p + theme(legend.position = "none")
  
  return(p)
}

plot_hcpc_inertia_ratios <- function(hcpc_obj) {
  library(ggplot2)

  # --- Extraction des données HCPC ---
  t      <- hcpc_obj$call$t
  k_vals <- hcpc_obj$call$min : hcpc_obj$call$max

  intra_all <- t$within
  inert_tot <- intra_all[1]
  inter_all <- inert_tot - intra_all

  quot_intra <- t$quot
  quot_inter_raw <- inter_all[k_vals] / inter_all[k_vals - 1]

  # --- Mise à l’échelle de la courbe inter ---
  min_intra <- min(quot_intra)
  max_intra <- max(quot_intra)
  min_inter <- min(quot_inter_raw)
  max_inter <- max(quot_inter_raw)

  scale_inter_to_intra <- function(x) {
    (x - min_inter) / (max_inter - min_inter) * (max_intra - min_intra) + min_intra
  }

  quot_inter_scaled <- scale_inter_to_intra(quot_inter_raw)

  # --- Construction du data frame ---
  df <- data.frame(
    k = k_vals,
    quot_intra = quot_intra,
    quot_inter_raw = quot_inter_raw,
    quot_inter_sc = quot_inter_scaled
  )

  # --- Graphique ggplot2 ---
  ggplot(df, aes(x = k)) +
    geom_line(aes(y = quot_intra, linetype = "Intra"), linewidth = 0.8) +
    geom_point(aes(y = quot_intra, shape = "Intra"), size = 2) +
    geom_line(aes(y = quot_inter_sc, linetype = "Inter"), linewidth = 0.8) +
    geom_point(aes(y = quot_inter_sc, shape = "Inter"), size = 2) +
    scale_linetype_manual(
      name = "",
      values = c("Intra" = "solid", "Inter" = "dashed")
    ) +
    scale_shape_manual(
      name = "",
      values = c("Intra" = 16, "Inter" = 17)
    ) +
    scale_y_continuous(
      name = "Intra(k) / Intra(k-1)",
      limits = c(min_intra, max_intra),
      sec.axis = sec_axis(
        ~ (.-min_intra) * (max_inter - min_inter) / (max_intra - min_intra) + min_inter,
        name = "Inter(k) / Inter(k-1)"
      )
    ) +
    scale_x_continuous(breaks = k_vals) +
    labs(
      x = "Nombre de classes (k)",
      title = "Évolution des rapports d'inertie intra et inter (HCPC)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.minor = element_blank()
    )
}
