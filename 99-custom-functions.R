tidy_mirt <- function(model, ci = 0.95, select = c("a", "b")) {
  if (
    !is.numeric(ci) || length(ci) != 1 || !is.finite(ci) || ci <= 0 || ci >= 1
  ) {
    stop("`ci` must be a single number between 0 and 1.", call. = FALSE)
  }

  item_parameters <- model |>
    mirt::coef(IRTpars = TRUE, CI = ci) |>
    purrr::discard_at("GroupPars")

  difficulty_count <- item_parameters |>
    purrr::map_int(\(parameters) {
      sum(grepl("^b[0-9]*$", colnames(parameters)))
    })

  average_difficulty <- if (any(difficulty_count > 1)) {
    mirt::gen.difficulty(model, type = "mean")
  } else {
    NULL
  }

  item_parameters |>
    purrr::imap_dfr(\(parameters, item) {
      ci_rows <- rownames(parameters) |>
        purrr::keep(\(row) stringr::str_starts(row, "CI_"))

      if (length(ci_rows) >= 2) {
        ci_rows <- ci_rows[order(readr::parse_number(ci_rows))]
        conf_low <- parameters[ci_rows[1], ]
        conf_high <- parameters[ci_rows[length(ci_rows)], ]
      } else {
        conf_low <- conf_high <- rep(NA_real_, ncol(parameters))
      }

      item_table <- tibble::tibble(
        item,
        parameter = colnames(parameters),
        estimate = as.numeric(parameters["par", ]),
        conf.low = as.numeric(conf_low),
        conf.high = as.numeric(conf_high)
      )

      difficulty_parameters <- grepl(
        "^b[0-9]*$",
        item_table$parameter
      )

      if (sum(difficulty_parameters) > 1) {
        item_table <- dplyr::bind_rows(
          dplyr::filter(item_table, !difficulty_parameters),
          tibble::tibble(
            item,
            parameter = "b",
            estimate = unname(average_difficulty[[item]]),
            # gen.difficulty() returns point estimates, but no intervals.
            conf.low = NA_real_,
            conf.high = NA_real_
          )
        )
      }

      item_table
    }) |>
    dplyr::filter(parameter %in% select)
}


ggplot_mirt <- function(model, params, sort_by = NULL) {
  parameter_names <- c(
    a = "Discrimination (a)",
    b = "Difficulty (b)",
    g = "Lower bound (g)",
    u = "Upper bound (u)"
  )

  if (!is.null(sort_by)) {
    if (!is.character(sort_by) || length(sort_by) != 1) {
      stop("`sort_by` must be a single parameter name.", call. = FALSE)
    }
  }

  d <- tidy_mirt(model, select = union(params, sort_by))

  if (!is.null(sort_by)) {
    item_levels <- d |>
      dplyr::filter(parameter == sort_by) |>
      dplyr::arrange(estimate) |>
      dplyr::pull(item)

    if (length(item_levels) == 0) {
      stop("`sort_by` must name a parameter in the model.", call. = FALSE)
    }

    d <- d |>
      dplyr::mutate(item = factor(item, levels = item_levels))
  }

  d <- d |>
    dplyr::filter(parameter %in% params) |>
    dplyr::mutate(parameter = factor(parameter, levels = unique(params)))

  has_ci <- !is.na(d$conf.low) & !is.na(d$conf.high)
  parameter_count <- dplyr::n_distinct(d$parameter)

  p <- d |>
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = item)) +
    ggplot2::labs(
      x = if (parameter_count == 1) {
        parameter_names[[as.character(d$parameter[[1]])]]
      } else {
        "Estimate"
      }
    )

  if (any(has_ci)) {
    p <- p +
      ggplot2::geom_pointrange(
        ggplot2::aes(xmin = conf.low, xmax = conf.high),
        data = d |> dplyr::filter(has_ci)
      )
  }

  if (any(!has_ci)) {
    p <- p +
      ggplot2::geom_point(data = d |> dplyr::filter(!has_ci))
  }

  if (parameter_count > 1) {
    p <- p +
      ggplot2::facet_wrap(
        ggplot2::vars(parameter),
        scales = "free_x",
        labeller = ggplot2::as_labeller(parameter_names)
      )
  }

  p
}


ggplot_itemfit <- function(model, sort_by = NULL) {
  statistic_names <- c(
    infit = "Infit",
    outfit = "Outfit"
  )

  if (!is.null(sort_by)) {
    if (
      !is.character(sort_by) ||
        length(sort_by) != 1 ||
        !sort_by %in% names(statistic_names)
    ) {
      stop("`sort_by` must be either \"infit\" or \"outfit\".", call. = FALSE)
    }
  }

  d <- model |>
    mirt::itemfit("infit") |>
    tibble::as_tibble()

  if (!is.null(sort_by)) {
    item_levels <- d |>
      dplyr::arrange(.data[[sort_by]]) |>
      dplyr::pull(item)

    d <- d |>
      dplyr::mutate(item = factor(item, levels = item_levels))
  }

  d |>
    tidyr::pivot_longer(
      cols = c(infit, outfit),
      names_to = "statistic",
      values_to = "estimate"
    ) |>
    dplyr::mutate(
      statistic = factor(statistic, levels = names(statistic_names))
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = item)) +
    ggplot2::geom_vline(xintercept = 1, colour = "grey60") +
    ggplot2::geom_vline(
      xintercept = c(0.5, 1.5),
      linetype = "dashed",
      colour = "grey60"
    ) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(
      ggplot2::vars(statistic),
      labeller = ggplot2::as_labeller(statistic_names)
    ) +
    ggplot2::labs(x = "Fit statistic")
}


ggplot_q3 <- function(model, cluster = FALSE) {
  if (!is.logical(cluster) || length(cluster) != 1 || is.na(cluster)) {
    stop("`cluster` must be either TRUE or FALSE.", call. = FALSE)
  }

  q3 <- mirt::residuals(model, type = "Q3", verbose = FALSE)
  item_order <- rownames(q3)

  if (cluster) {
    item_order <- item_order[
      stats::hclust(stats::as.dist(1 - q3))$order
    ]
  }

  q3 |>
    as.data.frame() |>
    tibble::rownames_to_column("item_y") |>
    tidyr::pivot_longer(
      cols = -item_y,
      names_to = "item_x",
      values_to = "residual"
    ) |>
    dplyr::mutate(
      item_x = factor(item_x, levels = item_order),
      item_y = factor(item_y, levels = rev(item_order))
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = item_x, y = item_y, fill = residual)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = "#B2182B",
      mid = "white",
      high = "#2166AC",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL, fill = "Q3 residual") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


ggplot_reliability <- function(model, theta = seq(-4, 4, length.out = 401)) {
  theta_matrix <- matrix(theta, ncol = 1)
  information <- mirt::testinfo(model, theta_matrix)

  latent_variance <- model |>
    mirt::coef(simplify = TRUE) |>
    purrr::pluck("cov", 1, 1)

  reliability <- information / (information + 1 / latent_variance)
  overall_reliability <- mirt::marginal_rxx(model)

  tibble::tibble(
    theta,
    reliability
  ) |>
    ggplot2::ggplot(ggplot2::aes(x = theta, y = reliability)) +
    ggplot2::geom_hline(
      yintercept = overall_reliability,
      linetype = "dashed",
      colour = "grey50"
    ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(
      x = expression(theta),
      y = "Conditional reliability"
    )
}
