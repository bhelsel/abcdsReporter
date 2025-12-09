# Make more flexible for different scenarios
# Some times I'll want to return first (demographics) and others I'll want a count.
# Will I always want to merge on the variables - maybe, to make sure they are the same in LONI and ATRI?

loni_join <- function(atri_data, loni_data, variables) {
  commonvars <- setdiff(
    intersect(colnames(loni_data), colnames(atri_data)),
    c("subject_label", "age_at_visit")
  )

  for (var in commonvars) {
    if (var != "subject_label") {
      if (typeof(loni_data[[var]]) != typeof(atri_data[[var]])) {
        loni_data[[var]] <- as.character(loni_data[[var]])
        atri_data[[var]] <- as.character(atri_data[[var]])
      }
    }
  }

  atri_ids <- get_ids(atri_data)

  check_vars <- colnames(atri_data)[which(
    !colnames(atri_data) %in% c(atri_ids, "ids")
  )]

  for (varname in check_vars) {
    if (!exists(varname, loni_data)) {
      vartype <- typeof(atri_data[[varname]])
      if (vartype == "integer") {
        loni_data[[varname]] <- 0L
      } else {
        loni_data[[varname]] <- NA
      }
    }
  }

  # Add Participant and Event Identifiers
  loni_data <- abcds::add_study_ids(loni_data, inner_join, subject_label)
  loni_data <- abcds::add_event_ids(
    loni_data,
    inner_join,
    subject_label,
    event_sequence
  )

  # Rename the subject_label in ATRI to match LONI
  colnames(atri_data)[which(
    colnames(atri_data) == "subject_label"
  )] <- "u19_bds_id"

  joined_df <- atri_join(
    atri_data,
    loni_data,
    by = c(
      "u19_bds_id",
      "event_code",
      commonvars
    ),
    join_type = full_join
  )

  # Resolve discrepancies by using LONI data as the standard
  cols <- colnames(joined_df)[grepl("\\.x$|\\.y$", colnames(joined_df))]

  for (vars in unique(gsub(".x$|.y$", "", cols))) {
    x <- paste0(vars, ".x")
    y <- paste0(vars, ".y")
    joined_df[[x]] <- dplyr::case_when(
      !is.na(joined_df[[x]]) & is.na(joined_df[[y]]) ~ joined_df[[x]],
      is.na(joined_df[[x]]) & !is.na(joined_df[[y]]) ~ joined_df[[y]],
      joined_df[[x]] != joined_df[[y]] ~ joined_df[[y]],
      TRUE ~ joined_df[[x]]
    )
    joined_df[[y]] <- NULL
  }

  colnames(joined_df) <- gsub("\\.x$", "", colnames(joined_df))

  joined_df$event_code = factor(
    joined_df$event_code,
    levels = c("bl", "c2", "c3", "c4", "cyc1", "cyc2", "cyc3", "cyc4")
  )

  joined_df$ids <- ifelse(
    !is.na(joined_df$u19_bds_id),
    joined_df$u19_bds_id,
    joined_df$u01_niad_adds_id
  )

  dplyr::arrange(joined_df, ids, event_code)
}

#' @title Flexible Join for dplyr
#'
#' @description
#' Performs a flexible join on two data frames using any \code{dplyr} join function
#' specified by the user. Supports joins specified as a string, symbol, or function.
#'
#' @inheritParams dplyr::full_join
#' @param join_type The type of \code{dplyr} join to perform. Can be a string
#'   (e.g., "inner_join"), a symbol (e.g., \code{inner_join}), or a function
#'   (e.g., \code{dplyr::inner_join}).
#' @param ... Additional arguments passed to the chosen \code{dplyr} join function.
#'
#' @return
#' A \code{\link[tibble]{tibble}} containing the joined data.
#'
#' @details
#' This function allows flexible selection of join type while joining two data
#' frames. It evaluates the provided \code{join_type} and applies it with any
#' additional arguments passed via \code{...}. Useful for programmatically
#' performing joins in pipelines.
#'
#' @seealso
#'  \code{\link[rlang]{sym}}, \code{\link[rlang]{eval_tidy}},
#'  \code{\link[rlang]{is_symbol}}, \code{\link[rlang]{is_call}},
#'  \code{\link[rlang]{abort}}, \code{\link[dplyr]{inner_join}},
#'  \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{full_join}},
#'  \code{\link[dplyr]{right_join}}
#'
#' @rdname atri_join
#' @export
#' @importFrom rlang sym eval_tidy is_symbol is_call abort

atri_join <- function(x, y, by, join_type, ...) {
  join_fn <- tryCatch(
    {
      join_type <- rlang::ensym(join_type)
      rlang::eval_tidy(join_type, env = asNamespace("dplyr"))
    },
    error = function(e) {
      rlang::eval_tidy(join_type)
    }
  )

  join_fn(x, y, by = by, ...)
}
