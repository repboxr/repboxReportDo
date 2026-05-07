!MODIFICATION rrd_html_summary_df in rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_summary_df"
description = "Restructure the summary to distinguish missing data from other errors for regression and non-regression commands, and only show regcheck-with-error rows when nonzero."
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
rrd_html_summary_df = function(cmd_df, issue_df = NULL, parcels = list(), opts = rrd_opts()) {
  restore.point("rrd_html_summary_df")

  empty_items = c(
    "Included commands",
    "Regression commands",
    "Correct checked regressions",
    "Regressions with regcheck issues",
    "Regressions with missing data",
    "Regressions with errors, no missing data",
    "Regressions without regcheck row, no recorded error or missing data",
    "Non-regression commands with missing data",
    "Non-regression commands with errors, no missing data",
    "Issues shown in issue tab"
  )

  if (is.null(cmd_df) || NROW(cmd_df) == 0) {
    return(data.frame(
      item = empty_items,
      value = rep(0L, length(empty_items)),
      stringsAsFactors = FALSE
    ))
  }

  get_bool_col = function(names) {
    cols = intersect(names, names(cmd_df))
    if (length(cols) == 0) {
      return(rep(FALSE, NROW(cmd_df)))
    }

    Reduce(`|`, lapply(cols, function(col) rrd_as_logical(cmd_df[[col]])))
  }

  is_reg = rrd_as_logical(cmd_df$is_reg)
  missing_data = get_bool_col(c("missing_data", "run_missing_data"))

  has_error = vapply(seq_len(NROW(cmd_df)), function(i) {
    rrd_cmd_has_error(cmd_df[i, , drop = FALSE])
  }, logical(1))

  has_problem_reg = if ("rrd_has_problem_reg" %in% names(cmd_df)) {
    rrd_as_logical(cmd_df$rrd_has_problem_reg)
  } else {
    rep(FALSE, NROW(cmd_df))
  }

  cmd_runids = lapply(seq_len(NROW(cmd_df)), function(i) {
    rrd_cmd_runids(cmd_df[i, , drop = FALSE], parcels = parcels)
  })

  regcheck_runids = integer(0)
  if (!is.null(parcels$regcheck) && NROW(parcels$regcheck) > 0 && "runid" %in% names(parcels$regcheck)) {
    regcheck_runids = unique(suppressWarnings(as.integer(parcels$regcheck$runid)))
    regcheck_runids = regcheck_runids[!is.na(regcheck_runids)]
  }

  in_regcheck = vapply(cmd_runids, function(runids) {
    any(runids %in% regcheck_runids)
  }, logical(1))

  reg_with_missing_data = is_reg & missing_data
  reg_with_error_no_missing = is_reg & has_error & !missing_data
  reg_in_regcheck_with_error = is_reg & in_regcheck & has_error
  reg_without_regcheck_no_error = is_reg & !in_regcheck & !has_error & !missing_data

  correct_checked_reg = is_reg &
    in_regcheck &
    !has_problem_reg &
    !has_error &
    !missing_data

  nonreg_with_missing_data = !is_reg & missing_data
  nonreg_with_error_no_missing = !is_reg & has_error & !missing_data

  items = c(
    "Included commands",
    "Regression commands",
    "Correct checked regressions",
    "Regressions with regcheck issues",
    "Regressions with missing data",
    "Regressions with errors, no missing data",
    "Regressions without regcheck row, no recorded error or missing data"
  )

  values = c(
    NROW(cmd_df),
    sum(is_reg),
    sum(correct_checked_reg),
    sum(is_reg & has_problem_reg),
    sum(reg_with_missing_data),
    sum(reg_with_error_no_missing),
    sum(reg_without_regcheck_no_error)
  )

  if (sum(reg_in_regcheck_with_error) > 0) {
    items = c(items, "Regressions in regcheck with errors")
    values = c(values, sum(reg_in_regcheck_with_error))
  }

  items = c(
    items,
    "Non-regression commands with missing data",
    "Non-regression commands with errors, no missing data",
    "Issues shown in issue tab"
  )

  values = c(
    values,
    sum(nonreg_with_missing_data),
    sum(nonreg_with_error_no_missing),
    if (is.null(issue_df)) 0L else NROW(issue_df)
  )

  data.frame(
    item = items,
    value = values,
    stringsAsFactors = FALSE
  )
}
```

!END_MODIFICATION rrd_html_summary_df in rrd_html.R
