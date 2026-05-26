Ah, the issue is that if the `cmd_df` dataframe doesn't contain the `missing_data` or `run_missing_data` columns, `rrd_as_logical(cmd_df$missing_data)` returns `logical(0)`, causing the `has_cmd_md` vector to be empty. This triggers an `if (NA)` evaluation when checking the runs in the loop.

Here is the fix for `rrd_html_summary_df` to safely handle missing columns:

!MODIFICATION rrd_html_summary_df rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_summary_df"
description = "Safely evaluate has_cmd_md to prevent NA/logical(0) errors if missing_data columns don't exist."
---
```r
rrd_html_summary_df = function(cmd_df, issue_df = NULL, parcels = list(), opts = rrd_opts()) {
  restore.point("rrd_html_summary_df")

  empty_items = c(
    "Included command runs",
    "Regression runs",
    "Correct checked regression runs",
    "Regression runs with regcheck issues",
    "Regression runs with missing data",
    "Regression runs with errors, no missing data",
    "Regression runs without regcheck row, no recorded error or missing data",
    "Non-regression command runs with missing data",
    "Non-regression command runs with errors, no missing data",
    "Issues shown in issue tab"
  )

  if (is.null(cmd_df) || NROW(cmd_df) == 0) {
    return(data.frame(
      item = empty_items,
      value = rep(0L, length(empty_items)),
      stringsAsFactors = FALSE
    ))
  }

  cmd_runids = lapply(seq_len(NROW(cmd_df)), function(i) {
    rrd_cmd_runids(cmd_df[i, , drop = FALSE], parcels = parcels)
  })

  n_runs = lengths(cmd_runids)
  n_runs[n_runs == 0] = 1L # Fallback so command is at least counted once if it didn't run

  is_reg = rrd_as_logical(cmd_df$is_reg)

  # Global sets to intersect
  regcheck_runids = integer(0)
  prob_runids = integer(0)
  if (!is.null(parcels$regcheck) && NROW(parcels$regcheck) > 0 && "runid" %in% names(parcels$regcheck)) {
    df = as.data.frame(parcels$regcheck)
    regcheck_runids = unique(suppressWarnings(as.integer(df$runid)))
    regcheck_runids = regcheck_runids[!is.na(regcheck_runids)]

    has_problem = rep(FALSE, NROW(df))
    if ("reg_ok" %in% names(df)) has_problem = has_problem | !rrd_as_logical(df$reg_ok)
    if ("problem" %in% names(df)) has_problem = has_problem | (!is.na(df$problem) & nzchar(as.character(df$problem)))
    if ("comment" %in% names(df)) has_problem = has_problem | (!is.na(df$comment) & nzchar(as.character(df$comment)))
    prob_runids = unique(suppressWarnings(as.integer(df$runid[has_problem])))
    prob_runids = prob_runids[!is.na(prob_runids)]
  }

  err_runids = integer(0)
  md_runids = integer(0)
  if (!is.null(parcels$stata_run_cmd) && NROW(parcels$stata_run_cmd) > 0) {
    df = as.data.frame(parcels$stata_run_cmd)
    err_mask = rep(FALSE, NROW(df))
    for (field in intersect(c("errcode", "rc", "error_code", "stata_rc"), names(df))) {
      val = suppressWarnings(as.numeric(df[[field]]))
      err_mask = err_mask | (!is.na(val) & val != 0)
    }
    for (field in intersect(c("err_msg", "error_msg", "error", "stderr"), names(df))) {
      val = as.character(df[[field]])
      err_mask = err_mask | (!is.na(val) & nzchar(val))
    }
    err_runids = unique(suppressWarnings(as.integer(df$runid[err_mask])))
    err_runids = err_runids[!is.na(err_runids)]

    if ("missing_data" %in% names(df)) {
      md_mask = rrd_as_logical(df$missing_data)
      md_runids = unique(suppressWarnings(as.integer(df$runid[md_mask])))
      md_runids = md_runids[!is.na(md_runids)]
    }
  }

  # Calculate exact run counts per command
  runs_in_regcheck = vapply(cmd_runids, function(ids) sum(ids %in% regcheck_runids), integer(1))
  runs_prob = vapply(cmd_runids, function(ids) sum(ids %in% prob_runids), integer(1))
  runs_err = vapply(cmd_runids, function(ids) sum(ids %in% err_runids), integer(1))
  runs_md = vapply(cmd_runids, function(ids) sum(ids %in% md_runids), integer(1))

  # Use booleans if a command didn't output a runid but has an error flag (e.g. compile error)
  has_cmd_err = vapply(seq_len(NROW(cmd_df)), function(i) rrd_cmd_has_error(cmd_df[i,,drop=FALSE]), logical(1))
  
  has_cmd_md = rep(FALSE, NROW(cmd_df))
  if ("missing_data" %in% names(cmd_df)) has_cmd_md = has_cmd_md | rrd_as_logical(cmd_df$missing_data)
  if ("run_missing_data" %in% names(cmd_df)) has_cmd_md = has_cmd_md | rrd_as_logical(cmd_df$run_missing_data)

  for (i in seq_along(cmd_runids)) {
    if (length(cmd_runids[[i]]) == 0) {
       runs_err[i] = if (has_cmd_err[i]) 1L else 0L
       runs_md[i] = if (has_cmd_md[i]) 1L else 0L
    }
  }

  reg_with_prob = sum(is_reg * runs_prob)
  
  runs_correct = vapply(cmd_runids, function(ids) {
      sum((ids %in% regcheck_runids) & !(ids %in% prob_runids) & !(ids %in% err_runids) & !(ids %in% md_runids))
  }, integer(1))
  correct_checked_reg = sum(is_reg * runs_correct)

  runs_err_no_md = vapply(seq_along(cmd_runids), function(i) {
      ids = cmd_runids[[i]]
      if (length(ids) == 0) return(runs_err[i] * (1L - runs_md[i]))
      sum((ids %in% err_runids) & !(ids %in% md_runids))
  }, integer(1))

  runs_reg_no_regcheck_no_err_no_md = vapply(seq_along(cmd_runids), function(i) {
      ids = cmd_runids[[i]]
      if (length(ids) == 0) {
         if (!has_cmd_err[i] && !has_cmd_md[i]) return(1L) else return(0L)
      }
      sum(!(ids %in% regcheck_runids) & !(ids %in% err_runids) & !(ids %in% md_runids))
  }, integer(1))

  reg_in_regcheck_with_error = vapply(seq_along(cmd_runids), function(i) {
      ids = cmd_runids[[i]]
      sum((ids %in% regcheck_runids) & (ids %in% err_runids))
  }, integer(1))

  items = c(
    "Included command runs",
    "Regression runs",
    "Correct checked regression runs",
    "Regression runs with regcheck issues",
    "Regression runs with missing data",
    "Regression runs with errors, no missing data",
    "Regression runs without regcheck row, no recorded error or missing data"
  )

  values = c(
    sum(n_runs),
    sum(n_runs[is_reg]),
    correct_checked_reg,
    reg_with_prob,
    sum(runs_md[is_reg]),
    sum(runs_err_no_md[is_reg]),
    sum(runs_reg_no_regcheck_no_err_no_md[is_reg])
  )

  if (sum(reg_in_regcheck_with_error[is_reg]) > 0) {
    items = c(items, "Regression runs in regcheck with errors")
    values = c(values, sum(reg_in_regcheck_with_error[is_reg]))
  }

  items = c(
    items,
    "Non-regression command runs with missing data",
    "Non-regression command runs with errors, no missing data",
    "Issues shown in issue tab"
  )

  values = c(
    values,
    sum(runs_md[!is_reg]),
    sum(runs_err_no_md[!is_reg]),
    if (is.null(issue_df)) 0L else NROW(issue_df)
  )

  if (!is.null(issue_df) && NROW(issue_df) > 0 && "issue_type" %in% names(issue_df) && "issue_title" %in% names(issue_df)) {
    reg_titles = issue_df$issue_title[issue_df$issue_type == "regcheck"]
    reg_titles = as.character(reg_titles)
    reg_titles = reg_titles[!is.na(reg_titles) & nzchar(reg_titles)]

    if (length(reg_titles) > 0) {
      base_titles = stringi::stri_replace_all_regex(reg_titles, "\\s+\\(\\d+\\s+runs\\)$", "")

      run_counts = vapply(issue_df$runids[issue_df$issue_type == "regcheck"], function(run_str) {
        if (stringi::stri_detect_regex(run_str, "\\(\\d+\\)$")) {
           as.integer(stringi::stri_extract_last_regex(run_str, "\\d+"))
        } else if (nzchar(run_str)) {
           length(stringi::stri_split_fixed(run_str, ",", omit_empty = TRUE)[[1]])
        } else {
           0L
        }
      }, integer(1))

      agg_counts = tapply(run_counts, base_titles, sum)
      agg_counts = sort(agg_counts, decreasing = TRUE)
      
      items = c(items, paste0("Regcheck issue (runs): ", names(agg_counts)))
      values = c(values, as.integer(agg_counts))
    }
  }

  data.frame(
    item = items,
    value = values,
    stringsAsFactors = FALSE
  )
}
```
!END_MODIFICATION rrd_html_summary_df rrd_html.R
