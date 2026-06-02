I will apply the updates to fix the "sb coefs missing" logic in the issue titles/summaries and massively optimize the code to make report generation significantly faster (avoiding unvectorized text formatting and repetitive subsetting).

Here are the requested modifications.

!MODIFICATION rrd_html_regcheck_title /home/rstudio/repbox/repboxReportDo/R/rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_regcheck_title"
description = "Update title logic to correctly detect missing sb coefs using NA or 0 values."
---
```r
rrd_html_regcheck_title = function(flags) {
  restore.point("rrd_html_regcheck_title")

  missing = c(
    if (!isTRUE(flags$so)) "so" else character(0),
    if (!isTRUE(flags$sb)) "sb" else character(0),
    if (!isTRUE(flags$rb)) "rb" else character(0)
  )

  if (length(missing) > 0) {
    if (isTRUE(flags$sb) && (is.na(flags$sb_num_coef) || identical(flags$sb_num_coef, 0L)) && !isTRUE(flags$rb)) {
      title = paste0(if (!isTRUE(flags$so)) "so missing, " else "", "sb coefs missing, no rb")
      return(title)
    }
    
    title = paste0(rrd_html_join_words(missing), " missing")
    if (!isTRUE(flags$sb)) {
        if (isTRUE(flags$sb_raw)) {
           title = paste0(title, " (sb_raw exists)")
        } else {
           title = paste0(title, " (sb and sb_raw missing)")
        }
    }
    return(title)
  }

  coef_bad = identical(flags$rb_sb_coef_same, FALSE)
  se_bad = identical(flags$rb_sb_se_same, FALSE)

  if (coef_bad && se_bad) {
    title = "sb vs rb coef/se mismatch"

    share_same = suppressWarnings(as.numeric(flags$rb_sb_share_coeff_same))
    sb_num_coef = suppressWarnings(as.integer(flags$sb_num_coef))

    details = character(0)

    if (!is.na(share_same)) {
      share_not_same = 100 * (1 - share_same)
      digits = if (abs(share_not_same) < 10) 1L else 0L
      share_not_same_txt = formatC(share_not_same, digits = digits, format = "f")
      details = c(details, paste0(share_not_same_txt, "% coeffs differ"))
    }

    if (!is.na(sb_num_coef)) {
      details = c(details, paste0(sb_num_coef, " sb coefs"))
    }

    if (length(details) > 0) {
      title = paste0(title, " (", paste0(details, collapse = ", "), ")")
    }

    return(title)
  }
  if (coef_bad) return("sb vs rb coef mismatch")
  if (se_bad) return("sb vs rb SE mismatch")

  if (identical(flags$sb_so_same, FALSE)) {
    return("sb vs so mismatch")
  }

  if (nzchar(flags$problem)) {
    return(flags$problem)
  }

  "Regression check issue"
}
```
!END_MODIFICATION rrd_html_regcheck_title /home/rstudio/repbox/repboxReportDo/R/rrd_html.R

!MODIFICATION rrd_html_regcheck_summary /home/rstudio/repbox/repboxReportDo/R/rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_regcheck_summary"
description = "Update summary logic to correspond with title update for missing sb coefs."
---
```r
rrd_html_regcheck_summary = function(flags, row) {
  restore.point("rrd_html_regcheck_summary")

  if (!isTRUE(flags$so) || !isTRUE(flags$sb) || !isTRUE(flags$rb)) {
    if (isTRUE(flags$sb) && (is.na(flags$sb_num_coef) || identical(flags$sb_num_coef, 0L)) && !isTRUE(flags$rb)) {
      return(paste0(if (!isTRUE(flags$so)) "Original so missing. " else "", "Stata base (sb) ran but yielded no coefficients, preventing R base (rb) execution."))
    }
    
    missing = c(
      if (!isTRUE(flags$so)) "so" else character(0),
      if (!isTRUE(flags$sb)) "sb" else character(0),
      if (!isTRUE(flags$rb)) "rb" else character(0)
    )

    raw_note = c(
      if (isTRUE(flags$so_raw) && !isTRUE(flags$so)) "so raw output exists but parcel is missing" else character(0),
      if (isTRUE(flags$sb_raw) && !isTRUE(flags$sb)) "sb raw output exists but parcel is missing" else character(0)
    )

    if (length(raw_note) > 0) {
      return(paste0(rrd_html_join_words(missing), " did not produce complete repdb results; ", paste0(raw_note, collapse = "; "), "."))
    }

    return(paste0("Expected ", rrd_html_join_words(missing), " results are missing."))
  }

  coef_bad = identical(flags$rb_sb_coef_same, FALSE)
  se_bad = identical(flags$rb_sb_se_same, FALSE)

  if (coef_bad && se_bad) {
    return("R and Stata base differ in coefficients and standard errors.")
  }
  if (coef_bad) {
    return("R and Stata base differ in coefficients.")
  }
  if (se_bad) {
    return("R and Stata base differ in standard errors.")
  }

  if (identical(flags$sb_so_same, FALSE)) {
    return("Stata base differs from the original Stata reproduction.")
  }

  if (nzchar(flags$problem)) {
    return(flags$problem)
  }

  "Regcheck reported a problem for this regression."
}
```
!END_MODIFICATION rrd_html_regcheck_summary /home/rstudio/repbox/repboxReportDo/R/rrd_html.R

!MODIFICATION rrd_cmd_all_runids /home/rstudio/repbox/repboxReportDo/R/rrd_plain.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_plain.R"
insert_bottom = true
description = "Add vectorized function for extracting all runids for all commands."
---
```r

rrd_cmd_all_runids = function(cmd_df) {
  restore.point("rrd_cmd_all_runids")
  if (NROW(cmd_df) == 0) return(list())
  
  res = vector("list", NROW(cmd_df))
  for (i in seq_along(res)) res[[i]] = integer(0)
  
  if ("all_runids" %in% names(cmd_df) && is.list(cmd_df$all_runids)) {
     for (i in seq_along(res)) {
        res[[i]] = suppressWarnings(as.integer(cmd_df$all_runids[[i]]))
     }
  }
  
  if ("runid" %in% names(cmd_df)) {
     r = suppressWarnings(as.integer(cmd_df$runid))
     for (i in seq_along(res)) if (!is.na(r[i])) res[[i]] = c(res[[i]], r[i])
  }
  
  if ("run_runid" %in% names(cmd_df)) {
     r = suppressWarnings(as.integer(cmd_df$run_runid))
     for (i in seq_along(res)) if (!is.na(r[i])) res[[i]] = c(res[[i]], r[i])
  }
  
  lapply(res, function(x) {
     x = unique(x)
     x[!is.na(x)]
  })
}
```
!END_MODIFICATION rrd_cmd_all_runids /home/rstudio/repbox/repboxReportDo/R/rrd_plain.R

!MODIFICATION rrd_cmd_has_error /home/rstudio/repbox/repboxReportDo/R/rrd_plain.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_plain.R"
function_name = "rrd_cmd_has_error"
description = "Vectorize rrd_cmd_has_error to work efficiently over dataframes."
---
```r
rrd_cmd_has_error = function(cmd) {
  restore.point("rrd_cmd_has_error")
  
  if (NROW(cmd) == 0) return(logical(0))
  has_err = rep(FALSE, NROW(cmd))

  err_fields = intersect(c("errcode", "rc", "error_code", "stata_rc", "run_errcode", "run_rc"), names(cmd))
  for (field in err_fields) {
    any_field = paste0("any_", field)
    if (any_field %in% names(cmd)) {
       has_err = has_err | rrd_as_logical(cmd[[any_field]])
    } else {
       val = suppressWarnings(as.numeric(cmd[[field]]))
       val[is.na(val)] = 0
       has_err = has_err | (val != 0)
    }
  }

  msg_fields = intersect(c("err_msg", "error_msg", "error", "stderr", "run_err_msg", "run_error_msg"), names(cmd))
  for (field in msg_fields) {
    val = as.character(cmd[[field]])
    val[is.na(val)] = ""
    has_err = has_err | nzchar(val)
  }

  has_err
}
```
!END_MODIFICATION rrd_cmd_has_error /home/rstudio/repbox/repboxReportDo/R/rrd_plain.R

!MODIFICATION rrd_cmd_has_run_output /home/rstudio/repbox/repboxReportDo/R/rrd_plain.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_plain.R"
function_name = "rrd_cmd_has_run_output"
description = "Optimize rrd_cmd_has_run_output without intensive string formatting."
---
```r
rrd_cmd_has_run_output = function(cmd_df, parcels = list(), opts = rrd_opts()) {
  restore.point("rrd_cmd_has_run_output")

  if (is.null(cmd_df) || NROW(cmd_df) == 0) return(logical(0))

  has_out = rep(FALSE, NROW(cmd_df))

  text_cols = intersect(
    c("log", "logtxt", "log_text", "cmd_log", "output", "out", "stdout", "stderr", 
      "text", "result", "msg", "message", "err_msg", "error_msg", 
      "run_log", "run_logtxt", "run_log_text", "run_cmd_log", "run_output", 
      "run_out", "run_stdout", "run_stderr", "run_text", "run_result", 
      "run_msg", "run_message", "run_err_msg", "run_error_msg"),
    names(cmd_df)
  )
  
  for (col in text_cols) {
    val = as.character(cmd_df[[col]])
    val[is.na(val)] = ""
    has_out = has_out | nzchar(val)
  }

  run_log = parcels$stata_run_log
  if (!is.null(run_log) && NROW(run_log) > 0 && "runid" %in% names(run_log)) {
     run_log_df = as.data.frame(run_log)
     
     log_text_cols = intersect(
       c("log", "logtxt", "log_text", "cmd_log", "output", "out", "stdout", "stderr", 
         "text", "result", "msg", "message", "err_msg", "error_msg"),
       names(run_log_df)
     )
     
     log_has_text = rep(FALSE, NROW(run_log_df))
     for (col in log_text_cols) {
        val = as.character(run_log_df[[col]])
        val[is.na(val)] = ""
        log_has_text = log_has_text | nzchar(val)
     }
     
     file_cols = intersect(
       c("log_file", "file", "path", "log_path", "smcl_file", "txt_file"),
       names(run_log_df)
     )
     for (col in file_cols) {
        val = as.character(run_log_df[[col]])
        val[is.na(val)] = ""
        log_has_text = log_has_text | nzchar(val)
     }
     
     valid_runids = suppressWarnings(as.integer(run_log_df$runid[log_has_text]))
     valid_runids = valid_runids[!is.na(valid_runids)]
     
     if (length(valid_runids) > 0) {
        all_runids = rrd_cmd_all_runids(cmd_df)
        has_log_out = vapply(all_runids, function(ids) any(ids %in% valid_runids), logical(1))
        has_out = has_out | has_log_out
     }
  }

  has_out
}
```
!END_MODIFICATION rrd_cmd_has_run_output /home/rstudio/repbox/repboxReportDo/R/rrd_plain.R

!MODIFICATION rrd_has_problem_reg /home/rstudio/repbox/repboxReportDo/R/rrd_plain.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_plain.R"
function_name = "rrd_has_problem_reg"
description = "Use vectorized rrd_cmd_all_runids."
---
```r
rrd_has_problem_reg = function(cmd_df, parcels = list()) {
  restore.point("rrd_has_problem_reg")

  if (is.null(cmd_df) || NROW(cmd_df) == 0) return(logical(0))

  regcheck = parcels$regcheck
  if (is.null(regcheck) || NROW(regcheck) == 0 || !"runid" %in% names(regcheck)) {
    return(rep(FALSE, NROW(cmd_df)))
  }

  df = as.data.frame(regcheck)

  problem = rep(FALSE, NROW(df))
  if ("reg_ok" %in% names(df)) {
    problem = problem | !rrd_as_logical(df$reg_ok)
  }
  if ("problem" %in% names(df)) {
    problem = problem | (!is.na(df$problem) & nzchar(as.character(df$problem)))
  }
  if ("comment" %in% names(df)) {
    problem = problem | (!is.na(df$comment) & nzchar(as.character(df$comment)))
  }

  prob_runids = unique(suppressWarnings(as.integer(df$runid[problem])))
  all_runids = rrd_cmd_all_runids(cmd_df)

  vapply(all_runids, function(runids) {
    any(runids %in% prob_runids)
  }, logical(1))
}
```
!END_MODIFICATION rrd_has_problem_reg /home/rstudio/repbox/repboxReportDo/R/rrd_plain.R

!MODIFICATION rrd_cmd_reg_status /home/rstudio/repbox/repboxReportDo/R/rrd_plain.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_plain.R"
function_name = "rrd_cmd_reg_status"
description = "Optimize rrd_cmd_reg_status with vectorized helpers."
---
```r
rrd_cmd_reg_status = function(cmd_df, parcels = list()) {
  restore.point("rrd_cmd_reg_status")
  
  if (is.null(cmd_df) || NROW(cmd_df) == 0) return(character(0))
  
  status = rep("ok", NROW(cmd_df))
  is_reg = rrd_as_logical(cmd_df$is_reg)
  status[!is_reg] = NA_character_
  
  has_error = rrd_cmd_has_error(cmd_df)
  
  has_md = rep(FALSE, NROW(cmd_df))
  if ("missing_data" %in% names(cmd_df)) has_md = has_md | rrd_as_logical(cmd_df$missing_data)
  if ("run_missing_data" %in% names(cmd_df)) has_md = has_md | rrd_as_logical(cmd_df$run_missing_data)
  
  status[has_error | has_md] = "not_run"
  
  prob_runids = integer(0)
  not_run_runids = integer(0)
  ok_runids = integer(0)

  regcheck = parcels$regcheck
  if (!is.null(regcheck) && NROW(regcheck) > 0 && "runid" %in% names(regcheck)) {
    df = as.data.frame(regcheck)
    
    problem = rep(FALSE, NROW(df))
    if ("reg_ok" %in% names(df)) problem = problem | !rrd_as_logical(df$reg_ok)
    if ("problem" %in% names(df)) problem = problem | (!is.na(df$problem) & nzchar(as.character(df$problem)))
    if ("comment" %in% names(df)) problem = problem | (!is.na(df$comment) & nzchar(as.character(df$comment)))
    
    so_missing = rep(FALSE, NROW(df))
    if ("so_did_run" %in% names(df)) {
      so_missing = !rrd_as_logical(df$so_did_run)
    }
    
    prob_runids = unique(suppressWarnings(as.integer(df$runid[problem])))
    not_run_runids = unique(suppressWarnings(as.integer(df$runid[so_missing])))
    ok_runids = unique(suppressWarnings(as.integer(df$runid[!problem & !so_missing])))
  }
  
  all_runids = rrd_cmd_all_runids(cmd_df)
  
  for (i in seq_len(NROW(cmd_df))) {
    if (is.na(status[i])) next
    
    runids = all_runids[[i]]
    
    if (length(runids) == 0) {
      status[i] = "not_run"
    } else {
      if (any(runids %in% not_run_runids)) {
         status[i] = "not_run"
      } else if (any(runids %in% prob_runids)) {
         status[i] = "issue"
      } else if (any(runids %in% ok_runids)) {
         status[i] = "ok"
      } else {
         if (status[i] == "ok") status[i] = "not_run"
      }
    }
  }
  
  status
}
```
!END_MODIFICATION rrd_cmd_reg_status /home/rstudio/repbox/repboxReportDo/R/rrd_plain.R

!MODIFICATION rrd_html_summary_df /home/rstudio/repbox/repboxReportDo/R/rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_summary_df"
description = "Use vectorized rrd_cmd_has_error and rrd_cmd_all_runids for speed."
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

  cmd_runids = rrd_cmd_all_runids(cmd_df)

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
  has_cmd_err = rrd_cmd_has_error(cmd_df)

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
!END_MODIFICATION rrd_html_summary_df /home/rstudio/repbox/repboxReportDo/R/rrd_html.R

!MODIFICATION rrd_html_issue_df /home/rstudio/repbox/repboxReportDo/R/rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_issue_df"
description = "Use precomputed rrd_cmd_has_error for speed."
---
```r
rrd_html_issue_df = function(cmd_df, parcels = list(), opts = rrd_opts()) {
  restore.point("rrd_html_issue_df")

  empty = data.frame(
    issue_id = character(0),
    issue_type = character(0),
    issue_title = character(0),
    issue_summary = character(0),
    issue_badge = character(0),
    severity = integer(0),
    file_idx = integer(0),
    file_path = character(0),
    line = integer(0),
    cmd_id = character(0),
    runids = character(0),
    cmdline = character(0),
    details_html = character(0),
    stringsAsFactors = FALSE
  )

  if (is.null(cmd_df) || NROW(cmd_df) == 0) {
    return(empty)
  }

  out = vector("list", NROW(cmd_df) * 2L)
  pos = 0L

  cmd_has_err = rrd_cmd_has_error(cmd_df)

  for (i in seq_len(NROW(cmd_df))) {
    cmd = cmd_df[i, , drop = FALSE]
    line = suppressWarnings(as.integer(cmd$rrd_attach_line[1]))
    if (is.na(line)) line = NA_integer_

    is_reg = isTRUE(cmd$is_reg[1])
    show_error_issue = is_reg || isTRUE(opts$show_nonreg_issue_errors)
    cmdline = if ("cmdline" %in% names(cmd)) rrd_chr_vec(cmd$cmdline[1]) else ""

    if (show_error_issue && cmd_has_err[i]) {
      err_txt = rrd_cmd_error_text(cmd)
      err_runids = rrd_cmd_error_runids(cmd, parcels = parcels)
      if (length(err_runids) == 0) err_runids = rrd_cmd_runids(cmd, parcels = parcels) # fallback

      pos = pos + 1L
      out[[pos]] = data.frame(
        issue_id = paste0("issue-", pos),
        issue_type = "error",
        issue_title = rrd_html_error_title(cmd),
        issue_summary = rrd_html_error_summary(cmd),
        issue_badge = "error",
        severity = 100L,
        file_idx = as.integer(cmd$rrd_file_idx[1]),
        file_path = as.character(cmd$file_path[1]),
        line = line,
        cmd_id = as.character(cmd$rrd_cmd_id[1]),
        runids = rrd_format_runids_for_issue(err_runids),
        cmdline = cmdline,
        details_html = rrd_html_pre_details(err_txt),
        stringsAsFactors = FALSE
      )
    }

    if ("rrd_has_problem_reg" %in% names(cmd) && isTRUE(cmd$rrd_has_problem_reg[1])) {
      all_runids = rrd_cmd_runids(cmd, parcels = parcels)
      reg_issue = rrd_html_regcheck_issue(all_runids, parcels = parcels, opts = opts)

      pos = pos + 1L
      out[[pos]] = data.frame(
        issue_id = paste0("issue-", pos),
        issue_type = "regcheck",
        issue_title = reg_issue$title,
        issue_summary = reg_issue$summary,
        issue_badge = reg_issue$badge,
        severity = reg_issue$severity,
        file_idx = as.integer(cmd$rrd_file_idx[1]),
        file_path = as.character(cmd$file_path[1]),
        line = line,
        cmd_id = as.character(cmd$rrd_cmd_id[1]),
        runids = rrd_format_runids_for_issue(reg_issue$prob_runids),
        cmdline = cmdline,
        details_html = reg_issue$details_html,
        stringsAsFactors = FALSE
      )
    }
  }

  out = out[seq_len(pos)]
  if (length(out) == 0) {
    return(empty)
  }

  res = do.call(rbind, out)
  res = res[order(-res$severity, res$file_idx, res$line), , drop = FALSE]
  rownames(res) = NULL
  res$issue_id = paste0("issue-", seq_len(NROW(res)))

  res
}
```
!END_MODIFICATION rrd_html_issue_df /home/rstudio/repbox/repboxReportDo/R/rrd_html.R

!MODIFICATION rrd_html_line_flags /home/rstudio/repbox/repboxReportDo/R/rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_line_flags"
description = "Use precomputed rrd_cmd_has_error for speed."
---
```r
rrd_html_line_flags = function(cmd_df, n) {
  restore.point("rrd_html_line_flags")

  empty_flag = list(
    has_cmd = FALSE,
    is_reg = FALSE,
    has_error = FALSE,
    has_problem_reg = FALSE,
    reg_status = NA_character_,
    has_cache = FALSE
  )
  res = rep(list(empty_flag), n)
  names(res) = as.character(seq_len(n))

  if (is.null(cmd_df) || NROW(cmd_df) == 0) {
    return(res)
  }

  line = suppressWarnings(as.integer(cmd_df$rrd_attach_line))
  ok = !is.na(line) & line >= 1L & line <= n

  if (!any(ok)) {
    return(res)
  }
  
  cmd_has_err = rrd_cmd_has_error(cmd_df)

  for (line_num in sort(unique(line[ok]))) {
    rows = which(ok & line == line_num)
    key = as.character(line_num)
    
    statuses = cmd_df$rrd_reg_status[rows]
    statuses = statuses[!is.na(statuses)]
    status = if (length(statuses) > 0) {
      if ("not_run" %in% statuses) "not_run"
      else if ("issue" %in% statuses) "issue"
      else "ok"
    } else NA_character_
    
    has_cache = FALSE
    if ("rrd_has_cache" %in% names(cmd_df)) {
       has_cache = any(rrd_as_logical(cmd_df$rrd_has_cache[rows]), na.rm = TRUE)
    }

    res[[key]] = list(
      has_cmd = TRUE,
      is_reg = any(rrd_as_logical(cmd_df$is_reg[rows]), na.rm = TRUE),
      has_error = any(cmd_has_err[rows], na.rm = TRUE),
      has_problem_reg = any(rrd_as_logical(cmd_df$rrd_has_problem_reg[rows]), na.rm = TRUE),
      reg_status = status,
      has_cache = has_cache
    )
  }

  res
}
```
!END_MODIFICATION rrd_html_line_flags /home/rstudio/repbox/repboxReportDo/R/rrd_html.R

!MODIFICATION rrd_html_missing_dataset_df /home/rstudio/repbox/repboxReportDo/R/rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_missing_dataset_df"
description = "Use vectorized rrd_cmd_has_error."
---
```r
rrd_html_missing_dataset_df = function(cmd_df, parcels = list(), opts = rrd_opts()) {
  restore.point("rrd_html_missing_dataset_df")

  empty = data.frame(
    file = character(0),
    file_path = character(0),
    line = integer(0),
    runids = character(0),
    stringsAsFactors = FALSE
  )

  if (is.null(cmd_df) || NROW(cmd_df) == 0) return(empty)

  has_error = rrd_cmd_has_error(cmd_df)

  if (!any(has_error)) return(empty)

  rows = which(has_error)
  parts = lapply(rows, function(i) {
    cmd = cmd_df[i, , drop = FALSE]
    txt = paste0(
      rrd_chr_vec(cmd$cmdline[1]),
      "\n",
      rrd_cmd_error_text(cmd),
      "\n",
      rrd_cmd_log_text(cmd, parcels = parcels, opts = opts)
    )

    if (!rrd_html_has_missing_file_hint(txt)) return(NULL)

    files = rrd_html_extract_dataset_names(txt)
    if (length(files) == 0) return(NULL)

    line = suppressWarnings(as.integer(cmd$rrd_attach_line[1]))
    if (is.na(line)) line = NA_integer_

    runids = rrd_cmd_runids(cmd, parcels = parcels)
    runids_txt = if (length(runids) == 0) "" else paste0(runids, collapse = ", ")

    data.frame(
      file = files,
      file_path = as.character(cmd$file_path[1]),
      line = line,
      runids = runids_txt,
      stringsAsFactors = FALSE
    )
  })

  parts = parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) return(empty)

  res = do.call(rbind, parts)
  res = res[!duplicated(res[, c("file", "file_path", "line"), drop = FALSE]), , drop = FALSE]
  rownames(res) = NULL

  res
}
```
!END_MODIFICATION rrd_html_missing_dataset_df /home/rstudio/repbox/repboxReportDo/R/rrd_html.R

!MODIFICATION rrd_html_render_do_file /home/rstudio/repbox/repboxReportDo/R/rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_render_do_file"
description = "Use vectorized rrd_cmd_has_error."
---
```r
rrd_html_render_do_file = function(do_row, cmd_df, parcels, opts, file_idx = 1L) {
  restore.point("rrd_html_render_do_file")

  file_path = do_row$file_path[1]
  txt = do_row$text[1]
  lines = stringi::stri_split_lines1(txt)

  if (length(lines) == 0) {
    lines = ""
  }

  output_by_line = rrd_outputs_by_line(cmd_df, parcels = parcels, opts = opts)

  attach_line = integer(0)
  reg_ok_lines = integer(0)
  reg_issue_lines = integer(0)
  reg_not_run_lines = integer(0)
  problem_lines = integer(0)
  error_lines = integer(0)
  cache_lines = integer(0)

  if (!is.null(cmd_df) && NROW(cmd_df) > 0) {
    attach_line = suppressWarnings(as.integer(cmd_df$rrd_attach_line))
    attach_line = attach_line[!is.na(attach_line) & attach_line > 0]

    if ("rrd_reg_status" %in% names(cmd_df)) {
       reg_ok_lines = suppressWarnings(as.integer(cmd_df$rrd_attach_line[cmd_df$rrd_reg_status == "ok"]))
       reg_ok_lines = reg_ok_lines[!is.na(reg_ok_lines) & reg_ok_lines > 0]
       
       reg_issue_lines = suppressWarnings(as.integer(cmd_df$rrd_attach_line[cmd_df$rrd_reg_status == "issue"]))
       reg_issue_lines = reg_issue_lines[!is.na(reg_issue_lines) & reg_issue_lines > 0]
       
       reg_not_run_lines = suppressWarnings(as.integer(cmd_df$rrd_attach_line[cmd_df$rrd_reg_status == "not_run"]))
       reg_not_run_lines = reg_not_run_lines[!is.na(reg_not_run_lines) & reg_not_run_lines > 0]
    } else if ("is_reg" %in% names(cmd_df)) {
       reg_ok_lines = suppressWarnings(as.integer(cmd_df$rrd_attach_line[rrd_as_logical(cmd_df$is_reg)]))
       reg_ok_lines = reg_ok_lines[!is.na(reg_ok_lines) & reg_ok_lines > 0]
    }

    if ("rrd_has_problem_reg" %in% names(cmd_df)) {
      problem_lines = suppressWarnings(as.integer(cmd_df$rrd_attach_line[rrd_as_logical(cmd_df$rrd_has_problem_reg)]))
      problem_lines = problem_lines[!is.na(problem_lines) & problem_lines > 0]
    }

    error_rows = rrd_cmd_has_error(cmd_df)

    if (any(error_rows)) {
      error_lines = suppressWarnings(as.integer(cmd_df$rrd_attach_line[error_rows]))
      error_lines = error_lines[!is.na(error_lines) & error_lines > 0]
    }
    
    if ("rrd_has_cache" %in% names(cmd_df)) {
       cache_lines = suppressWarnings(as.integer(cmd_df$rrd_attach_line[rrd_as_logical(cmd_df$rrd_has_cache)]))
       cache_lines = cache_lines[!is.na(cache_lines) & cache_lines > 0]
    }
  }

  line_nodes = vector("list", length(lines) * 2L)
  pos = 0L

  for (i in seq_along(lines)) {
    line_class = "rrd-code-row"
    if (i %in% error_lines) line_class = paste(line_class, "rrd-error-line")
    
    if (i %in% reg_ok_lines) {
       line_class = paste(line_class, "rrd-reg-line-ok")
    } else if (i %in% reg_issue_lines) {
       line_class = paste(line_class, "rrd-reg-line-issue", "rrd-problem-reg-line")
    } else if (i %in% reg_not_run_lines) {
       line_class = paste(line_class, "rrd-reg-line-not-run")
    } else if (i %in% problem_lines) {
       line_class = paste(line_class, "rrd-problem-reg-line")
    }
    
    cache_html = NULL
    if (i %in% cache_lines) {
      cache_html = htmltools::tags$span(class = "rrd-cache-icon", title = "Cache file exists", "\U0001F4C1 ")
    }

    pos = pos + 1L
    line_nodes[[pos]] = htmltools::tags$div(
      id = paste0("rrd-line-", file_idx, "-", i),
      class = line_class,
      `data-file-idx` = file_idx,
      `data-line` = i,
      htmltools::tags$div(
        class = "rrd-line-num",
        if (isTRUE(opts$show_line_num)) i else ""
      ),
      htmltools::tags$pre(
        class = "rrd-do-pre",
        cache_html,
        htmltools::tags$code(lines[i])
      )
    )

    key = as.character(i)
    if (!is.null(output_by_line[[key]]) && nzchar(output_by_line[[key]])) {
      output_class = "rrd-output-block"
      if (i %in% error_lines) output_class = paste(output_class, "rrd-output-error")
      if (i %in% problem_lines) output_class = paste(output_class, "rrd-output-problem")

      pos = pos + 1L
      line_nodes[[pos]] = htmltools::tags$div(
        class = "rrd-output-row",
        `data-file-idx` = file_idx,
        `data-line` = i,
        htmltools::tags$div(class = "rrd-output-spacer"),
        htmltools::tags$details(
          class = output_class,
          htmltools::tags$summary(
            htmltools::tags$span(class = "rrd-output-label", "repbox output")
          ),
          htmltools::tags$pre(
            class = "rrd-output-pre",
            htmltools::tags$code(output_by_line[[key]])
          )
        )
      )
    }
  }

  line_nodes = line_nodes[seq_len(pos)]

  htmltools::tags$div(
    id = paste0("rrd-do-tab-", file_idx),
    class = paste0("rrd-do-tab-pane", if (file_idx == 1L) " active" else ""),
    `data-file-idx` = file_idx,
    htmltools::tags$div(
      class = "rrd-file-title",
      htmltools::tags$span(class = "rrd-file-badge", "File"),
      htmltools::tags$span(class = "rrd-file-path", file_path)
    ),
    htmltools::tags$div(class = "rrd-code-lines", line_nodes)
  )
}
```
!END_MODIFICATION rrd_html_render_do_file /home/rstudio/repbox/repboxReportDo/R/rrd_html.R
