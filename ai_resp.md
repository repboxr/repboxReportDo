Below are the proposed changes.

!MODIFICATION rrd_html_do in rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_do"
description = "Load problem information, compute a problems data frame, and pass it to the HTML report panel."
--------------------------------------------------------------------------------------------------------------

```r
#' Create a human-friendly HTML report for Stata reproduction diagnostics
#'
#' @param project_dir Repbox project directory.
#' @param parcels Optional preloaded parcel list.
#' @param opts Options, typically from rrd_opts().
#' @param output_dir Output directory. Defaults to project_dir/reports.
#' @param output_file Output HTML file name.
#' @param copy_assets Shall CSS and JS assets from inst/www be copied?
#' @return Invisibly returns the generated HTML file path.
#' @export
rrd_html_do = function(
  project_dir,
  parcels = list(),
  opts = rrd_opts(),
  output_dir = file.path(project_dir, "reports"),
  output_file = "do_report.html",
  copy_assets = TRUE
) {
  restore.point("rrd_html_do")

  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is needed for rrd_html_do().", call. = FALSE)
  }

  project_dir = normalizePath(project_dir, mustWork = FALSE)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (isTRUE(copy_assets)) {
    rrd_copy_html_assets(output_dir)
  }

  parcels = rrd_load_report_parcels(project_dir, parcels = parcels)
  do_df = rrd_get_do_files(project_dir, parcels = parcels)
  cmd_df = rrd_get_cmd_df(project_dir, parcels = parcels)

  if (NROW(cmd_df) > 0) {
    cmd_df$rrd_has_run_output = rrd_cmd_has_run_output(cmd_df, parcels, opts = opts)
    cmd_df$rrd_has_problem_reg = rrd_has_problem_reg(cmd_df, parcels = parcels)
  } else {
    cmd_df$rrd_has_run_output = logical(0)
    cmd_df$rrd_has_problem_reg = logical(0)
  }

  if (NROW(do_df) > 0) {
    keep = rep(TRUE, NROW(do_df))

    if (isTRUE(opts$only_do_with_reg)) {
      keep = keep & rrd_do_has_cmd_flag(do_df$file_path, cmd_df, "is_reg")
    }

    if (isTRUE(opts$only_do_with_prob_reg)) {
      keep = keep & rrd_do_has_cmd_flag(do_df$file_path, cmd_df, "rrd_has_problem_reg")
    }

    do_df = do_df[keep, , drop = FALSE]
  }

  do_df$rrd_file_idx = seq_len(NROW(do_df))

  if (NROW(cmd_df) > 0 && NROW(do_df) > 0) {
    cmd_df$rrd_file_idx = vapply(cmd_df$file_path, function(file_path) {
      ind = which(rrd_same_file(do_df$file_path, file_path))
      if (length(ind) == 0) return(NA_integer_)
      do_df$rrd_file_idx[ind[[1]]]
    }, integer(1))

    cmd_df = cmd_df[!is.na(cmd_df$rrd_file_idx), , drop = FALSE]
    cmd_df$rrd_cmd_idx = seq_len(NROW(cmd_df))
    cmd_df$rrd_cmd_id = paste0("rrd-cmd-", cmd_df$rrd_file_idx, "-", cmd_df$rrd_cmd_idx)
  } else {
    cmd_df$rrd_file_idx = integer(0)
    cmd_df$rrd_cmd_idx = integer(0)
    cmd_df$rrd_cmd_id = character(0)
  }

  issue_df = rrd_html_issue_df(cmd_df, parcels = parcels, opts = opts)
  summary_df = rrd_html_summary_df(cmd_df, issue_df = issue_df, parcels = parcels, opts = opts)
  problem_df = rrd_html_problem_df(project_dir = project_dir, parcels = parcels)

  html_content = htmltools::tagList(
    htmltools::tags$html(
      htmltools::tags$head(
        htmltools::tags$meta(charset = "UTF-8"),
        htmltools::tags$meta(`http-equiv` = "X-UA-Compatible", content = "IE=edge"),
        htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
        htmltools::tags$title(paste0("repboxReportDo: ", basename(project_dir))),
        htmltools::tags$link(href = "shared/rrd_html.css", rel = "stylesheet")
      ),
      htmltools::tags$body(
        htmltools::tags$div(
          class = "rrd-page",
          htmltools::tags$div(
            class = "rrd-main",
            htmltools::tags$div(
              id = "rrd-code-col",
              class = "rrd-code-col",
              rrd_html_do_panel(do_df, cmd_df, parcels = parcels, opts = opts)
            ),
            htmltools::tags$div(
              id = "rrd-report-col",
              class = "rrd-report-col",
              rrd_html_report_panel(
                issue_df,
                summary_df = summary_df,
                problem_df = problem_df
              )
            )
          )
        ),
        htmltools::tags$script(src = "shared/rrd_html.js")
      )
    )
  )

  report_path = file.path(output_dir, output_file)
  htmltools::save_html(html_content, file = report_path)

  invisible(report_path)
}
```

!END_MODIFICATION rrd_html_do in rrd_html.R

!MODIFICATION rrd_html_report_panel in rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_report_panel"
description = "Add a separate Problems tab next to Issues and Summary."
-----------------------------------------------------------------------

```r
rrd_html_report_panel = function(issue_df, summary_df = NULL, problem_df = NULL) {
  restore.point("rrd_html_report_panel")

  htmltools::tagList(
    htmltools::tags$div(
      class = "rrd-report-titlebar",
      htmltools::tags$button(
        type = "button",
        class = "rrd-report-title active",
        `data-tab-target` = "rrd-report-issues",
        "Issues"
      ),
      htmltools::tags$button(
        type = "button",
        class = "rrd-report-title",
        `data-tab-target` = "rrd-report-summary",
        "Summary"
      ),
      htmltools::tags$button(
        type = "button",
        class = "rrd-report-title",
        `data-tab-target` = "rrd-report-problems",
        "Problems"
      )
    ),
    htmltools::tags$div(
      class = "rrd-report-tab-content",
      htmltools::tags$div(
        id = "rrd-report-issues",
        class = "rrd-report-tab-pane active",
        rrd_html_issue_panel(issue_df)
      ),
      htmltools::tags$div(
        id = "rrd-report-summary",
        class = "rrd-report-tab-pane",
        rrd_html_summary_panel(summary_df)
      ),
      htmltools::tags$div(
        id = "rrd-report-problems",
        class = "rrd-report-tab-pane",
        rrd_html_problem_panel(problem_df)
      )
    )
  )
}
```

!END_MODIFICATION rrd_html_report_panel in rrd_html.R

!MODIFICATION rrd_html_summary_df in rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_summary_df"
description = "Append counts for regcheck issue types that occur at least once."
--------------------------------------------------------------------------------

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

  if (!is.null(issue_df) && NROW(issue_df) > 0 && "issue_type" %in% names(issue_df) && "issue_title" %in% names(issue_df)) {
    reg_titles = issue_df$issue_title[issue_df$issue_type == "regcheck"]
    reg_titles = as.character(reg_titles)
    reg_titles = reg_titles[!is.na(reg_titles) & nzchar(reg_titles)]

    if (length(reg_titles) > 0) {
      reg_counts = sort(table(reg_titles), decreasing = TRUE)
      items = c(items, paste0("Regcheck issue: ", names(reg_counts)))
      values = c(values, as.integer(reg_counts))
    }
  }

  data.frame(
    item = items,
    value = values,
    stringsAsFactors = FALSE
  )
}
```

!END_MODIFICATION rrd_html_summary_df in rrd_html.R

!MODIFICATION problems helpers in rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
insert_before_fun = "rrd_html_issue_panel"
description = "Add helpers to collect project problems from the problem parcel or from single RDS files in project_dir/problems."
---------------------------------------------------------------------------------------------------------------------------------

```r

rrd_html_problem_df = function(project_dir, parcels = list()) {
  restore.point("rrd_html_problem_df")

  empty = data.frame(
    problem_type = character(0),
    problem_descr = character(0),
    stringsAsFactors = FALSE
  )

  if (!is.null(parcels$problem)) {
    df = rrd_html_normalize_problem_obj(parcels$problem)
    if (NROW(df) > 0) {
      return(df)
    }
  }

  problem_dir = file.path(project_dir, "problems")
  if (!dir.exists(problem_dir)) {
    return(empty)
  }

  prob_files = list.files(problem_dir, pattern = "\\.Rds$", full.names = TRUE)
  if (length(prob_files) == 0) {
    return(empty)
  }

  parts = lapply(prob_files, function(file) {
    rrd_html_normalize_problem_obj(readRDS(file))
  })

  parts = parts[NROW(parts) > 0]
  if (length(parts) == 0) {
    return(empty)
  }

  res = do.call(rbind, parts)
  res = res[!duplicated(res[, c("problem_type", "problem_descr"), drop = FALSE]), , drop = FALSE]
  rownames(res) = NULL

  res
}


rrd_html_normalize_problem_obj = function(obj) {
  restore.point("rrd_html_normalize_problem_obj")

  empty = data.frame(
    problem_type = character(0),
    problem_descr = character(0),
    stringsAsFactors = FALSE
  )

  if (is.null(obj)) {
    return(empty)
  }

  if (is.list(obj) && !inherits(obj, "data.frame") && "problem" %in% names(obj)) {
    obj = obj$problem
  }

  if (inherits(obj, "data.frame")) {
    df = as.data.frame(obj, stringsAsFactors = FALSE)

    if (!"problem_type" %in% names(df)) {
      if ("type" %in% names(df)) {
        df$problem_type = df$type
      } else if (NCOL(df) >= 1) {
        df$problem_type = df[[1]]
      } else {
        df$problem_type = ""
      }
    }

    if (!"problem_descr" %in% names(df)) {
      if ("msg" %in% names(df)) {
        df$problem_descr = df$msg
      } else if ("descr" %in% names(df)) {
        df$problem_descr = df$descr
      } else if ("description" %in% names(df)) {
        df$problem_descr = df$description
      } else if (NCOL(df) >= 2) {
        df$problem_descr = df[[2]]
      } else {
        df$problem_descr = ""
      }
    }

    df$problem_type = as.character(df$problem_type)
    df$problem_descr = as.character(df$problem_descr)
    df$problem_type[is.na(df$problem_type)] = ""
    df$problem_descr[is.na(df$problem_descr)] = ""

    df = df[nzchar(df$problem_type) | nzchar(df$problem_descr), , drop = FALSE]
    if (NROW(df) == 0) {
      return(empty)
    }

    df = df[, c("problem_type", "problem_descr"), drop = FALSE]
    rownames(df) = NULL

    return(df)
  }

  if (is.list(obj)) {
    problem_type = if ("type" %in% names(obj)) obj$type else obj[[1]]
    problem_descr = if ("msg" %in% names(obj)) {
      obj$msg
    } else if ("descr" %in% names(obj)) {
      obj$descr
    } else if ("description" %in% names(obj)) {
      obj$description
    } else if (length(obj) >= 2) {
      obj[[2]]
    } else {
      ""
    }

    problem_type = paste0(rrd_chr_vec(problem_type), collapse = ", ")
    problem_descr = paste0(rrd_chr_vec(problem_descr), collapse = "\n")

    if (!nzchar(problem_type) && !nzchar(problem_descr)) {
      return(empty)
    }

    return(data.frame(
      problem_type = problem_type,
      problem_descr = problem_descr,
      stringsAsFactors = FALSE
    ))
  }

  empty
}


rrd_html_problem_panel = function(problem_df) {
  restore.point("rrd_html_problem_panel")

  if (is.null(problem_df) || NROW(problem_df) == 0) {
    return(htmltools::tags$div(
      class = "rrd-no-issues",
      htmltools::tags$h4("No run problems found"),
      htmltools::tags$p("No problem parcel or individual problem files reported project-level run problems.")
    ))
  }

  problem_df = as.data.frame(problem_df, stringsAsFactors = FALSE)

  counts = sort(table(problem_df$problem_type), decreasing = TRUE)
  count_rows = lapply(seq_along(counts), function(i) {
    htmltools::tags$tr(
      htmltools::tags$td(htmltools::htmlEscape(names(counts)[i])),
      htmltools::tags$td(class = "rrd-summary-value", htmltools::htmlEscape(as.integer(counts[[i]])))
    )
  })

  detail_rows = lapply(seq_len(NROW(problem_df)), function(i) {
    htmltools::tags$tr(
      htmltools::tags$td(htmltools::htmlEscape(problem_df$problem_type[i])),
      htmltools::tags$td(htmltools::htmlEscape(problem_df$problem_descr[i]))
    )
  })

  htmltools::tagList(
    htmltools::tags$div(
      class = "rrd-panel-intro",
      htmltools::tags$strong(NROW(problem_df)),
      " noted run problems"
    ),
    htmltools::tags$table(
      class = "rrd-summary-table",
      htmltools::tags$tbody(count_rows)
    ),
    htmltools::tags$div(
      class = "rrd-panel-intro",
      "Problem details"
    ),
    htmltools::tags$table(
      class = "rrd-summary-table",
      htmltools::tags$thead(
        htmltools::tags$tr(
          htmltools::tags$th("Type"),
          htmltools::tags$th("Description")
        )
      ),
      htmltools::tags$tbody(detail_rows)
    )
  )
}
```

!END_MODIFICATION problems helpers in rrd_html.R

!MODIFICATION rrd_load_report_parcels in rrd_plain.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_plain.R"
function_name = "rrd_load_report_parcels"
description = "Also load the project-level problem parcel when it exists."
--------------------------------------------------------------------------

```r
rrd_load_report_parcels = function(project_dir, parcels = list()) {
  restore.point("rrd_load_report_parcels")

  parcel_names = c(
    "stata_source",
    "stata_file",
    "stata_cmd",
    "stata_run_cmd",
    "stata_run_log",
    "stata_run_info",
    "stata_do_run_info",
    "regcheck",
    "regcoef",
    "regcoef_so",
    "regcoef_rb",
    "reg",
    "reg_rb",
    "problem"
  )

  repdb_dir = file.path(project_dir, "repdb")

  for (parcel_name in parcel_names) {
    if (!is.null(parcels[[parcel_name]])) next

    file = file.path(repdb_dir, paste0(parcel_name, ".Rds"))
    if (!file.exists(file)) next

    obj = readRDS(file)

    if (is.list(obj) && !inherits(obj, "data.frame") && parcel_name %in% names(obj)) {
      obj = obj[[parcel_name]]
    } else if (is.list(obj) && !inherits(obj, "data.frame") && length(obj) == 1) {
      if (inherits(obj[[1]], "data.frame")) {
        obj = obj[[1]]
      }
    }

    parcels[[parcel_name]] = obj
  }

  attr(parcels, "project_dir") = project_dir

  parcels
}
```

!END_MODIFICATION rrd_load_report_parcels in rrd_plain.R
