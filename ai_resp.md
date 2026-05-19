You are right. The current filters rely on `cmd_df$is_reg`, so any regression that is only evidenced by `regcheck` or `reg` can be excluded before the issue list is built. The fix below enriches `cmd_df$is_reg` early, using runids from `reg`, `reg_rb`, and `regcheck`. For `regcheck`, it does not require `rb_did_run`, so regressions where `rb` was not run are still included. It only skips regcheck rows with explicit missing-data flags in common missing-data columns.

!MODIFICATION rrd_add_regcheck_reg_flags in rrd_plain.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_plain.R"
insert_after_fun = "rrd_get_cmd_df"
description = "Add a helper that marks commands as regressions if their runid appears in reg, reg_rb, or non-missing-data regcheck rows."
-----------------------------------------------------------------------------------------------------------------------------------------

```r
rrd_add_regcheck_reg_flags = function(cmd_df, parcels = list()) {
  restore.point("rrd_add_regcheck_reg_flags")

  if (is.null(cmd_df)) {
    return(cmd_df)
  }

  if (!"is_reg" %in% names(cmd_df)) {
    cmd_df$is_reg = rep(FALSE, NROW(cmd_df))
  }

  cmd_df$is_reg = rrd_as_logical(cmd_df$is_reg)

  if (NROW(cmd_df) == 0) {
    cmd_df$rrd_is_reg_from_reg_parcel = logical(0)
    cmd_df$rrd_is_reg_from_regcheck = logical(0)
    return(cmd_df)
  }

  parcel_runids = function(name) {
    df = parcels[[name]]
    if (is.null(df) || NROW(df) == 0 || !"runid" %in% names(df)) {
      return(integer(0))
    }

    ids = suppressWarnings(as.integer(as.data.frame(df)$runid))
    unique(ids[!is.na(ids)])
  }

  missing_data_flag = function(df) {
    if (is.null(df) || NROW(df) == 0) {
      return(logical(0))
    }

    df = as.data.frame(df)

    cols = intersect(
      c(
        "missing_data",
        "run_missing_data",
        "data_missing",
        "has_missing_data",
        "missing_dataset",
        "missing_datasets",
        "input_missing",
        "no_data"
      ),
      names(df)
    )

    if (length(cols) == 0) {
      return(rep(FALSE, NROW(df)))
    }

    Reduce(`|`, lapply(cols, function(col) {
      x = df[[col]]

      if (is.logical(x) || is.numeric(x) || is.integer(x)) {
        return(rrd_as_logical(x))
      }

      x = stringi::stri_trim_both(tolower(as.character(x)))
      x[is.na(x)] = ""

      nzchar(x) & !x %in% c("false", "f", "0", "no", "n", "none", "na")
    }))
  }

  reg_runids = unique(c(
    parcel_runids("reg"),
    parcel_runids("reg_rb")
  ))

  regcheck = parcels$regcheck
  regcheck_runids = integer(0)

  if (!is.null(regcheck) && NROW(regcheck) > 0 && "runid" %in% names(regcheck)) {
    regcheck = as.data.frame(regcheck)
    miss = missing_data_flag(regcheck)

    ids = suppressWarnings(as.integer(regcheck$runid[!miss]))
    regcheck_runids = unique(ids[!is.na(ids)])
  }

  cmd_runids = lapply(seq_len(NROW(cmd_df)), function(i) {
    rrd_cmd_runids(cmd_df[i, , drop = FALSE], parcels = parcels)
  })

  from_reg_parcel = vapply(cmd_runids, function(ids) {
    any(ids %in% reg_runids)
  }, logical(1))

  from_regcheck = vapply(cmd_runids, function(ids) {
    any(ids %in% regcheck_runids)
  }, logical(1))

  cmd_df$rrd_is_reg_from_reg_parcel = from_reg_parcel
  cmd_df$rrd_is_reg_from_regcheck = from_regcheck
  cmd_df$is_reg = cmd_df$is_reg | from_reg_parcel | from_regcheck

  cmd_df
}
```

!END_MODIFICATION rrd_add_regcheck_reg_flags in rrd_plain.R

!MODIFICATION rrd_plain_do in rrd_plain.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_plain.R"
function_name = "rrd_plain_do"
description = "Mark regcheck and reg parcel commands as regressions before filtering do files and building the report."
-----------------------------------------------------------------------------------------------------------------------

```r
# A plain text "report" that shows original do files combined with selected
# output information as specified by opts.
#
# The plain version is mainly intended as a diagnostic tool for AI to detect
# problems in the repbox pipeline for reproductions including metaregBase
# replications. HTML reports that are easier for humans to read can be
# generated with rrd_html_do instead.
rrd_plain_do = function(
  project_dir,
  parcels = list(),
  opts = rrd_opts(),
  outfile = "do_report.txt",
  split_outdir = "do_report"
) {
  restore.point("rrd_plain_do")

  project_dir = normalizePath(project_dir, mustWork = FALSE)

  outdir = file.path(project_dir, "reports")
  if (isTRUE(opts$split_by_do)) {
    if (basename(split_outdir) == split_outdir) {
      split_outdir = file.path(outdir, split_outdir)
    }
    outdir = split_outdir
  } else {
    if (basename(outfile) == outfile) {
      outfile = file.path(outdir, outfile)
    }
    outdir = dirname(outfile)
  }

  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  }

  parcels = rrd_load_report_parcels(project_dir, parcels = parcels)
  do_df = rrd_get_do_files(project_dir, parcels = parcels)
  cmd_df = rrd_get_cmd_df(project_dir, parcels = parcels)
  cmd_df = rrd_add_regcheck_reg_flags(cmd_df, parcels = parcels)

  if (NROW(cmd_df) > 0) {
    cmd_df$rrd_has_run_output = rrd_cmd_has_run_output(cmd_df, parcels, opts = opts)
    cmd_df$rrd_has_problem_reg = rrd_has_problem_reg(cmd_df, parcels = parcels)
  } else {
    cmd_df$rrd_has_run_output = logical(0)
    cmd_df$rrd_has_problem_reg = logical(0)
  }

  if (NROW(do_df) == 0) {
    txt = paste0(
      "# repboxReportDo plain do report\n\n",
      "Project dir: ", project_dir, "\n\n",
      "No Stata do files were found.\n"
    )
    if (!isTRUE(opts$split_by_do)) writeLines(txt, outfile, useBytes = TRUE)
    return(invisible(if (isTRUE(opts$split_by_do)) character() else outfile))
  }

  keep = rep(TRUE, NROW(do_df))

  if (isTRUE(opts$only_do_with_reg)) {
    keep = keep & rrd_do_has_cmd_flag(do_df$file_path, cmd_df, "is_reg")
  }

  if (isTRUE(opts$only_do_with_prob_reg)) {
    keep = keep & rrd_do_has_cmd_flag(do_df$file_path, cmd_df, "rrd_has_problem_reg")
  }

  do_df = do_df[keep, , drop = FALSE]

  header = rrd_report_header(
    project_dir = project_dir,
    opts = opts,
    num_do_files = NROW(do_df),
    num_cmds = NROW(cmd_df),
    parcels = parcels
  )

  if (isTRUE(opts$split_by_do)) {
    files = character(NROW(do_df))

    for (i in seq_len(NROW(do_df))) {
      file_cmd_df = rrd_cmds_for_file(cmd_df, do_df$file_path[i])
      txt = paste0(
        header,
        "\n",
        rrd_render_do_file(do_df[i, , drop = FALSE], file_cmd_df, parcels, opts)
      )

      out_file = file.path(
        outdir,
        paste0(sprintf("%03d_", i), rrd_safe_file_name(do_df$file_path[i]), ".txt")
      )
      writeLines(txt, out_file, useBytes = TRUE)
      files[i] = out_file
    }

    return(invisible(files))
  }

  parts = lapply(seq_len(NROW(do_df)), function(i) {
    file_cmd_df = rrd_cmds_for_file(cmd_df, do_df$file_path[i])
    rrd_render_do_file(do_df[i, , drop = FALSE], file_cmd_df, parcels, opts)
  })

  txt = paste0(c(header, unlist(parts)), collapse = "\n")
  writeLines(txt, outfile, useBytes = TRUE)

  invisible(outfile)
}
```

!END_MODIFICATION rrd_plain_do in rrd_plain.R

!MODIFICATION rrd_html_do in rrd_html.R
scope = "function"
file = "/home/rstudio/repbox/repboxReportDo/R/rrd_html.R"
function_name = "rrd_html_do"
description = "Mark regcheck and reg parcel commands as regressions before filtering do files and building HTML issues."
------------------------------------------------------------------------------------------------------------------------

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
  cmd_df = rrd_add_regcheck_reg_flags(cmd_df, parcels = parcels)

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
