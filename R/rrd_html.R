# Human-friendly HTML report for Stata reproduction diagnostics.
#
# This report complements rrd_plain_do(). It keeps the do-file oriented
# diagnostic content, but displays it in a two-panel layout:
#
# Left: do files with tabs and selected command output.
# Right: compact issue list with details and optional coef/se mismatch tables.

example = function() {
  project_dir = "~/repbox/projects/aer_101_2_5"
  opts = rrd_opts()
  rep_file = rrd_html_do(project_dir, opts = opts)
  browseURL(rep_file)
  rstudioapi::filesPaneNavigate(dirname(rep_file))
}


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
              rrd_html_report_panel(issue_df)
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


rrd_copy_html_assets = function(output_dir) {
  restore.point("rrd_copy_html_assets")

  pkg_www_dir = system.file("www", package = "repboxReportDo")
  if (pkg_www_dir == "") {
    warning("Could not find inst/www directory in repboxReportDo package. HTML report assets will be missing.")
    return(invisible(FALSE))
  }

  shared_dir = file.path(output_dir, "shared")
  if (!dir.exists(shared_dir)) {
    dir.create(shared_dir, recursive = TRUE, showWarnings = FALSE)
  }

  files = list.files(pkg_www_dir, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    warning("No HTML assets found in repboxReportDo/inst/www.")
    return(invisible(FALSE))
  }

  file.copy(files, shared_dir, recursive = TRUE, overwrite = TRUE)
  invisible(TRUE)
}


rrd_html_do_panel = function(do_df, cmd_df, parcels, opts) {
  restore.point("rrd_html_do_panel")

  if (NROW(do_df) == 0) {
    return(htmltools::tags$div(
      class = "rrd-empty",
      htmltools::tags$h4("No do files found"),
      htmltools::tags$p("No Stata do files were available after applying the report filters.")
    ))
  }

  tab_buttons = lapply(seq_len(NROW(do_df)), function(i) {
    file_idx = do_df$rrd_file_idx[i]
    active_class = if (i == 1) " active" else ""

    file_label = do_df$file_name[i]
    if (is.na(file_label) || !nzchar(file_label)) {
      file_label = basename(do_df$file_path[i])
    }

    htmltools::tags$button(
      type = "button",
      class = paste0("rrd-do-tab-btn", active_class),
      `data-tab-target` = paste0("rrd-do-tab-", file_idx),
      `data-file-idx` = file_idx,
      title = do_df$file_path[i],
      htmltools::htmlEscape(file_label)
    )
  })

  tab_panes = lapply(seq_len(NROW(do_df)), function(i) {
    row = do_df[i, , drop = FALSE]
    file_cmd_df = rrd_cmds_for_file(cmd_df, row$file_path[1])

    rrd_html_do_file_pane(
      do_row = row,
      cmd_df = file_cmd_df,
      parcels = parcels,
      opts = opts,
      active = i == 1
    )
  })

  htmltools::tagList(
    htmltools::tags$div(class = "rrd-do-tabs", tab_buttons),
    htmltools::tags$div(class = "rrd-do-tab-content", tab_panes)
  )
}


rrd_html_do_file_pane = function(do_row, cmd_df, parcels, opts, active = FALSE) {
  restore.point("rrd_html_do_file_pane")

  file_idx = do_row$rrd_file_idx[1]
  file_path = do_row$file_path[1]
  txt = do_row$text[1]
  lines = stringi::stri_split_lines1(txt)

  if (length(lines) == 0) {
    lines = ""
  }

  output_by_line = rrd_html_outputs_by_line(cmd_df, parcels = parcels, opts = opts)
  line_flags = rrd_html_line_flags(cmd_df, n = length(lines))

  line_nodes = vector("list", length(lines))

  for (line_num in seq_along(lines)) {
    key = as.character(line_num)
    flags = line_flags[[key]]

    line_class = c("rrd-code-row")
    if (isTRUE(flags$is_reg)) line_class = c(line_class, "rrd-reg-line")
    if (isTRUE(flags$has_error)) line_class = c(line_class, "rrd-error-line")
    if (isTRUE(flags$has_problem_reg)) line_class = c(line_class, "rrd-problem-reg-line")
    if (!isTRUE(flags$has_cmd)) line_class = c(line_class, "rrd-no-command-line")

    extra_html = output_by_line[[key]]

    line_nodes[[line_num]] = htmltools::tags$div(
      id = paste0("rrd-line-", file_idx, "-", line_num),
      class = paste0(line_class, collapse = " "),
      `data-file-idx` = file_idx,
      `data-line` = line_num,
      htmltools::tags$span(
        class = "rrd-line-num",
        if (isTRUE(opts$show_line_num)) line_num else ""
      ),
      htmltools::tags$span(
        class = "rrd-code-cell",
        htmltools::tags$code(
          class = "rrd-code-text",
          htmltools::htmlEscape(lines[[line_num]])
        ),
        if (!is.null(extra_html) && nzchar(extra_html)) {
          htmltools::HTML(extra_html)
        } else {
          NULL
        }
      )
    )
  }

  pane_class = if (active) "rrd-do-tab-pane active" else "rrd-do-tab-pane"

  htmltools::tags$div(
    id = paste0("rrd-do-tab-", file_idx),
    class = pane_class,
    `data-file-idx` = file_idx,
    htmltools::tags$div(
      class = "rrd-file-title",
      htmltools::tags$span(class = "rrd-file-badge", "File"),
      htmltools::tags$span(class = "rrd-file-path", htmltools::htmlEscape(file_path))
    ),
    htmltools::tags$div(
      class = "rrd-code-lines",
      line_nodes
    )
  )
}


rrd_html_line_flags = function(cmd_df, n) {
  restore.point("rrd_html_line_flags")

  empty_flag = list(
    has_cmd = FALSE,
    is_reg = FALSE,
    has_error = FALSE,
    has_problem_reg = FALSE
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

  for (line_num in sort(unique(line[ok]))) {
    rows = which(ok & line == line_num)
    key = as.character(line_num)
    res[[key]] = list(
      has_cmd = TRUE,
      is_reg = any(rrd_as_logical(cmd_df$is_reg[rows]), na.rm = TRUE),
      has_error = any(vapply(rows, function(i) rrd_cmd_has_error(cmd_df[i, , drop = FALSE]), logical(1))),
      has_problem_reg = any(rrd_as_logical(cmd_df$rrd_has_problem_reg[rows]), na.rm = TRUE)
    )
  }

  res
}


rrd_html_outputs_by_line = function(cmd_df, parcels, opts) {
  restore.point("rrd_html_outputs_by_line")

  res = list()
  if (is.null(cmd_df) || NROW(cmd_df) == 0) return(res)

  for (i in seq_len(NROW(cmd_df))) {
    cmd = cmd_df[i, , drop = FALSE]
    line = suppressWarnings(as.integer(cmd$rrd_attach_line[1]))
    if (is.na(line) || line <= 0) next

    txt = rrd_format_cmd_output(cmd, parcels = parcels, opts = opts)
    if (!nzchar(txt)) next

    key = as.character(line)
    html = rrd_html_output_block(cmd, txt)

    if (is.null(res[[key]])) {
      res[[key]] = html
    } else {
      res[[key]] = paste0(res[[key]], "\n", html)
    }
  }

  res
}


rrd_html_output_block = function(cmd, txt) {
  restore.point("rrd_html_output_block")

  runids = rrd_cmd_runids(cmd)
  runid_label = if (length(runids) == 0) {
    "runid NA"
  } else {
    paste0("runid ", paste0(runids, collapse = ", "))
  }

  cls = c("rrd-output-block")
  if (isTRUE(cmd$is_reg[1])) cls = c(cls, "rrd-output-reg")
  if (rrd_cmd_has_error(cmd)) cls = c(cls, "rrd-output-error")
  if ("rrd_has_problem_reg" %in% names(cmd) && isTRUE(cmd$rrd_has_problem_reg[1])) {
    cls = c(cls, "rrd-output-problem")
  }

  paste0(
    '<details class="', paste0(cls, collapse = " "), '" id="', rrd_html_attr(cmd$rrd_cmd_id[1]), '">',
    '<summary>',
    '<span class="rrd-output-label">repbox output</span>',
    '<span class="rrd-output-runid">', htmltools::htmlEscape(runid_label), '</span>',
    '</summary>',
    '<pre class="rrd-output-pre"><code>',
    htmltools::htmlEscape(txt),
    '</code></pre>',
    '</details>'
  )
}


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

  for (i in seq_len(NROW(cmd_df))) {
    cmd = cmd_df[i, , drop = FALSE]
    line = suppressWarnings(as.integer(cmd$rrd_attach_line[1]))
    if (is.na(line)) line = NA_integer_

    runids = rrd_cmd_runids(cmd, parcels = parcels)
    runids_txt = if (length(runids) == 0) "" else paste0(runids, collapse = ", ")
    cmdline = if ("cmdline" %in% names(cmd)) rrd_chr_vec(cmd$cmdline[1]) else ""

    if (rrd_cmd_has_error(cmd)) {
      err_txt = rrd_cmd_error_text(cmd)

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
        runids = runids_txt,
        cmdline = cmdline,
        details_html = rrd_html_pre_details(err_txt),
        stringsAsFactors = FALSE
      )
    }

    if ("rrd_has_problem_reg" %in% names(cmd) && isTRUE(cmd$rrd_has_problem_reg[1])) {
      reg_issue = rrd_html_regcheck_issue(runids, parcels = parcels, opts = opts)

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
        runids = runids_txt,
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


rrd_html_error_title = function(cmd) {
  restore.point("rrd_html_error_title")

  fields = intersect(c("errcode", "rc", "error_code", "stata_rc", "run_errcode", "run_rc"), names(cmd))
  for (field in fields) {
    val = suppressWarnings(as.integer(cmd[[field]][1]))
    if (!is.na(val) && val != 0L) {
      return(paste0("Stata rc ", val))
    }
  }

  "Command error"
}


rrd_html_error_summary = function(cmd) {
  restore.point("rrd_html_error_summary")

  fields = intersect(c("err_msg", "error_msg", "error", "stderr", "run_err_msg", "run_error_msg"), names(cmd))
  for (field in fields) {
    val = rrd_chr_vec(cmd[[field]][1])
    val = val[nzchar(val)]
    if (length(val) > 0) {
      return(val[[1]])
    }
  }

  "The command reported a nonzero return code or error output."
}


rrd_html_regcheck_issue = function(runids, parcels = list(), opts = rrd_opts()) {
  restore.point("rrd_html_regcheck_issue")

  runids = unique(suppressWarnings(as.integer(runids)))
  runids = runids[!is.na(runids)]

  default = list(
    title = "Regression check issue",
    summary = "Regcheck reported a problem for this regression.",
    badge = "regcheck",
    severity = 80L,
    details_html = rrd_html_pre_details(rrd_regcheck_text(runids, parcels = parcels, opts = opts))
  )

  if (length(runids) == 0) {
    return(default)
  }

  regcheck = parcels$regcheck
  if (is.null(regcheck) || NROW(regcheck) == 0 || !"runid" %in% names(regcheck)) {
    return(default)
  }

  df = as.data.frame(regcheck)
  df = df[df$runid %in% runids, , drop = FALSE]
  if (NROW(df) == 0) {
    return(default)
  }

  row = df[1, , drop = FALSE]
  flags = rrd_html_regcheck_flags(row)

  title = rrd_html_regcheck_title(flags)
  summary = rrd_html_regcheck_summary(flags, row)
  badge = rrd_html_regcheck_badge(flags)
  severity = rrd_html_regcheck_severity(flags)
  details_html = rrd_html_regcheck_details_html(row, parcels = parcels, opts = opts)

  list(
    title = title,
    summary = summary,
    badge = badge,
    severity = severity,
    details_html = details_html
  )
}


rrd_html_regcheck_flags = function(row) {
  restore.point("rrd_html_regcheck_flags")

  get_bool = function(name, default = FALSE) {
    if (!name %in% names(row)) return(default)
    val = row[[name]][1]
    if (is.na(val)) return(default)
    isTRUE(as.logical(val))
  }

  get_chr = function(name) {
    if (!name %in% names(row)) return("")
    val = as.character(row[[name]][1])
    if (is.na(val)) "" else val
  }

  list(
    so = get_bool("so_did_run"),
    sb = get_bool("sb_did_run"),
    rb = get_bool("rb_did_run"),
    so_raw = get_bool("so_raw_did_run", default = get_bool("so_did_run")),
    sb_raw = get_bool("sb_raw_did_run", default = get_bool("sb_did_run")),
    sb_so_same = get_bool("sb_so_identical", default = NA),
    rb_sb_coef_same = get_bool("rb_sb_coef_same", default = NA),
    rb_sb_se_same = get_bool("rb_sb_se_same", default = NA),
    problem = get_chr("problem"),
    comment = get_chr("comment")
  )
}


rrd_html_regcheck_title = function(flags) {
  restore.point("rrd_html_regcheck_title")

  missing = c(
    if (!isTRUE(flags$so)) "so" else character(0),
    if (!isTRUE(flags$sb)) "sb" else character(0),
    if (!isTRUE(flags$rb)) "rb" else character(0)
  )

  if (length(missing) > 0) {
    return(paste0(rrd_html_join_words(missing), " missing"))
  }

  coef_bad = identical(flags$rb_sb_coef_same, FALSE)
  se_bad = identical(flags$rb_sb_se_same, FALSE)

  if (coef_bad && se_bad) return("sb vs rb coef/se mismatch")
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


rrd_html_regcheck_summary = function(flags, row) {
  restore.point("rrd_html_regcheck_summary")

  if (!isTRUE(flags$so) || !isTRUE(flags$sb) || !isTRUE(flags$rb)) {
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


rrd_html_regcheck_badge = function(flags) {
  coef_bad = identical(flags$rb_sb_coef_same, FALSE)
  se_bad = identical(flags$rb_sb_se_same, FALSE)

  if (!isTRUE(flags$so) || !isTRUE(flags$sb) || !isTRUE(flags$rb)) return("missing")
  if (coef_bad && se_bad) return("coef/se")
  if (coef_bad) return("coef")
  if (se_bad) return("se")
  if (identical(flags$sb_so_same, FALSE)) return("sb/so")
  "regcheck"
}


rrd_html_regcheck_severity = function(flags) {
  if (!isTRUE(flags$rb)) return(100L)
  if (!isTRUE(flags$sb)) return(95L)
  if (!isTRUE(flags$so)) return(90L)
  if (identical(flags$rb_sb_coef_same, FALSE)) return(85L)
  if (identical(flags$sb_so_same, FALSE)) return(80L)
  if (identical(flags$rb_sb_se_same, FALSE)) return(70L)
  60L
}


rrd_html_regcheck_details_html = function(row, parcels = list(), opts = rrd_opts()) {
  restore.point("rrd_html_regcheck_details_html")

  runid = suppressWarnings(as.integer(row$runid[1]))
  flags = rrd_html_regcheck_flags(row)

  has_all_results = isTRUE(flags$so) && isTRUE(flags$sb) && isTRUE(flags$rb)

  pair_specs = list()

  if (
    has_all_results &&
    (identical(flags$rb_sb_coef_same, FALSE) || identical(flags$rb_sb_se_same, FALSE))
  ) {
    pair_specs[[length(pair_specs) + 1]] = list(
      label = "sb vs rb",
      expected = "regcoef",
      observed = "regcoef_rb",
      expected_label = "sb",
      observed_label = "rb"
    )
  }

  if (
    has_all_results &&
    identical(flags$sb_so_same, FALSE)
  ) {
    pair_specs[[length(pair_specs) + 1]] = list(
      label = "sb vs so",
      expected = "regcoef",
      observed = "regcoef_so",
      expected_label = "sb",
      observed_label = "so"
    )
  }

  diff_html = rrd_html_coef_diff_details(
    runid = runid,
    parcels = parcels,
    pair_specs = pair_specs
  )

  regcheck_txt = rrd_df_to_text(row, opts = opts)
  regcheck_html = paste0(
    '<div class="rrd-details-subtitle">Regcheck row</div>',
    '<pre class="rrd-details-pre"><code>',
    htmltools::htmlEscape(regcheck_txt),
    '</code></pre>'
  )

  paste0(diff_html, regcheck_html)
}


rrd_html_coef_diff_details = function(runid, parcels = list(), pair_specs = list(), max_rows = 8) {
  restore.point("rrd_html_coef_diff_details")

  if (length(pair_specs) == 0) {
    return("")
  }

  parts = lapply(pair_specs, function(pair) {
    tab = rrd_html_coef_diff_table(
      runid = runid,
      expected = parcels[[pair$expected]],
      observed = parcels[[pair$observed]],
      expected_label = pair$expected_label,
      observed_label = pair$observed_label
    )

    if (is.null(tab) || NROW(tab) == 0) {
      return("")
    }

    tab = tab[seq_len(min(NROW(tab), max_rows)), , drop = FALSE]

    paste0(
      '<div class="rrd-details-subtitle">', htmltools::htmlEscape(pair$label), ' mismatch examples</div>',
      rrd_html_diff_table_html(tab)
    )
  })

  parts = unlist(parts, use.names = FALSE)
  parts = parts[nzchar(parts)]

  if (length(parts) == 0) {
    return("")
  }

  paste0(parts, collapse = "")
}


rrd_html_coef_diff_table = function(
  runid,
  expected,
  observed,
  expected_label = "expected",
  observed_label = "observed",
  tol = 1e-6
) {
  restore.point("rrd_html_coef_diff_table")

  if (is.null(expected) || is.null(observed)) return(NULL)
  if (NROW(expected) == 0 || NROW(observed) == 0) return(NULL)

  expected = as.data.frame(expected)
  observed = as.data.frame(observed)

  if (!"runid" %in% names(expected) || !"runid" %in% names(observed)) return(NULL)
  if (!"cterm" %in% names(expected) || !"cterm" %in% names(observed)) return(NULL)

  expected = expected[expected$runid == runid, , drop = FALSE]
  observed = observed[observed$runid == runid, , drop = FALSE]

  if (NROW(expected) == 0 || NROW(observed) == 0) return(NULL)

  expected = rrd_html_prepare_coef_df(expected)
  observed = rrd_html_prepare_coef_df(observed)

  by_cols = intersect(c("eq", "cterm"), names(expected))
  by_cols = intersect(by_cols, names(observed))
  if (!"cterm" %in% by_cols) return(NULL)

  df = merge(
    expected,
    observed,
    by = by_cols,
    all = TRUE,
    suffixes = c("_expected", "_observed")
  )

  if (NROW(df) == 0) return(NULL)

  coef_diff = rrd_html_num_diff(df$coef_expected, df$coef_observed, tol = tol)
  se_diff = rrd_html_num_diff(df$se_expected, df$se_observed, tol = tol)

  df$coef_diff = coef_diff
  df$se_diff = se_diff
  df$any_diff = coef_diff | se_diff

  df = df[df$any_diff, , drop = FALSE]
  if (NROW(df) == 0) return(NULL)

  df$status = ifelse(
    df$coef_diff & df$se_diff,
    "coef/se mismatch",
    ifelse(df$coef_diff, "coef mismatch", "SE mismatch")
  )

  df$term = if ("eq" %in% names(df) && any(nzchar(df$eq), na.rm = TRUE)) {
    ifelse(nzchar(df$eq), paste0(df$eq, ":", df$cterm), df$cterm)
  } else {
    df$cterm
  }

  df$expected = rrd_html_coef_se_text(df$coef_expected, df$se_expected)
  df$observed = rrd_html_coef_se_text(df$coef_observed, df$se_observed)

  df = df[, c("term", "expected", "observed", "status"), drop = FALSE]
  names(df)[2:3] = c(expected_label, observed_label)

  df
}


rrd_html_prepare_coef_df = function(df) {
  restore.point("rrd_html_prepare_coef_df")

  if (!"eq" %in% names(df)) {
    df$eq = ""
  }
  if (!"coef" %in% names(df)) {
    df$coef = NA_real_
  }
  if (!"se" %in% names(df)) {
    df$se = NA_real_
  }

  df$eq = as.character(df$eq)
  df$eq[is.na(df$eq)] = ""
  df$cterm = as.character(df$cterm)
  df$coef = suppressWarnings(as.numeric(df$coef))
  df$se = suppressWarnings(as.numeric(df$se))

  df = df[!duplicated(df[, c("eq", "cterm"), drop = FALSE]), , drop = FALSE]
  df[, c("eq", "cterm", "coef", "se"), drop = FALSE]
}


rrd_html_num_diff = function(x, y, tol = 1e-6) {
  restore.point("rrd_html_num_diff")

  missing_one = xor(is.na(x), is.na(y))
  diff = abs(x - y)
  missing_one | (!is.na(diff) & diff > tol)
}


rrd_html_coef_se_text = function(coef, se) {
  coef_txt = rrd_html_fmt_num(coef)
  se_txt = rrd_html_fmt_num(se)

  ifelse(
    is.na(se),
    coef_txt,
    paste0(coef_txt, " (", se_txt, ")")
  )
}


rrd_html_fmt_num = function(x, digits = 5) {
  x = suppressWarnings(as.numeric(x))
  ifelse(
    is.na(x),
    "NA",
    formatC(x, digits = digits, format = "fg", flag = "#")
  )
}


rrd_html_diff_table_html = function(df) {
  restore.point("rrd_html_diff_table_html")

  if (is.null(df) || NROW(df) == 0) return("")

  head = paste0(
    "<thead><tr>",
    paste0("<th>", htmltools::htmlEscape(names(df)), "</th>", collapse = ""),
    "</tr></thead>"
  )

  rows = vapply(seq_len(NROW(df)), function(i) {
    vals = vapply(df[i, , drop = FALSE], function(x) {
      htmltools::htmlEscape(as.character(x[[1]]))
    }, character(1))

    vals[length(vals)] = paste0('<span class="rrd-status-bad">', vals[length(vals)], '</span>')

    paste0("<tr>", paste0("<td>", vals, "</td>", collapse = ""), "</tr>")
  }, character(1))

  paste0(
    '<table class="rrd-diff-table">',
    head,
    "<tbody>",
    paste0(rows, collapse = "\n"),
    "</tbody></table>"
  )
}


rrd_html_report_panel = function(issue_df) {
  restore.point("rrd_html_report_panel")

  htmltools::tagList(
    htmltools::tags$div(
      class = "rrd-report-titlebar",
      htmltools::tags$div(class = "rrd-report-title active", "Issues")
    ),
    htmltools::tags$div(
      class = "rrd-report-tab-content",
      htmltools::tags$div(
        id = "rrd-report-issues",
        class = "rrd-report-tab-pane active",
        rrd_html_issue_panel(issue_df)
      )
    )
  )
}


rrd_html_issue_panel = function(issue_df) {
  restore.point("rrd_html_issue_panel")

  if (NROW(issue_df) == 0) {
    return(htmltools::tags$div(
      class = "rrd-no-issues",
      htmltools::tags$h4("No issues found"),
      htmltools::tags$p("No command errors or regcheck issues were found for the included do files.")
    ))
  }

  items = lapply(seq_len(NROW(issue_df)), function(i) {
    row = issue_df[i, , drop = FALSE]

    type_class = if (row$issue_type[1] == "error") {
      "rrd-issue-error"
    } else {
      "rrd-issue-regcheck"
    }

    details_html = row$details_html[1]
    if (is.na(details_html)) details_html = ""

    line_txt = ifelse(is.na(row$line[1]), "line NA", paste0("line ", row$line[1]))
    runid_txt = ifelse(nzchar(row$runids[1]), paste0("runid ", row$runids[1]), "runid NA")

    htmltools::tags$div(
      id = row$issue_id[1],
      class = paste0("rrd-issue-item ", type_class),
      `data-file-idx` = row$file_idx[1],
      `data-line` = row$line[1],
      `data-cmd-id` = row$cmd_id[1],
      htmltools::tags$div(
        class = "rrd-issue-main",
        htmltools::tags$div(
          class = "rrd-issue-head",
          htmltools::tags$div(class = "rrd-issue-title", row$issue_title[1]),
          htmltools::tags$div(
            class = "rrd-issue-right",
            htmltools::tags$span(class = "rrd-issue-line", line_txt),
            htmltools::tags$span(class = "rrd-issue-runids", runid_txt)
          )
        ),
        htmltools::tags$pre(
          class = "rrd-issue-cmd",
          htmltools::htmlEscape(row$cmdline[1])
        ),
        if (nzchar(details_html)) {
          htmltools::tags$details(
            class = "rrd-issue-details",
            htmltools::tags$summary("Details"),
            htmltools::HTML(details_html)
          )
        } else {
          NULL
        }
      )
    )
  })

  htmltools::tagList(
    htmltools::tags$div(
      class = "rrd-panel-intro",
      htmltools::tags$strong(NROW(issue_df)),
      " issues"
    ),
    htmltools::tags$div(class = "rrd-issue-list", items)
  )
}


rrd_html_pre_details = function(txt) {
  if (is.null(txt) || length(txt) == 0 || is.na(txt) || !nzchar(txt)) {
    return("")
  }

  paste0(
    '<pre class="rrd-details-pre"><code>',
    htmltools::htmlEscape(txt),
    '</code></pre>'
  )
}


rrd_html_join_words = function(x) {
  x = x[nzchar(x)]
  n = length(x)
  if (n == 0) return("")
  if (n == 1) return(x[[1]])
  if (n == 2) return(paste0(x[[1]], " and ", x[[2]]))
  paste0(paste0(x[seq_len(n - 1)], collapse = ", "), " and ", x[[n]])
}


rrd_html_attr = function(x) {
  x = as.character(x)
  x[is.na(x)] = ""
  htmltools::htmlEscape(x, attribute = TRUE)
}

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
  reg_lines = integer(0)
  problem_lines = integer(0)
  error_lines = integer(0)

  if (!is.null(cmd_df) && NROW(cmd_df) > 0) {
    attach_line = suppressWarnings(as.integer(cmd_df$rrd_attach_line))
    attach_line = attach_line[!is.na(attach_line) & attach_line > 0]

    if ("is_reg" %in% names(cmd_df)) {
      reg_lines = suppressWarnings(as.integer(cmd_df$rrd_attach_line[rrd_as_logical(cmd_df$is_reg)]))
      reg_lines = reg_lines[!is.na(reg_lines) & reg_lines > 0]
    }

    if ("rrd_has_problem_reg" %in% names(cmd_df)) {
      problem_lines = suppressWarnings(as.integer(cmd_df$rrd_attach_line[rrd_as_logical(cmd_df$rrd_has_problem_reg)]))
      problem_lines = problem_lines[!is.na(problem_lines) & problem_lines > 0]
    }

    error_rows = vapply(seq_len(NROW(cmd_df)), function(i) {
      rrd_cmd_has_error(cmd_df[i, , drop = FALSE])
    }, logical(1))

    if (any(error_rows)) {
      error_lines = suppressWarnings(as.integer(cmd_df$rrd_attach_line[error_rows]))
      error_lines = error_lines[!is.na(error_lines) & error_lines > 0]
    }
  }

  line_nodes = vector("list", length(lines) * 2L)
  pos = 0L

  for (i in seq_along(lines)) {
    line_class = "rrd-code-row"
    if (i %in% reg_lines) line_class = paste(line_class, "rrd-reg-line")
    if (i %in% problem_lines) line_class = paste(line_class, "rrd-problem-reg-line")
    if (i %in% error_lines) line_class = paste(line_class, "rrd-error-line")

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
        htmltools::tags$code(htmltools::htmlEscape(lines[i]))
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
            htmltools::tags$code(htmltools::htmlEscape(output_by_line[[key]]))
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
      htmltools::tags$span(class = "rrd-file-path", htmltools::htmlEscape(file_path))
    ),
    htmltools::tags$div(class = "rrd-code-lines", line_nodes)
  )
}


rrd_html_do_panel_html = function(do_df, cmd_df, parcels, opts) {
  restore.point("rrd_html_do_panel_html")

  if (is.null(do_df) || NROW(do_df) == 0) {
    return(htmltools::tags$div(
      class = "rrd-empty",
      htmltools::tags$h4("No do files"),
      htmltools::tags$p("No Stata do files were found for this report.")
    ))
  }

  tab_buttons = lapply(seq_len(NROW(do_df)), function(i) {
    file_label = do_df$file_name[i]
    if (is.na(file_label) || !nzchar(file_label)) {
      file_label = basename(do_df$file_path[i])
    }

    htmltools::tags$button(
      type = "button",
      class = paste0("rrd-do-tab-btn", if (i == 1) " active" else ""),
      `data-tab-target` = paste0("rrd-do-tab-", i),
      `data-file-idx` = i,
      htmltools::htmlEscape(file_label)
    )
  })

  tab_panes = lapply(seq_len(NROW(do_df)), function(i) {
    file_cmd_df = rrd_cmds_for_file(cmd_df, do_df$file_path[i])
    rrd_html_render_do_file(
      do_row = do_df[i, , drop = FALSE],
      cmd_df = file_cmd_df,
      parcels = parcels,
      opts = opts,
      file_idx = i
    )
  })

  htmltools::tagList(
    htmltools::tags$div(class = "rrd-do-tabs", tab_buttons),
    htmltools::tags$div(class = "rrd-do-tab-content", tab_panes)
  )
}
