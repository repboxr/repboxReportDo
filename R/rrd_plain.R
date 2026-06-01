example = function() {
  project_dir = "~/repbox/projects/aer_101_2_5"
  opts = rrd_opts()
  rrd_plain_do(project_dir, opts = opts)
  rstudioapi::filesPaneNavigate(project_dir)
}


#' Options for report
#' @param show_regcheck Shall all problems noted in regcheck parcel be shown for regressions
#' @param show_all_err Shall all errors in any command be noted in output
#' @param show_all_log Shall the logs of all commands be shown, not only regression logs or logs with error
#' @param show_reg_log Shall the log of regression commands be shown
#' @param only_do_with_reg Shall only do files that have regressions shall be included in report
#' @param only_do_with_prob_reg Shall only do files that have regressions with a regcheck problem be considered
#' @param split_by_do Shall we generate separate output files for each do file?
#' @param show_line_num Shall line numbers be shown before original do-file lines?
#' @param show_nonreg_issue_errors Shall non-regression command errors be shown in the HTML issue list?
#' @param df_exclude_vars Character vector of variables to exclude from printed data frames
#' @param df_long_nchar Minimum number of characters after which one-row string variables are printed as long variables
#' @param df_long_max_chars Optional maximum string length for all long variables. By default, no shortening is applied.
#' @param df_long_max_chars_by_var Optional named numeric vector or list with variable-specific maximum string lengths for long variables.
#' @param max_runs_shown Max number of runs (e.g. from a loop) to show output for per command.
#' @param max_regcheck_runs_shown Max number of runs (e.g. from a loop) to show regcheck details for per command.
rrd_opts = function(
  show_regcheck = TRUE,
  show_all_err = TRUE,
  show_all_log = FALSE,
  show_reg_log = TRUE,
  only_do_with_reg = TRUE,
  only_do_with_prob_reg = FALSE,
  split_by_do = FALSE,
  show_line_num = TRUE,
  show_nonreg_issue_errors = FALSE,
  df_exclude_vars = character(0),
  df_long_nchar = 10,
  df_long_max_chars = NULL,
  df_long_max_chars_by_var = NULL,
  max_runs_shown = 5,
  max_regcheck_runs_shown = 5
) {
  as.list(environment())
}


# A plain text "report" that shows original do files combined with selected
# output information as specified by opts.
#
# The plain version is mainly intended as a diagnostic tool for AI to detect
# problems in the repbox pipeline for reproductions including metaregBase
# replications. HTML reports that are easier for humans to read can be
# generated with rrd_html_do instead.
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

  if (NROW(cmd_df) > 0) {
    cmd_df$rrd_has_run_output = rrd_cmd_has_run_output(cmd_df, parcels, opts = opts)
    cmd_df$rrd_has_problem_reg = rrd_has_problem_reg(cmd_df, parcels = parcels)
    cmd_df$rrd_reg_status = rrd_cmd_reg_status(cmd_df, parcels = parcels)

    cache_dir = file.path(project_dir, "drf", "cached_dta")
    cache_runids = integer(0)
    if (dir.exists(cache_dir)) {
      cache_files = list.files(cache_dir, pattern = "_cache\\.dta$")
      cache_runids = suppressWarnings(as.integer(stringi::stri_replace_first_regex(cache_files, "_cache\\.dta$", "")))
      cache_runids = cache_runids[!is.na(cache_runids)]
    }
    cmd_df$rrd_has_cache = vapply(seq_len(NROW(cmd_df)), function(i) {
      runids = rrd_cmd_runids(cmd_df[i, , drop = FALSE], parcels = parcels)
      any(runids %in% cache_runids)
    }, logical(1))
  } else {
    cmd_df$rrd_has_run_output = logical(0)
    cmd_df$rrd_has_problem_reg = logical(0)
    cmd_df$rrd_reg_status = character(0)
    cmd_df$rrd_has_cache = logical(0)
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


rrd_get_do_files = function(project_dir, parcels = list()) {
  restore.point("rrd_get_do_files")

  df = NULL

  if (!is.null(parcels$stata_source) && NROW(parcels$stata_source) > 0) {
    df = as.data.frame(parcels$stata_source)
  } else if (!is.null(parcels$stata_file) && NROW(parcels$stata_file) > 0) {
    df = as.data.frame(parcels$stata_file)
  }

  if (!is.null(df)) {
    if (!"file_path" %in% names(df)) {
      if ("script_path" %in% names(df)) {
        df$file_path = df$script_path
      } else if ("file" %in% names(df)) {
        df$file_path = df$file
      } else if ("path" %in% names(df)) {
        df$file_path = df$path
      } else if ("file_name" %in% names(df)) {
        df$file_path = df$file_name
      } else {
        df$file_path = paste0("do_file_", seq_len(NROW(df)), ".do")
      }
    }

    if (!"text" %in% names(df)) {
      if ("source" %in% names(df)) {
        df$text = df$source
      } else if ("code" %in% names(df)) {
        df$text = df$code
      } else {
        df$text = NA_character_
      }
    }

    df$file_path = as.character(df$file_path)
    df$text = as.character(df$text)

    missing_text = is.na(df$text) | !nzchar(df$text)
    if (any(missing_text)) {
      df$text[missing_text] = vapply(
        df$file_path[missing_text],
        function(file_path) rrd_read_project_do_file(project_dir, file_path),
        character(1)
      )
    }

    keep = !is.na(df$text) & nzchar(df$text)
    df = df[keep, , drop = FALSE]

    if (!"file_name" %in% names(df)) {
      df$file_name = basename(df$file_path)
    }

    df = df[, unique(c("file_path", "file_name", "text")), drop = FALSE]
    rownames(df) = NULL

    return(df)
  }

  org_dir = file.path(project_dir, "org")
  files = list.files(org_dir, pattern = glob2rx("*.do"), recursive = TRUE, full.names = FALSE)

  if (length(files) == 0) {
    return(data.frame(file_path = character(0), file_name = character(0), text = character(0)))
  }

  text = vapply(files, function(file_path) rrd_read_project_do_file(project_dir, file_path), character(1))

  data.frame(
    file_path = files,
    file_name = basename(files),
    text = text,
    stringsAsFactors = FALSE
  )
}


rrd_read_project_do_file = function(project_dir, file_path) {
  restore.point("rrd_read_project_do_file")

  candidates = c(
    file_path,
    file.path(project_dir, "org", file_path),
    file.path(project_dir, "mod", file_path),
    file.path(project_dir, file_path)
  )
  candidates = candidates[!is.na(candidates) & nzchar(candidates)]
  candidates = candidates[file.exists(candidates)]

  if (length(candidates) == 0) {
    return(NA_character_)
  }

  paste0(readLines(candidates[1], warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}


rrd_get_cmd_df = function(project_dir, parcels = list()) {
  restore.point("rrd_get_cmd_df")

  if (is.null(parcels$stata_cmd) || NROW(parcels$stata_cmd) == 0) {
    return(data.frame())
  }

  cmd_df = as.data.frame(parcels$stata_cmd)
  cmd_df = rrd_normalize_stata_cmd(cmd_df)
  cmd_df = rrd_attach_run_cmd_info(cmd_df, parcels = parcels)

  cmd_df$rrd_attach_line = ifelse(
    !is.na(cmd_df$orgline_end),
    cmd_df$orgline_end,
    ifelse(!is.na(cmd_df$orgline_start), cmd_df$orgline_start, cmd_df$orgline)
  )

  cmd_df = cmd_df[order(cmd_df$file_path, cmd_df$rrd_attach_line, cmd_df$line, cmd_df$runid), , drop = FALSE]
  rownames(cmd_df) = NULL

  cmd_df
}


rrd_normalize_stata_cmd = function(cmd_df) {
  restore.point("rrd_normalize_stata_cmd")

  if (!"file_path" %in% names(cmd_df)) {
    if ("script_path" %in% names(cmd_df)) {
      cmd_df$file_path = cmd_df$script_path
    } else if ("found_path" %in% names(cmd_df)) {
      cmd_df$file_path = cmd_df$found_path
    } else if ("file" %in% names(cmd_df)) {
      cmd_df$file_path = cmd_df$file
    } else {
      cmd_df$file_path = NA_character_
    }
  }

  if (!"line" %in% names(cmd_df)) {
    cmd_df$line = NA_integer_
  }

  if (!"orgline" %in% names(cmd_df)) {
    if ("orgline_start" %in% names(cmd_df)) {
      cmd_df$orgline = cmd_df$orgline_start
    } else if ("code_line_start" %in% names(cmd_df)) {
      cmd_df$orgline = cmd_df$code_line_start
    } else {
      cmd_df$orgline = cmd_df$line
    }
  }

  if (!"orgline_start" %in% names(cmd_df)) {
    if ("code_line_start" %in% names(cmd_df)) {
      cmd_df$orgline_start = cmd_df$code_line_start
    } else {
      cmd_df$orgline_start = cmd_df$orgline
    }
  }

  if (!"orgline_end" %in% names(cmd_df)) {
    if ("code_line_end" %in% names(cmd_df)) {
      cmd_df$orgline_end = cmd_df$code_line_end
    } else {
      cmd_df$orgline_end = cmd_df$orgline_start
    }
  }

  if (!"cmdline" %in% names(cmd_df)) {
    if ("code" %in% names(cmd_df)) {
      cmd_df$cmdline = cmd_df$code
    } else if ("cmd" %in% names(cmd_df)) {
      cmd_df$cmdline = cmd_df$cmd
    } else {
      cmd_df$cmdline = ""
    }
  }

  if (!"is_reg" %in% names(cmd_df)) {
    cmd_df$is_reg = FALSE
  }

  if (!"runid" %in% names(cmd_df)) {
    cmd_df$runid = NA_integer_
  }

  cmd_df$file_path = as.character(cmd_df$file_path)
  cmd_df$cmdline = as.character(cmd_df$cmdline)
  cmd_df$line = suppressWarnings(as.integer(cmd_df$line))
  cmd_df$orgline = suppressWarnings(as.integer(cmd_df$orgline))
  cmd_df$orgline_start = suppressWarnings(as.integer(cmd_df$orgline_start))
  cmd_df$orgline_end = suppressWarnings(as.integer(cmd_df$orgline_end))
  cmd_df$runid = suppressWarnings(as.integer(cmd_df$runid))
  cmd_df$is_reg = rrd_as_logical(cmd_df$is_reg)

  cmd_df
}


rrd_attach_run_cmd_info = function(cmd_df, parcels = list()) {
  restore.point("rrd_attach_run_cmd_info")

  run_cmd = parcels$stata_run_cmd
  if (is.null(run_cmd) || NROW(run_cmd) == 0) {
    cmd_df$rrd_run_match = FALSE
    return(cmd_df)
  }

  run_cmd = as.data.frame(run_cmd)
  run_cmd = rrd_normalize_stata_run_cmd(run_cmd)

  cmd_file_norm = rrd_norm_path(cmd_df$file_path)
  cmd_key_file_line = paste0(cmd_file_norm, "\r", cmd_df$line)

  match_list = lapply(seq_len(NROW(cmd_df)), function(i) {
    idx = which(run_cmd$rrd_key_file_line == cmd_key_file_line[i])
    if (length(idx) == 0) {
       idx = which(run_cmd$rrd_key_line == as.character(cmd_df$line[i]))
    }
    idx
  })

  cmd_df$rrd_run_match = lengths(match_list) > 0

  run_cols = setdiff(
    names(run_cmd),
    c("file_path", "line", "rrd_file_norm", "rrd_key_file_line", "rrd_key_line")
  )

  for (col in run_cols) {
    vals = lapply(match_list, function(idx) {
       if (length(idx) == 0) return(NA)
       run_cmd[[col]][idx]
    })

    first_vals = sapply(vals, function(x) x[[1]])

    if (col == "runid") {
      cmd_df$runid = ifelse(is.na(cmd_df$runid), suppressWarnings(as.integer(first_vals)), cmd_df$runid)
      cmd_df$all_runids = lapply(vals, function(x) {
         res = suppressWarnings(as.integer(x))
         res[!is.na(res)]
      })
    } else if (col %in% names(cmd_df)) {
      cmd_df[[paste0("run_", col)]] = first_vals
    } else {
      cmd_df[[col]] = first_vals
    }

    if (col %in% c("errcode", "rc", "error_code", "stata_rc")) {
       cmd_df[[paste0("any_", col)]] = sapply(vals, function(x) {
          any(suppressWarnings(as.numeric(x)) != 0, na.rm = TRUE)
       })
    }
  }

  cmd_df
}


rrd_normalize_stata_run_cmd = function(run_cmd) {
  restore.point("rrd_normalize_stata_run_cmd")

  if (!"file_path" %in% names(run_cmd)) {
    if ("script_path" %in% names(run_cmd)) {
      run_cmd$file_path = run_cmd$script_path
    } else if ("found_path" %in% names(run_cmd)) {
      run_cmd$file_path = run_cmd$found_path
    } else if ("file" %in% names(run_cmd)) {
      run_cmd$file_path = run_cmd$file
    } else {
      run_cmd$file_path = NA_character_
    }
  }

  if (!"line" %in% names(run_cmd)) {
    run_cmd$line = NA_integer_
  }

  if (!"runid" %in% names(run_cmd)) {
    run_cmd$runid = NA_integer_
  }

  run_cmd$file_path = as.character(run_cmd$file_path)
  run_cmd$line = suppressWarnings(as.integer(run_cmd$line))
  run_cmd$runid = suppressWarnings(as.integer(run_cmd$runid))

  run_cmd$rrd_file_norm = rrd_norm_path(run_cmd$file_path)
  run_cmd$rrd_key_file_line = paste0(run_cmd$rrd_file_norm, "\r", run_cmd$line)
  run_cmd$rrd_key_line = as.character(run_cmd$line)

  run_cmd
}


rrd_match_run_cmd_pos = function(cmd_df, run_cmd) {
  restore.point("rrd_match_run_cmd_pos")

  cmd_file_norm = rrd_norm_path(cmd_df$file_path)
  cmd_key_file_line = paste0(cmd_file_norm, "\r", cmd_df$line)
  run_key_file_line = run_cmd$rrd_key_file_line

  pos = match(cmd_key_file_line, run_key_file_line)

  missing = is.na(pos)
  if (any(missing)) {
    pos[missing] = match(as.character(cmd_df$line[missing]), run_cmd$rrd_key_line)
  }

  pos
}


rrd_report_header = function(project_dir, opts, num_do_files, num_cmds, parcels = list()) {
  restore.point("rrd_report_header")

  opt_lines = paste0(
    "  ",
    names(opts),
    " = ",
    vapply(opts, function(x) paste0(as.character(x), collapse = ", "), character(1))
  )

  parcel_lines = rrd_parcel_summary_lines(parcels)

  paste0(
    "# repboxReportDo plain do report\n\n",
    "Project dir: ", project_dir, "\n",
    "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "Included do files: ", num_do_files, "\n",
    "Known Stata command rows from stata_cmd: ", num_cmds, "\n\n",
    "Loaded parcel summary:\n",
    paste0(parcel_lines, collapse = "\n"),
    "\n\n",
    "Options:\n",
    paste0(opt_lines, collapse = "\n"),
    "\n\n",
    "Report convention: selected diagnostics are inserted directly below the Stata command line ",
    "to which they most likely belong. Regression commands are identified only by stata_cmd$is_reg.\n"
  )
}


rrd_parcel_summary_lines = function(parcels = list()) {
  names = c("stata_source", "stata_cmd", "stata_run_cmd", "stata_run_log", "regcheck")

  vapply(names, function(name) {
    obj = parcels[[name]]
    n = if (is.null(obj)) 0L else NROW(obj)
    paste0("  ", name, ": ", n, " rows")
  }, character(1))
}


rrd_render_do_file = function(do_row, cmd_df, parcels, opts) {
  restore.point("rrd_render_do_file")

  file_path = do_row$file_path[1]
  txt = do_row$text[1]
  lines = stringi::stri_split_lines1(txt)

  if (length(lines) == 0) {
    lines = ""
  }

  line_prefix = if (isTRUE(opts$show_line_num)) {
    sprintf("%5d  ", seq_along(lines))
  } else {
    rep("", length(lines))
  }

  out = paste0(
    "\n\n",
    strrep("=", 80),
    "\nDO FILE: ", file_path,
    "\n",
    strrep("=", 80),
    "\n\n"
  )

  output_by_line = rrd_outputs_by_line(cmd_df, parcels = parcels, opts = opts)

  rendered = character(0)
  for (i in seq_along(lines)) {
    rendered = c(rendered, paste0(line_prefix[i], lines[i]))

    key = as.character(i)
    if (!is.null(output_by_line[[key]]) && nzchar(output_by_line[[key]])) {
      rendered = c(rendered, output_by_line[[key]])
    }
  }

  paste0(out, paste0(rendered, collapse = "\n"), "\n")
}


rrd_outputs_by_line = function(cmd_df, parcels, opts) {
  restore.point("rrd_outputs_by_line")

  res = list()
  if (is.null(cmd_df) || NROW(cmd_df) == 0) return(res)

  for (i in seq_len(NROW(cmd_df))) {
    cmd = cmd_df[i, , drop = FALSE]
    line = suppressWarnings(as.integer(cmd$rrd_attach_line[1]))
    if (is.na(line) || line <= 0) next

    txt = rrd_format_cmd_output(cmd, parcels = parcels, opts = opts)
    if (!nzchar(txt)) next

    key = as.character(line)
    if (is.null(res[[key]])) {
      res[[key]] = txt
    } else {
      res[[key]] = paste0(res[[key]], "\n", txt)
    }
  }

  res
}


rrd_format_cmd_output = function(cmd, parcels, opts) {
  restore.point("rrd_format_cmd_output")

  is_reg = isTRUE(cmd$is_reg[1])
  has_error = rrd_cmd_has_error(cmd)
  has_run_output = isTRUE(cmd$rrd_has_run_output[1])

  show =
    isTRUE(opts$show_all_log) ||
    (isTRUE(opts$show_reg_log) && is_reg) ||
    (isTRUE(opts$show_all_err) && has_error) ||
    (isTRUE(opts$show_regcheck) && is_reg)

  if (!show) return("")

  runids = rrd_cmd_runids(cmd, parcels = parcels)

  parts = character(0)

  if (isTRUE(opts$show_all_err) && has_error) {
    err_txt = rrd_cmd_error_text(cmd)
    if (nzchar(err_txt)) {
      parts = c(parts, paste0("[command error]\n", err_txt))
    }
  }

  if (isTRUE(opts$show_regcheck) && is_reg) {
    regcheck_txt = rrd_regcheck_text(runids, parcels = parcels, opts = opts)
    if (nzchar(regcheck_txt)) {
      parts = c(parts, paste0("[regcheck]\n", regcheck_txt))
    }
  }

  show_log =
    isTRUE(opts$show_all_log) ||
    (isTRUE(opts$show_reg_log) && is_reg) ||
    (isTRUE(opts$show_all_err) && has_error)

  if (show_log) {
    log_txt = rrd_cmd_log_text(cmd, parcels = parcels, opts = opts)
    if (nzchar(log_txt)) {
      parts = c(parts, paste0("[stata output]\n", log_txt))
    } else if (has_run_output || is_reg) {
      debug_txt = rrd_cmd_match_debug_text(cmd, runids = runids)
      if (nzchar(debug_txt)) {
        parts = c(parts, paste0("[no extracted log text found; matched run command row]\n", debug_txt))
      }
    }
  }

  if (length(parts) == 0) return("")

  runid_lines = if (length(runids) == 0) {
    "runid=NA"
  } else {
    paste0("runid=", runids)
  }

  block = paste0(
    "\n",
    rrd_indent("----- repbox output below this command", 2),
    "\n",
    rrd_indent(paste0(runid_lines, collapse = "\n"), 2),
    "\n",
    rrd_indent(paste0(parts, collapse = "\n\n"), 2),
    "\n",
    rrd_indent("----- end repbox output", 2)
  )

  block
}


rrd_cmd_runids = function(cmd, parcels = list()) {
  restore.point("rrd_cmd_runids")

  ids = integer(0)

  if ("all_runids" %in% names(cmd) && is.list(cmd$all_runids)) {
    ids = c(ids, cmd$all_runids[[1]])
  }

  if ("runid" %in% names(cmd)) {
    ids = c(ids, suppressWarnings(as.integer(cmd$runid)))
  }

  if ("run_runid" %in% names(cmd)) {
    ids = c(ids, suppressWarnings(as.integer(cmd$run_runid)))
  }

  ids = unique(ids[!is.na(ids)])
  ids
}


rrd_cmd_log_text = function(cmd, parcels = list(), opts = rrd_opts()) {
  restore.point("rrd_cmd_log_text")

  pieces = character(0)

  run_cmd_txt = rrd_cmd_inline_output_text(cmd)
  if (nzchar(run_cmd_txt)) {
    pieces = c(pieces, run_cmd_txt)
  }

  runids = rrd_cmd_runids(cmd, parcels = parcels)
  run_log_txt = rrd_stata_run_log_text(runids = runids, parcels = parcels, opts = opts)
  if (nzchar(run_log_txt)) {
    pieces = c(pieces, run_log_txt)
  }

  pieces = pieces[nzchar(pieces)]
  if (length(pieces) == 0) return("")

  paste0(pieces, collapse = "\n\n")
}


rrd_cmd_inline_output_text = function(cmd) {
  restore.point("rrd_cmd_inline_output_text")

  text_cols = intersect(
    c(
      "log",
      "logtxt",
      "log_text",
      "cmd_log",
      "output",
      "out",
      "stdout",
      "stderr",
      "text",
      "result",
      "msg",
      "message",
      "err_msg",
      "error_msg",
      "run_log",
      "run_logtxt",
      "run_log_text",
      "run_cmd_log",
      "run_output",
      "run_out",
      "run_stdout",
      "run_stderr",
      "run_text",
      "run_result",
      "run_msg",
      "run_message",
      "run_err_msg",
      "run_error_msg"
    ),
    names(cmd)
  )

  pieces = character(0)

  for (col in text_cols) {
    vals = rrd_chr_vec(cmd[[col]])
    vals = vals[nzchar(vals)]
    if (length(vals) > 0) {
      pieces = c(pieces, paste0("stata_run_cmd.", col, ":\n", paste0(vals, collapse = "\n")))
    }
  }

  paste0(pieces, collapse = "\n\n")
}


rrd_stata_run_log_text = function(runids, parcels = list(), opts = rrd_opts()) {
  restore.point("rrd_stata_run_log_text")

  runids = unique(suppressWarnings(as.integer(runids)))
  runids = runids[!is.na(runids)]
  if (length(runids) == 0) return("")

  run_log = parcels$stata_run_log
  if (is.null(run_log) || NROW(run_log) == 0) return("")

  run_log = as.data.frame(run_log)

  if (!"runid" %in% names(run_log)) {
    return("")
  }

  log_runid = suppressWarnings(as.integer(run_log$runid))
  rows = which(log_runid %in% runids)
  if (length(rows) == 0) return("")

  max_runs = opts$max_runs_shown
  if (is.null(max_runs)) max_runs = 5

  n_runs = length(rows)
  if (n_runs > max_runs) {
     rows = rows[seq_len(max_runs)]
  }

  run_log = run_log[rows, , drop = FALSE]

  text_cols = intersect(
    c(
      "log",
      "logtxt",
      "log_text",
      "cmd_log",
      "output",
      "out",
      "stdout",
      "stderr",
      "text",
      "result",
      "msg",
      "message",
      "err_msg",
      "error_msg"
    ),
    names(run_log)
  )

  pieces = character(0)

  if (n_runs > 1) {
     pieces = c(pieces, paste0("[Showing logs for ", length(rows), " of ", n_runs, " runs in this loop]"))
  }

  for (col in text_cols) {
    vals = sapply(seq_len(NROW(run_log)), function(i) {
       v = rrd_chr_vec(run_log[[col]][i])
       v = v[nzchar(v)]
       if (length(v) > 0) {
          if (n_runs > 1) paste0("--- runid ", run_log$runid[i], " ---\n", paste0(v, collapse = "\n"))
          else paste0(v, collapse = "\n")
       } else ""
    })
    vals = vals[nzchar(vals)]

    if (length(vals) > 0) {
      pieces = c(pieces, paste0("stata_run_log.", col, ":\n", paste0(vals, collapse = "\n\n")))
    }
  }

  file_pieces = rrd_stata_run_log_file_text(run_log, parcels = parcels)
  if (length(file_pieces) > 0) {
    pieces = c(pieces, file_pieces)
  }

  if (length(pieces) == (if (n_runs > 1) 1 else 0)) {
    pieces = c(pieces, rrd_df_to_text(run_log, opts = opts))
  }

  pieces = pieces[nzchar(pieces)]
  paste0(pieces, collapse = "\n\n")
}


rrd_stata_run_log_file_text = function(run_log, parcels = list()) {
  restore.point("rrd_stata_run_log_file_text")

  file_cols = intersect(
    c("log_file", "file", "path", "log_path", "smcl_file", "txt_file"),
    names(run_log)
  )

  if (length(file_cols) == 0) {
    return(character(0))
  }

  project_dir = attr(parcels, "project_dir")
  pieces = character(0)

  for (col in file_cols) {
    files = rrd_chr_vec(run_log[[col]])
    files = files[nzchar(files)]

    if (!is.null(project_dir)) {
      files = ifelse(file.exists(files), files, file.path(project_dir, files))
    }

    files = files[file.exists(files)]

    if (length(files) > 0) {
      txt = vapply(files, function(file) {
        lines = readLines(file, warn = FALSE, encoding = "UTF-8")
        paste0("stata_run_log.", col, " file ", file, ":\n", paste0(lines, collapse = "\n"))
      }, character(1))
      pieces = c(pieces, txt)
    }
  }

  pieces
}


rrd_regcheck_text = function(runids, parcels = list(), opts = rrd_opts()) {
  restore.point("rrd_regcheck_text")

  runids = unique(suppressWarnings(as.integer(runids)))
  runids = runids[!is.na(runids)]
  if (length(runids) == 0) return("")

  regcheck = parcels$regcheck
  if (is.null(regcheck) || NROW(regcheck) == 0 || !"runid" %in% names(regcheck)) {
    return("")
  }

  df = as.data.frame(regcheck)
  df = df[df$runid %in% runids, , drop = FALSE]
  if (NROW(df) == 0) return("")

  has_problem = rep(TRUE, NROW(df))

  if ("reg_ok" %in% names(df)) {
    has_problem = !rrd_as_logical(df$reg_ok)
  }

  if ("problem" %in% names(df)) {
    has_problem = has_problem | (!is.na(df$problem) & nzchar(as.character(df$problem)))
  }

  if ("comment" %in% names(df)) {
    has_problem = has_problem | (!is.na(df$comment) & nzchar(as.character(df$comment)))
  }

  df = df[has_problem, , drop = FALSE]
  if (NROW(df) == 0) return("")

  max_runs = opts$max_regcheck_runs_shown
  if (is.null(max_runs)) max_runs = 5

  n_runs = NROW(df)
  if (n_runs > max_runs) {
     df = df[seq_len(max_runs), , drop = FALSE]
  }

  keep_cols = intersect(
    c(
      "runid",
      "reg_ok",
      "so_did_run",
      "sb_did_run",
      "rb_did_run",
      "so_raw_did_run",
      "sb_raw_did_run",
      "sb_so_identical",
      "rb_sb_coef_same",
      "rb_sb_se_same",
      "sb_so_coef_max_rel",
      "sb_so_se_max_rel",
      "rb_sb_coef_max_rel",
      "rb_sb_se_max_rel",
      "repair_code",
      "problem",
      "comment"
    ),
    names(df)
  )

  if (length(keep_cols) > 0) {
    df = df[, keep_cols, drop = FALSE]
  }

  txt = rrd_df_to_text(df, opts = opts)
  if (n_runs > max_runs) {
     txt = paste0(txt, "\n... and ", n_runs - max_runs, " more runs with issues.")
  }
  txt
}


rrd_cmd_has_error = function(cmd) {
  restore.point("rrd_cmd_has_error")

  err_fields = intersect(c("errcode", "rc", "error_code", "stata_rc", "run_errcode", "run_rc"), names(cmd))
  for (field in err_fields) {
    any_field = paste0("any_", field)
    if (any_field %in% names(cmd)) {
       if (isTRUE(cmd[[any_field]][1])) return(TRUE)
    } else {
       val = suppressWarnings(as.numeric(cmd[[field]][1]))
       if (!is.na(val) && val != 0) return(TRUE)
    }
  }

  msg_fields = intersect(c("err_msg", "error_msg", "error", "stderr", "run_err_msg", "run_error_msg"), names(cmd))
  for (field in msg_fields) {
    val = rrd_chr_vec(cmd[[field]][1])
    if (length(val) > 0 && any(nzchar(val))) return(TRUE)
  }

  FALSE
}
rrd_cmd_error_runids = function(cmd, parcels = list()) {
  restore.point("rrd_cmd_error_runids")

  runids = rrd_cmd_runids(cmd, parcels = parcels)
  if (length(runids) == 0) return(integer(0))

  run_cmd = parcels$stata_run_cmd
  if (is.null(run_cmd) || NROW(run_cmd) == 0) return(integer(0))

  df = as.data.frame(run_cmd)
  df = df[df$runid %in% runids, , drop = FALSE]
  if (NROW(df) == 0) return(integer(0))

  err_mask = rep(FALSE, NROW(df))
  for (field in intersect(c("errcode", "rc", "error_code", "stata_rc"), names(df))) {
    val = suppressWarnings(as.numeric(df[[field]]))
    err_mask = err_mask | (!is.na(val) & val != 0)
  }
  for (field in intersect(c("err_msg", "error_msg", "error", "stderr"), names(df))) {
    val = as.character(df[[field]])
    err_mask = err_mask | (!is.na(val) & nzchar(val))
  }

  unique(suppressWarnings(as.integer(df$runid[err_mask])))
}


rrd_cmd_error_text = function(cmd) {
  restore.point("rrd_cmd_error_text")

  fields = intersect(
    c(
      "errcode",
      "rc",
      "error_code",
      "stata_rc",
      "err_msg",
      "error_msg",
      "error",
      "stderr",
      "run_errcode",
      "run_rc",
      "run_err_msg",
      "run_error_msg"
    ),
    names(cmd)
  )

  if (length(fields) == 0) return("")

  vals = vapply(fields, function(field) {
    val = rrd_chr_vec(cmd[[field]][1])
    val = val[nzchar(val)]
    if (length(val) == 0) return("")
    paste0(field, ": ", paste0(val, collapse = "\n"))
  }, character(1))

  vals = vals[nzchar(vals)]

  # Append a note if there are multiple runs with errors
  has_any_err = FALSE
  for (field in intersect(c("errcode", "rc", "error_code", "stata_rc"), names(cmd))) {
     if (isTRUE(cmd[[paste0("any_", field)]][1])) has_any_err = TRUE
  }

  res = paste0(vals, collapse = "\n")
  if (has_any_err && !nzchar(res)) {
      res = "An error occurred in one or more runs of this command in a loop."
  }

  res
}


rrd_cmd_has_run_output = function(cmd_df, parcels = list(), opts = rrd_opts()) {
  restore.point("rrd_cmd_has_run_output")

  if (is.null(cmd_df) || NROW(cmd_df) == 0) return(logical(0))

  vapply(seq_len(NROW(cmd_df)), function(i) {
    cmd = cmd_df[i, , drop = FALSE]
    runids = rrd_cmd_runids(cmd, parcels = parcels)
    nzchar(rrd_cmd_inline_output_text(cmd)) ||
      nzchar(rrd_stata_run_log_text(runids = runids, parcels = parcels, opts = opts))
  }, logical(1))
}


rrd_cmd_match_debug_text = function(cmd, runids = integer()) {
  restore.point("rrd_cmd_match_debug_text")

  keep = intersect(
    c(
      "file_path",
      "line",
      "orgline",
      "orgline_start",
      "orgline_end",
      "runid",
      "is_reg",
      "rrd_run_match",
      "cmdline"
    ),
    names(cmd)
  )

  if (length(keep) == 0) return("")

  df = cmd[, keep, drop = FALSE]
  rrd_df_to_text(df)
}


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

  vapply(seq_len(NROW(cmd_df)), function(i) {
    cmd = cmd_df[i, , drop = FALSE]
    runids = rrd_cmd_runids(cmd, parcels = parcels)
    any(runids %in% prob_runids)
  }, logical(1))
}


rrd_do_has_cmd_flag = function(file_paths, cmd_df, flag_col) {
  restore.point("rrd_do_has_cmd_flag")

  if (is.null(cmd_df) || NROW(cmd_df) == 0 || !flag_col %in% names(cmd_df)) {
    return(rep(FALSE, length(file_paths)))
  }

  vapply(file_paths, function(file_path) {
    rows = rrd_same_file(cmd_df$file_path, file_path)
    any(rrd_as_logical(cmd_df[[flag_col]][rows]), na.rm = TRUE)
  }, logical(1))
}


rrd_cmds_for_file = function(cmd_df, file_path) {
  restore.point("rrd_cmds_for_file")

  if (is.null(cmd_df) || NROW(cmd_df) == 0) return(data.frame())
  cmd_df[rrd_same_file(cmd_df$file_path, file_path), , drop = FALSE]
}


rrd_same_file = function(x, file_path) {
  restore.point("rrd_same_file")

  x = rrd_norm_path(x)
  file_path = rrd_norm_path(file_path)

  x == file_path |
    basename(x) == basename(file_path) |
    endsWith(x, paste0("/", file_path)) |
    endsWith(file_path, paste0("/", x))
}


rrd_norm_path = function(x) {
  x = as.character(x)
  x[is.na(x)] = ""
  x = stringi::stri_replace_all_fixed(x, "\\", "/")
  x = stringi::stri_replace_all_regex(x, "^.*/org/", "")
  x = stringi::stri_replace_all_regex(x, "^.*/mod/", "")
  x = stringi::stri_replace_all_regex(x, "^/+", "")
  x
}


rrd_as_logical = function(x) {
  if (is.logical(x)) {
    x[is.na(x)] = FALSE
    return(x)
  }

  if (is.numeric(x) || is.integer(x)) {
    x[is.na(x)] = 0
    return(x != 0)
  }

  x = tolower(trimws(as.character(x)))
  x[is.na(x)] = ""
  x %in% c("true", "t", "yes", "y", "1")
}


rrd_chr_vec = function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))

  if (is.list(x) && !is.data.frame(x)) {
    x = unlist(x, recursive = TRUE, use.names = FALSE)
  }

  x = as.character(x)
  x[is.na(x)] = ""
  x
}


rrd_df_to_text = function(
  df,
  max_width = 1000,
  opts = rrd_opts(),
  long_nchar = opts$df_long_nchar,
  exclude_vars = opts$df_exclude_vars,
  max_string_length = opts$df_long_max_chars,
  max_string_length_by_var = opts$df_long_max_chars_by_var
) {
  restore.point("rrd_df_to_text")

  if (is.null(df) || NROW(df) == 0) return("")

  df_to_plain_str(
    df = df,
    long_nchar = long_nchar,
    exclude_vars = exclude_vars,
    max_string_length = max_string_length,
    max_string_length_by_var = max_string_length_by_var,
    max_width = max_width
  )
}


rrd_indent = function(txt, n = 2) {
  restore.point("rrd_indent")

  if (!nzchar(txt)) return(txt)

  prefix = paste0(rep(" ", n), collapse = "")
  lines = stringi::stri_split_lines1(txt)
  paste0(prefix, lines, collapse = "\n")
}


rrd_safe_file_name = function(x) {
  restore.point("rrd_safe_file_name")

  x = stringi::stri_replace_all_fixed(x, "\\", "/")
  x = stringi::stri_replace_all_regex(x, "[^A-Za-z0-9_.-]+", "_")
  x = stringi::stri_replace_all_regex(x, "_+", "_")
  x = stringi::stri_replace_all_regex(x, "^_+|_+$", "")

  if (!nzchar(x)) x = "do_file"

  x
}


rrd_cmd_reg_status = function(cmd_df, parcels = list()) {
  restore.point("rrd_cmd_reg_status")

  if (is.null(cmd_df) || NROW(cmd_df) == 0) return(character(0))

  status = rep("ok", NROW(cmd_df))
  is_reg = rrd_as_logical(cmd_df$is_reg)
  status[!is_reg] = NA_character_

  has_error = vapply(seq_len(NROW(cmd_df)), function(i) {
    rrd_cmd_has_error(cmd_df[i, , drop = FALSE])
  }, logical(1))

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

  for (i in seq_len(NROW(cmd_df))) {
    if (is.na(status[i])) next

    cmd = cmd_df[i, , drop = FALSE]
    runids = rrd_cmd_runids(cmd, parcels = parcels)

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
