.jaspToolsSubprocessEnv <- function(childFlag, inherited = character()) {
  env <- if (length(inherited) > 0L)
    as.list(Sys.getenv(inherited, unset = ""))
  else
    list()
  env[[childFlag]] <- "true"
  env
}

.jaspToolsSubprocessPayload <- function(extra = list(), env = list()) {
  payload <- list(
    wd = getwd(),
    libPaths = .libPaths(),
    sourcePath = .jaspToolsSourcePath(),
    pkgOptions = .pkgenv[["pkgOptions"]],
    rOptions = .jaspToolsSubprocessOptions(),
    env = env
  )

  if (length(extra) > 0L) {
    payload[names(extra)] <- extra
  }
  payload
}

.jaspToolsSubprocessOptions <- function() {
  optionNames <- c("jaspLegacyRngKind")
  currentOptions <- options()
  currentOptions[intersect(optionNames, names(currentOptions))]
}

.jaspToolsRunSubprocess <- function(prefix, payload, scriptLines,
                                    failureMessage,
                                    launcher = .jaspToolsLaunchSubprocess,
                                    readResult = readRDS,
                                    isError = function(result) FALSE) {
  scriptPath <- tempfile(prefix, fileext = ".R")
  inputPath <- tempfile(prefix, fileext = ".rds")
  outputPath <- tempfile(prefix, fileext = ".rds")
  logPath <- tempfile(prefix, fileext = ".log")
  on.exit(unlink(c(scriptPath, inputPath, outputPath), force = TRUE), add = TRUE)

  saveRDS(payload, inputPath)
  writeLines(scriptLines, scriptPath)

  status <- launcher(scriptPath, inputPath, outputPath, logPath)
  if (!file.exists(outputPath)) {
    stop(
      failureMessage,
      if (!is.null(status)) paste0(" (exit status ", status, ")") else "",
      ". Log: ", logPath,
      .jaspToolsSubprocessLogTail(logPath)
    )
  }

  result <- readResult(outputPath)
  resultIsError <- isTRUE(isError(result))
  if (!is.null(status) && status != 0L && !resultIsError) {
    stop(
      failureMessage,
      " (exit status ", status, "). Log: ", logPath,
      .jaspToolsSubprocessLogTail(logPath),
      call. = FALSE
    )
  }

  if (!resultIsError) {
    unlink(logPath, force = TRUE)
  }
  result
}

.jaspToolsLaunchSubprocess <- function(scriptPath, inputPath, outputPath, logPath) {
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  system2(
    rscript,
    c(normalizePath(scriptPath, winslash = "/", mustWork = TRUE),
      normalizePath(inputPath, winslash = "/", mustWork = TRUE),
      normalizePath(outputPath, winslash = "/", mustWork = FALSE)),
    stdout = logPath,
    stderr = logPath
  )
}

.jaspToolsSubprocessLogTail <- function(logPath, n = 40L) {
  if (!file.exists(logPath)) {
    return("")
  }

  logTail <- paste(utils::tail(readLines(logPath, warn = FALSE), n), collapse = "\n")
  if (nzchar(logTail)) paste0("\n", logTail) else ""
}

.jaspToolsSubprocessScript <- function(resultLines, saveLines,
                                       beforeResultLines = character(0),
                                       statusExpression = "inherits(result, 'jaspTools.subprocessError')") {
  c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "payload <- readRDS(args[[1L]])",
    ".libPaths(payload$libPaths)",
    "setwd(payload$wd)",
    ".jaspToolsSetEnvForChild <- function(env) {",
    "  if (is.list(env) && length(env) > 0L)",
    "    do.call(Sys.setenv, env)",
    "}",
    ".jaspToolsSetEnvForChild(payload$env)",
    ".jaspToolsRestoreROptionsForChild <- function(rOptions) {",
    "  if (is.list(rOptions) && length(rOptions) > 0L)",
    "    do.call(options, rOptions)",
    "  invisible(NULL)",
    "}",
    ".jaspToolsRestoreROptionsForChild(payload$rOptions)",
    ".loadJaspToolsForChild <- function(sourcePath) {",
    "  if (!is.null(sourcePath) && is.character(sourcePath) && length(sourcePath) == 1L &&",
    "      file.exists(file.path(sourcePath, 'R', 'run.R'))) {",
    "    if (!requireNamespace('pkgload', quietly = TRUE))",
    "      stop('pkgload is required to run jaspTools child processes from a source checkout')",
    "    pkgload::load_all(sourcePath, quiet = TRUE)",
    "  } else {",
    "    suppressPackageStartupMessages(library(jaspTools))",
    "  }",
    "}",
    ".jaspToolsSubprocessError <- function(e) {",
    "  structure(list(message = conditionMessage(e)), class = 'jaspTools.subprocessError')",
    "}",
    ".loadJaspToolsForChild(payload$sourcePath)",
    ".jaspToolsRestorePkgOptionsForChild <- function(pkgOptions) {",
    "  if (!is.list(pkgOptions) || length(pkgOptions) == 0L)",
    "    return(invisible(NULL))",
    "  pkgEnv <- get('.pkgenv', envir = asNamespace('jaspTools'), inherits = FALSE)",
    "  pkgEnv[['internal']][['setupCompleteOverride']] <- TRUE",
    "  pkgEnv[['pkgOptions']][names(pkgOptions)] <- pkgOptions",
    "  get('.initOutputDirs', envir = asNamespace('jaspTools'), inherits = FALSE)()",
    "  invisible(NULL)",
    "}",
    ".jaspToolsRestorePkgOptionsForChild(payload$pkgOptions)",
    beforeResultLines,
    resultLines,
    saveLines,
    paste0("quit(save = 'no', status = if (", statusExpression, ") 2L else 0L)")
  )
}

.jaspToolsSourcePath <- function() {
  namespacePath <- tryCatch(
    getNamespaceInfo("jaspTools", "path"),
    error = function(e) NULL
  )

  if (!is.character(namespacePath) || length(namespacePath) != 1L) {
    return(NULL)
  }

  if (!file.exists(file.path(namespacePath, "R", "run.R"))) {
    return(NULL)
  }

  normalizePath(namespacePath, winslash = "/", mustWork = FALSE)
}

.stopIfJaspToolsSubprocessError <- function(result) {
  if (inherits(result, "jaspTools.subprocessError")) {
    stop(result$message, call. = FALSE)
  }

  invisible(result)
}
