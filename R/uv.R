#' @export
install_uv <- function(version = NULL) {
  if (!is.null(version)) {
    version <- paste0("/", version)
  }
  uv_path <- uv_reticulate()
  if (is_windows()) {
    # TODO: Add Windows support
    stop("Installation for Windows is not supported yet")
  } else {
    temp_sh <- tempfile(fileext = ".sh")
    download.file(
      url = paste0("https://astral.sh/uv", version, "/install.sh"),
      destfile = temp_sh
    )
    Sys.chmod(temp_sh, mode = "0755")
    withr::with_envvar(
      new = c("UV_UNMANAGED_INSTALL" = uv_path),
      system(temp_sh)
    )
    uv_files <- list.files(uv_path, recursive = TRUE)
    Sys.chmod(file.path(uv_path, uv_files), mode = "0755")
    unlink(temp_sh)
    invisible()
  }
}

uv_reticulate <- function() {
  path.expand(file.path(rappdirs::user_data_dir(), "r-reticulate", "uv"))
}

uv_requirements_python <- function() {
  venv_path <- NULL
  python <- NULL
  reqs <- py_require()
  if (reqs$python_version != "") {
    python <- reqs$python_version
  }
  packages <- reqs$packages
  has_reqs <- !is.null(uv_exe()) && !is.null(python) | !is.null(packages)
  if (has_reqs) {
    if (is.null(uv_exe()) && interactive()) {
      cat(c(
        "\n`reticulate` recommends using `uv` for Python environments, \n",
        "but it is not installed.\n\n",
        "Learn more:",
        "https://rstudio.github.io/reticulate/articles/uv.html\n\n",
        "Would you like for `reticulate` to install `uv`?\n"
      ))
      selection <- menu(c("Yes", "Cancel"))
      if (selection == 1) {
        cat("\n\nInstalling `uv` ...\n\n")
        install_uv()
        cat("\nPreparing the `uv` environment ... \n\n")
      } else {
        return(invisible())
      }
    }
    venv_path <- uv_req_environment_path(
      packages = packages,
      python = python
    )
    venv_path <- venv_exec_path(venv_path)
  }
  venv_path
}

# Splitting this into its own function because there seems to be
# other places in 'reticulate' where it could be used
venv_exec_path <- function(path) {
  suffix <- if (is_windows()) {
    suffix <- file.path("Scripts", "python.exe")
  } else {
    suffix <- file.path("bin", "python")
  }
  file.path(path, suffix)
}

uv_req_environment_path <- function(packages = NULL, python = NULL) {
  if (!is.null(packages)) {
    packages <- as.vector(rbind("--with", maybe_shQuote(packages)))
  }
  if (!is.null(python)) {
    python <- c("--python", python)
  }
  script_py <- tempfile(fileext = ".py")
  writeLines("import sys; print(sys.prefix)", con = script_py)
  out <- uv_process(
    args = c("run", packages, python, "--script", script_py),
    output = "console"
  )
  unlink(script_py)
  out
}

uv_process <- function(args = c(), output = c("full", "silent", "console")) {
  output <- match.arg(output)
  out <- invisible(system2(
    command = uv_exe(),
    args = args,
    stdout = TRUE
  ))
  if (output == "full") {
    for (line in out) {
      cat(line, "\n")
    }
  }
  if (output == "console") {
    return(out)
  }
  invisible()
}

uv_exe <- function() {
  out <- NULL
  uv_path <- uv_reticulate()
  system_path <- path.expand("~/.local/bin/uv")
  if (dir.exists(uv_path)) {
    out <- file.path(uv_path, "uv")
  } else if (file.exists(system_path)) {
    out <- NULL # system_path
  }
  out
}

uv_found <- function() {
  !is.null(uv_exe())
}

is_uv_environment <- function(dir) {
  !is.null(uv_version(dir))
}

uv_version <- function(path) {
  out <- NULL
  if (file.exists(path)) {
    # If it is a file, it will assume that the Python executable was
    # passed, so it will back out one folder assuming we are in the
    # 'bin' or 'Scripts' sub-folder
    path <- dirname(dirname(path))
  }
  if (dir.exists(path)) {
    cfg_file <- file.path(path, "pyvenv.cfg")
    if (file.exists(cfg_file)) {
      cfg <- readLines(cfg_file)
      uv_grep <- grepl("uv = ", cfg)
      if (any(uv_grep)) {
        out <- unlist(strsplit(cfg[uv_grep], "uv = "))[[2]]
      }
    }
  }
  out
}

# TODO: Probably need to merge this with python_info_virtualenv() at one point
python_info_uv <- function(path) {
  python <- venv_exec_path(path)
  out <- list(
    python = python,
    type = "uv",
    root = path
  )
  if (file.exists(cfg <- file.path(out$root, "pyvenv.cfg"))) {
    starter <- grep("^home = ", readLines(cfg), value = TRUE)
    if (length(starter)) {
      out$starter <- str_drop_prefix(starter, "home = ")
    }
  }
  out
}
