#' @import methods
#' @import data.table
#' @import stringr
#' @import whisker
#' @import yaml
NULL


#' Get the file name from a full path.
#'
#' Such as the file name of folder/file.txt is file.txt.
#'
#' @param path character a file path which can be a full path or not.
#' @return character the file name of the given full path
#' @export
get_name_from_path <- function(path) {
  stopifnot(length(path) == 1)
  name <- str_split(path, "/")[[1]]
  return(name[length(name)])
}


#' Create script from template
create_script <- function(cmd, job_name, log_path, nnodes=1, nproc=1, 
                          timelim="480:00:00", template="slurm") {
  param <- list()
  param[["job_name"]] <- job_name
  param[["log_path"]] <- log_path
  param[["nnodes"]] <- nnodes
  param[["nproc"]] <- nproc
  param[["timelim"]] <- timelim
  param[["cmd"]] <- cmd
  script <- readLines(system.file(template, package="hpcr"))
  script <- whisker.render(script, param)
  return(script)
}


slurm_job <- function(script, job_name, log_path=".", 
                         nnodes=1, nproc=1, timelim="48:00:00", 
                         verbose=FALSE) {
  script <- create_script(cmd=script, job_name=job_name, 
                          log_path=log_path, 
                          nnodes=nnodes, nproc=nproc,
                          timelim=timelim,
                          template="slurm")
  file <- tempfile()
  writeLines(script, file)
  if (verbose) {
    f <- readLines(file)
    cat(f, sep="\n")
  }

  system2("sbatch", file)
}

#' @export
submit_slurm <- function(scirpt, param, job_name, log_path, 
                         nnodes=1, nproc=1, timelim="48:00:00", 
                         verbose=FALSE) {
  for (i in seq(param)) {
    p <- as.list(param[i, ])
    s <- whisker.render(script, p)
    slurm_job(s, job_name, log_path, nnodes,
              nproc, timelim, verbose)
  }
}
