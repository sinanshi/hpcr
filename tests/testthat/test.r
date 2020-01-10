context("test submission of jobs")

test_that("prepare script", {
  create_script(cmd="script\n", job_name="job", log_path="logpath")
})


test_that("", {
#    slurm_job(script="hostname", job_name="test_slurm", 
#                 log_path="..",
#                 timelim="00:01:00", verbose=TRUE)
#    system("rm ../*.e*")
#    system("rm ../*.o*")
    script <- "echo {{text}}"
    expect_equal(submit_bash(script, list(text="xx")), "xx")
})



