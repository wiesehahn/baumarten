
library("workflowr")
install.packages("workflowr")
library("workflowr")
#wflow_git_config(user.name = "Your Name", user.email = "email@domain")
wflow_start("baumarten")
#install.packages("workflowr")
library("workflowr")
#wflow_git_config(user.name = "Your Name", user.email = "email@domain")
wflow_start("C:/Users/jwiesehahn/Arbeit/projects/baumarten")
knit_root_dir: "analysis"
?wflow_html
# insert 'knit_root_dir: "analysis"' in '_workflowr.yml'
wflow_build()
wflow_status()
wflow_publish(c("analysis/index.Rmd", "analysis/about.Rmd", "analysis/license.Rmd"),
              "Publish the initial files for myproject")
wflow_status()
wflow_use_github("wiesehahn")
wflow_git_push()
wflow_open("analysis/evaluation.Rmd")
wflow_status()
wflow_git_commit()
wflow_git_commit("evaluation.Rmd")
wflow_build()
wflow_git_commit(all=T)
wflow_git_push()

wflow_open("preprocessing.Rmd")

wflow_view()
wflow_publish(c("analysis/classification.Rmd", "analysis/evaluation.Rmd", "analysis/index.Rmd", "analysis/preprocessing.Rmd", "analysis/probability.Rmd"),
              "Publish all files for myproject")
wflow_status()
wflow_publish(c("analysis/index.Rmd", "code/workflow_project_setup.R", "data/reference/train_test/train_test.rds"),
              "update index")
