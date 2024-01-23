create_project_directory <- function(project_name) {
  dir.create(project_name)
  setwd(project_name)
  
  dir.create("docs")
  dir.create("examples")
  dir.create("src/main")
  dir.create("src/test")
  dir.create("R")
  
  writeLines("#", file = "README.md")
  writeLines("# LICENSE", file = "LICENSE")
  writeLines(".RData", file = ".gitignore")
  writeLines(".Rhistory", file = ".gitignore")
  writeLines("src/test/testthat.R", file = ".gitignore")
  
  file.create("docs/index.md")
  file.create("docs/getting-started.md")
  file.create("docs/user-guide.md")
  
  file.create("R/analysis.R")
  file.create("R/preprocessing.R")
  file.create("R/visualization.R")
}
create_project_directory("choki")
