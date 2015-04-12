#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
## Module Project : help
# Project creation and selection
# Meta data visualisation

manageProject=list(
  "Manage project"=list(
    "Open project"=list(
      "selection"="select input")
    
    ))

module_project=list(
  #section
  title="Project management module",
  desc="description",
  content=list(
    #subsection
    manageProject=list(
      title="Panel to manage project",
      description='description',
      content=list(
        #subsubsection
        manageProject1=list(
          title='Panel item to choose a project',
          description="description",
          content=list(
            #elements
            selectProject=list(
              title="Project selection",
              description="Select input to choose an existing project"
              )
            )
          )
        )
      )

    )
  )


module_project<-list(
  manageProject=list(
    amDesc="Description",
    amTitle="Title"
    manageProject1=list(),
    manageProject2=list()
    )
)


amHelp(section,title,idEl,idElParent,html)


amHelp<-function(type=c('section','subsection','subsubsection','item'),title,idel,idelp,html){


}

# module_project
@
In this module, you can load an existing project or create a new one 
### projectManager
This panel switch between load and create new project
### projectManager1
In this part, you can choose an existing project
### projectManager2
In this part, you can upload a DEM file to create a new project
~          i


library(R6)
Numbers <- R6Class("Numbers",
  public = list(
    x = 100
    ),
  active = list(
    x2 = function(value) {
      if (missing(value)) return(self$x * 2)
      else self$x <- value/2
    },
    rand = function() rnorm(1)
    )
  )

n <- Numbers$new()
n$x


library(R6)

Person <- R6Class("Person",
  public = list(
    name = NA,
    hair = NA,
    initialize = function(name, hair) {
      if (!missing(name)) self$name <- name
      if (!missing(hair)) self$hair <- hair
      self$greet()
    },
    set_hair = function(val) {
      self$hair <- val
    },
    greet = function() {
      cat(paste0("Hello, my name is ", self$name, ".\n"))
    }
    )
  )

list(
  section='module_project',
  )



a<-yaml.load(
"#module_project:
  parent: project
  type: section
  title: Module_project
  abstract: Module to load or create project
  description: test description
")

b<-yaml.load(
  "#manageProject:
    parent: module_project
    type: subsection
    title: project manage panel
    abstract: Switch betweeen create or load project
  "
  
  )





library(yaml)
library("yaml")
z <- yaml.load(
"tree:
  format: newick
  translate: ./My_example-1.translation
  file: ./Xtol_example-1.tree
tracks:
  - class: colorstrip
    source: ./My_example-1.catdomain
    rel_height: 0.6
    title: Catalytic domain
  - class: colorstrip
    source: ./My_example-1.kingdom
    rel_height: 0.6
    title: Itsname
  - class: colorstrip
    source: ./My_example-1.temp")
  names(z)
  names(z$tracks)
  y <- as.yaml(z)
