wellPanel(
  amCenterTitle(div(icon('check-square-o'),'Validation'),m=0,h=3,sub=
    "Add short tags, review validation issues and compute."
    ),
  textInput('costTag','Add short tags',value=''),
  uiOutput('msgModule3'),
  uiOutput('msgModule3outData'),
  actionButton('btnComputeAccessibility','Compute'), 
  conditionalPanel("input.moduleSelector != 'module_6'",
    amProgressBar('cumulative-progress')
    )
  )

