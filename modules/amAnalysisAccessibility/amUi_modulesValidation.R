wellPanel(
  amCenterTitle(div(icon('check-square-o'),'Validation'),m=0,h=3,sub=
    "Review validation issues and compute."
  ),
conditionalPanel(condition="(
  input.moduleSelector=='module_5'
  )",
actionButton("btnZonalStat","Update")
),
conditionalPanel(condition="(
  input.moduleSelector!='module_5'
  )",
textInput('costTag','Add short tags',value=''),
actionButton('btnComputeAccessibility','Compute'),
uiOutput('msgModule3outData')
),
uiOutput('msgModule3')
)


