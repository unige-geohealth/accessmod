wellPanel(
  amCenterTitle(div(icon('check-square-o'),'Validation'),m=0,h=3,sub=
    "Review validation issues and compute."
  ),
uiOutput('msgModule3'),
conditionalPanel(condition="(
  input.moduleSelector=='module_5'
  )",
actionButton("btnZonalStat","Update")
),
conditionalPanel(condition="(
  input.moduleSelector!='module_5'
  )",
actionButton('btnComputeAccessibility','Compute')
)
)


