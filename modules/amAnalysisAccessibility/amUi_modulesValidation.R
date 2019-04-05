wellPanel(
  amCenterTitle(div(icon('check-square-o'), amt(
   id = "analysis_validation_title"
   )),
   m = 0,
   h = 3,
   sub = amt(
     id = "analysis_validation_sub"
     )
  ),
uiOutput('msgModule3'),
conditionalPanel(condition = "(
  input.moduleSelector=='module_5'
  )",
  actionButton("btnZonalStat", amt(
    id = "analysis_validation_update_btn"
    ))
),
conditionalPanel(condition = "(
  input.moduleSelector!='module_5'
  )",
  actionButton('btnComputeAccessibility', amt(
    id = "analysis_validation_compute_btn"
    ))
  )
)


