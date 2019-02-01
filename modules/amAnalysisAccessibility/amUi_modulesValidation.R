wellPanel(
  amCenterTitle(div(icon('check-square-o'), amt(
   id = "analysis_validation_title",
   str = 'Validation'
   )),
   m = 0,
   h = 3,
   sub = amt(
     id = "analysis_validation_sub",
     str = "Review validation issues and compute."
     )
  ),
uiOutput('msgModule3'),
conditionalPanel(condition = "(
  input.moduleSelector=='module_5'
  )",
  actionButton("btnZonalStat", amt(
    id = "analysis_validation_update_btn",
    str = "Update"
    ))
),
conditionalPanel(condition = "(
  input.moduleSelector!='module_5'
  )",
  actionButton('btnComputeAccessibility', amt(
    id = "analysis_validation_compute_btn",
    str = 'Compute'
    ))
  )
)


