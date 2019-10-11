tagList(
  tags$div(class = "col-sm-8",
    tags$div(class = "col-sm-12 well",
      h4(amt("about_license")),
      p(amt("about_license_text")),
      hr(),
      h4(amt("about_acknowledge")),
      p(amt("about_acknowledge_text")),
      hr(),
      h4(amt("help_learn_accessmod")),
      p(
        tags$b("English"),
        a(
          href='https://doc-accessmod.unepgrid.ch/display/EN/AccessMod+5+user+manual',
          target="_blank",
          "Access to online user manual"
          )
        ),
      p(
        tags$b("Français"),
        a(
          href='https://doc-accessmod.unepgrid.ch/display/FRAN/AccessMod+5+manuel+de+l%27utilisateur',
          target="_blank",
          "Accès en ligne au manuel de l'utilisateur"
          )
        )
      )
    )
  )
