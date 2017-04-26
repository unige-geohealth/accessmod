#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# description of the project 
# TODO : link the wiki here.



sidebarPanel(
  tagList(
    tags$h4(img(src="logo/icons/logo32x32.png"),span(id="amVersionTitle")), 
    #
    # Advanced options
    #
    checkboxInput("showAdvancedTools","Enable advanced options in modules"),
    #
    # Admin tools : restart, update.
    #
    checkboxInput('showAdminTools','Show advanced settings'),
    conditionalPanel(condition="input.showAdminTools==true",
      #
      # display information update
      #
      p("Accessmod version: ",span(id="txtAccessmodVersion")),
      #
      # Text and button for update
      #
      uiOutput("amUpdate"),
      #
      # Restart application (do not update)
      #
      actionButton('btnRestart',"Restart Accessmod"),
      actionButton("btnClearCache","Clear cache and restart"),
      #
      # Expert tools
      #
      checkboxInput('showDevelTools', 'Show expert options'),
      conditionalPanel(condition='input.showDevelTools == true',
        p("Warning: those options could break this application."),

        #
        # Change upload limit.
        #
        numericInput("numSetUploadLimit",
          "Temporary limit for data importing (Megabytes)",
          min=10,
          max=1000,
          value=config$maxUploadSize,
          step=1),
        
        actionButton("btnSetFileSizeLimit","Apply the temporary data importing limit"),
        #
        # In some case, grass lost spatial settings
        #
        actionButton('grassResetRegion',
          label='Reload spatial settings'
          )
        #
        # Show interactive browser
        #
        # actionButton('showBrowser',
        # Interractive debugger (break the application)'
        # )
        )
      )
    )
  )


