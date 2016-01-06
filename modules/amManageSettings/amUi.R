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
    tags$h4(img(src="logo/icons/logo32x32.png"),"AccessMod 5"),
    p("Accessmod version: ",span(id="txtAccessmodVersion")),
    #
    # Admin tools : restart, update.
    #
    checkboxInput('showAdminTools','Show admin tools'),
    conditionalPanel(condition="input.showAdminTools==true",
      #
      # Restart application (do not update)
      #
      actionButton('btnRestart',"Restart Accessmod"),
      #
      # Text and button for update
      #
      uiOutput("amUpdate"),
      #
      # Expert tools
      #
      checkboxInput('showDevelTools', 'Show expert tools'),
      conditionalPanel(condition='input.showDevelTools == true',
        p("Warnings: those options could break this application."),

        #
        # Change upload limit.
        #
        numericInput("numSetUploadLimit",
          "Temporary change the maximum upload file size limit (MB)",
          min=10,
          max=1000,
          value=config$maxUploadSize,
          step=1),
        
        actionButton("btnSetFileSizeLimit","Apply new temporary file size limit"),
        #
        # In some case, grass lost spatial settings
        #
        actionButton('grassResetRegion',
          label='Reload spatial settings'
          ),
        #
        # Show interactive browser
        #
        actionButton('showBrowser',
          'Interractive debugger (break the application)'
          )
        )
      )
    )
  )


