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
    tags$h4(img(src="logo/icons/logo32x32.png"),'Accessmod 5'),
    uiOutput('appVersionLocalText'),
    uiOutput('appVersionRemoteText')
    ),
  p('This is the development version of accessmod.'),
  actionButton('appFetchGit',label="Check for new version",icon=icon("cloud-download")),
  actionButton('appUpdate',label="Update and restart",icon=icon("refresh")),
  actionButton('showBrowser','Debug mode (show browser in terminal. Development only)')
  )



