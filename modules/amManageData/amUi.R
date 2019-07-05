#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module manage_data 
#
# USER INTERFACE

fluidRow(
  div(class = "col-xs-12 col-md-4",
    amAccordionGroup(id = 'manageData',
      itemList = list(
        'addData' = list(
          title = div(icon('plus-circle'), amt(
              id = "data_import_data"
              )),
          content = tagList(
            selectInput('dataClass',
              label = amt(
                id = "data_select_data_class"
                ),
              choices = ""
              ),
            textInput('dataTag',
              label = amt(
                id = "data_add_tag"
                ),
              value = ''
              ),
            uiOutput('msgModuleData'
              ),
            amFileInput('btnDataNew',
              label = amt(
                id = "data_choose_import_btn"
                ) 
              )
            )
          ),
        'filtData' = list(
          title = div(icon('filter'), amt(
              id = "data_filter_data"
              )),
          content = tagList(
            conditionalPanel(
              condition = "input.checkFilterLastOutput == false",
              amRadioButtons('typeDataChoice', amt(
                  id = "data_type_data_format"
                  ),
                choiceValues = c('vector','raster','table','list','all'),
                choiceNames = list(
                  amt('data_vector'),
                  amt('data_raster'),
                  amt('data_table'),
                  amt('data_list'),
                  amt('data_all')
                  ),
                selected = "all",
                inline = TRUE
                ),
              textInput(inputId = 'filtData', amt(
                  id = "data_filter_text"
                  ),
                ''
                ), 
              selectInput(inputId = 'filtDataTags', amt(
                  id = "data_filter_tag"
                  ),
                choices = '',
                selected = '',
                multiple = T
                )
              ),
            tags$input(type = "checkbox",
              id = "checkShowLastOutputButton",
              style = "display:none"
              ),
            conditionalPanel(
              condition = "input.checkShowLastOutputButton === true",
              checkboxInput('checkFilterLastOutput', amt(
                  id = "data_last_analysis_filter"
                  ))
              ),
            conditionalPanel(
              condition = "input.showAdvancedTools === true",
              checkboxInput("internalDataChoice", amt(
                  id = "data_internal_choice"
                  ),
                value = FALSE)
              )
            )
          ), 
        'renameData' = list(
          title = div(icon('refresh'), amt(
              id = "data_rename_data"
              )),
          content = tagList(
            actionButton('btnUpdateName', amt(
                id = "data_update_name_btn"
                )),
            tags$small(class = "text-muted", amt(
                id = "data_tag_manual_change"
                ))
            )
          ),
        'archiveData' = list(
          title = div(icon('download'), amt(
              id = "data_archive_data"
              )),
          content = tagList(
            textInput('txtArchiveName', amt(
                id = "data_file_prefix"
                )),
            actionButton('createArchive', amt(
                id = "data_archive_create"
                )),
            tags$small(class = "text-muted", amt(
                id = "data_archive_data_selected_table"
                )),
            hr(),
            selectInput('selArchive', amt(
                id = "data_archive_select"
                ),
              choices = ""
              ),
            tags$small(class = "text-muted", amt(
                id = "data_download_delete_archive"
                )),
            actionButton('getArchive', amt(
                id = "data_archive_export"
                )),
            actionButton('btnDeleteArchive', amt(
                id = "data_archive_delete_btn"
                ))
            )
          ),
        'remData' = list(
          title = div(icon('trash-o'), amt(
              id = "data_delete_data"
              )),
          content = tagList(
            actionButton('delDataSelect', amt(
                id = "data_delete_permanently"
                )),
            tags$small(class = "text-muted", amt(
                id = "data_delete_warning"
                )),
            p(id = "txtDelMessage",
              ""
              )
            )
          )
        )
      ) 
    ),
  div(class = "col-xs-12 col-md-8",
    amCenterTitle(amt(
        id = "data_available_title"
        ),
      sub = amt(
        id = "data_available_sub"
        )
      ),
    hotable('dataListTable',
      height = "90vh"
      )
    )
  )


