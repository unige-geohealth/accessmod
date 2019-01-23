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
            id = "data_import",
            str = "Import"
            )),
          content = tagList(
            selectInput('dataClass',
              label = amt(
                id = "data_select_class",
                str = "Select data class"
                ),
              choices = ""
              ),
            textInput('dataTag',
              label = amt(
                id = "data_add_tag",
                str = 'Add short tags'
                ),
              value = ''
            ),
            uiOutput('msgModuleData'
            ),
            amFileInput('btnDataNew',
              label = amt(
                id = "data_import_btn",
                str = 'Choose and import data'
                ) 
              )
            )
          ),
        'filtData' = list(
          title = div(icon('filter'), amt(
            id = "data_filter",
            str = 'Filter'
            )),
          content = tagList(
            conditionalPanel(
              condition = "input.checkFilterLastOutput == false",
              radioButtons('typeDataChoice', amt(
                id = "data_type_choice",
                str = 'Data type'
                ),
                c("Vectors" = "vector",
                  "Rasters" = "raster",
                  "Tables"  = "table",
                  "Lists"   = "list",
                  "All"     = "all"
                  ),
                selected = "all",
                inline = TRUE
                ),
              textInput(inputId = 'filtData', amt(
                id = "data_text_filter",
                str = 'Text (any field, case sensitive)'
                ),
                ''
                ), 
              selectInput(inputId = 'filtDataTags', amt(
                id = "data_tags_filter",
                str = 'Tags filter'
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
                id = "data_last_analysis_filter",
                str = "Filter last analysis only"
                ))
              ),
            conditionalPanel(
              condition = "input.showAdvancedTools === true",
              checkboxInput("internalDataChoice", amt(
                id = "data_internal_choice",
                str = 'Show internal data'
                ),
                value = FALSE)
              )
            )
          ), 
        'renameData' = list(
          title = div(icon('refresh'), amt(
            id = "data_rename",
            str = 'Rename'
            )),
          content = tagList(
            actionButton('btnUpdateName', amt(
              id = "data_update_name_btn",
              str = 'Update modified tag(s)'
              )),
            tags$small(class = "text-muted", amt(
              id = "data_tag_manual_chg",
              str = 'Manually modify the tag(s) in the adjacent table and click on the button to implement the change (does not work with the DEM)'
              ))
            )
          ),
        'archiveData' = list(
          title = div(icon('download'), amt(
            id = "data_archive",
            str = 'Archive'
            )),
          content = tagList(
            textInput('txtArchiveName', amt(
              id = "data_file_prefix",
              str = 'File prefix. Default is "am5"'
              )),
            actionButton('createArchive', amt(
              id = "data_archive_create",
              str = 'Create archive'
              )),
            tags$small(class = "text-muted", amt(
              id = "data_archive_tbl_selected",
              str = 'Click to archive the data appearing as selected in the right table'
              )),
            hr(),
            selectInput('selArchive', amt(
              id = "data_archive_select",
              str = 'Select archive'
              ),
              choices = ""
              ),
            tags$small(class = "text-muted", amt(
              id = "data_download_delete",
              str = 'Click on the buttons below to download or delete the selected archive'
              )),
            actionButton('getArchive', amt(
              id = "data_archive_export",
              str = 'Export archive'
              )),
            actionButton('btnDeleteArchive', amt(
              id = "data_archive_delete",
              str = 'Delete archive'
              ))
            )
          ),
        'remData' = list(
          title = div(icon('trash-o'), amt(
            id = "data_delete",
            str = 'Delete'
            )),
          content = tagList(
            actionButton('delDataSelect', amt(
              id = "data_delete_permanent",
              str = 'Delete permanently'
              )),
            tags$small(class = "text-muted", amt(
              id = "data_delete_warning",
              str = 'This action will delete the selected data'
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
        id = "data_available",
        str ='Available data'
        ),
        sub = amt(
          id = "data_available_sub",
          str = "Data imported in the project or generated by AccessMod."
          )
        ),
    tags$div(
      class = "amTableControls",
      tags$a(
        onclick = "hotableSetColValues('dataListTable',{col:'Select',set:true})",
        ' [ All ]'
        ),
        ' ',
      tags$a(
        onclick = "hotableSetColValues('dataListTable',{col:'Select',set:false})",
        ' [ None ]'
        ),
        ' ',
      HTML("<div data-opt={\"col\":\"Select\",\"valueSet\":true,\"valueUnset\":false,\"labelSet\":\"Select\",\"labelUnset\":\"Unselect\"} id=\"dataListTableSelectTools\"></div>")
      ),
    hotable('dataListTable'
	))
  )


