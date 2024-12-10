###  Refactoring AccessMod from `handsontable` to `amtabulator`

**Goal**: Replace all instances of `handsontable` in AccessMod with the new `amtabulator` package, which is an HTML widget wrapper for Tabulator.js.

**IMPORTANT**
This task requires your full planning abilities and unwavering focus. AccessMod is a critical WHO software used for analyzing healthcare accessibility, and your work directly impacts its functionality. You are expected to perform this task as though you are the entire IT department. The codebase is legacy, with sparse tests that do not cover the UI.

Key Rule: Do not alter unrelated code outside the scope of this project. Stay focused on the current task. Use all your neurons to understand the reactive nature of this Shiny app. 

---

#### **Checklist and Steps for Refactoring**

1. **Document `amtabulator` functionality**  
   - Explore key functions: `amtabulator::tabulator`, `_proxy`, `_output`, `_to_df`.  
   - Update documentation in R to reflect usage details for the new package.

2. **Identify Files to Refactor**  
   - Locate UI files containing table IDs for replacement:  
     ```bash
     git grep -l 'hotable(' > _handson_refactor_tables.txt
     ```  
   - Locate Server files using `handsontable`:  
     ```bash
     git grep -l 'renderHotable(' >> _handson_refactor_tables.txt
     ```  
   - Save the list of files to `_handson_refactor_tables.txt`.  
   - If interrupted, reload progress by referencing this file.

---

#### **Refactoring Steps**

##### **1. Handle Input Data from Client**
   - **Tabulator**:
     ```r
     tabulator_to_df(input$<id>_data)        # Includes the selection column
     tabulator_to_df(input$<id>_selection)  # Table is the selection (no selection column)
     ```
   - **Handsontable**:
     ```r
     hotToDf(input$<id>, ...)  # Include column renaming if necessary
     ```

##### **2. Create Rendered Instances**
   - **Tabulator**:
     ```r
     output$<id> <- render_tabulator({
       tbl <- data()
       tabulator(
         data = tbl,
         ...options
       )
     })
     ```
   - **Handsontable**:
     ```r
     output$<id> <- renderHotable({
       tbl <- data()
       tbl
     },
     ...options  # Renderer specified directly
     )
     ```

##### **3. Update UI Elements**
   - **Tabulator**:  
     ```r
     tabulator_output(<id>)
     ```
   - **Handsontable**:  
     ```r
     hotable(<id>, height = "90vh")
     ```

---

#### **4. Example Renderer Comparison**

- **Tabulator Example**:
  ```r
  output$dataListTable <- render_tabulator({
    tbl <- dataListTable()

    if (isEmpty(tbl)) {
      tbl <- data.frame("-", "-", "-", "-", "-", "-")
    } else {
      message(tbl$origName)
    }

    tabulator(
      data = tbl,
      add_selector_bar = TRUE,
      add_select_column = TRUE,
      return_select_column_name = "am_select",
      return_select_column = TRUE,
      stretched = "last",
      columnHeaders = c("_", "Tags", "Type", "_", "_", "_", "Class", "_"),
      readOnly = c("type", "displayClass"),
      hide = c("class", "searchCol", "origName", "displayName", "select"),
      columnOrder = c("Type", "Class", "Tags"),
      options = list(index = "cat")
    )
  })
  ```

- **Handsontable Example**:
  ```r
  output$dataListTable <- renderHotable({
    tbl <- dataListTable()

    if (length(tbl) > 0) {
      tbl <- tbl[c("class", "origName", "select", "type", "displayClass", "tags")]
    } else {
      tbl <- data.frame("-", "-", "-", "-", "-", "-")
    }
    tbl
  },
  stretch = "last",
  readOnly = c(1, 2, 4, 5),
  hide = c(1, 2),
  columnHeaders = c("class", "origName", "Select", "Type", "Class", "Tags"),
  toolsConditionalColumn = list(
    column = "Select",
    valueSet = TRUE,
    valueUnset = FALSE,
    columnSelectInput = c("type", "displayClass", "tags")
  ))
  ```

---

### **Notes**  
- Progress is saved in `_handson_refactor_tables.txt`. Ensure the file is up to date after each session.  
- Refactoring has **already started** but requires completion. Prioritize consistency and verify functionality after each replacement.
