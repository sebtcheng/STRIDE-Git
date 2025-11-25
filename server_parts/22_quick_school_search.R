# --- 1. Initialize Map ---
output$TextMapping <- renderLeaflet({
  leaflet() %>%
    setView(lng = 122, lat = 13, zoom = 5) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Road Map") %>%
    addMeasure(position = "topright", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters") %>%
    addLayersControl(baseGroups = c("Satellite", "Road Map"))
})

# --- 2. Update Picker Choices Dynamically ---
observeEvent(input$search_mode, {
  req(!is.null(input$search_mode))
  if (input$search_mode == FALSE) {
    updateTextInput(session, "text_advanced", value = "")
    updatePickerInput(session, "qss_region", selected = character(0))
    updatePickerInput(session, "qss_division", selected = character(0))
    updatePickerInput(session, "qss_legdist", selected = character(0))
    updatePickerInput(session, "qss_municipality", selected = character(0))
  } else {
    updateTextInput(session, "text_simple", value = "")
    updatePickerInput(session, "qss_region", selected = character(0))
    updatePickerInput(session, "qss_division", selected = character(0))
    updatePickerInput(session, "qss_legdist", selected = character(0))
    updatePickerInput(session, "qss_municipality", selected = character(0))
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Pickers Logic (Region -> Division -> etc.)
observeEvent(input$qss_region, {
  data <- uni 
  if (!is.null(input$qss_region)) data <- data %>% filter(Region %in% input$qss_region)
  updatePickerInput(session, "qss_division", choices = sort(unique(data$Division)), selected = character(0))
  updatePickerInput(session, "qss_legdist", choices = sort(unique(data$Legislative.District)), selected = character(0))
  updatePickerInput(session, "qss_municipality", choices = sort(unique(data$Municipality)), selected = character(0))
}, ignoreNULL = FALSE, ignoreInit = TRUE)

observeEvent(input$qss_division, {
  data <- uni 
  if (!is.null(input$qss_region)) data <- data %>% filter(Region %in% input$qss_region)
  if (!is.null(input$qss_division)) data <- data %>% filter(Division %in% input$qss_division)
  updatePickerInput(session, "qss_legdist", choices = sort(unique(data$Legislative.District)), selected = character(0))
  updatePickerInput(session, "qss_municipality", choices = sort(unique(data$Municipality)), selected = character(0))
}, ignoreNULL = FALSE, ignoreInit = TRUE)

observeEvent(input$qss_legdist, {
  data <- uni 
  if (!is.null(input$qss_region)) data <- data %>% filter(Region %in% input$qss_region)
  if (!is.null(input$qss_division)) data <- data %>% filter(Division %in% input$qss_division)
  if (!is.null(input$qss_legdist)) data <- data %>% filter(Legislative.District %in% input$qss_legdist)
  updatePickerInput(session, "qss_municipality", choices = sort(unique(data$Municipality)), selected = character(0))
}, ignoreNULL = FALSE, ignoreInit = TRUE)

# --- 3. Update Button State ---
observe({
  req(!is.null(input$search_mode)) 
  is_advanced_mode <- isTRUE(input$search_mode)
  adv_pickers_filled <- !is.null(input$qss_region) || !is.null(input$qss_division) || !is.null(input$qss_legdist) || !is.null(input$qss_municipality)
  can_run <- FALSE
  warning_msg <- ""
  
  if (is_advanced_mode) {
    txt <- trimws(input$text_advanced)
    can_run <- (txt != "" || adv_pickers_filled)
    if (!can_run) warning_msg <- "⚠ Please enter a school name or use advanced search filters."
  } else {
    txt <- trimws(input$text_simple)
    can_run <- (txt != "")
    if (!can_run) warning_msg <- "⚠ Please enter a school name."
  }
  shinyjs::toggleState("TextRun", condition = can_run)
  output$text_warning_ui <- renderUI({ if (!can_run) tags$small(style = "color: red; font-style: italic;", warning_msg) else "" })
})

# --- 4. Main Data Filtering (Snapshot) ---
data_snapshot <- reactiveVal(NULL)

observeEvent(input$TextRun, {
  is_advanced <- isTRUE(input$search_mode)
  Text_pattern <- "" 
  if (is_advanced) {
    if (!is.null(input$text_advanced) && input$text_advanced != "") Text_pattern <- trimws(input$text_advanced)
  } else {
    if (!is.null(input$text_simple) && input$text_simple != "") Text_pattern <- trimws(input$text_simple)
  }
  filtered_data <- uni
  if (Text_pattern != "") filtered_data <- filtered_data %>% filter(grepl(Text_pattern, as.character(School.Name), ignore.case = TRUE))
  
  if (is_advanced) {
    if (!is.null(input$qss_region)) filtered_data <- filtered_data %>% filter(Region %in% input$qss_region)
    if (!is.null(input$qss_division)) filtered_data <- filtered_data %>% filter(Division %in% input$qss_division)
    if (!is.null(input$qss_legdist)) filtered_data <- filtered_data %>% filter(Legislative.District %in% input$qss_legdist)
    if (!is.null(input$qss_municipality)) filtered_data <- filtered_data %>% filter(Municipality %in% input$qss_municipality)
  } 
  final_data <- filtered_data %>% arrange(Region, Division, Municipality, School.Name)
  data_snapshot(final_data)
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# --- 5. Update Map and Table ---
observe({
  data <- data_snapshot()
  if (is.null(data)) { output$text_warning_ui <- renderUI(""); leafletProxy("TextMapping") %>% clearMarkers(); return() }
  if (nrow(data) == 0) { output$text_warning_ui <- renderUI({ tags$small(style = "color: red; font-style: italic;", "⚠ No results found.") }); leafletProxy("TextMapping") %>% clearMarkers(); return() }
  
  output$text_warning_ui <- renderUI("") 
  values.comp <- paste(strong("SCHOOL INFORMATION"), "<br>School Name:", data$School.Name, "<br>School ID:", data$SchoolID) %>% lapply(htmltools::HTML)
  
  leafletProxy("TextMapping") %>%
    clearMarkers() %>% clearMarkerClusters() %>%
    flyToBounds(lng1 = min(data$Longitude), lat1 = min(data$Latitude), lng2 = max(data$Longitude), lat2 = max(data$Latitude)) %>%
    addAwesomeMarkers(lng = data$Longitude, lat = data$Latitude, icon = makeAwesomeIcon(icon = "education", library = "glyphicon", markerColor = "blue"), label = values.comp, labelOptions = labelOptions(noHide = FALSE, textsize = "12px", direction = "top"))
})

output$TextTable <- DT::renderDT(server = TRUE, {
  data <- data_snapshot()
  if (is.null(data)) {
    df <- data.frame(Region = character(), Division = character(), School = character())
  } else {
    df <- data %>% select("Region", "Division", "Legislative.District", "Municipality", "School.Name") %>% rename("School" = "School.Name")
  }
  datatable(df, extension = 'Buttons', rownames = FALSE, selection = 'single', options = list(scrollX = TRUE, pageLength = 10, dom = 'lrtip'), filter = "top")
})

# --- 6. Row Selection & Details Logic ---
qs_data <- reactive({
  idx <- input$TextTable_rows_selected
  req(idx)
  table_data <- data_snapshot()
  req(nrow(table_data) >= idx)
  return(table_data[idx, , drop = FALSE])
})

observeEvent(input$TextTable_rows_selected, {
  idx <- input$TextTable_rows_selected
  req(idx)
  data <- data_snapshot()
  req(nrow(data) >= idx)
  row <- data[idx, ]
  
  leafletProxy("TextMapping") %>% flyTo(lng = row$Longitude, lat = row$Latitude, zoom = 15)
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# --- 7. Detail Tables Renderers ---
make_bold <- function(df) { df[] <- lapply(df, function(x) paste0("<strong>", x, "</strong>")); return(df) }

output$qs_basic <- renderTable({ data <- qs_data(); req(nrow(data)>0); make_bold(data.frame(Metric=c("School Name","School ID","School Head","Position","Curricular Offering","Typology"), Value=as.character(c(data$School.Name, data$SchoolID, data$School.Head.Name, data$SH.Position, data$Modified.COC, data$School.Size.Typology)))) }, striped=T, bordered=T, colnames=F, sanitize.text.function=function(x) x)
output$qs_location <- renderTable({ data <- qs_data(); req(nrow(data)>0); make_bold(data.frame(Metric=c("Region","Division","District","Municipality","Barangay","Latitude","Longitude"), Value=as.character(c(data$Region, data$Division, data$District, data$Municipality, data$Barangay, data$Latitude, data$Longitude)))) }, striped=T, bordered=T, colnames=F, sanitize.text.function=function(x) x)
output$qs_enrolment <- renderTable({ data <- qs_data(); req(nrow(data)>0); df <- data.frame(Level=c("Kinder", paste0("Grade ", 1:12), "Total"), Count=as.character(c(data$Kinder, data$G1, data$G2, data$G3, data$G4, data$G5, data$G6, data$G7, data$G8, data$G9, data$G10, data$G11, data$G12, data$TotalEnrolment))); make_bold(df[df$Count != "0" & !is.na(df$Count), ]) }, striped=T, bordered=T, sanitize.text.function=function(x) x)
output$qs_teachers <- renderTable({ data <- qs_data(); req(nrow(data)>0); make_bold(data.frame(Metric=c("ES Teachers","JHS Teachers","SHS Teachers","Total"), Value=as.character(c(data$ES.Teachers, data$JHS.Teachers, data$SHS.Teachers, data$TotalTeachers)))) }, striped=T, bordered=T, colnames=F, sanitize.text.function=function(x) x)
output$qs_teacher_needs <- renderTable({ data <- qs_data(); req(nrow(data)>0); make_bold(data.frame(Metric=c("ES Shortage","JHS Shortage","SHS Shortage","Total Shortage","ES Excess","JHS Excess","SHS Excess","Total Excess"), Value=as.character(c(data$ES.Shortage, data$JHS.Shortage, data$SHS.Shortage, data$Total.Shortage, data$ES.Excess, data$JHS.Excess, data$SHS.Excess, data$Total.Excess)))) }, striped=T, bordered=T, colnames=F, sanitize.text.function=function(x) x)
output$qs_classrooms <- renderTable({ data <- qs_data(); req(nrow(data)>0); make_bold(data.frame(Metric=c("Total Buildings","Total Classrooms"), Value=as.character(c(data$Buildings, data$Instructional.Rooms.2023.2024)))) }, striped=T, bordered=T, colnames=F, sanitize.text.function=function(x) x)
output$qs_classroom_needs <- renderTable({ data <- qs_data(); req(nrow(data)>0); b_val <- if(is.list(data$With_Buildable_space)) unlist(data$With_Buildable_space) else data$With_Buildable_space; make_bold(data.frame(Metric=c("Requirement","Shortage","Major Repairs","Shifting","Buildable Space"), Value=as.character(c(data$Classroom.Requirement, data$Est.CS, data$Major.Repair.2023.2024, data$Shifting, b_val)))) }, striped=T, bordered=T, colnames=F, sanitize.text.function=function(x) x)
output$qs_utilities <- renderTable({ data <- qs_data(); req(nrow(data)>0); make_bold(data.frame(Metric=c("Electricity","Water","Ownership","Total Seats","Seats Shortage"), Value=as.character(c(data$ElectricitySource, data$WaterSource, data$OwnershipType, data$Total.Seats.2023.2024, data$Total.Seats.Shortage.2023.2024)))) }, striped=T, bordered=T, colnames=F, sanitize.text.function=function(x) x)
output$qs_ntp <- renderTable({ data <- qs_data(); req(nrow(data)>0); make_bold(data.frame(Metric=c("AO II Status","PDO I Status","COS Status"), Value=as.character(c(data$Clustering.Status, data$PDOI_Deployment, data$Outlier.Status)))) }, striped=T, bordered=T, colnames=F, sanitize.text.function=function(x) x)
output$qs_specialization <- renderTable({ data <- qs_data(); req(nrow(data)>0); lbls <- c("English","Math","Science","Bio Sci","Phys Sci","Gen Ed","AP","TLE","MAPEH","Filipino","ESP","Agri","ECE","SPED"); df <- if(!is.na(data$Modified.COC) && data$Modified.COC=="Purely ES") data.frame(M="Note",V="N/A for Purely ES") else data.frame(M=lbls, V=as.character(c(data$English,data$Mathematics,data$Science,data$Biological.Sciences,data$Physical.Sciences,data$General.Ed,data$Araling.Panlipunan,data$TLE,data$MAPEH,data$Filipino,data$ESP,data$Agriculture,data$ECE,data$SPED))); make_bold(df) }, striped=T, bordered=T, colnames=F, sanitize.text.function=function(x) x)

# --- 8. PDF DOWNLOAD HANDLER ---
# --- 7. HTML DOWNLOAD HANDLER (Final Version) ---
output$download_school_profile <- downloadHandler(
  filename = function() {
    req(qs_data())
    safe_name <- gsub("[^[:alnum:]]", "_", qs_data()$School.Name)
    paste0("STRIDE_Profile_", safe_name, "_", Sys.Date(), ".html") # <--- .html extension
  },
  
  content = function(file) {
    
    data <- qs_data()
    req(nrow(data) > 0)
    
    # 2. Helper Function (No sanitizer needed for HTML)
    create_filtered_table <- function(metrics, values) {
      df <- data.frame(
        Metric = metrics,
        Value = as.character(values), 
        stringsAsFactors = FALSE
      )
      df %>% 
        filter(!Value %in% c("0", "N/A", "-", "", NA, "NA", "0.0", "0.00")) %>% 
        filter(!is.na(Value))
    }
    
    # 3. Prepare Data Frames
    df_basic <- create_filtered_table(
      c("School Name", "School ID", "School Head", "Position", "Curricular Offering", "Typology", "Region", "Division", "District", "Municipality", "Barangay"),
      c(data$School.Name, data$SchoolID, data$School.Head.Name, data$SH.Position, data$Modified.COC, data$School.Size.Typology, data$Region, data$Division, data$District, data$Municipality, data$Barangay)
    )
    
    df_enrol <- create_filtered_table(
      c("Kinder", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Total Enrolment"),
      c(data$Kinder, data$G1, data$G2, data$G3, data$G4, data$G5, data$G6, data$G7, data$G8, data$G9, data$G10, data$G11, data$G12, data$TotalEnrolment)
    )
    
    df_teachers <- create_filtered_table(
      c("Elementary Teachers", "JHS Teachers", "SHS Teachers", "Total Teachers", "ES Shortage", "JHS Shortage", "SHS Shortage", "Total Shortage", "ES Excess", "JHS Excess", "SHS Excess", "Total Excess"),
      c(data$ES.Teachers, data$JHS.Teachers, data$SHS.Teachers, data$TotalTeachers, data$ES.Shortage, data$JHS.Shortage, data$SHS.Shortage, data$Total.Shortage, data$ES.Excess, data$JHS.Excess, data$SHS.Excess, data$Total.Excess)
    )
    
    buildable_val <- if(is.list(data$With_Buildable_space)) unlist(data$With_Buildable_space) else data$With_Buildable_space
    df_infra <- create_filtered_table(
      c("Total Buildings", "Total Classrooms", "Classroom Requirement", "Estimated Shortage", "Major Repairs Needed", "Shifting Schedule", "Buildable Space Available", "Electricity Source", "Water Source", "Ownership Type", "Total Seats", "Seats Shortage"),
      c(data$Buildings, data$Instructional.Rooms.2023.2024, data$Classroom.Requirement, data$Est.CS, data$Major.Repair.2023.2024, data$Shifting, buildable_val, data$ElectricitySource, data$WaterSource, data$OwnershipType, data$Total.Seats.2023.2024, data$Total.Seats.Shortage.2023.2024)
    )
    
    df_spec <- create_filtered_table(
      c("English", "Mathematics", "Science", "Biological Sciences", "Physical Sciences", "General Education", "Araling Panlipunan", "TLE", "MAPEH", "Filipino", "ESP", "Agriculture", "ECE", "SPED"),
      c(data$English, data$Mathematics, data$Science, data$Biological.Sciences, data$Physical.Sciences, data$General.Ed, data$Araling.Panlipunan, data$TLE, data$MAPEH, data$Filipino, data$ESP, data$Agriculture, data$ECE, data$SPED)
    )
    
    # 4. Render Template
    tempReport <- file.path(tempdir(), "school_profile_template.Rmd")
    if (file.exists("school_profile_template.Rmd")) {
      file.copy("school_profile_template.Rmd", tempReport, overwrite = TRUE)
    } else {
      file.copy("www/school_profile_template.Rmd", tempReport, overwrite = TRUE)
    }
    
    params_list <- list(
      school_name = data$School.Name,
      df_basic = df_basic,
      df_enrol = df_enrol,
      df_teachers = df_teachers,
      df_infra = df_infra,
      df_spec = df_spec
    )
    
    rmarkdown::render(tempReport, output_file = file, params = params_list, envir = new.env(parent = globalenv()))
  }
)