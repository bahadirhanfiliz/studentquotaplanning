library(shiny)
library(stats)
library(dplyr)

Logged = FALSE

my_username <- "username"
my_password <- "password"

# Login Screen
ui1 <- function() {
  tagList(
    title = "Placement Planning",
    fluidRow(column(
      9, tags$img(
        height = 100,
        width = 450,
        src = "koc_logo.png"
      )
    ),
    column(
      3, tags$h3("VPAA Office Placement Planning Software"), align = "bottom"
    )),
    tags$hr(),
    div(
      id = "login",
      wellPanel(
        textInput("userName", "Username"),
        passwordInput("passwd", "Password"),
        br(),
        actionButton("Login", "Log in")
      )
    ),
    tags$style(
      type = "text/css",
      "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}"
    )
  )
}

# Main Screen
ui2 <- function() {
  fluidPage(
    title = "Placement Planning",
    fluidRow(column(
      9, tags$img(
        height = 100,
        width = 450,
        src = "koc_logo.png"
      )
    ),
    column(
      3, tags$h3("VPAA Office Placement Planning Software"), align = "bottom"
    )),
    tags$hr(),
    tabsetPanel(
      id = "mainui",
      type = "tabs",
      tabPanel(
        title = ("Data Visualization"),
        wellPanel(
          style = "background: #dfe4ea",
          fluidRow(
            #Selector for file upload
            column(2, fileInput(
              'file2',
              'Choose CSV Placement',
              accept =
                c('text/csv', 'text/comma-separated-values,text/plain')
            )),
            column(
              1,
              selectInput(
                "dataType",
                label = "Data Type",
                choices = c("KU", "Score Type", "College", "Department"),
                selected = "KU"
              )
            ),
            
            column(
              1,
              conditionalPanel(condition = "input.dataType == 'Score Type'",
                               selectInput(
                                 "scoreType", label = "Score Type", c("EA", "SAY", "SOZ", "DIL")
                               ))
            ),
            column(
              1,
              conditionalPanel(condition = "input.dataType == 'College'",
                               selectInput(
                                 "college", label = "College", c("CASE", "CE", "CS", "CSSH")
                               ))
            ),
            column(
              1,
              conditionalPanel(
                condition = "input.dataType == 'Department'",
                selectInput(
                  "department",
                  label = "Department",
                  c(
                    "BUSAD",
                    "ECON",
                    "INTL",
                    "CHBI",
                    "COMP",
                    "ELEC",
                    "INDR",
                    "MECH",
                    "CHEM",
                    "MATH",
                    "MBGE",
                    "PHYS",
                    "ARHA",
                    "ENCL",
                    "HIST",
                    "MAVA",
                    "PHIL",
                    "PSYC",
                    "SOCI",
                    "LAW",
                    "MEDI",
                    "NURS"
                  )
                )
              )
            ),
            column(1, downloadButton("graphDownload", "Export Graph"), align = "center")
          )
        ),
        plotOutput("graph")
      ),
      
      tabPanel(
        title = ("Selecting Scenario"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            title = ("Scenario"),
            wellPanel(style = "background: #dfe4ea",
                      fluidRow(
                        column(
                          2,
                          fileInput(
                            "file0",
                            "Choose CSV Forecated Rankings",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv"
                            )
                          )
                        ),
                        column(2,  fileInput(
                          "file1",
                          "Choose CSV Scenario",
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv"
                          )
                        )),
                        column(
                          1,
                          downloadButton("scenarioDownload", "Export Scenario"),
                          align = "center"
                        )
                      )),
            wellPanel(
              style = "background: #dfe4ea",
              fluidRow(
                column(1, h3("College")),
                column(1, h3("Program")),
                column(1, h3("100%")),
                column(1, h3("50%")),
                column(1, h3("25%")),
                column(1, h3("Paid")),
                column(1, h3("Total Quota"), align = "center"),
                column(1, h3("Full Scholarship Ratio"), align = "center"),
                column(1, h3("Tutition Paying Equivalent"), align = "center")
              )
            ),
            wellPanel(
              style = "background: #dfe4ea",
              fluidRow(
                column(1, h3("CL")),
                column(1, h3("LAW")),
                column(1, numericInput(
                  inputId = "law_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "law_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "law_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "law_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "law_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("law_fsr")), align = "center"),
                column(1, h3(textOutput("law_tpe")), align = "center")
              )
            ),
            wellPanel(
              style = "background: #dfe4ea",
              fluidRow(
                column(1, h3("SOM")),
                column(1, h3("MEDI")),
                column(1, numericInput(
                  inputId = "medi_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "medi_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "medi_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "medi_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "medi_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("medi_fsr")), align = "center"),
                column(1, h3(textOutput("medi_tpe")), align = "center")
              )
            ),
            wellPanel(
              style = "background: #dfe4ea",
              fluidRow(
                column(1, h3("")),
                column(1, h3("ARHA")),
                column(1, numericInput(
                  inputId = "arha_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "arha_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "arha_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "arha_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "arha_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("arha_fsr")), align = "center"),
                column(1, h3(textOutput("arha_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("PHIL")),
                column(1, numericInput(
                  inputId = "phil_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "phil_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "phil_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "phil_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "phil_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("phil_fsr")), align = "center"),
                column(1, h3(textOutput("phil_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("ENCL")),
                column(1, numericInput(
                  inputId = "encl_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "encl_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "encl_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "encl_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "encl_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("encl_fsr")), align = "center"),
                column(1, h3(textOutput("encl_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("CSSH")),
                column(1, h3("MAVA")),
                column(1, numericInput(
                  inputId = "mava_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "mava_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "mava_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "mava_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "mava_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("mava_fsr")), align = "center"),
                column(1, h3(textOutput("mava_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("PSYC")),
                column(1, numericInput(
                  inputId = "psyc_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "psyc_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "psyc_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "psyc_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "psyc_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("psyc_fsr")), align = "center"),
                column(1, h3(textOutput("psyc_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("SOCI")),
                column(1, numericInput(
                  inputId = "soci_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "soci_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "soci_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "soci_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "soci_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("soci_fsr")), align = "center"),
                column(1, h3(textOutput("soci_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("HIST")),
                column(1, numericInput(
                  inputId = "hist_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "hist_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "hist_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "hist_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "hist_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("hist_fsr")), align = "center"),
                column(1, h3(textOutput("hist_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("CSSH Total")),
                column(1, h3(textOutput("cssh_100")), align = "center"),
                column(1, h3(textOutput("cssh_50")), align = "center"),
                column(1, h3(textOutput("cssh_25")), align = "center"),
                column(1, h3(textOutput("cssh_0")), align = "center"),
                column(1, h3(textOutput(
                  "cssh_quota_total"
                )), align = "center"),
                column(1, h3(textOutput("cssh_fsr")), align = "center"),
                column(1, h3(textOutput("cssh_tpe")), align = "center")
              )
            ),
            wellPanel(
              style = "background: #dfe4ea",
              fluidRow(
                column(1, h3("")),
                column(1, h3("ECON")),
                column(1, numericInput(
                  inputId = "econ_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "econ_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "econ_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "econ_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "econ_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("econ_fsr")), align = "center"),
                column(1, h3(textOutput("econ_tpe")), align = "center")
              ),
              fluidRow(
                style = "background: #dfe4ea",
                column(1, h3("CASE")),
                column(1, h3("BUSAD")),
                column(1, numericInput(
                  inputId = "busad_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(
                  1,
                  numericInput(
                    inputId = "busad_50",
                    label = "",
                    value = 0,
                    min = 0
                  )
                ),
                column(
                  1,
                  numericInput(
                    inputId = "busad_25",
                    label = "",
                    value = 0,
                    min = 0
                  )
                ),
                column(1, numericInput(
                  inputId = "busad_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "busad_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("busad_fsr")), align = "center"),
                column(1, h3(textOutput("busad_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("INTL")),
                column(1, numericInput(
                  inputId = "intl_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "intl_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "intl_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "intl_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "intl_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("intl_fsr")), align = "center"),
                column(1, h3(textOutput("intl_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("CASE TOTAL")),
                column(1, h3(textOutput("case_100")), align = "center"),
                column(1, h3(textOutput("case_50")), align = "center"),
                column(1, h3(textOutput("case_25")), align = "center"),
                column(1, h3(textOutput("case_0")), align = "center"),
                column(1, h3(textOutput(
                  "case_quota_total"
                )), align = "center"),
                column(1, h3(textOutput("case_fsr")), align = "center"),
                column(1, h3(textOutput("case_tpe")), align = "center")
              )
            ),
            wellPanel(
              style = "background: #dfe4ea",
              fluidRow(
                column(1, h3("")),
                column(1, h3("COMP")),
                column(1, numericInput(
                  inputId = "comp_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "comp_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "comp_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "comp_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "comp_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("comp_fsr")), align = "center"),
                column(1, h3(textOutput("comp_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("ELEC")),
                column(1, numericInput(
                  inputId = "elec_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "elec_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "elec_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "elec_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "elec_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("elec_fsr")), align = "center"),
                column(1, h3(textOutput("elec_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("CE")),
                column(1, h3("INDR")),
                column(1, numericInput(
                  inputId = "indr_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "indr_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "indr_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "indr_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "indr_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("indr_fsr")), align = "center"),
                column(1, h3(textOutput("indr_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("CHBI")),
                column(1, numericInput(
                  inputId = "chbi_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "chbi_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "chbi_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "chbi_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "chbi_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("chbi_fsr")), align = "center"),
                column(1, h3(textOutput("chbi_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("MECH")),
                column(1, numericInput(
                  inputId = "mech_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "mech_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "mech_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "mech_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "mech_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("mech_fsr")), align = "center"),
                column(1, h3(textOutput("mech_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("CE TOTAL")),
                column(1, h3(textOutput("ce_100")), align = "center"),
                column(1, h3(textOutput("ce_50")), align = "center"),
                column(1, h3(textOutput("ce_25")), align = "center"),
                column(1, h3(textOutput("ce_0")), align = "center"),
                column(1, h3(textOutput("ce_quota_total")), align = "center"),
                column(1, h3(textOutput("ce_fsr")), align = "center"),
                column(1, h3(textOutput("ce_tpe")), align = "center")
              )
            ),
            wellPanel(
              style = "background: #dfe4ea",
              fluidRow(
                column(1, h3("")),
                column(1, h3("PHYS")),
                column(1, numericInput(
                  inputId = "phys_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "phys_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "phys_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "phys_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "phys_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("phys_fsr")), align = "center"),
                column(1, h3(textOutput("phys_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("CHEM")),
                column(1, numericInput(
                  inputId = "chem_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "chem_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "chem_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "chem_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "chem_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("chem_fsr")), align = "center"),
                column(1, h3(textOutput("chem_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("CS")),
                column(1, h3("MATH")),
                column(1, numericInput(
                  inputId = "math_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "math_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "math_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "math_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "math_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("math_fsr")), align = "center"),
                column(1, h3(textOutput("math_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("MBGE")),
                column(1, numericInput(
                  inputId = "mbge_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "mbge_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "mbge_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "mbge_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "mbge_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("mbge_fsr")), align = "center"),
                column(1, h3(textOutput("mbge_tpe")), align = "center")
              ),
              fluidRow(
                column(1, h3("")),
                column(1, h3("CS TOTAL")),
                column(1, h3(textOutput("cs_100")), align = "center"),
                column(1, h3(textOutput("cs_50")), align = "center"),
                column(1, h3(textOutput("cs_25")), align = "center"),
                column(1, h3(textOutput("cs_0")), align = "center"),
                column(1, h3(textOutput("cs_quota_total")), align = "center"),
                column(1, h3(textOutput("cs_fsr")), align = "center"),
                column(1, h3(textOutput("cs_tpe")), align = "center")
              )
            ),
            wellPanel(
              style = "background: #dfe4ea",
              fluidRow(
                column(1, h3("SON")),
                column(1, h3("NURS")),
                column(1, numericInput(
                  inputId = "nurs_100",
                  "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "nurs_50",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "nurs_25",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, numericInput(
                  inputId = "nurs_0",
                  label = "",
                  value = 0,
                  min = 0
                )),
                column(1, h3(textOutput(
                  "nurs_quota_total"
                )), align = "center"),
                column(1, h3(htmlOutput("nurs_fsr")), align = "center"),
                column(1, h3(textOutput("nurs_tpe")), align = "center")
              )
            ),
            wellPanel(
              style = "background: #dfe4ea",
              fluidRow(
                column(2, h2("KU TOTAL"), align = "center"),
                column(1, h2(textOutput("ku_100")), align = "center"),
                column(1, h2(textOutput("ku_50")), align = "center"),
                column(1, h2(textOutput("ku_25")), align = "center"),
                column(1, h2(textOutput("ku_0")), align = "center"),
                column(1, h2(textOutput("ku_quota_total")), align = "center"),
                column(1, h2(textOutput("ku_fsr")), align = "center"),
                column(1, h2(textOutput("ku_tpe")), align = "center")
              )
            ),
            
            plotOutput("histogram"),
            verbatimTextOutput("wBolum")
          ),
          tabPanel(
            title = ("Raw Data"),
            column(6, tableOutput("table")),
            column(2, downloadButton("rawDataDownload", "Export Raw Data")),
            tags$head(
              tags$style(
                ".butt2{background-color:black;} .butt2{color: white;} .butt2{font-style: italic;}"
              )
            )
          ),
          tabPanel(
            title = ("Statistics"),
            column(7, tableOutput("statisticsTable")),
            column(
              2,
              downloadButton("statisticsDownload", "Export Statistics")
            ),
            # making the font italics this time
            tags$head(
              tags$style(
                ".butt{background-color:orange;} .butt{color: black;} .butt{height: 100px;} .butt{width: 100%;} .butt{font-family: Courier New}"
              )
            ),
            tags$head(
              tags$style(
                ".myButton {
                                                                                white-space: normal;
                                                                                text-align:center;
                                                                                background-color:#44c767;
                                                                                -moz-border-radius:28px;
                                                                                -webkit-border-radius:28px;
                                                                                border-radius:28px;
                                                                                border:1px solid #18ab29;
                                                                                display:inline-block;
                                                                                cursor:pointer;
                                                                                color:#ffffff;
                                                                                font-family:Arial;
                                                                                font-size:18px;
                                                                                padding:16px 31px;
                                                                                text-decoration:none;
                                                                                text-shadow:0px 1px 0px #2f6627;
                                                                                }
                                                                                .myButton:hover {
                                                                                background-color:#5cbf2a;
                                                                                }
                                                                                .myButton:active {
                                                                                position:relative;
                                                                                top:1px;
                                                                                }"
              )
            )
          ),
          tabPanel(
            title = ("Yearly Comparison"),
            wellPanel(style = "background: #dfe4ea",
                      fluidRow(
                        column(
                          2,
                          fileInput(
                            'file4',
                            'Choose CSV Multiple Years Placement',
                            accept =
                              c('text/csv', 'text/comma-separated-values, text/plain')
                          )
                        ),
                        column(
                          1,
                          checkboxGroupInput(
                            "inCheckbox",
                            label = h3("Checkbox group"),
                            choiceNames = "Predicted Year",
                            choiceValues = 99,
                            selected = 99
                          )
                        ),
                        column(1, actionButton("buttonPlot", "Plot Selected Years")),
                        column(1, downloadButton("yealyGraphDownload", "Export Plot"))
                      )),
            plotOutput("yearlyPlot")
          )
        )
      ),
      tabPanel(
        title = "Placement Statistics",
        wellPanel(style = "background: #dfe4ea",
                  fluidRow(
                    column(2, fileInput(
                      'file3',
                      'Choose CSV Placement',
                      accept =
                        c('text/csv', 'text/comma-separated-values, text/plain')
                    )),
                    column(
                      1,
                      downloadButton("yerlestirmeStatisticsDownload", "Export Statistics")
                    )
                  )),
        tableOutput("yerlestirmeStatistics")
      )
      # tabPanel(title = "Documentation")
    )
  )
}

ui = (htmlOutput("page"))

# Front End Finsihes / Backend starts

server <- function(input, output, session) {
  options(scipen = 999) # Dont show scientific units
  
  USER <- reactiveValues(Logged = Logged)
  
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            }
          }
        }
      }
    }
  })
  observe({
    if (USER$Logged == FALSE) {
      output$page <- renderUI({
        div(class = "outer", do.call(bootstrapPage, c("", ui1())))
      })
    }
    if (USER$Logged == TRUE) {
      output$page <- renderUI({
        div(class = "outer", do.call(bootstrapPage, c("", ui2())))
      })
    }
  })
  
  
  # download csv
  output$scenarioDownload <- downloadHandler(
    filename = function() {
      paste("scenario" ,
            ku_tpe(),
            paste(ku_fsr(), "%"),
            paste(ku_quota_total(), ".csv"),
            sep = "_")
    },
    content = function(file) {
      write.csv(wBolum(), file)
    }
  )
  
  output$rawDataDownload <- downloadHandler(
    filename = function() {
      paste("rawdata" ,
            ku_tpe(),
            paste(ku_fsr(), "%"),
            paste(ku_quota_total(), ".csv"),
            sep = "_")
    },
    content = function(file) {
      write.csv(yResult(), file)
    }
  )
  
  output$statisticsDownload <- downloadHandler(
    filename = function() {
      paste(
        "statistics" ,
        ku_tpe(),
        paste(ku_fsr(), "%"),
        paste(ku_quota_total(), ".csv"),
        sep = "_"
      )
    },
    content = function(file) {
      write.csv(All_ist(), file)
    }
  )
  # read uploaded file
  uplodedScenario <- reactive({
    req(input$file1)
    
    df <- read.csv(
      input$file1$datapath,
      sep = ",",
      header = TRUE,
      check.names = FALSE
    )
    
    return(df)
    
  })
  
  
  observeEvent(input$file4, {
    df <- read.csv(
      input$file4$datapath,
      sep = ",",
      header = TRUE,
      check.names = FALSE
    )
    
    distinctVector <- unique(df[, 1])
    
    updateCheckboxGroupInput(
      session,
      "inCheckbox",
      choiceNames = as.list(c("Predicted Year", distinctVector)),
      choiceValues = as.list(c(99, seq(
        length(distinctVector)
      ))),
      selected = 99
    )
    
    
  })
  
  yearlyPlot <- eventReactive(input$buttonPlot, {
    plot(
      0,
      0,
      main = "Yearly Ranking Comparison Graph",
      xlab = "nt Student",
      ylab = "RANKING",
      xlim = c(-1, 1500),
      ylim = c(-1, 500000)
    )
    if (99 %in% input$inCheckbox && !is.null(yResult())) {
      points(1:nrow(yResult()),
             sort(yResult()$YERKULLANILANBASARISIRASI))
    }
    
    if (!is.null(input$file4)) {
      df <- read.csv(
        input$file4$datapath,
        sep = ",",
        header = TRUE,
        check.names = FALSE
      )
      
      distinctVector <- unique(df[, 1])
      
      yearGroupList <-
        list()  # placement data stored in list according to their years
      color <-
        c(
          "red",
          "blue",
          "green",
          "cyan",
          "azure",
          "deeppink",
          "darkgrey",
          "blueviolet",
          "darkorange"
        )
      
      for (i in (1:length(distinctVector))) {
        yearGroupList[[i]] = as.data.frame(df[which(df[, 1] == distinctVector[i]), ])
        if (i %in% input$inCheckbox) {
          lines(1:nrow(yearGroupList[[i]]), yearGroupList[[i]][order(as.numeric(as.character(yearGroupList[[i]][, 4]))), 4], col = color[i])
        }
      }
      legend("topleft",
             legend = distinctVector,
             col = color,
             pch = 19)
    }
  })
  
  output$yearlyPlot <- renderPlot({
    yearlyPlot()
  })
  
  
  
  # updateNumericInput from uplaoded file
  observeEvent(uplodedScenario(), {
    x <- uplodedScenario()
    updateNumericInput(session, "law_100", value = x[1, 2])
    updateNumericInput(session, "law_50", value = x[1, 3])
    updateNumericInput(session, "law_25", value = x[1, 4])
    updateNumericInput(session, "law_0", value = x[1, 5])
    updateNumericInput(session, "medi_100", value = x[2, 2])
    updateNumericInput(session, "medi_50", value = x[2, 3])
    updateNumericInput(session, "medi_25", value = x[2, 4])
    updateNumericInput(session, "medi_0", value = x[2, 5])
    updateNumericInput(session, "arha_100", value = x[3, 2])
    updateNumericInput(session, "arha_50", value = x[3, 3])
    updateNumericInput(session, "arha_25", value = x[3, 4])
    updateNumericInput(session, "arha_0", value = x[3, 5])
    updateNumericInput(session, "phil_100", value = x[4, 2])
    updateNumericInput(session, "phil_50", value = x[4, 3])
    updateNumericInput(session, "phil_25", value = x[4, 4])
    updateNumericInput(session, "phil_0", value = x[4, 5])
    updateNumericInput(session, "encl_100", value = x[5, 2])
    updateNumericInput(session, "encl_50", value = x[5, 3])
    updateNumericInput(session, "encl_25", value = x[5, 4])
    updateNumericInput(session, "encl_0", value = x[5, 5])
    updateNumericInput(session, "mava_100", value = x[6, 2])
    updateNumericInput(session, "mava_50", value = x[6, 3])
    updateNumericInput(session, "mava_25", value = x[6, 4])
    updateNumericInput(session, "mava_0", value = x[6, 5])
    updateNumericInput(session, "psyc_100", value = x[7, 2])
    updateNumericInput(session, "psyc_50", value = x[7, 3])
    updateNumericInput(session, "psyc_25", value = x[7, 4])
    updateNumericInput(session, "psyc_0", value = x[7, 5])
    updateNumericInput(session, "soci_100", value = x[8, 2])
    updateNumericInput(session, "soci_50", value = x[8, 3])
    updateNumericInput(session, "soci_25", value = x[8, 4])
    updateNumericInput(session, "soci_0", value = x[8, 5])
    updateNumericInput(session, "hist_100", value = x[9, 2])
    updateNumericInput(session, "hist_50", value = x[9, 3])
    updateNumericInput(session, "hist_25", value = x[9, 4])
    updateNumericInput(session, "hist_0", value = x[9, 5])
    updateNumericInput(session, "econ_100", value = x[10, 2])
    updateNumericInput(session, "econ_50", value = x[10, 3])
    updateNumericInput(session, "econ_25", value = x[10, 4])
    updateNumericInput(session, "econ_0", value = x[10, 5])
    updateNumericInput(session, "busad_100", value = x[11, 2])
    updateNumericInput(session, "busad_50", value = x[11, 3])
    updateNumericInput(session, "busad_25", value = x[11, 4])
    updateNumericInput(session, "busad_0", value = x[11, 5])
    updateNumericInput(session, "intl_100", value = x[12, 2])
    updateNumericInput(session, "intl_50", value = x[12, 3])
    updateNumericInput(session, "intl_25", value = x[12, 4])
    updateNumericInput(session, "intl_0", value = x[12, 5])
    updateNumericInput(session, "comp_100", value = x[13, 2])
    updateNumericInput(session, "comp_50", value = x[13, 3])
    updateNumericInput(session, "comp_25", value = x[13, 4])
    updateNumericInput(session, "comp_0", value = x[13, 5])
    updateNumericInput(session, "elec_100", value = x[14, 2])
    updateNumericInput(session, "elec_50", value = x[14, 3])
    updateNumericInput(session, "elec_25", value = x[14, 4])
    updateNumericInput(session, "elec_0", value = x[14, 5])
    updateNumericInput(session, "indr_100", value = x[15, 2])
    updateNumericInput(session, "indr_50", value = x[15, 3])
    updateNumericInput(session, "indr_25", value = x[15, 4])
    updateNumericInput(session, "indr_0", value = x[15, 5])
    updateNumericInput(session, "chbi_100", value = x[16, 2])
    updateNumericInput(session, "chbi_50", value = x[16, 3])
    updateNumericInput(session, "chbi_25", value = x[16, 4])
    updateNumericInput(session, "chbi_0", value = x[16, 5])
    updateNumericInput(session, "mech_100", value = x[17, 2])
    updateNumericInput(session, "mech_50", value = x[17, 3])
    updateNumericInput(session, "mech_25", value = x[17, 4])
    updateNumericInput(session, "mech_0", value = x[17, 5])
    updateNumericInput(session, "phys_100", value = x[18, 2])
    updateNumericInput(session, "phys_50", value = x[18, 3])
    updateNumericInput(session, "phys_25", value = x[18, 4])
    updateNumericInput(session, "phys_0", value = x[18, 5])
    updateNumericInput(session, "chem_100", value = x[19, 2])
    updateNumericInput(session, "chem_50", value = x[19, 3])
    updateNumericInput(session, "chem_25", value = x[19, 4])
    updateNumericInput(session, "chem_0", value = x[19, 5])
    updateNumericInput(session, "math_100", value = x[20, 2])
    updateNumericInput(session, "math_50", value = x[20, 3])
    updateNumericInput(session, "math_25", value = x[20, 4])
    updateNumericInput(session, "math_0", value = x[20, 5])
    updateNumericInput(session, "mbge_100", value = x[21, 2])
    updateNumericInput(session, "mbge_50", value = x[21, 3])
    updateNumericInput(session, "mbge_25", value = x[21, 4])
    updateNumericInput(session, "mbge_0", value = x[21, 5])
    updateNumericInput(session, "nurs_100", value = x[22, 2])
    updateNumericInput(session, "nurs_50", value = x[22, 3])
    updateNumericInput(session, "nurs_25", value = x[22, 4])
    updateNumericInput(session, "nurs_0", value = x[22, 5])
  })
  
  
  
  
  # Law Total
  output$law_quota_total <- reactive({
    input$law_100 + input$law_50 + input$law_25 + input$law_0
  })
  law_fsr <- reactive({
    input$law_100 / (input$law_100 + input$law_50 + input$law_25 + input$law_0) *
      100
  })
  output$law_fsr <- renderText({
    if (is.nan(law_fsr())) {
      return("")
    }
    else if (law_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(law_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(law_fsr(), 0), "%"))
    }
  })
  output$law_tpe <- reactive({
    digit(input$law_0 + 0.75 * input$law_25 + 0.5 * input$law_50, 2)
  })
  # Medi Total
  output$medi_quota_total <- reactive({
    input$medi_100 + input$medi_50 + input$medi_25 + input$medi_0
  })
  medi_fsr <- reactive({
    input$medi_100 / (input$medi_100 + input$medi_50 + input$medi_25 + input$medi_0) *
      100
  })
  output$medi_fsr <- renderText({
    if (is.nan(medi_fsr())) {
      return("")
    }
    else if (medi_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(medi_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(medi_fsr(), 0), "%"))
    }
  })
  output$medi_tpe <- reactive({
    digit(input$medi_0 + 0.75 * input$medi_25 + 0.5 * input$medi_50, 2)
  })
  # Arha Total
  output$arha_quota_total <- reactive({
    input$arha_100 + input$arha_50 + input$arha_25 + input$arha_0
  })
  arha_fsr <- reactive({
    input$arha_100 / (input$arha_100 + input$arha_50 + input$arha_25 + input$arha_0) *
      100
  })
  output$arha_fsr <- renderText({
    if (is.nan(arha_fsr())) {
      return("")
    }
    else if (arha_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(arha_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(arha_fsr(), 0), "%"))
    }
  })
  output$arha_tpe <- reactive({
    digit(input$arha_0 + 0.75 * input$arha_25 + 0.5 * input$arha_50, 2)
  })
  # Phil Total
  output$phil_quota_total <- reactive({
    input$phil_100 + input$phil_50 + input$phil_25 + input$phil_0
  })
  phil_fsr <- reactive({
    input$phil_100 / (input$phil_100 + input$phil_50 + input$phil_25 + input$phil_0) *
      100
  })
  output$phil_fsr <- renderText({
    if (is.nan(phil_fsr())) {
      return("")
    }
    else if (phil_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(phil_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(phil_fsr(), 0), "%"))
    }
  })
  output$phil_tpe <- reactive({
    digit(input$phil_0 + 0.75 * input$phil_25 + 0.5 * input$phil_50, 2)
  })
  # Encl Total
  output$encl_quota_total <- reactive({
    input$encl_100 + input$encl_50 + input$encl_25 + input$encl_0
  })
  encl_fsr <- reactive({
    input$encl_100 / (input$encl_100 + input$encl_50 + input$encl_25 + input$encl_0) *
      100
  })
  output$encl_fsr <- renderText({
    if (is.nan(encl_fsr())) {
      return("")
    }
    else if (encl_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(encl_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(encl_fsr(), 0), "%"))
    }
  })
  output$encl_tpe <- reactive({
    digit(input$encl_0 + 0.75 * input$encl_25 + 0.5 * input$encl_50, 2)
  })
  # Mava Total
  output$mava_quota_total <- reactive({
    input$mava_100 + input$mava_50 + input$mava_25 + input$mava_0
  })
  mava_fsr <- reactive({
    input$mava_100 / (input$mava_100 + input$mava_50 + input$mava_25 + input$mava_0) *
      100
  })
  output$mava_fsr <- renderText({
    if (is.nan(mava_fsr())) {
      return("")
    }
    else if (mava_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(mava_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(mava_fsr(), 0), "%"))
    }
  })
  output$mava_tpe <- reactive({
    digit(input$mava_0 + 0.75 * input$mava_25 + 0.5 * input$mava_50, 2)
  })
  # Psyc Total
  output$psyc_quota_total <- reactive({
    input$psyc_100 + input$psyc_50 + input$psyc_25 + input$psyc_0
  })
  psyc_fsr <- reactive({
    input$psyc_100 / (input$psyc_100 + input$psyc_50 + input$psyc_25 + input$psyc_0) *
      100
  })
  output$psyc_fsr <- renderText({
    if (is.nan(psyc_fsr())) {
      return("")
    }
    else if (psyc_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(psyc_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(psyc_fsr(), 0), "%"))
    }
  })
  output$psyc_tpe <- reactive({
    digit(input$psyc_0 + 0.75 * input$psyc_25 + 0.5 * input$psyc_50, 2)
  })
  # Soci Total
  output$soci_quota_total <- reactive({
    input$soci_100 + input$soci_50 + input$soci_25 + input$soci_0
  })
  soci_fsr <- reactive({
    input$soci_100 / (input$soci_100 + input$soci_50 + input$soci_25 + input$soci_0) *
      100
  })
  output$soci_fsr <- renderText({
    if (is.nan(soci_fsr())) {
      return("")
    }
    else if (soci_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(soci_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(soci_fsr(), 0), "%"))
    }
  })
  output$soci_tpe <- reactive({
    digit(input$soci_0 + 0.75 * input$soci_25 + 0.5 * input$soci_50, 2)
  })
  # Hist Total
  output$hist_quota_total <- reactive({
    input$hist_100 + input$hist_50 + input$hist_25 + input$hist_0
  })
  hist_fsr <- reactive({
    input$hist_100 / (input$hist_100 + input$hist_50 + input$hist_25 + input$hist_0) *
      100
  })
  output$hist_fsr <- renderText({
    if (is.nan(hist_fsr())) {
      return("")
    }
    else if (hist_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(hist_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(hist_fsr(), 0), "%"))
    }
  })
  output$hist_tpe <- reactive({
    digit(input$hist_0 + 0.75 * input$hist_25 + 0.5 * input$hist_50, 2)
  })
  # Econ Total
  output$econ_quota_total <- reactive({
    input$econ_100 + input$econ_50 + input$econ_25 + input$econ_0
  })
  econ_fsr <- reactive({
    input$econ_100 / (input$econ_100 + input$econ_50 + input$econ_25 + input$econ_0) *
      100
  })
  output$econ_fsr <- renderText({
    if (is.nan(econ_fsr())) {
      return("")
    }
    else if (econ_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(econ_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(econ_fsr(), 0), "%"))
    }
  })
  output$econ_tpe <- reactive({
    digit(input$econ_0 + 0.75 * input$econ_25 + 0.5 * input$econ_50, 2)
  })
  # Busad Total
  output$busad_quota_total <- reactive({
    input$busad_100 + input$busad_50 + input$busad_25 + input$busad_0
  })
  busad_fsr <- reactive({
    input$busad_100 / (input$busad_100 + input$busad_50 + input$busad_25 + input$busad_0) *
      100
  })
  output$busad_fsr <- renderText({
    if (is.nan(busad_fsr())) {
      return("")
    }
    else if (busad_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(busad_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(busad_fsr(), 0), "%"))
    }
  })
  output$busad_tpe <- reactive({
    digit(input$busad_0 + 0.75 * input$busad_25 + 0.5 * input$busad_50, 2)
  })
  # Intl Total
  output$intl_quota_total <- reactive({
    input$intl_100 + input$intl_50 + input$intl_25 + input$intl_0
  })
  intl_fsr <- reactive({
    input$intl_100 / (input$intl_100 + input$intl_50 + input$intl_25 + input$intl_0) *
      100
  })
  output$intl_fsr <- renderText({
    if (is.nan(intl_fsr())) {
      return("")
    }
    else if (intl_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(intl_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(intl_fsr(), 0), "%"))
    }
  })
  output$intl_tpe <- reactive({
    digit(input$intl_0 + 0.75 * input$intl_25 + 0.5 * input$intl_50, 2)
  })
  # Comp Total
  output$comp_quota_total <- reactive({
    input$comp_100 + input$comp_50 + input$comp_25 + input$comp_0
  })
  comp_fsr <- reactive({
    input$comp_100 / (input$comp_100 + input$comp_50 + input$comp_25 + input$comp_0) *
      100
  })
  output$comp_fsr <- renderText({
    if (is.nan(comp_fsr())) {
      return("")
    }
    else if (comp_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(comp_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(comp_fsr(), 0), "%"))
    }
  })
  output$comp_tpe <- reactive({
    digit(input$comp_0 + 0.75 * input$comp_25 + 0.5 * input$comp_50, 2)
  })
  # Elec Total
  output$elec_quota_total <- reactive({
    input$elec_100 + input$elec_50 + input$elec_25 + input$elec_0
  })
  elec_fsr <- reactive({
    input$elec_100 / (input$elec_100 + input$elec_50 + input$elec_25 + input$elec_0) *
      100
  })
  output$elec_fsr <- renderText({
    if (is.nan(elec_fsr())) {
      return("")
    }
    else if (elec_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(elec_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(elec_fsr(), 0), "%"))
    }
  })
  output$elec_tpe <- reactive({
    digit(input$elec_0 + 0.75 * input$elec_25 + 0.5 * input$elec_50, 2)
  })
  # Indr Total
  output$indr_quota_total <- reactive({
    input$indr_100 + input$indr_50 + input$indr_25 + input$indr_0
  })
  indr_fsr <- reactive({
    input$indr_100 / (input$indr_100 + input$indr_50 + input$indr_25 + input$indr_0) *
      100
  })
  output$indr_fsr <- renderText({
    if (is.nan(indr_fsr())) {
      return("")
    }
    else if (indr_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(indr_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(indr_fsr(), 0), "%"))
    }
  })
  output$indr_tpe <- reactive({
    digit(input$indr_0 + 0.75 * input$indr_25 + 0.5 * input$indr_50, 2)
  })
  # Chbi Total
  output$chbi_quota_total <- reactive({
    input$chbi_100 + input$chbi_50 + input$chbi_25 + input$chbi_0
  })
  chbi_fsr <- reactive({
    input$chbi_100 / (input$chbi_100 + input$chbi_50 + input$chbi_25 + input$chbi_0) *
      100
  })
  output$chbi_fsr <- renderText({
    if (is.nan(chbi_fsr())) {
      return("")
    }
    else if (chbi_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(chbi_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(chbi_fsr(), 0), "%"))
    }
  })
  output$chbi_tpe <- reactive({
    digit(input$chbi_0 + 0.75 * input$chbi_25 + 0.5 * input$chbi_50, 2)
  })
  # Mech Total
  output$mech_quota_total <- reactive({
    input$mech_100 + input$mech_50 + input$mech_25 + input$mech_0
  })
  mech_fsr <- reactive({
    input$mech_100 / (input$mech_100 + input$mech_50 + input$mech_25 + input$mech_0) *
      100
  })
  output$mech_fsr <- renderText({
    if (is.nan(mech_fsr())) {
      return("")
    }
    else if (mech_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(mech_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(mech_fsr(), 0), "%"))
    }
  })
  output$mech_tpe <- reactive({
    digit(input$mech_0 + 0.75 * input$mech_25 + 0.5 * input$mech_50, 2)
  })
  # Phys Total
  output$phys_quota_total <- reactive({
    input$phys_100 + input$phys_50 + input$phys_25 + input$phys_0
  })
  phys_fsr <- reactive({
    input$phys_100 / (input$phys_100 + input$phys_50 + input$phys_25 + input$phys_0) *
      100
  })
  output$phys_fsr <- renderText({
    if (is.nan(phys_fsr())) {
      return("")
    }
    else if (phys_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(phys_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(phys_fsr(), 0), "%"))
    }
  })
  output$phys_tpe <- reactive({
    digit(input$phys_0 + 0.75 * input$phys_25 + 0.5 * input$phys_50, 2)
  })
  # Chem Total
  output$chem_quota_total <- reactive({
    input$chem_100 + input$chem_50 + input$chem_25 + input$chem_0
  })
  chem_fsr <- reactive({
    input$chem_100 / (input$chem_100 + input$chem_50 + input$chem_25 + input$chem_0) *
      100
  })
  output$chem_fsr <- renderText({
    if (is.nan(chem_fsr())) {
      return("")
    }
    else if (chem_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(chem_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(chem_fsr(), 0), "%"))
    }
  })
  output$chem_tpe <- reactive({
    digit(input$chem_0 + 0.75 * input$chem_25 + 0.5 * input$chem_50, 2)
  })
  # Math Total
  output$math_quota_total <- reactive({
    input$math_100 + input$math_50 + input$math_25 + input$math_0
  })
  math_fsr <- reactive({
    input$math_100 / (input$math_100 + input$math_50 + input$math_25 + input$math_0) *
      100
  })
  output$math_fsr <- renderText({
    if (is.nan(math_fsr())) {
      return("")
    }
    else if (math_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(math_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(math_fsr(), 0), "%"))
    }
  })
  output$math_tpe <- reactive({
    digit(input$math_0 + 0.75 * input$math_25 + 0.5 * input$math_50, 2)
  })
  # Mbge Total
  output$mbge_quota_total <- reactive({
    input$mbge_100 + input$mbge_50 + input$mbge_25 + input$mbge_0
  })
  mbge_fsr <- reactive({
    input$mbge_100 / (input$mbge_100 + input$mbge_50 + input$mbge_25 + input$mbge_0) *
      100
  })
  output$mbge_fsr <- renderText({
    if (is.nan(mbge_fsr())) {
      return("")
    }
    else if (mbge_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(mbge_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(mbge_fsr(), 0), "%"))
    }
  })
  output$mbge_tpe <- reactive({
    digit(input$mbge_0 + 0.75 * input$mbge_25 + 0.5 * input$mbge_50, 2)
  })
  # Nurs Total
  output$nurs_quota_total <- reactive({
    input$nurs_100 + input$nurs_50 + input$nurs_25 + input$nurs_0
  })
  nurs_fsr <- reactive({
    input$nurs_100 / (input$nurs_100 + input$nurs_50 + input$nurs_25 + input$nurs_0) *
      100
  })
  output$nurs_fsr <- renderText({
    if (is.nan(nurs_fsr())) {
      return("")
    }
    else if (nurs_fsr() < 10) {
      return(paste(tags$span(style = "color:red", paste(
        digit(nurs_fsr(), 1), "%"
      ))))
    }
    else {
      return(paste(digit(nurs_fsr(), 0), "%"))
    }
  })
  output$nurs_tpe <- reactive({
    digit(input$nurs_0 + 0.75 * input$nurs_25 + 0.5 * input$nurs_50, 2)
  })
  # CSSH Total
  cssh_100 <- reactive({
    input$arha_100 + input$phil_100 + input$encl_100 + input$mava_100 + input$psyc_100 + input$soci_100 + input$hist_100
  })
  cssh_50 <- reactive({
    input$arha_50 + input$phil_50 + input$encl_50 + input$mava_50 + input$psyc_50 + input$soci_50 + input$hist_50
  })
  cssh_25 <- reactive({
    input$arha_25 + input$phil_25 + input$encl_25 + input$mava_25 + input$psyc_25 + input$soci_25 + input$hist_25
  })
  cssh_0 <- reactive({
    input$arha_0 + input$phil_0 + input$encl_0 + input$mava_0 + input$psyc_0 + input$soci_0 + input$hist_0
  })
  output$cssh_100 <- reactive({
    cssh_100()
  })
  output$cssh_50 <- reactive({
    cssh_50()
  })
  output$cssh_25 <- reactive({
    cssh_25()
  })
  output$cssh_0 <- reactive({
    cssh_0()
  })
  output$cssh_quota_total <- reactive({
    cssh_100() + cssh_50() + cssh_25() + cssh_0()
  })
  cssh_fsr <- reactive({
    cssh_100() / (cssh_100() + cssh_50 () + cssh_25() + cssh_0()) * 100
  })
  output$cssh_fsr <- renderText({
    if (is.nan(cssh_fsr())) {
      return("")
    }
    else {
      return(paste(digit(cssh_fsr(), 0), "%"))
    }
  })
  output$cssh_tpe <- reactive({
    digit(cssh_0() + 0.75 * cssh_25() + 0.5 * cssh_50(), 2)
  })
  # CASE Total
  case_100 <- reactive({
    input$econ_100 + input$busad_100 + input$intl_100
  })
  case_50 <- reactive({
    input$econ_50 + input$busad_50 + input$intl_50
  })
  case_25 <- reactive({
    input$econ_25 + input$busad_25 + input$intl_25
  })
  case_0 <- reactive({
    input$econ_0 + input$busad_0 + input$intl_0
  })
  output$case_100 <- reactive({
    case_100()
  })
  output$case_50 <- reactive({
    case_50()
  })
  output$case_25 <- reactive({
    case_25()
  })
  output$case_0 <- reactive({
    case_0()
  })
  output$case_quota_total <- reactive({
    case_100() + case_50() + case_25() + case_0()
  })
  case_fsr <- reactive({
    case_100() / (case_100() + case_50 () + case_25() + case_0()) * 100
  })
  output$case_fsr <- renderText({
    if (is.nan(case_fsr())) {
      return("")
    }
    else {
      return(paste(digit(case_fsr(), 0), "%"))
    }
  })
  output$case_tpe <- reactive({
    digit(case_0() + 0.75 * case_25() + 0.5 * case_50(), 2)
  })
  # CE Total
  ce_100 <- reactive({
    input$comp_100 + input$elec_100 + input$indr_100 + input$chbi_100 + input$mech_100
  })
  ce_50 <- reactive({
    input$comp_50 + input$elec_50 + input$indr_50 + input$chbi_50 + input$mech_50
  })
  ce_25 <- reactive({
    input$comp_25 + input$elec_25 + input$indr_25 + input$chbi_25 + input$mech_25
  })
  ce_0 <- reactive({
    input$comp_0 + input$elec_0 + input$indr_0 + input$chbi_0 + input$mech_0
  })
  output$ce_100 <- reactive({
    ce_100()
  })
  output$ce_50 <- reactive({
    ce_50()
  })
  output$ce_25 <- reactive({
    ce_25()
  })
  output$ce_0 <- reactive({
    ce_0()
  })
  output$ce_quota_total <- reactive({
    ce_100() + ce_50() + ce_25() + ce_0()
  })
  ce_fsr <- reactive({
    ce_100() / (ce_100() + ce_50 () + ce_25() + ce_0()) * 100
  })
  output$ce_fsr <- renderText({
    if (is.nan(ce_fsr())) {
      return("")
    }
    else {
      return(paste(digit(ce_fsr(), 0), "%"))
    }
  })
  output$ce_tpe <- reactive({
    digit(ce_0() + 0.75 * ce_25() + 0.5 * ce_50(), 2)
  })
  # CS Total
  cs_100 <- reactive({
    input$phys_100 + input$chem_100 + input$math_100 + input$mbge_100
  })
  cs_50 <- reactive({
    input$phys_50 + input$chem_50 + input$math_50 + input$mbge_50
  })
  cs_25 <- reactive({
    input$phys_25 + input$chem_25 + input$math_25 + input$mbge_25
  })
  cs_0 <- reactive({
    input$phys_0 + input$chem_0 + input$math_0 + input$mbge_0
  })
  output$cs_100 <- reactive({
    cs_100()
  })
  output$cs_50 <- reactive({
    cs_50()
  })
  output$cs_25 <- reactive({
    cs_25()
  })
  output$cs_0 <- reactive({
    cs_0()
  })
  output$cs_quota_total <- reactive({
    cs_100() + cs_50() + cs_25() + cs_0()
  })
  cs_fsr <- reactive({
    cs_100() / (cs_100() + cs_50 () + cs_25() + cs_0()) * 100
  })
  output$cs_fsr <- renderText({
    if (is.nan(cs_fsr())) {
      return("")
    }
    else {
      return(paste(digit(cs_fsr(), 0), "%"))
    }
  })
  output$cs_tpe <- reactive({
    digit(cs_0() + 0.75 * cs_25() + 0.5 * cs_50(), 2)
  })
  # KU Total
  ku_100 <- reactive({
    input$arha_100 + input$phil_100 + input$encl_100 + input$mava_100 + input$psyc_100 + input$soci_100 + input$hist_100 + input$econ_100 + input$busad_100 + input$intl_100 + input$comp_100 + input$elec_100 + input$indr_100 + input$chbi_100 + input$mech_100 + input$phys_100 + input$chem_100 + input$math_100 + input$mbge_100 + input$law_100 + input$medi_100 + input$nurs_100
  })
  ku_50 <- reactive({
    input$arha_50 + input$phil_50 + input$encl_50 + input$mava_50 + input$psyc_50 + input$soci_50 + input$hist_50 + input$econ_50 + input$busad_50 + input$intl_50 + input$comp_50 + input$elec_50 + input$indr_50 + input$chbi_50 + input$mech_50 + input$phys_50 + input$chem_50 + input$math_50 + input$mbge_50 + input$law_50 + input$medi_50 + input$nurs_50
  })
  ku_25 <- reactive({
    input$arha_25 + input$phil_25 + input$encl_25 + input$mava_25 + input$psyc_25 + input$soci_25 + input$hist_25 + input$econ_25 + input$busad_25 + input$intl_25 + input$comp_25 + input$elec_25 + input$indr_25 + input$chbi_25 + input$mech_25 + input$phys_25 + input$chem_25 + input$math_25 + input$mbge_25 + input$law_25 + input$medi_25 + input$nurs_25
  })
  ku_0 <- reactive({
    input$arha_0 + input$phil_0 + input$encl_0 + input$mava_0 + input$psyc_0 + input$soci_0 + input$hist_0 + input$econ_0 + input$busad_0 + input$intl_0 + input$comp_0 + input$elec_0 + input$indr_0 + input$chbi_0 + input$mech_0 + input$phys_0 + input$chem_0 + input$math_0 + input$mbge_0 + input$law_0 + input$medi_0 + input$nurs_0
  })
  output$ku_100 <- reactive({
    ku_100()
  })
  output$ku_50 <- reactive({
    ku_50()
  })
  output$ku_25 <- reactive({
    ku_25()
  })
  output$ku_0 <- reactive({
    ku_0()
  })
  ku_quota_total <- reactive({
    ku_100() + ku_50() + ku_25() + ku_0()
  })
  output$ku_quota_total <- reactive({
    ku_quota_total()
  })
  ku_fsr <- reactive({
    digit((ku_100() / (ku_100() + ku_50 () + ku_25() + ku_0()) * 100) , 2)
  })
  output$ku_fsr <- renderText({
    if (ku_fsr() == "NaN") {
      return("")
    }
    else {
      return(paste(ku_fsr(), "%"))
    }
  })
  ku_tpe <- reactive({
    digit(ku_0() + 0.75 * ku_25() + 0.5 * ku_50(), 2)
  })
  output$ku_tpe <- reactive({
    ku_tpe()
  })
  
  
  # Decimal Place function
  digit <- function(x, k)
    trimws(format(round(x, k), nsmall = k))
  wBolum100 <- reactive({
    c(
      input$law_100,
      input$medi_100,
      input$arha_100,
      input$phil_100,
      input$encl_100,
      input$mava_100,
      input$psyc_100,
      input$soci_100,
      input$hist_100,
      input$econ_100,
      input$busad_100,
      input$intl_100,
      input$comp_100,
      input$elec_100,
      input$indr_100,
      input$chbi_100,
      input$mech_100,
      input$phys_100,
      input$chem_100,
      input$math_100,
      input$mbge_100,
      input$nurs_100
    )
  })
  wBolum50 <- reactive({
    c(
      input$law_50,
      input$medi_50,
      input$arha_50,
      input$phil_50,
      input$encl_50,
      input$mava_50,
      input$psyc_50,
      input$soci_50,
      input$hist_50,
      input$econ_50,
      input$busad_50,
      input$intl_50,
      input$comp_50,
      input$elec_50,
      input$indr_50,
      input$chbi_50,
      input$mech_50,
      input$phys_50,
      input$chem_50,
      input$math_50,
      input$mbge_50,
      input$nurs_50
    )
  })
  wBolum25 <- reactive({
    c(
      input$law_25,
      input$medi_25,
      input$arha_25,
      input$phil_25,
      input$encl_25,
      input$mava_25,
      input$psyc_25,
      input$soci_25,
      input$hist_25,
      input$econ_25,
      input$busad_25,
      input$intl_25,
      input$comp_25,
      input$elec_25,
      input$indr_25,
      input$chbi_25,
      input$mech_25,
      input$phys_25,
      input$chem_25,
      input$math_25,
      input$mbge_25,
      input$nurs_25
    )
  })
  wBolum0 <- reactive({
    c(
      input$law_0,
      input$medi_0,
      input$arha_0,
      input$phil_0,
      input$encl_0,
      input$mava_0,
      input$psyc_0,
      input$soci_0,
      input$hist_0,
      input$econ_0,
      input$busad_0,
      input$intl_0,
      input$comp_0,
      input$elec_0,
      input$indr_0,
      input$chbi_0,
      input$mech_0,
      input$phys_0,
      input$chem_0,
      input$math_0,
      input$mbge_0,
      input$nurs_0
    )
  })
  
  wBolum <- reactive({
    wBolum = cbind(wBolum100(), wBolum50(), wBolum25(), wBolum0())
    rownames(wBolum) = alt_bolum_names
    colnames(wBolum) = burslar
    return(wBolum)
  })
  
  
  
  # vector of department names
  Bolum_names = c(
    "BUSAD",
    "ECON",
    "INTL",
    "CHBI",
    "COMP",
    "ELEC",
    "INDR",
    "MECH",
    "CHEM",
    "MATH",
    "MBGE",
    "PHYS",
    "ARHA",
    "ENCL",
    "HIST",
    "MAVA",
    "PHIL",
    "PSYC",
    "SOCI",
    "LAW",
    "MEDI",
    "NURS"
  )
  alt_bolum_names = c(
    "LAW",
    "MEDI",
    "ARHA",
    "PHIL",
    "ENCL",
    "MAVA",
    "PSYC",
    "SOCI",
    "HIST",
    "ECON",
    "BUSAD",
    "INTL",
    "COMP",
    "ELEC",
    "INDR",
    "CHBI",
    "MECH",
    "PHYS",
    "CHEM",
    "MATH",
    "MBGE",
    "NURS"
  )
  burslar <- c("100%", "50%", "25%", "0%")
  # vector of college names
  Fakulte_names = c("CASE", "CE", "CS", "CSSH")
  
  # vector of score type names
  Puan_names = c("EA", "SAY", "SOZ", "DIL")
  
  # names of GIRDIGIBOLUMADI column
  # divided into four according to their scholarship values
  Bolum_Burslu_names = c(
    "ISLETME (INGILIZCE) (BURSLU)",
    "EKONOMI (INGILIZCE) (BURSLU)",
    "ULUSLARARASI ILISKILER (INGILIZCE) (BURSLU)",
    "KIMYA-BIYOLOJI MUHENDISLIGI (INGILIZCE) (BURSLU)",
    "BILGISAYAR MUHENDISLIGI (INGILIZCE) (BURSLU)",
    "ELEKTRIK-ELEKTRONIK MUHENDISLIGI (INGILIZCE) (BURSLU)",
    "ENDUSTRI MUHENDISLIGI (INGILIZCE) (BURSLU)",
    "MAKINE MUHENDISLIGI (INGILIZCE) (BURSLU)",
    "KIMYA (INGILIZCE) (BURSLU)",
    "MATEMATIK (INGILIZCE) (BURSLU)",
    "MOLEKULER BIYOLOJI VE GENETIK (INGILIZCE) (BURSLU)",
    "FIZIK (INGILIZCE) (BURSLU)",
    "ARKEOLOJI VE SANAT TARIHI (INGILIZCE) (BURSLU)",
    "INGILIZ DILI VE KARSILASTIRMALI EDEBIYAT (INGILIZCE) (BURSLU)",
    "TARIH (INGILIZCE) (BURSLU)",
    "MEDYA VE GORSEL SANATLAR (INGILIZCE) (BURSLU)",
    "FELSEFE (INGILIZCE) (BURSLU)",
    "PSIKOLOJI (INGILIZCE) (BURSLU)",
    "SOSYOLOJI (INGILIZCE) (BURSLU)",
    "HUKUK FAKULTESI (BURSLU)",
    "TIP FAKULTESI (INGILIZCE) (BURSLU)",
    "HEMSIRELIK (BURSLU)"
  )
  Bolum_50_names = c(
    "ISLETME (INGILIZCE) (%50 INDIRIMLI)",
    "EKONOMI (INGILIZCE) (%50 INDIRIMLI)",
    "ULUSLARARASI ILISKILER (INGILIZCE) (%50 INDIRIMLI)",
    "KIMYA-BIYOLOJI MUHENDISLIGI (INGILIZCE) (%50 INDIRIMLI)",
    "BILGISAYAR MUHENDISLIGI (INGILIZCE) (%50 INDIRIMLI)",
    "ELEKTRIK-ELEKTRONIK MUHENDISLIGI (INGILIZCE) (%50 INDIRIMLI)",
    "ENDUSTRI MUHENDISLIGI (INGILIZCE) (%50 INDIRIMLI)",
    "MAKINE MUHENDISLIGI (INGILIZCE) (%50 INDIRIMLI)",
    "KIMYA (INGILIZCE) (%50 INDIRIMLI)",
    "MATEMATIK (INGILIZCE) (%50 INDIRIMLI)",
    "MOLEKULER BIYOLOJI VE GENETIK (INGILIZCE) (%50 INDIRIMLI)",
    "FIZIK (INGILIZCE) (%50 INDIRIMLI)",
    "ARKEOLOJI VE SANAT TARIHI (INGILIZCE) (%50 INDIRIMLI)",
    "INGILIZ DILI VE KARSILASTIRMALI EDEBIYAT (INGILIZCE) (%50 INDIRIMLI)",
    "TARIH (INGILIZCE) (%50 INDIRIMLI)",
    "MEDYA VE GORSEL SANATLAR (INGILIZCE) (%50 INDIRIMLI)",
    "FELSEFE (INGILIZCE) (%50 INDIRIMLI)",
    "PSIKOLOJI (INGILIZCE) (%50 INDIRIMLI)",
    "SOSYOLOJI (INGILIZCE) (%50 INDIRIMLI)",
    "HUKUK FAKULTESI (%50 INDIRIMLI)",
    "TIP FAKULTESI (INGILIZCE) (%50 INDIRIMLI)",
    "HEMSIRELIK (%50 INDIRIMLI)"
  )
  Bolum_25_names = c(
    "ISLETME (INGILIZCE) (%25 INDIRIMLI)",
    "EKONOMI (INGILIZCE) (%25 INDIRIMLI)",
    "ULUSLARARASI ILISKILER (INGILIZCE) (%25 INDIRIMLI)",
    "KIMYA-BIYOLOJI MUHENDISLIGI (INGILIZCE) (%25 INDIRIMLI)",
    "BILGISAYAR MUHENDISLIGI (INGILIZCE) (%25 INDIRIMLI)",
    "ELEKTRIK-ELEKTRONIK MUHENDISLIGI (INGILIZCE) (%25 INDIRIMLI)",
    "ENDUSTRI MUHENDISLIGI (INGILIZCE) (%25 INDIRIMLI)",
    "MAKINE MUHENDISLIGI (INGILIZCE) (%25 INDIRIMLI)",
    "KIMYA (INGILIZCE) (%25 INDIRIMLI)",
    "MATEMATIK (INGILIZCE) (%25 INDIRIMLI)",
    "MOLEKULER BIYOLOJI VE GENETIK (INGILIZCE) (%25 INDIRIMLI)",
    "FIZIK (INGILIZCE) (%25 INDIRIMLI)",
    "ARKEOLOJI VE SANAT TARIHI (INGILIZCE) (%25 INDIRIMLI)",
    "INGILIZ DILI VE KARSILASTIRMALI EDEBIYAT (INGILIZCE) (%25 INDIRIMLI)",
    "TARIH (INGILIZCE) (%25 INDIRIMLI)",
    "MEDYA VE GORSEL SANATLAR (INGILIZCE) (%25 INDIRIMLI)",
    "FELSEFE (INGILIZCE) (%25 INDIRIMLI)",
    "PSIKOLOJI (INGILIZCE) (%25 INDIRIMLI)",
    "SOSYOLOJI (INGILIZCE) (%25 INDIRIMLI)",
    "HUKUK FAKULTESI (%25 INDIRIMLI)",
    "TIP FAKULTESI (INGILIZCE) (%25 INDIRIMLI)",
    "HEMSIRELIK (%25 INDIRIMLI)"
  )
  Bolum_0_names = c(
    "ISLETME (INGILIZCE) (0)",
    "EKONOMI (INGILIZCE) (0)",
    "ULUSLARARASI ILISKILER (INGILIZCE) (0)",
    "KIMYA-BIYOLOJI MUHENDISLIGI (INGILIZCE) (0)",
    "BILGISAYAR MUHENDISLIGI (INGILIZCE) (0)",
    "ELEKTRIK-ELEKTRONIK MUHENDISLIGI (INGILIZCE) (0)",
    "ENDUSTRI MUHENDISLIGI (INGILIZCE) (0)",
    "MAKINE MUHENDISLIGI (INGILIZCE) (0)",
    "KIMYA (INGILIZCE) (0)",
    "MATEMATIK (INGILIZCE) (0)",
    "MOLEKULER BIYOLOJI VE GENETIK (INGILIZCE) (0)",
    "FIZIK (INGILIZCE) (0)",
    "ARKEOLOJI VE SANAT TARIHI (INGILIZCE) (0)",
    "INGILIZ DILI VE KARSILASTIRMALI EDEBIYAT (INGILIZCE) (0)",
    "TARIH (INGILIZCE) (0)",
    "MEDYA VE GORSEL SANATLAR (INGILIZCE) (0)",
    "FELSEFE (INGILIZCE) (0)",
    "PSIKOLOJI (INGILIZCE) (0)",
    "SOSYOLOJI (INGILIZCE) (0)",
    "HUKUK FAKULTESI (0)",
    "TIP FAKULTESI (INGILIZCE) (0)",
    "HEMSIRELIK (0)"
  )
  
  # declaration of dataframes which will be assigned after slicing
  # yBolum -> list of departments containing final students
  # ex: yBolum[[1]] contains final students of BUSAD, yBolum[[2]] contains final students of ECON, ...
  # yFakulte -> list of colleges containing final students
  # ex: yFakulte[[1]] contains final students of CASE, yFakulte[[2]] contains final students of CE, ...
  # yPuan -> list of score types containing final students
  # ex: yPuan[[2]] contains final students of EA, yPuan[[2]] contains final students of SAY, ...
  yPuan <- yFakulte <- yBolum <- list()
  
  
  # decleration of dataframes which is used in slicing part
  # zBolum.. -> as an intermediate step in slicing part
  # yBolum.. -> as an output of slicing part
  yBolum100_1 <-
    yBolum100_2 <-
    yBolum50_1 <-
    yBolum50_2 <-
    yBolum25_1 <-
    yBolum25_2 <-
    yBolum0_1 <-
    yBolum0_2 <- zBolum50_1 <- zBolum25_1 <- zBolum0_1 <- list()
  
  # declerations for statistics extractions:
  # ex: yBolum_ilk_5bin[[1]] contains the final students of BUSAD whose rankings are below 5 thousand, yBolum_ilk_5bin[[2]] contains final students of ECON whose rankings are below 5 thousand
  yBolum_Rows <-
    yBolum_ilk_bin <-
    yBolum_ilk_5bin <-
    yBolum_ilk_10bin <-
    yBolum_ilk_20bin <-
    yBolum_ilk_50bin <-
    yBolum_ilk_100bin <-
    yBolum_ilk_200bin <-
    yBolum_ust_50bin <-
    yBolum_ust_100bin <-
    yBolum_25 <-
    yBolum_50 <-
    yBolum_75 <- yBolum_90 <- yBolum_95 <- yBolum_son <- vector()
  yFakulte_Rows <-
    yFakulte_ilk_bin <-
    yFakulte_ilk_5bin <-
    yFakulte_ilk_10bin <-
    yFakulte_ilk_20bin <-
    yFakulte_ilk_50bin <-
    yFakulte_ilk_100bin <-
    yFakulte_ilk_200bin <-
    yFakulte_ust_50bin <-
    yFakulte_ust_100bin <-
    yFakulte_25 <-
    yFakulte_50 <-
    yFakulte_75 <-
    yFakulte_90 <- yFakulte_95 <- yFakulte_son <- vector()
  yPuan_Rows <-
    yPuan_ilk_bin <-
    yPuan_ilk_5bin <-
    yPuan_ilk_10bin <-
    yPuan_ilk_20bin <-
    yPuan_ilk_50bin <-
    yPuan_ilk_100bin <-
    yPuan_ilk_200bin <-
    yPuan_ust_50bin <-
    yPuan_ust_100bin <-
    yPuan_25 <-
    yPuan_50 <-
    yPuan_75 <- yPuan_90 <- yPuan_95 <- yPuan_son <- vector()
  
  dataset <- reactive ({
    req(input$file0)
    # read dataset which in csv file format
    df <- read.csv(
      input$file0$datapath,
      sep = ",",
      header = TRUE,
      check.names = FALSE
    )
    df <- as.data.frame(df)
    df <- df[order(df$YERKULLANILANBASARISIRASI), ]
    return(df)
    
  })
  pozValue <- function(x) {
    if (x < 0) {
      return (0)
    }
    else {
      return (x)
    }
  }
  my.max <- function(x, y) {
    if (length(x) > 0) {
      x = x
    }
    else {
      x = 0
    }
    if (length(y) > 0) {
      y = y
    }
    else {
      y = 0
    }
    return (max(x, y))
  }
  
  yBolum_busad <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[1]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[1]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[1]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[1]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[1]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[1]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[1]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[1]])
    
    
    if (input$busad_100 >= nrow(Bolum100_1)) {
      yBusad100_1 = Bolum100_1
    }
    else {
      yBusad100_1 = Bolum100_1[0:input$busad_100,]
    }
    
    
    yBusad100_2 = Bolum100_2[0:(pozValue((input$busad_100 - nrow(yBusad100_1)))),]
    
    
    
    z = my.max((yBusad100_2[which.max(yBusad100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yBusad100_1[which.max(yBusad100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$busad_50 >= nrow(zBolum50_1)) {
      yBusad50_1 = zBolum50_1
    }
    else {
      yBusad50_1 = zBolum50_1[0:input$busad_50,]
    }
    
    
    yBusad50_2 = Bolum50_2[0:pozValue((input$busad_50 - nrow(yBusad50_1))),]
    
    x = my.max(
      yBusad50_2[which.max(yBusad50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
      yBusad50_1[which.max(yBusad50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI
    )
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$busad_25 >= nrow(zBolum25_1)) {
      yBusad25_1 = zBolum25_1
    }
    else {
      yBusad25_1 = zBolum25_1[0:input$busad_25,]
    }
    
    
    yBusad25_2 = Bolum25_2[0:pozValue((input$busad_25 - nrow(yBusad25_1))),]
    
    c = my.max(
      yBusad25_2[which.max(yBusad25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
      yBusad25_1[which.max(yBusad25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI
    )
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$busad_0 >= nrow(zBolum0_1)) {
      yBusad0_1 = zBolum0_1
    }
    else {
      yBusad0_1 = zBolum0_1[0:input$busad_0,]
    }
    
    yBusad0_2 = Bolum0_2[0:pozValue((input$busad_0 - nrow(yBusad0_1))),]
    
    return(
      bind_rows(
        yBusad100_1,
        yBusad100_2,
        yBusad50_1,
        yBusad50_2,
        yBusad25_1,
        yBusad25_2,
        yBusad0_1,
        yBusad0_2
      )
    )
  })
  yBolum_econ <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[2]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[2]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[2]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[2]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[2]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[2]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[2]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[2]])
    
    if (input$econ_100 >= nrow(Bolum100_1)) {
      yEcon100_1 = Bolum100_1
    }
    else {
      yEcon100_1 = Bolum100_1[0:input$econ_100,]
    }
    
    
    yEcon100_2 = Bolum100_2[0:(pozValue((input$econ_100 - nrow(yEcon100_1)))),]
    
    
    
    z = my.max((yEcon100_2[which.max(yEcon100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yEcon100_1[which.max(yEcon100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$econ_50 >= nrow(zBolum50_1)) {
      yEcon50_1 = zBolum50_1
    }
    else {
      yEcon50_1 = zBolum50_1[0:input$econ_50,]
    }
    
    
    yEcon50_2 = Bolum50_2[0:pozValue((input$econ_50 - nrow(yEcon50_1))),]
    
    x = my.max(yEcon50_2[which.max(yEcon50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yEcon50_1[which.max(yEcon50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$econ_25 >= nrow(zBolum25_1)) {
      yEcon25_1 = zBolum25_1
    }
    else {
      yEcon25_1 = zBolum25_1[0:input$econ_25,]
    }
    
    
    yEcon25_2 = Bolum25_2[0:pozValue((input$econ_25 - nrow(yEcon25_1))),]
    
    c = my.max(yEcon25_2[which.max(yEcon25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yEcon25_1[which.max(yEcon25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$econ_0 >= nrow(zBolum0_1)) {
      yEcon0_1 = zBolum0_1
    }
    else {
      yEcon0_1 = zBolum0_1[0:input$econ_0,]
    }
    
    yEcon0_2 = Bolum0_2[0:pozValue((input$econ_0 - nrow(yEcon0_1))),]
    
    return(
      bind_rows(
        yEcon100_1,
        yEcon100_2,
        yEcon50_1,
        yEcon50_2,
        yEcon25_1,
        yEcon25_2,
        yEcon0_1,
        yEcon0_2
      )
    )
  })
  yBolum_intl <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[3]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[3]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[3]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[3]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[3]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[3]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[3]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[3]])
    if (input$intl_100 >= nrow(Bolum100_1)) {
      yIntl100_1 = Bolum100_1
    }
    else {
      yIntl100_1 = Bolum100_1[0:input$intl_100,]
    }
    
    
    yIntl100_2 = Bolum100_2[0:(pozValue((input$intl_100 - nrow(yIntl100_1)))),]
    
    
    
    z = my.max((yIntl100_2[which.max(yIntl100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yIntl100_1[which.max(yIntl100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$intl_50 >= nrow(zBolum50_1)) {
      yIntl50_1 = zBolum50_1
    }
    else {
      yIntl50_1 = zBolum50_1[0:input$intl_50,]
    }
    
    
    yIntl50_2 = Bolum50_2[0:pozValue((input$intl_50 - nrow(yIntl50_1))),]
    
    x = my.max(yIntl50_2[which.max(yIntl50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yIntl50_1[which.max(yIntl50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$intl_25 >= nrow(zBolum25_1)) {
      yIntl25_1 = zBolum25_1
    }
    else {
      yIntl25_1 = zBolum25_1[0:input$intl_25,]
    }
    
    
    yIntl25_2 = Bolum25_2[0:pozValue((input$intl_25 - nrow(yIntl25_1))),]
    
    c = my.max(yIntl25_2[which.max(yIntl25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yIntl25_1[which.max(yIntl25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$intl_0 >= nrow(zBolum0_1)) {
      yIntl0_1 = zBolum0_1
    }
    else {
      yIntl0_1 = zBolum0_1[0:input$intl_0,]
    }
    
    yIntl0_2 = Bolum0_2[0:pozValue((input$intl_0 - nrow(yIntl0_1))),]
    
    return(
      bind_rows(
        yIntl100_1,
        yIntl100_2,
        yIntl50_1,
        yIntl50_2,
        yIntl25_1,
        yIntl25_2,
        yIntl0_1,
        yIntl0_2
      )
    )
  })
  yBolum_chbi <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[4]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[4]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[4]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[4]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[4]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[4]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[4]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[4]])
    if (input$chbi_100 >= nrow(Bolum100_1)) {
      yChbi100_1 = Bolum100_1
    }
    else {
      yChbi100_1 = Bolum100_1[0:input$chbi_100,]
    }
    
    
    yChbi100_2 = Bolum100_2[0:(pozValue((input$chbi_100 - nrow(yChbi100_1)))),]
    
    
    
    z = my.max((yChbi100_2[which.max(yChbi100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yChbi100_1[which.max(yChbi100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$chbi_50 >= nrow(zBolum50_1)) {
      yChbi50_1 = zBolum50_1
    }
    else {
      yChbi50_1 = zBolum50_1[0:input$chbi_50,]
    }
    
    
    yChbi50_2 = Bolum50_2[0:pozValue((input$chbi_50 - nrow(yChbi50_1))),]
    
    x = my.max(yChbi50_2[which.max(yChbi50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yChbi50_1[which.max(yChbi50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$chbi_25 >= nrow(zBolum25_1)) {
      yChbi25_1 = zBolum25_1
    }
    else {
      yChbi25_1 = zBolum25_1[0:input$chbi_25,]
    }
    
    
    yChbi25_2 = Bolum25_2[0:pozValue((input$chbi_25 - nrow(yChbi25_1))),]
    
    c = my.max(yChbi25_2[which.max(yChbi25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yChbi25_1[which.max(yChbi25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$chbi_0 >= nrow(zBolum0_1)) {
      yChbi0_1 = zBolum0_1
    }
    else {
      yChbi0_1 = zBolum0_1[0:input$chbi_0,]
    }
    
    yChbi0_2 = Bolum0_2[0:pozValue((input$chbi_0 - nrow(yChbi0_1))),]
    
    return(
      bind_rows(
        yChbi100_1,
        yChbi100_2,
        yChbi50_1,
        yChbi50_2,
        yChbi25_1,
        yChbi25_2,
        yChbi0_1,
        yChbi0_2
      )
    )
  })
  yBolum_comp <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[5]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[5]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[5]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[5]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[5]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[5]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[5]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[5]])
    if (input$comp_100 >= nrow(Bolum100_1)) {
      yComp100_1 = Bolum100_1
    }
    else {
      yComp100_1 = Bolum100_1[0:input$comp_100,]
    }
    
    
    yComp100_2 = Bolum100_2[0:(pozValue((input$comp_100 - nrow(yComp100_1)))),]
    
    
    
    z = my.max((yComp100_2[which.max(yComp100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yComp100_1[which.max(yComp100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$comp_50 >= nrow(zBolum50_1)) {
      yComp50_1 = zBolum50_1
    }
    else {
      yComp50_1 = zBolum50_1[0:input$comp_50,]
    }
    
    
    yComp50_2 = Bolum50_2[0:pozValue((input$comp_50 - nrow(yComp50_1))),]
    
    x = my.max(yComp50_2[which.max(yComp50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yComp50_1[which.max(yComp50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$comp_25 >= nrow(zBolum25_1)) {
      yComp25_1 = zBolum25_1
    }
    else {
      yComp25_1 = zBolum25_1[0:input$comp_25,]
    }
    
    
    yComp25_2 = Bolum25_2[0:pozValue((input$comp_25 - nrow(yComp25_1))),]
    
    c = my.max(yComp25_2[which.max(yComp25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yComp25_1[which.max(yComp25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$comp_0 >= nrow(zBolum0_1)) {
      yComp0_1 = zBolum0_1
    }
    else {
      yComp0_1 = zBolum0_1[0:input$comp_0,]
    }
    
    yComp0_2 = Bolum0_2[0:pozValue((input$comp_0 - nrow(yComp0_1))),]
    
    return(
      bind_rows(
        yComp100_1,
        yComp100_2,
        yComp50_1,
        yComp50_2,
        yComp25_1,
        yComp25_2,
        yComp0_1,
        yComp0_2
      )
    )
  })
  yBolum_elec <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[6]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[6]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[6]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[6]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[6]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[6]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[6]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[6]])
    if (input$elec_100 >= nrow(Bolum100_1)) {
      yElec100_1 = Bolum100_1
    }
    else {
      yElec100_1 = Bolum100_1[0:input$elec_100,]
    }
    
    
    yElec100_2 = Bolum100_2[0:(pozValue((input$elec_100 - nrow(yElec100_1)))),]
    
    
    
    z = my.max((yElec100_2[which.max(yElec100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yElec100_1[which.max(yElec100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$elec_50 >= nrow(zBolum50_1)) {
      yElec50_1 = zBolum50_1
    }
    else {
      yElec50_1 = zBolum50_1[0:input$elec_50,]
    }
    
    
    yElec50_2 = Bolum50_2[0:pozValue((input$elec_50 - nrow(yElec50_1))),]
    
    x = my.max(yElec50_2[which.max(yElec50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yElec50_1[which.max(yElec50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$elec_25 >= nrow(zBolum25_1)) {
      yElec25_1 = zBolum25_1
    }
    else {
      yElec25_1 = zBolum25_1[0:input$elec_25,]
    }
    
    
    yElec25_2 = Bolum25_2[0:pozValue((input$elec_25 - nrow(yElec25_1))),]
    
    c = my.max(yElec25_2[which.max(yElec25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yElec25_1[which.max(yElec25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$elec_0 >= nrow(zBolum0_1)) {
      yElec0_1 = zBolum0_1
    }
    else {
      yElec0_1 = zBolum0_1[0:input$elec_0,]
    }
    
    yElec0_2 = Bolum0_2[0:pozValue((input$elec_0 - nrow(yElec0_1))),]
    
    return(
      bind_rows(
        yElec100_1,
        yElec100_2,
        yElec50_1,
        yElec50_2,
        yElec25_1,
        yElec25_2,
        yElec0_1,
        yElec0_2
      )
    )
  })
  yBolum_indr <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[7]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[7]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[7]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[7]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[7]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[7]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[7]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[7]])
    if (input$indr_100 >= nrow(Bolum100_1)) {
      yIndr100_1 = Bolum100_1
    }
    else {
      yIndr100_1 = Bolum100_1[0:input$indr_100,]
    }
    
    
    yIndr100_2 = Bolum100_2[0:(pozValue((input$indr_100 - nrow(yIndr100_1)))),]
    
    
    
    z = my.max((yIndr100_2[which.max(yIndr100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yIndr100_1[which.max(yIndr100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$indr_50 >= nrow(zBolum50_1)) {
      yIndr50_1 = zBolum50_1
    }
    else {
      yIndr50_1 = zBolum50_1[0:input$indr_50,]
    }
    
    
    yIndr50_2 = Bolum50_2[0:pozValue((input$indr_50 - nrow(yIndr50_1))),]
    
    x = my.max(yIndr50_2[which.max(yIndr50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yIndr50_1[which.max(yIndr50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$indr_25 >= nrow(zBolum25_1)) {
      yIndr25_1 = zBolum25_1
    }
    else {
      yIndr25_1 = zBolum25_1[0:input$indr_25,]
    }
    
    
    yIndr25_2 = Bolum25_2[0:pozValue((input$indr_25 - nrow(yIndr25_1))),]
    
    c = my.max(yIndr25_2[which.max(yIndr25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yIndr25_1[which.max(yIndr25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$indr_0 >= nrow(zBolum0_1)) {
      yIndr0_1 = zBolum0_1
    }
    else {
      yIndr0_1 = zBolum0_1[0:input$indr_0,]
    }
    
    yIndr0_2 = Bolum0_2[0:pozValue((input$indr_0 - nrow(yIndr0_1))),]
    
    return(
      bind_rows(
        yIndr100_1,
        yIndr100_2,
        yIndr50_1,
        yIndr50_2,
        yIndr25_1,
        yIndr25_2,
        yIndr0_1,
        yIndr0_2
      )
    )
  })
  yBolum_mech <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[8]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[8]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[8]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[8]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[8]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[8]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[8]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[8]])
    if (input$mech_100 >= nrow(Bolum100_1)) {
      yMech100_1 = Bolum100_1
    }
    else {
      yMech100_1 = Bolum100_1[0:input$mech_100,]
    }
    
    
    yMech100_2 = Bolum100_2[0:(pozValue((input$mech_100 - nrow(yMech100_1)))),]
    
    
    
    z = my.max((yMech100_2[which.max(yMech100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yMech100_1[which.max(yMech100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$mech_50 >= nrow(zBolum50_1)) {
      yMech50_1 = zBolum50_1
    }
    else {
      yMech50_1 = zBolum50_1[0:input$mech_50,]
    }
    
    
    yMech50_2 = Bolum50_2[0:pozValue((input$mech_50 - nrow(yMech50_1))),]
    
    x = my.max(yMech50_2[which.max(yMech50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yMech50_1[which.max(yMech50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$mech_25 >= nrow(zBolum25_1)) {
      yMech25_1 = zBolum25_1
    }
    else {
      yMech25_1 = zBolum25_1[0:input$mech_25,]
    }
    
    
    yMech25_2 = Bolum25_2[0:pozValue((input$mech_25 - nrow(yMech25_1))),]
    
    c = my.max(yMech25_2[which.max(yMech25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yMech25_1[which.max(yMech25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$mech_0 >= nrow(zBolum0_1)) {
      yMech0_1 = zBolum0_1
    }
    else {
      yMech0_1 = zBolum0_1[0:input$mech_0,]
    }
    
    yMech0_2 = Bolum0_2[0:pozValue((input$mech_0 - nrow(yMech0_1))),]
    
    return(
      bind_rows(
        yMech100_1,
        yMech100_2,
        yMech50_1,
        yMech50_2,
        yMech25_1,
        yMech25_2,
        yMech0_1,
        yMech0_2
      )
    )
  })
  yBolum_chem <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[9]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[9]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[9]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[9]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[9]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[9]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[9]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[9]])
    if (input$chem_100 >= nrow(Bolum100_1)) {
      yChem100_1 = Bolum100_1
    }
    else {
      yChem100_1 = Bolum100_1[0:input$chem_100,]
    }
    
    
    yChem100_2 = Bolum100_2[0:(pozValue((input$chem_100 - nrow(yChem100_1)))),]
    
    
    
    z = my.max((yChem100_2[which.max(yChem100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yChem100_1[which.max(yChem100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$chem_50 >= nrow(zBolum50_1)) {
      yChem50_1 = zBolum50_1
    }
    else {
      yChem50_1 = zBolum50_1[0:input$chem_50,]
    }
    
    
    yChem50_2 = Bolum50_2[0:pozValue((input$chem_50 - nrow(yChem50_1))),]
    
    x = my.max(yChem50_2[which.max(yChem50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yChem50_1[which.max(yChem50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$chem_25 >= nrow(zBolum25_1)) {
      yChem25_1 = zBolum25_1
    }
    else {
      yChem25_1 = zBolum25_1[0:input$chem_25,]
    }
    
    
    yChem25_2 = Bolum25_2[0:pozValue((input$chem_25 - nrow(yChem25_1))),]
    
    c = my.max(yChem25_2[which.max(yChem25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yChem25_1[which.max(yChem25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$chem_0 >= nrow(zBolum0_1)) {
      yChem0_1 = zBolum0_1
    }
    else {
      yChem0_1 = zBolum0_1[0:input$chem_0,]
    }
    
    yChem0_2 = Bolum0_2[0:pozValue((input$chem_0 - nrow(yChem0_1))),]
    
    return(
      bind_rows(
        yChem100_1,
        yChem100_2,
        yChem50_1,
        yChem50_2,
        yChem25_1,
        yChem25_2,
        yChem0_1,
        yChem0_2
      )
    )
  })
  yBolum_math <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[10]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[10]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[10]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[10]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[10]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[10]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[10]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[10]])
    if (input$math_100 >= nrow(Bolum100_1)) {
      yMath100_1 = Bolum100_1
    }
    else {
      yMath100_1 = Bolum100_1[0:input$math_100,]
    }
    
    
    yMath100_2 = Bolum100_2[0:(pozValue((input$math_100 - nrow(yMath100_1)))),]
    
    
    
    z = my.max((yMath100_2[which.max(yMath100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yMath100_1[which.max(yMath100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$math_50 >= nrow(zBolum50_1)) {
      yMath50_1 = zBolum50_1
    }
    else {
      yMath50_1 = zBolum50_1[0:input$math_50,]
    }
    
    
    yMath50_2 = Bolum50_2[0:pozValue((input$math_50 - nrow(yMath50_1))),]
    
    x = my.max(yMath50_2[which.max(yMath50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yMath50_1[which.max(yMath50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$math_25 >= nrow(zBolum25_1)) {
      yMath25_1 = zBolum25_1
    }
    else {
      yMath25_1 = zBolum25_1[0:input$math_25,]
    }
    
    
    yMath25_2 = Bolum25_2[0:pozValue((input$math_25 - nrow(yMath25_1))),]
    
    c = my.max(yMath25_2[which.max(yMath25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yMath25_1[which.max(yMath25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$math_0 >= nrow(zBolum0_1)) {
      yMath0_1 = zBolum0_1
    }
    else {
      yMath0_1 = zBolum0_1[0:input$math_0,]
    }
    
    yMath0_2 = Bolum0_2[0:pozValue((input$math_0 - nrow(yMath0_1))),]
    
    return(
      bind_rows(
        yMath100_1,
        yMath100_2,
        yMath50_1,
        yMath50_2,
        yMath25_1,
        yMath25_2,
        yMath0_1,
        yMath0_2
      )
    )
  })
  yBolum_mbge <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[11]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[11]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[11]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[11]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[11]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[11]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[11]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[11]])
    if (input$mbge_100 >= nrow(Bolum100_1)) {
      yMbge100_1 = Bolum100_1
    }
    else {
      yMbge100_1 = Bolum100_1[0:input$mbge_100,]
    }
    
    
    yMbge100_2 = Bolum100_2[0:(pozValue((input$mbge_100 - nrow(yMbge100_1)))),]
    
    
    
    z = my.max((yMbge100_2[which.max(yMbge100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yMbge100_1[which.max(yMbge100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$mbge_50 >= nrow(zBolum50_1)) {
      yMbge50_1 = zBolum50_1
    }
    else {
      yMbge50_1 = zBolum50_1[0:input$mbge_50,]
    }
    
    
    yMbge50_2 = Bolum50_2[0:pozValue((input$mbge_50 - nrow(yMbge50_1))),]
    
    x = my.max(yMbge50_2[which.max(yMbge50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yMbge50_1[which.max(yMbge50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$mbge_25 >= nrow(zBolum25_1)) {
      yMbge25_1 = zBolum25_1
    }
    else {
      yMbge25_1 = zBolum25_1[0:input$mbge_25,]
    }
    
    
    yMbge25_2 = Bolum25_2[0:pozValue((input$mbge_25 - nrow(yMbge25_1))),]
    
    c = my.max(yMbge25_2[which.max(yMbge25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yMbge25_1[which.max(yMbge25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$mbge_0 >= nrow(zBolum0_1)) {
      yMbge0_1 = zBolum0_1
    }
    else {
      yMbge0_1 = zBolum0_1[0:input$mbge_0,]
    }
    
    yMbge0_2 = Bolum0_2[0:pozValue((input$mbge_0 - nrow(yMbge0_1))),]
    
    return(
      bind_rows(
        yMbge100_1,
        yMbge100_2,
        yMbge50_1,
        yMbge50_2,
        yMbge25_1,
        yMbge25_2,
        yMbge0_1,
        yMbge0_2
      )
    )
  })
  yBolum_phys <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[12]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[12]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[12]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[12]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[12]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[12]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[12]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[12]])
    if (input$phys_100 >= nrow(Bolum100_1)) {
      yPhys100_1 = Bolum100_1
    }
    else {
      yPhys100_1 = Bolum100_1[0:input$phys_100,]
    }
    
    
    yPhys100_2 = Bolum100_2[0:(pozValue((input$phys_100 - nrow(yPhys100_1)))),]
    
    
    
    z = my.max((yPhys100_2[which.max(yPhys100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yPhys100_1[which.max(yPhys100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$phys_50 >= nrow(zBolum50_1)) {
      yPhys50_1 = zBolum50_1
    }
    else {
      yPhys50_1 = zBolum50_1[0:input$phys_50,]
    }
    
    
    yPhys50_2 = Bolum50_2[0:pozValue((input$phys_50 - nrow(yPhys50_1))),]
    
    x = my.max(yPhys50_2[which.max(yPhys50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yPhys50_1[which.max(yPhys50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$phys_25 >= nrow(zBolum25_1)) {
      yPhys25_1 = zBolum25_1
    }
    else {
      yPhys25_1 = zBolum25_1[0:input$phys_25,]
    }
    
    
    yPhys25_2 = Bolum25_2[0:pozValue((input$phys_25 - nrow(yPhys25_1))),]
    
    c = my.max(yPhys25_2[which.max(yPhys25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yPhys25_1[which.max(yPhys25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$phys_0 >= nrow(zBolum0_1)) {
      yPhys0_1 = zBolum0_1
    }
    else {
      yPhys0_1 = zBolum0_1[0:input$phys_0,]
    }
    
    yPhys0_2 = Bolum0_2[0:pozValue((input$phys_0 - nrow(yPhys0_1))),]
    
    return(
      bind_rows(
        yPhys100_1,
        yPhys100_2,
        yPhys50_1,
        yPhys50_2,
        yPhys25_1,
        yPhys25_2,
        yPhys0_1,
        yPhys0_2
      )
    )
  })
  yBolum_arha <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[13]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[13]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[13]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[13]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[13]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[13]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[13]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[13]])
    if (input$arha_100 >= nrow(Bolum100_1)) {
      yArha100_1 = Bolum100_1
    }
    else {
      yArha100_1 = Bolum100_1[0:input$arha_100,]
    }
    
    
    yArha100_2 = Bolum100_2[0:(pozValue((input$arha_100 - nrow(yArha100_1)))),]
    
    
    
    z = my.max((yArha100_2[which.max(yArha100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yArha100_1[which.max(yArha100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$arha_50 >= nrow(zBolum50_1)) {
      yArha50_1 = zBolum50_1
    }
    else {
      yArha50_1 = zBolum50_1[0:input$arha_50,]
    }
    
    
    yArha50_2 = Bolum50_2[0:pozValue((input$arha_50 - nrow(yArha50_1))),]
    
    x = my.max(yArha50_2[which.max(yArha50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yArha50_1[which.max(yArha50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$arha_25 >= nrow(zBolum25_1)) {
      yArha25_1 = zBolum25_1
    }
    else {
      yArha25_1 = zBolum25_1[0:input$arha_25,]
    }
    
    
    yArha25_2 = Bolum25_2[0:pozValue((input$arha_25 - nrow(yArha25_1))),]
    
    c = my.max(yArha25_2[which.max(yArha25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yArha25_1[which.max(yArha25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$arha_0 >= nrow(zBolum0_1)) {
      yArha0_1 = zBolum0_1
    }
    else {
      yArha0_1 = zBolum0_1[0:input$arha_0,]
    }
    
    yArha0_2 = Bolum0_2[0:pozValue((input$arha_0 - nrow(yArha0_1))),]
    
    return(
      bind_rows(
        yArha100_1,
        yArha100_2,
        yArha50_1,
        yArha50_2,
        yArha25_1,
        yArha25_2,
        yArha0_1,
        yArha0_2
      )
    )
  })
  yBolum_encl <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[14]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[14]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[14]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[14]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[14]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[14]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[14]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[14]])
    if (input$encl_100 >= nrow(Bolum100_1)) {
      yEncl100_1 = Bolum100_1
    }
    else {
      yEncl100_1 = Bolum100_1[0:input$encl_100,]
    }
    
    
    yEncl100_2 = Bolum100_2[0:(pozValue((input$encl_100 - nrow(yEncl100_1)))),]
    
    
    
    z = my.max((yEncl100_2[which.max(yEncl100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yEncl100_1[which.max(yEncl100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$encl_50 >= nrow(zBolum50_1)) {
      yEncl50_1 = zBolum50_1
    }
    else {
      yEncl50_1 = zBolum50_1[0:input$encl_50,]
    }
    
    
    yEncl50_2 = Bolum50_2[0:pozValue((input$encl_50 - nrow(yEncl50_1))),]
    
    x = my.max(yEncl50_2[which.max(yEncl50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yEncl50_1[which.max(yEncl50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$encl_25 >= nrow(zBolum25_1)) {
      yEncl25_1 = zBolum25_1
    }
    else {
      yEncl25_1 = zBolum25_1[0:input$encl_25,]
    }
    
    
    yEncl25_2 = Bolum25_2[0:pozValue((input$encl_25 - nrow(yEncl25_1))),]
    
    c = my.max(yEncl25_2[which.max(yEncl25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yEncl25_1[which.max(yEncl25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$encl_0 >= nrow(zBolum0_1)) {
      yEncl0_1 = zBolum0_1
    }
    else {
      yEncl0_1 = zBolum0_1[0:input$encl_0,]
    }
    
    yEncl0_2 = Bolum0_2[0:pozValue((input$encl_0 - nrow(yEncl0_1))),]
    
    return(
      bind_rows(
        yEncl100_1,
        yEncl100_2,
        yEncl50_1,
        yEncl50_2,
        yEncl25_1,
        yEncl25_2,
        yEncl0_1,
        yEncl0_2
      )
    )
  })
  yBolum_hist <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[15]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[15]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[15]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[15]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[15]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[15]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[15]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[15]])
    if (input$hist_100 >= nrow(Bolum100_1)) {
      yHist100_1 = Bolum100_1
    }
    else {
      yHist100_1 = Bolum100_1[0:input$hist_100,]
    }
    
    
    yHist100_2 = Bolum100_2[0:(pozValue((input$hist_100 - nrow(yHist100_1)))),]
    
    
    
    z = my.max((yHist100_2[which.max(yHist100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yHist100_1[which.max(yHist100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$hist_50 >= nrow(zBolum50_1)) {
      yHist50_1 = zBolum50_1
    }
    else {
      yHist50_1 = zBolum50_1[0:input$hist_50,]
    }
    
    
    yHist50_2 = Bolum50_2[0:pozValue((input$hist_50 - nrow(yHist50_1))),]
    
    x = my.max(yHist50_2[which.max(yHist50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yHist50_1[which.max(yHist50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$hist_25 >= nrow(zBolum25_1)) {
      yHist25_1 = zBolum25_1
    }
    else {
      yHist25_1 = zBolum25_1[0:input$hist_25,]
    }
    
    
    yHist25_2 = Bolum25_2[0:pozValue((input$hist_25 - nrow(yHist25_1))),]
    
    c = my.max(yHist25_2[which.max(yHist25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yHist25_1[which.max(yHist25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$hist_0 >= nrow(zBolum0_1)) {
      yHist0_1 = zBolum0_1
    }
    else {
      yHist0_1 = zBolum0_1[0:input$hist_0,]
    }
    
    yHist0_2 = Bolum0_2[0:pozValue((input$hist_0 - nrow(yHist0_1))),]
    
    return(
      bind_rows(
        yHist100_1,
        yHist100_2,
        yHist50_1,
        yHist50_2,
        yHist25_1,
        yHist25_2,
        yHist0_1,
        yHist0_2
      )
    )
  })
  yBolum_mava <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[16]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[16]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[16]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[16]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[16]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[16]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[16]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[16]])
    if (input$mava_100 >= nrow(Bolum100_1)) {
      yMava100_1 = Bolum100_1
    }
    else {
      yMava100_1 = Bolum100_1[0:input$mava_100,]
    }
    
    
    yMava100_2 = Bolum100_2[0:(pozValue((input$mava_100 - nrow(yMava100_1)))),]
    
    
    
    z = my.max((yMava100_2[which.max(yMava100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yMava100_1[which.max(yMava100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$mava_50 >= nrow(zBolum50_1)) {
      yMava50_1 = zBolum50_1
    }
    else {
      yMava50_1 = zBolum50_1[0:input$mava_50,]
    }
    
    
    yMava50_2 = Bolum50_2[0:pozValue((input$mava_50 - nrow(yMava50_1))),]
    
    x = my.max(yMava50_2[which.max(yMava50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yMava50_1[which.max(yMava50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$mava_25 >= nrow(zBolum25_1)) {
      yMava25_1 = zBolum25_1
    }
    else {
      yMava25_1 = zBolum25_1[0:input$mava_25,]
    }
    
    
    yMava25_2 = Bolum25_2[0:pozValue((input$mava_25 - nrow(yMava25_1))),]
    
    c = my.max(yMava25_2[which.max(yMava25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yMava25_1[which.max(yMava25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$mava_0 >= nrow(zBolum0_1)) {
      yMava0_1 = zBolum0_1
    }
    else {
      yMava0_1 = zBolum0_1[0:input$mava_0,]
    }
    
    yMava0_2 = Bolum0_2[0:pozValue((input$mava_0 - nrow(yMava0_1))),]
    
    return(
      bind_rows(
        yMava100_1,
        yMava100_2,
        yMava50_1,
        yMava50_2,
        yMava25_1,
        yMava25_2,
        yMava0_1,
        yMava0_2
      )
    )
  })
  yBolum_phil <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[17]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[17]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[17]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[17]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[17]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[17]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[17]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[17]])
    if (input$phil_100 >= nrow(Bolum100_1)) {
      yPhil100_1 = Bolum100_1
    }
    else {
      yPhil100_1 = Bolum100_1[0:input$phil_100,]
    }
    
    
    yPhil100_2 = Bolum100_2[0:(pozValue((input$phil_100 - nrow(yPhil100_1)))),]
    
    
    
    z = my.max((yPhil100_2[which.max(yPhil100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yPhil100_1[which.max(yPhil100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$phil_50 >= nrow(zBolum50_1)) {
      yPhil50_1 = zBolum50_1
    }
    else {
      yPhil50_1 = zBolum50_1[0:input$phil_50,]
    }
    
    
    yPhil50_2 = Bolum50_2[0:pozValue((input$phil_50 - nrow(yPhil50_1))),]
    
    x = my.max(yPhil50_2[which.max(yPhil50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yPhil50_1[which.max(yPhil50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$phil_25 >= nrow(zBolum25_1)) {
      yPhil25_1 = zBolum25_1
    }
    else {
      yPhil25_1 = zBolum25_1[0:input$phil_25,]
    }
    
    
    yPhil25_2 = Bolum25_2[0:pozValue((input$phil_25 - nrow(yPhil25_1))),]
    
    c = my.max(yPhil25_2[which.max(yPhil25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yPhil25_1[which.max(yPhil25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$phil_0 >= nrow(zBolum0_1)) {
      yPhil0_1 = zBolum0_1
    }
    else {
      yPhil0_1 = zBolum0_1[0:input$phil_0,]
    }
    
    yPhil0_2 = Bolum0_2[0:pozValue((input$phil_0 - nrow(yPhil0_1))),]
    
    return(
      bind_rows(
        yPhil100_1,
        yPhil100_2,
        yPhil50_1,
        yPhil50_2,
        yPhil25_1,
        yPhil25_2,
        yPhil0_1,
        yPhil0_2
      )
    )
  })
  yBolum_psyc <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[18]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[18]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[18]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[18]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[18]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[18]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[18]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[18]])
    if (input$psyc_100 >= nrow(Bolum100_1)) {
      yPsyc100_1 = Bolum100_1
    }
    else {
      yPsyc100_1 = Bolum100_1[0:input$psyc_100,]
    }
    
    
    yPsyc100_2 = Bolum100_2[0:(pozValue((input$psyc_100 - nrow(yPsyc100_1)))),]
    
    
    
    z = my.max((yPsyc100_2[which.max(yPsyc100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yPsyc100_1[which.max(yPsyc100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$psyc_50 >= nrow(zBolum50_1)) {
      yPsyc50_1 = zBolum50_1
    }
    else {
      yPsyc50_1 = zBolum50_1[0:input$psyc_50,]
    }
    
    
    yPsyc50_2 = Bolum50_2[0:pozValue((input$psyc_50 - nrow(yPsyc50_1))),]
    
    x = my.max(yPsyc50_2[which.max(yPsyc50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yPsyc50_1[which.max(yPsyc50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$psyc_25 >= nrow(zBolum25_1)) {
      yPsyc25_1 = zBolum25_1
    }
    else {
      yPsyc25_1 = zBolum25_1[0:input$psyc_25,]
    }
    
    
    yPsyc25_2 = Bolum25_2[0:pozValue((input$psyc_25 - nrow(yPsyc25_1))),]
    
    c = my.max(yPsyc25_2[which.max(yPsyc25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yPsyc25_1[which.max(yPsyc25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$psyc_0 >= nrow(zBolum0_1)) {
      yPsyc0_1 = zBolum0_1
    }
    else {
      yPsyc0_1 = zBolum0_1[0:input$psyc_0,]
    }
    
    yPsyc0_2 = Bolum0_2[0:pozValue((input$psyc_0 - nrow(yPsyc0_1))),]
    
    return(
      bind_rows(
        yPsyc100_1,
        yPsyc100_2,
        yPsyc50_1,
        yPsyc50_2,
        yPsyc25_1,
        yPsyc25_2,
        yPsyc0_1,
        yPsyc0_2
      )
    )
  })
  yBolum_soci <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[19]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[19]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[19]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[19]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[19]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[19]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[19]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[19]])
    if (input$soci_100 >= nrow(Bolum100_1)) {
      ySoci100_1 = Bolum100_1
    }
    else {
      ySoci100_1 = Bolum100_1[0:input$soci_100,]
    }
    
    
    ySoci100_2 = Bolum100_2[0:(pozValue((input$soci_100 - nrow(ySoci100_1)))),]
    
    
    
    z = my.max((ySoci100_2[which.max(ySoci100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (ySoci100_1[which.max(ySoci100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$soci_50 >= nrow(zBolum50_1)) {
      ySoci50_1 = zBolum50_1
    }
    else {
      ySoci50_1 = zBolum50_1[0:input$soci_50,]
    }
    
    
    ySoci50_2 = Bolum50_2[0:pozValue((input$soci_50 - nrow(ySoci50_1))),]
    
    x = my.max(ySoci50_2[which.max(ySoci50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               ySoci50_1[which.max(ySoci50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$soci_25 >= nrow(zBolum25_1)) {
      ySoci25_1 = zBolum25_1
    }
    else {
      ySoci25_1 = zBolum25_1[0:input$soci_25,]
    }
    
    
    ySoci25_2 = Bolum25_2[0:pozValue((input$soci_25 - nrow(ySoci25_1))),]
    
    c = my.max(ySoci25_2[which.max(ySoci25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               ySoci25_1[which.max(ySoci25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$soci_0 >= nrow(zBolum0_1)) {
      ySoci0_1 = zBolum0_1
    }
    else {
      ySoci0_1 = zBolum0_1[0:input$soci_0,]
    }
    
    ySoci0_2 = Bolum0_2[0:pozValue((input$soci_0 - nrow(ySoci0_1))),]
    
    return(
      bind_rows(
        ySoci100_1,
        ySoci100_2,
        ySoci50_1,
        ySoci50_2,
        ySoci25_1,
        ySoci25_2,
        ySoci0_1,
        ySoci0_2
      )
    )
  })
  yBolum_law <- reactive ({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[20]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[20]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[20]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[20]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[20]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[20]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[20]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[20]])
    if (input$law_100 >= nrow(Bolum100_1)) {
      yLaw100_1 = Bolum100_1
    }
    else {
      yLaw100_1 = Bolum100_1[0:input$law_100,]
    }
    
    
    yLaw100_2 = Bolum100_2[0:(pozValue((input$law_100 - nrow(yLaw100_1)))),]
    
    
    
    z = my.max((yLaw100_2[which.max(yLaw100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yLaw100_1[which.max(yLaw100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$law_50 >= nrow(zBolum50_1)) {
      yLaw50_1 = zBolum50_1
    }
    else {
      yLaw50_1 = zBolum50_1[0:input$law_50,]
    }
    
    
    yLaw50_2 = Bolum50_2[0:pozValue((input$law_50 - nrow(yLaw50_1))),]
    
    x = my.max(yLaw50_2[which.max(yLaw50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yLaw50_1[which.max(yLaw50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$law_25 >= nrow(zBolum25_1)) {
      yLaw25_1 = zBolum25_1
    }
    else {
      yLaw25_1 = zBolum25_1[0:input$law_25,]
    }
    
    
    yLaw25_2 = Bolum25_2[0:pozValue((input$law_25 - nrow(yLaw25_1))),]
    
    c = my.max(yLaw25_2[which.max(yLaw25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yLaw25_1[which.max(yLaw25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$law_0 >= nrow(zBolum0_1)) {
      yLaw0_1 = zBolum0_1
    }
    else {
      yLaw0_1 = zBolum0_1[0:input$law_0,]
    }
    
    yLaw0_2 = Bolum0_2[0:pozValue((input$law_0 - nrow(yLaw0_1))),]
    
    return(
      bind_rows(
        yLaw100_1,
        yLaw100_2,
        yLaw50_1,
        yLaw50_2,
        yLaw25_1,
        yLaw25_2,
        yLaw0_1,
        yLaw0_2
      )
    )
  })
  yBolum_medi <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[21]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[21]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[21]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[21]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[21]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[21]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[21]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[21]])
    if (input$medi_100 >= nrow(Bolum100_1)) {
      yMedi100_1 = Bolum100_1
    }
    else {
      yMedi100_1 = Bolum100_1[0:input$medi_100,]
    }
    
    
    yMedi100_2 = Bolum100_2[0:(pozValue((input$medi_100 - nrow(yMedi100_1)))),]
    
    
    
    z = my.max((yMedi100_2[which.max(yMedi100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yMedi100_1[which.max(yMedi100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$medi_50 >= nrow(zBolum50_1)) {
      yMedi50_1 = zBolum50_1
    }
    else {
      yMedi50_1 = zBolum50_1[0:input$medi_50,]
    }
    
    
    yMedi50_2 = Bolum50_2[0:pozValue((input$medi_50 - nrow(yMedi50_1))),]
    
    x = my.max(yMedi50_2[which.max(yMedi50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yMedi50_1[which.max(yMedi50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$medi_25 >= nrow(zBolum25_1)) {
      yMedi25_1 = zBolum25_1
    }
    else {
      yMedi25_1 = zBolum25_1[0:input$medi_25,]
    }
    
    
    yMedi25_2 = Bolum25_2[0:pozValue((input$medi_25 - nrow(yMedi25_1))),]
    
    c = my.max(yMedi25_2[which.max(yMedi25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yMedi25_1[which.max(yMedi25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$medi_0 >= nrow(zBolum0_1)) {
      yMedi0_1 = zBolum0_1
    }
    else {
      yMedi0_1 = zBolum0_1[0:input$medi_0,]
    }
    
    yMedi0_2 = Bolum0_2[0:pozValue((input$medi_0 - nrow(yMedi0_1))),]
    
    return(
      bind_rows(
        yMedi100_1,
        yMedi100_2,
        yMedi50_1,
        yMedi50_2,
        yMedi25_1,
        yMedi25_2,
        yMedi0_1,
        yMedi0_2
      )
    )
  })
  yBolum_nurs <- reactive({
    reel_data = subset(dataset(), TCKN > 0)
    generated_data = subset(dataset(), TCKN < 0)
    Bolum100_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[22]])
    Bolum50_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_50_names[[22]])
    Bolum25_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_25_names[[22]])
    Bolum0_1 = subset(reel_data, GIRDIGIBOLUMADI == Bolum_0_names[[22]])
    Bolum100_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_Burslu_names[[22]])
    Bolum50_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_50_names[[22]])
    Bolum25_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_25_names[[22]])
    Bolum0_2 = subset(generated_data, GIRDIGIBOLUMADI == Bolum_0_names[[22]])
    if (input$nurs_100 >= nrow(Bolum100_1)) {
      yNurs100_1 = Bolum100_1
    }
    else {
      yNurs100_1 = Bolum100_1[0:input$nurs_100,]
    }
    
    
    yNurs100_2 = Bolum100_2[0:(pozValue((input$nurs_100 - nrow(yNurs100_1)))),]
    
    
    
    z = my.max((yNurs100_2[which.max(yNurs100_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI),
               (yNurs100_1[which.max(yNurs100_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    )
    
    zBolum50_1 = subset(Bolum50_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > z)))
    
    if (input$nurs_50 >= nrow(zBolum50_1)) {
      yNurs50_1 = zBolum50_1
    }
    else {
      yNurs50_1 = zBolum50_1[0:input$nurs_50,]
    }
    
    
    yNurs50_2 = Bolum50_2[0:pozValue((input$nurs_50 - nrow(yNurs50_1))),]
    
    x = my.max(yNurs50_2[which.max(yNurs50_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yNurs50_1[which.max(yNurs50_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum25_1 = subset(Bolum25_1, ((ANADOLUBURS == "AB") |
                                      (YERKULLANILANBASARISIRASI > x)))
    
    if (input$nurs_25 >= nrow(zBolum25_1)) {
      yNurs25_1 = zBolum25_1
    }
    else {
      yNurs25_1 = zBolum25_1[0:input$nurs_25,]
    }
    
    
    yNurs25_2 = Bolum25_2[0:pozValue((input$nurs_25 - nrow(yNurs25_1))),]
    
    c = my.max(yNurs25_2[which.max(yNurs25_2$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI,
               yNurs25_1[which.max(yNurs25_1$YERKULLANILANBASARISIRASI),]$YERKULLANILANBASARISIRASI)
    
    zBolum0_1 = subset(Bolum0_1, ((ANADOLUBURS == "AB") |
                                    (YERKULLANILANBASARISIRASI > c)))
    
    if (input$nurs_0 >= nrow(zBolum0_1)) {
      yNurs0_1 = zBolum0_1
    }
    else {
      yNurs0_1 = zBolum0_1[0:input$nurs_0,]
    }
    
    yNurs0_2 = Bolum0_2[0:pozValue((input$nurs_0 - nrow(yNurs0_1))),]
    
    return(
      bind_rows(
        yNurs100_1,
        yNurs100_2,
        yNurs50_1,
        yNurs50_2,
        yNurs25_1,
        yNurs25_2,
        yNurs0_1,
        yNurs0_2
      )
    )
  })
  yResult <- reactive({
    bind_rows(
      yBolum_busad(),
      yBolum_econ(),
      yBolum_intl(),
      yBolum_chbi(),
      yBolum_comp(),
      yBolum_elec(),
      yBolum_indr(),
      yBolum_mech(),
      yBolum_chem(),
      yBolum_math(),
      yBolum_mbge(),
      yBolum_phys(),
      yBolum_arha(),
      yBolum_encl(),
      yBolum_hist(),
      yBolum_mava(),
      yBolum_phil(),
      yBolum_psyc(),
      yBolum_soci(),
      yBolum_law(),
      yBolum_medi(),
      yBolum_nurs()
    )
  })
  output$table <- renderTable({
    yResult()
  })
  
  # statistics
  KU_ist <- reactive({
    yKU             = yResult()
    KU_total        = nrow(yKU)
    KU_ilk_bin      = digit(nrow(subset(
      yKU, YERKULLANILANBASARISIRASI <= 1000
    ))   * 100 / KU_total, 2)
    KU_ilk_5bin     = digit(nrow(subset(
      yKU, YERKULLANILANBASARISIRASI <= 5000
    ))   * 100 / KU_total, 2)
    KU_ilk_10bin    = digit(nrow(subset(
      yKU, YERKULLANILANBASARISIRASI <= 10000
    ))  * 100 / KU_total, 2)
    KU_ilk_20bin    = digit(nrow(subset(
      yKU, YERKULLANILANBASARISIRASI <= 20000
    ))  * 100 / KU_total, 2)
    KU_ilk_50bin    = digit(nrow(subset(
      yKU, YERKULLANILANBASARISIRASI <= 50000
    ))  * 100 / KU_total, 2)
    KU_ilk_100bin   = digit(nrow(subset(
      yKU, YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / KU_total, 2)
    KU_ilk_200bin   = digit(nrow(subset(
      yKU, YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / KU_total, 2)
    KU_ust_50bin    = digit(nrow(subset(
      yKU, YERKULLANILANBASARISIRASI >= 50000
    ))  * 100 / KU_total, 2)
    KU_ust_100bin   = digit(nrow(subset(
      yKU, YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / KU_total, 2)
    KU_25           = digit(quantile(yKU$YERKULLANILANBASARISIRASI, .25), 0)
    KU_50           = digit(quantile(yKU$YERKULLANILANBASARISIRASI, .50), 0)
    KU_75           = digit(quantile(yKU$YERKULLANILANBASARISIRASI, .75), 0)
    KU_90           = digit(quantile(yKU$YERKULLANILANBASARISIRASI, .90), 0)
    KU_95           = digit(quantile(yKU$YERKULLANILANBASARISIRASI, .95), 0)
    KU_son          = digit(quantile(yKU$YERKULLANILANBASARISIRASI, 1), 0)
    KU_ab           = nrow(subset(yKU, ANADOLUBURS == "AB"))
    return(
      c(
        KU_ilk_bin,
        KU_ilk_5bin,
        KU_ilk_10bin,
        KU_ilk_20bin,
        KU_ilk_50bin,
        KU_ilk_100bin,
        KU_ilk_200bin,
        KU_ust_50bin,
        KU_ust_100bin,
        KU_25,
        KU_50,
        KU_75,
        KU_90,
        KU_95,
        KU_son,
        KU_ab
      )
    )
  })
  busad_ist <- reactive({
    ybusad_Rows = nrow(yBolum_busad())
    ybusad_ilk_bin = digit(nrow(subset(
      yBolum_busad(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ybusad_Rows, 2)
    ybusad_ilk_5bin = digit(nrow(subset(
      yBolum_busad(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ybusad_Rows, 2)
    ybusad_ilk_10bin = digit(nrow(subset(
      yBolum_busad(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ybusad_Rows, 2)
    ybusad_ilk_20bin = digit(nrow(subset(
      yBolum_busad(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ybusad_Rows, 2)
    ybusad_ilk_50bin = digit(nrow(subset(
      yBolum_busad(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ybusad_Rows, 2)
    ybusad_ilk_100bin = digit(nrow(
      subset(yBolum_busad(), YERKULLANILANBASARISIRASI <= 100000)
    ) * 100 / ybusad_Rows, 2)
    ybusad_ilk_200bin = digit(nrow(
      subset(yBolum_busad(), YERKULLANILANBASARISIRASI <= 200000)
    ) * 100 / ybusad_Rows, 2)
    ybusad_ust_50bin = digit(nrow(subset(
      yBolum_busad(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ybusad_Rows, 2)
    ybusad_ust_100bin = digit(nrow(
      subset(yBolum_busad(), YERKULLANILANBASARISIRASI >= 100000)
    ) * 100 / ybusad_Rows, 2)
    ybusad_25 = digit(quantile(yBolum_busad()$YERKULLANILANBASARISIRASI, 0.25),
                      0)
    ybusad_50 = digit(quantile(yBolum_busad()$YERKULLANILANBASARISIRASI, 0.5),
                      0)
    ybusad_75 = digit(quantile(yBolum_busad()$YERKULLANILANBASARISIRASI, 0.75),
                      0)
    ybusad_90 = digit(quantile(yBolum_busad()$YERKULLANILANBASARISIRASI, 0.9),
                      0)
    ybusad_95 = digit(quantile(yBolum_busad()$YERKULLANILANBASARISIRASI, 0.95),
                      0)
    ybusad_son = digit(quantile(yBolum_busad()$YERKULLANILANBASARISIRASI, 1), 0)
    ybusad_ab = nrow(subset(yBolum_busad(), ANADOLUBURS == "AB"))
    return(
      c(
        ybusad_ilk_bin,
        ybusad_ilk_5bin,
        ybusad_ilk_10bin,
        ybusad_ilk_20bin,
        ybusad_ilk_50bin,
        ybusad_ilk_100bin,
        ybusad_ilk_200bin,
        ybusad_ust_50bin,
        ybusad_ust_100bin,
        ybusad_25,
        ybusad_50,
        ybusad_75,
        ybusad_90,
        ybusad_95,
        ybusad_son,
        ybusad_ab
      )
    )
  })
  econ_ist <- reactive({
    yecon_Rows = nrow(yBolum_econ())
    yecon_ilk_bin = digit(nrow(subset(
      yBolum_econ(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / yecon_Rows, 2)
    yecon_ilk_5bin = digit(nrow(subset(
      yBolum_econ(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / yecon_Rows, 2)
    yecon_ilk_10bin = digit(nrow(subset(
      yBolum_econ(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / yecon_Rows, 2)
    yecon_ilk_20bin = digit(nrow(subset(
      yBolum_econ(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / yecon_Rows, 2)
    yecon_ilk_50bin = digit(nrow(subset(
      yBolum_econ(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / yecon_Rows, 2)
    yecon_ilk_100bin = digit(nrow(subset(
      yBolum_econ(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / yecon_Rows, 2)
    yecon_ilk_200bin = digit(nrow(subset(
      yBolum_econ(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / yecon_Rows, 2)
    yecon_ust_50bin = digit(nrow(subset(
      yBolum_econ(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / yecon_Rows, 2)
    yecon_ust_100bin = digit(nrow(subset(
      yBolum_econ(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / yecon_Rows, 2)
    yecon_25 = digit(quantile(yBolum_econ()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    yecon_50 = digit(quantile(yBolum_econ()$YERKULLANILANBASARISIRASI, 0.5), 0)
    yecon_75 = digit(quantile(yBolum_econ()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    yecon_90 = digit(quantile(yBolum_econ()$YERKULLANILANBASARISIRASI, 0.9), 0)
    yecon_95 = digit(quantile(yBolum_econ()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    yecon_son = digit(quantile(yBolum_econ()$YERKULLANILANBASARISIRASI, 1), 0)
    yecon_ab = nrow(subset(yBolum_econ(), ANADOLUBURS == "AB"))
    return(
      c(
        yecon_ilk_bin,
        yecon_ilk_5bin,
        yecon_ilk_10bin,
        yecon_ilk_20bin,
        yecon_ilk_50bin,
        yecon_ilk_100bin,
        yecon_ilk_200bin,
        yecon_ust_50bin,
        yecon_ust_100bin,
        yecon_25,
        yecon_50,
        yecon_75,
        yecon_90,
        yecon_95,
        yecon_son,
        yecon_ab
      )
    )
  })
  intl_ist <- reactive({
    yintl_Rows = nrow(yBolum_intl())
    yintl_ilk_bin = digit(nrow(subset(
      yBolum_intl(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / yintl_Rows, 2)
    yintl_ilk_5bin = digit(nrow(subset(
      yBolum_intl(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / yintl_Rows, 2)
    yintl_ilk_10bin = digit(nrow(subset(
      yBolum_intl(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / yintl_Rows, 2)
    yintl_ilk_20bin = digit(nrow(subset(
      yBolum_intl(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / yintl_Rows, 2)
    yintl_ilk_50bin = digit(nrow(subset(
      yBolum_intl(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / yintl_Rows, 2)
    yintl_ilk_100bin = digit(nrow(subset(
      yBolum_intl(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / yintl_Rows, 2)
    yintl_ilk_200bin = digit(nrow(subset(
      yBolum_intl(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / yintl_Rows, 2)
    yintl_ust_50bin = digit(nrow(subset(
      yBolum_intl(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / yintl_Rows, 2)
    yintl_ust_100bin = digit(nrow(subset(
      yBolum_intl(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / yintl_Rows, 2)
    yintl_25 = digit(quantile(yBolum_intl()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    yintl_50 = digit(quantile(yBolum_intl()$YERKULLANILANBASARISIRASI, 0.5), 0)
    yintl_75 = digit(quantile(yBolum_intl()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    yintl_90 = digit(quantile(yBolum_intl()$YERKULLANILANBASARISIRASI, 0.9), 0)
    yintl_95 = digit(quantile(yBolum_intl()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    yintl_son = digit(quantile(yBolum_intl()$YERKULLANILANBASARISIRASI, 1), 0)
    yintl_ab = nrow(subset(yBolum_intl(), ANADOLUBURS == "AB"))
    return(
      c(
        yintl_ilk_bin,
        yintl_ilk_5bin,
        yintl_ilk_10bin,
        yintl_ilk_20bin,
        yintl_ilk_50bin,
        yintl_ilk_100bin,
        yintl_ilk_200bin,
        yintl_ust_50bin,
        yintl_ust_100bin,
        yintl_25,
        yintl_50,
        yintl_75,
        yintl_90,
        yintl_95,
        yintl_son,
        yintl_ab
      )
    )
  })
  chbi_ist <- reactive({
    ychbi_Rows = nrow(yBolum_chbi())
    ychbi_ilk_bin = digit(nrow(subset(
      yBolum_chbi(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ychbi_Rows, 2)
    ychbi_ilk_5bin = digit(nrow(subset(
      yBolum_chbi(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ychbi_Rows, 2)
    ychbi_ilk_10bin = digit(nrow(subset(
      yBolum_chbi(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ychbi_Rows, 2)
    ychbi_ilk_20bin = digit(nrow(subset(
      yBolum_chbi(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ychbi_Rows, 2)
    ychbi_ilk_50bin = digit(nrow(subset(
      yBolum_chbi(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ychbi_Rows, 2)
    ychbi_ilk_100bin = digit(nrow(subset(
      yBolum_chbi(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ychbi_Rows, 2)
    ychbi_ilk_200bin = digit(nrow(subset(
      yBolum_chbi(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ychbi_Rows, 2)
    ychbi_ust_50bin = digit(nrow(subset(
      yBolum_chbi(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ychbi_Rows, 2)
    ychbi_ust_100bin = digit(nrow(subset(
      yBolum_chbi(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ychbi_Rows, 2)
    ychbi_25 = digit(quantile(yBolum_chbi()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    ychbi_50 = digit(quantile(yBolum_chbi()$YERKULLANILANBASARISIRASI, 0.5), 0)
    ychbi_75 = digit(quantile(yBolum_chbi()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    ychbi_90 = digit(quantile(yBolum_chbi()$YERKULLANILANBASARISIRASI, 0.9), 0)
    ychbi_95 = digit(quantile(yBolum_chbi()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    ychbi_son = digit(quantile(yBolum_chbi()$YERKULLANILANBASARISIRASI, 1), 0)
    ychbi_ab = nrow(subset(yBolum_chbi(), ANADOLUBURS == "AB"))
    return(
      c(
        ychbi_ilk_bin,
        ychbi_ilk_5bin,
        ychbi_ilk_10bin,
        ychbi_ilk_20bin,
        ychbi_ilk_50bin,
        ychbi_ilk_100bin,
        ychbi_ilk_200bin,
        ychbi_ust_50bin,
        ychbi_ust_100bin,
        ychbi_25,
        ychbi_50,
        ychbi_75,
        ychbi_90,
        ychbi_95,
        ychbi_son,
        ychbi_ab
      )
    )
  })
  comp_ist <- reactive({
    ycomp_Rows = nrow(yBolum_comp())
    ycomp_ilk_bin = digit(nrow(subset(
      yBolum_comp(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ycomp_Rows, 2)
    ycomp_ilk_5bin = digit(nrow(subset(
      yBolum_comp(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ycomp_Rows, 2)
    ycomp_ilk_10bin = digit(nrow(subset(
      yBolum_comp(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ycomp_Rows, 2)
    ycomp_ilk_20bin = digit(nrow(subset(
      yBolum_comp(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ycomp_Rows, 2)
    ycomp_ilk_50bin = digit(nrow(subset(
      yBolum_comp(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ycomp_Rows, 2)
    ycomp_ilk_100bin = digit(nrow(subset(
      yBolum_comp(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ycomp_Rows, 2)
    ycomp_ilk_200bin = digit(nrow(subset(
      yBolum_comp(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ycomp_Rows, 2)
    ycomp_ust_50bin = digit(nrow(subset(
      yBolum_comp(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ycomp_Rows, 2)
    ycomp_ust_100bin = digit(nrow(subset(
      yBolum_comp(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ycomp_Rows, 2)
    ycomp_25 = digit(quantile(yBolum_comp()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    ycomp_50 = digit(quantile(yBolum_comp()$YERKULLANILANBASARISIRASI, 0.5), 0)
    ycomp_75 = digit(quantile(yBolum_comp()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    ycomp_90 = digit(quantile(yBolum_comp()$YERKULLANILANBASARISIRASI, 0.9), 0)
    ycomp_95 = digit(quantile(yBolum_comp()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    ycomp_son = digit(quantile(yBolum_comp()$YERKULLANILANBASARISIRASI, 1), 0)
    ycomp_ab = nrow(subset(yBolum_comp(), ANADOLUBURS == "AB"))
    return(
      c(
        ycomp_ilk_bin,
        ycomp_ilk_5bin,
        ycomp_ilk_10bin,
        ycomp_ilk_20bin,
        ycomp_ilk_50bin,
        ycomp_ilk_100bin,
        ycomp_ilk_200bin,
        ycomp_ust_50bin,
        ycomp_ust_100bin,
        ycomp_25,
        ycomp_50,
        ycomp_75,
        ycomp_90,
        ycomp_95,
        ycomp_son,
        ycomp_ab
      )
    )
  })
  elec_ist <- reactive({
    yelec_Rows = nrow(yBolum_elec())
    yelec_ilk_bin = digit(nrow(subset(
      yBolum_elec(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / yelec_Rows, 2)
    yelec_ilk_5bin = digit(nrow(subset(
      yBolum_elec(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / yelec_Rows, 2)
    yelec_ilk_10bin = digit(nrow(subset(
      yBolum_elec(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / yelec_Rows, 2)
    yelec_ilk_20bin = digit(nrow(subset(
      yBolum_elec(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / yelec_Rows, 2)
    yelec_ilk_50bin = digit(nrow(subset(
      yBolum_elec(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / yelec_Rows, 2)
    yelec_ilk_100bin = digit(nrow(subset(
      yBolum_elec(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / yelec_Rows, 2)
    yelec_ilk_200bin = digit(nrow(subset(
      yBolum_elec(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / yelec_Rows, 2)
    yelec_ust_50bin = digit(nrow(subset(
      yBolum_elec(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / yelec_Rows, 2)
    yelec_ust_100bin = digit(nrow(subset(
      yBolum_elec(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / yelec_Rows, 2)
    yelec_25 = digit(quantile(yBolum_elec()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    yelec_50 = digit(quantile(yBolum_elec()$YERKULLANILANBASARISIRASI, 0.5), 0)
    yelec_75 = digit(quantile(yBolum_elec()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    yelec_90 = digit(quantile(yBolum_elec()$YERKULLANILANBASARISIRASI, 0.9), 0)
    yelec_95 = digit(quantile(yBolum_elec()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    yelec_son = digit(quantile(yBolum_elec()$YERKULLANILANBASARISIRASI, 1), 0)
    yelec_ab = nrow(subset(yBolum_elec(), ANADOLUBURS == "AB"))
    return(
      c(
        yelec_ilk_bin,
        yelec_ilk_5bin,
        yelec_ilk_10bin,
        yelec_ilk_20bin,
        yelec_ilk_50bin,
        yelec_ilk_100bin,
        yelec_ilk_200bin,
        yelec_ust_50bin,
        yelec_ust_100bin,
        yelec_25,
        yelec_50,
        yelec_75,
        yelec_90,
        yelec_95,
        yelec_son,
        yelec_ab
      )
    )
  })
  indr_ist <- reactive({
    yindr_Rows = nrow(yBolum_indr())
    yindr_ilk_bin = digit(nrow(subset(
      yBolum_indr(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / yindr_Rows, 2)
    yindr_ilk_5bin = digit(nrow(subset(
      yBolum_indr(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / yindr_Rows, 2)
    yindr_ilk_10bin = digit(nrow(subset(
      yBolum_indr(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / yindr_Rows, 2)
    yindr_ilk_20bin = digit(nrow(subset(
      yBolum_indr(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / yindr_Rows, 2)
    yindr_ilk_50bin = digit(nrow(subset(
      yBolum_indr(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / yindr_Rows, 2)
    yindr_ilk_100bin = digit(nrow(subset(
      yBolum_indr(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / yindr_Rows, 2)
    yindr_ilk_200bin = digit(nrow(subset(
      yBolum_indr(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / yindr_Rows, 2)
    yindr_ust_50bin = digit(nrow(subset(
      yBolum_indr(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / yindr_Rows, 2)
    yindr_ust_100bin = digit(nrow(subset(
      yBolum_indr(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / yindr_Rows, 2)
    yindr_25 = digit(quantile(yBolum_indr()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    yindr_50 = digit(quantile(yBolum_indr()$YERKULLANILANBASARISIRASI, 0.5), 0)
    yindr_75 = digit(quantile(yBolum_indr()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    yindr_90 = digit(quantile(yBolum_indr()$YERKULLANILANBASARISIRASI, 0.9), 0)
    yindr_95 = digit(quantile(yBolum_indr()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    yindr_son = digit(quantile(yBolum_indr()$YERKULLANILANBASARISIRASI, 1), 0)
    yindr_ab = nrow(subset(yBolum_indr(), ANADOLUBURS == "AB"))
    return(
      c(
        yindr_ilk_bin,
        yindr_ilk_5bin,
        yindr_ilk_10bin,
        yindr_ilk_20bin,
        yindr_ilk_50bin,
        yindr_ilk_100bin,
        yindr_ilk_200bin,
        yindr_ust_50bin,
        yindr_ust_100bin,
        yindr_25,
        yindr_50,
        yindr_75,
        yindr_90,
        yindr_95,
        yindr_son,
        yindr_ab
      )
    )
  })
  mech_ist <- reactive({
    ymech_Rows = nrow(yBolum_mech())
    ymech_ilk_bin = digit(nrow(subset(
      yBolum_mech(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ymech_Rows, 2)
    ymech_ilk_5bin = digit(nrow(subset(
      yBolum_mech(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ymech_Rows, 2)
    ymech_ilk_10bin = digit(nrow(subset(
      yBolum_mech(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ymech_Rows, 2)
    ymech_ilk_20bin = digit(nrow(subset(
      yBolum_mech(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ymech_Rows, 2)
    ymech_ilk_50bin = digit(nrow(subset(
      yBolum_mech(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ymech_Rows, 2)
    ymech_ilk_100bin = digit(nrow(subset(
      yBolum_mech(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ymech_Rows, 2)
    ymech_ilk_200bin = digit(nrow(subset(
      yBolum_mech(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ymech_Rows, 2)
    ymech_ust_50bin = digit(nrow(subset(
      yBolum_mech(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ymech_Rows, 2)
    ymech_ust_100bin = digit(nrow(subset(
      yBolum_mech(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ymech_Rows, 2)
    ymech_25 = digit(quantile(yBolum_mech()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    ymech_50 = digit(quantile(yBolum_mech()$YERKULLANILANBASARISIRASI, 0.5), 0)
    ymech_75 = digit(quantile(yBolum_mech()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    ymech_90 = digit(quantile(yBolum_mech()$YERKULLANILANBASARISIRASI, 0.9), 0)
    ymech_95 = digit(quantile(yBolum_mech()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    ymech_son = digit(quantile(yBolum_mech()$YERKULLANILANBASARISIRASI, 1), 0)
    ymech_ab = nrow(subset(yBolum_mech(), ANADOLUBURS == "AB"))
    return(
      c(
        ymech_ilk_bin,
        ymech_ilk_5bin,
        ymech_ilk_10bin,
        ymech_ilk_20bin,
        ymech_ilk_50bin,
        ymech_ilk_100bin,
        ymech_ilk_200bin,
        ymech_ust_50bin,
        ymech_ust_100bin,
        ymech_25,
        ymech_50,
        ymech_75,
        ymech_90,
        ymech_95,
        ymech_son,
        ymech_ab
      )
    )
  })
  chem_ist <- reactive({
    ychem_Rows = nrow(yBolum_chem())
    ychem_ilk_bin = digit(nrow(subset(
      yBolum_chem(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ychem_Rows, 2)
    ychem_ilk_5bin = digit(nrow(subset(
      yBolum_chem(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ychem_Rows, 2)
    ychem_ilk_10bin = digit(nrow(subset(
      yBolum_chem(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ychem_Rows, 2)
    ychem_ilk_20bin = digit(nrow(subset(
      yBolum_chem(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ychem_Rows, 2)
    ychem_ilk_50bin = digit(nrow(subset(
      yBolum_chem(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ychem_Rows, 2)
    ychem_ilk_100bin = digit(nrow(subset(
      yBolum_chem(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ychem_Rows, 2)
    ychem_ilk_200bin = digit(nrow(subset(
      yBolum_chem(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ychem_Rows, 2)
    ychem_ust_50bin = digit(nrow(subset(
      yBolum_chem(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ychem_Rows, 2)
    ychem_ust_100bin = digit(nrow(subset(
      yBolum_chem(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ychem_Rows, 2)
    ychem_25 = digit(quantile(yBolum_chem()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    ychem_50 = digit(quantile(yBolum_chem()$YERKULLANILANBASARISIRASI, 0.5), 0)
    ychem_75 = digit(quantile(yBolum_chem()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    ychem_90 = digit(quantile(yBolum_chem()$YERKULLANILANBASARISIRASI, 0.9), 0)
    ychem_95 = digit(quantile(yBolum_chem()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    ychem_son = digit(quantile(yBolum_chem()$YERKULLANILANBASARISIRASI, 1), 0)
    ychem_ab = nrow(subset(yBolum_chem(), ANADOLUBURS == "AB"))
    return(
      c(
        ychem_ilk_bin,
        ychem_ilk_5bin,
        ychem_ilk_10bin,
        ychem_ilk_20bin,
        ychem_ilk_50bin,
        ychem_ilk_100bin,
        ychem_ilk_200bin,
        ychem_ust_50bin,
        ychem_ust_100bin,
        ychem_25,
        ychem_50,
        ychem_75,
        ychem_90,
        ychem_95,
        ychem_son,
        ychem_ab
      )
    )
  })
  math_ist <- reactive({
    ymath_Rows = nrow(yBolum_math())
    ymath_ilk_bin = digit(nrow(subset(
      yBolum_math(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ymath_Rows, 2)
    ymath_ilk_5bin = digit(nrow(subset(
      yBolum_math(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ymath_Rows, 2)
    ymath_ilk_10bin = digit(nrow(subset(
      yBolum_math(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ymath_Rows, 2)
    ymath_ilk_20bin = digit(nrow(subset(
      yBolum_math(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ymath_Rows, 2)
    ymath_ilk_50bin = digit(nrow(subset(
      yBolum_math(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ymath_Rows, 2)
    ymath_ilk_100bin = digit(nrow(subset(
      yBolum_math(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ymath_Rows, 2)
    ymath_ilk_200bin = digit(nrow(subset(
      yBolum_math(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ymath_Rows, 2)
    ymath_ust_50bin = digit(nrow(subset(
      yBolum_math(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ymath_Rows, 2)
    ymath_ust_100bin = digit(nrow(subset(
      yBolum_math(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ymath_Rows, 2)
    ymath_25 = digit(quantile(yBolum_math()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    ymath_50 = digit(quantile(yBolum_math()$YERKULLANILANBASARISIRASI, 0.5), 0)
    ymath_75 = digit(quantile(yBolum_math()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    ymath_90 = digit(quantile(yBolum_math()$YERKULLANILANBASARISIRASI, 0.9), 0)
    ymath_95 = digit(quantile(yBolum_math()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    ymath_son = digit(quantile(yBolum_math()$YERKULLANILANBASARISIRASI, 1), 0)
    ymath_ab = nrow(subset(yBolum_math(), ANADOLUBURS == "AB"))
    return(
      c(
        ymath_ilk_bin,
        ymath_ilk_5bin,
        ymath_ilk_10bin,
        ymath_ilk_20bin,
        ymath_ilk_50bin,
        ymath_ilk_100bin,
        ymath_ilk_200bin,
        ymath_ust_50bin,
        ymath_ust_100bin,
        ymath_25,
        ymath_50,
        ymath_75,
        ymath_90,
        ymath_95,
        ymath_son,
        ymath_ab
      )
    )
  })
  mbge_ist <- reactive({
    ymbge_Rows = nrow(yBolum_mbge())
    ymbge_ilk_bin = digit(nrow(subset(
      yBolum_mbge(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ymbge_Rows, 2)
    ymbge_ilk_5bin = digit(nrow(subset(
      yBolum_mbge(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ymbge_Rows, 2)
    ymbge_ilk_10bin = digit(nrow(subset(
      yBolum_mbge(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ymbge_Rows, 2)
    ymbge_ilk_20bin = digit(nrow(subset(
      yBolum_mbge(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ymbge_Rows, 2)
    ymbge_ilk_50bin = digit(nrow(subset(
      yBolum_mbge(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ymbge_Rows, 2)
    ymbge_ilk_100bin = digit(nrow(subset(
      yBolum_mbge(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ymbge_Rows, 2)
    ymbge_ilk_200bin = digit(nrow(subset(
      yBolum_mbge(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ymbge_Rows, 2)
    ymbge_ust_50bin = digit(nrow(subset(
      yBolum_mbge(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ymbge_Rows, 2)
    ymbge_ust_100bin = digit(nrow(subset(
      yBolum_mbge(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ymbge_Rows, 2)
    ymbge_25 = digit(quantile(yBolum_mbge()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    ymbge_50 = digit(quantile(yBolum_mbge()$YERKULLANILANBASARISIRASI, 0.5), 0)
    ymbge_75 = digit(quantile(yBolum_mbge()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    ymbge_90 = digit(quantile(yBolum_mbge()$YERKULLANILANBASARISIRASI, 0.9), 0)
    ymbge_95 = digit(quantile(yBolum_mbge()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    ymbge_son = digit(quantile(yBolum_mbge()$YERKULLANILANBASARISIRASI, 1), 0)
    ymbge_ab = nrow(subset(yBolum_mbge(), ANADOLUBURS == "AB"))
    return(
      c(
        ymbge_ilk_bin,
        ymbge_ilk_5bin,
        ymbge_ilk_10bin,
        ymbge_ilk_20bin,
        ymbge_ilk_50bin,
        ymbge_ilk_100bin,
        ymbge_ilk_200bin,
        ymbge_ust_50bin,
        ymbge_ust_100bin,
        ymbge_25,
        ymbge_50,
        ymbge_75,
        ymbge_90,
        ymbge_95,
        ymbge_son,
        ymbge_ab
      )
    )
  })
  phys_ist <- reactive({
    yphys_Rows = nrow(yBolum_phys())
    yphys_ilk_bin = digit(nrow(subset(
      yBolum_phys(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / yphys_Rows, 2)
    yphys_ilk_5bin = digit(nrow(subset(
      yBolum_phys(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / yphys_Rows, 2)
    yphys_ilk_10bin = digit(nrow(subset(
      yBolum_phys(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / yphys_Rows, 2)
    yphys_ilk_20bin = digit(nrow(subset(
      yBolum_phys(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / yphys_Rows, 2)
    yphys_ilk_50bin = digit(nrow(subset(
      yBolum_phys(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / yphys_Rows, 2)
    yphys_ilk_100bin = digit(nrow(subset(
      yBolum_phys(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / yphys_Rows, 2)
    yphys_ilk_200bin = digit(nrow(subset(
      yBolum_phys(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / yphys_Rows, 2)
    yphys_ust_50bin = digit(nrow(subset(
      yBolum_phys(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / yphys_Rows, 2)
    yphys_ust_100bin = digit(nrow(subset(
      yBolum_phys(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / yphys_Rows, 2)
    yphys_25 = digit(quantile(yBolum_phys()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    yphys_50 = digit(quantile(yBolum_phys()$YERKULLANILANBASARISIRASI, 0.5), 0)
    yphys_75 = digit(quantile(yBolum_phys()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    yphys_90 = digit(quantile(yBolum_phys()$YERKULLANILANBASARISIRASI, 0.9), 0)
    yphys_95 = digit(quantile(yBolum_phys()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    yphys_son = digit(quantile(yBolum_phys()$YERKULLANILANBASARISIRASI, 1), 0)
    yphys_ab = nrow(subset(yBolum_phys(), ANADOLUBURS == "AB"))
    return(
      c(
        yphys_ilk_bin,
        yphys_ilk_5bin,
        yphys_ilk_10bin,
        yphys_ilk_20bin,
        yphys_ilk_50bin,
        yphys_ilk_100bin,
        yphys_ilk_200bin,
        yphys_ust_50bin,
        yphys_ust_100bin,
        yphys_25,
        yphys_50,
        yphys_75,
        yphys_90,
        yphys_95,
        yphys_son,
        yphys_ab
      )
    )
  })
  arha_ist <- reactive({
    yarha_Rows = nrow(yBolum_arha())
    yarha_ilk_bin = digit(nrow(subset(
      yBolum_arha(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / yarha_Rows, 2)
    yarha_ilk_5bin = digit(nrow(subset(
      yBolum_arha(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / yarha_Rows, 2)
    yarha_ilk_10bin = digit(nrow(subset(
      yBolum_arha(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / yarha_Rows, 2)
    yarha_ilk_20bin = digit(nrow(subset(
      yBolum_arha(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / yarha_Rows, 2)
    yarha_ilk_50bin = digit(nrow(subset(
      yBolum_arha(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / yarha_Rows, 2)
    yarha_ilk_100bin = digit(nrow(subset(
      yBolum_arha(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / yarha_Rows, 2)
    yarha_ilk_200bin = digit(nrow(subset(
      yBolum_arha(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / yarha_Rows, 2)
    yarha_ust_50bin = digit(nrow(subset(
      yBolum_arha(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / yarha_Rows, 2)
    yarha_ust_100bin = digit(nrow(subset(
      yBolum_arha(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / yarha_Rows, 2)
    yarha_25 = digit(quantile(yBolum_arha()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    yarha_50 = digit(quantile(yBolum_arha()$YERKULLANILANBASARISIRASI, 0.5), 0)
    yarha_75 = digit(quantile(yBolum_arha()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    yarha_90 = digit(quantile(yBolum_arha()$YERKULLANILANBASARISIRASI, 0.9), 0)
    yarha_95 = digit(quantile(yBolum_arha()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    yarha_son = digit(quantile(yBolum_arha()$YERKULLANILANBASARISIRASI, 1), 0)
    yarha_ab = nrow(subset(yBolum_arha(), ANADOLUBURS == "AB"))
    return(
      c(
        yarha_ilk_bin,
        yarha_ilk_5bin,
        yarha_ilk_10bin,
        yarha_ilk_20bin,
        yarha_ilk_50bin,
        yarha_ilk_100bin,
        yarha_ilk_200bin,
        yarha_ust_50bin,
        yarha_ust_100bin,
        yarha_25,
        yarha_50,
        yarha_75,
        yarha_90,
        yarha_95,
        yarha_son,
        yarha_ab
      )
    )
  })
  encl_ist <- reactive({
    yencl_Rows = nrow(yBolum_encl())
    yencl_ilk_bin = digit(nrow(subset(
      yBolum_encl(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / yencl_Rows, 2)
    yencl_ilk_5bin = digit(nrow(subset(
      yBolum_encl(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / yencl_Rows, 2)
    yencl_ilk_10bin = digit(nrow(subset(
      yBolum_encl(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / yencl_Rows, 2)
    yencl_ilk_20bin = digit(nrow(subset(
      yBolum_encl(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / yencl_Rows, 2)
    yencl_ilk_50bin = digit(nrow(subset(
      yBolum_encl(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / yencl_Rows, 2)
    yencl_ilk_100bin = digit(nrow(subset(
      yBolum_encl(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / yencl_Rows, 2)
    yencl_ilk_200bin = digit(nrow(subset(
      yBolum_encl(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / yencl_Rows, 2)
    yencl_ust_50bin = digit(nrow(subset(
      yBolum_encl(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / yencl_Rows, 2)
    yencl_ust_100bin = digit(nrow(subset(
      yBolum_encl(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / yencl_Rows, 2)
    yencl_25 = digit(quantile(yBolum_encl()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    yencl_50 = digit(quantile(yBolum_encl()$YERKULLANILANBASARISIRASI, 0.5), 0)
    yencl_75 = digit(quantile(yBolum_encl()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    yencl_90 = digit(quantile(yBolum_encl()$YERKULLANILANBASARISIRASI, 0.9), 0)
    yencl_95 = digit(quantile(yBolum_encl()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    yencl_son = digit(quantile(yBolum_encl()$YERKULLANILANBASARISIRASI, 1), 0)
    yencl_ab = nrow(subset(yBolum_encl(), ANADOLUBURS == "AB"))
    return(
      c(
        yencl_ilk_bin,
        yencl_ilk_5bin,
        yencl_ilk_10bin,
        yencl_ilk_20bin,
        yencl_ilk_50bin,
        yencl_ilk_100bin,
        yencl_ilk_200bin,
        yencl_ust_50bin,
        yencl_ust_100bin,
        yencl_25,
        yencl_50,
        yencl_75,
        yencl_90,
        yencl_95,
        yencl_son,
        yencl_ab
      )
    )
  })
  hist_ist <- reactive({
    yhist_Rows = nrow(yBolum_hist())
    yhist_ilk_bin = digit(nrow(subset(
      yBolum_hist(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / yhist_Rows, 2)
    yhist_ilk_5bin = digit(nrow(subset(
      yBolum_hist(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / yhist_Rows, 2)
    yhist_ilk_10bin = digit(nrow(subset(
      yBolum_hist(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / yhist_Rows, 2)
    yhist_ilk_20bin = digit(nrow(subset(
      yBolum_hist(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / yhist_Rows, 2)
    yhist_ilk_50bin = digit(nrow(subset(
      yBolum_hist(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / yhist_Rows, 2)
    yhist_ilk_100bin = digit(nrow(subset(
      yBolum_hist(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / yhist_Rows, 2)
    yhist_ilk_200bin = digit(nrow(subset(
      yBolum_hist(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / yhist_Rows, 2)
    yhist_ust_50bin = digit(nrow(subset(
      yBolum_hist(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / yhist_Rows, 2)
    yhist_ust_100bin = digit(nrow(subset(
      yBolum_hist(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / yhist_Rows, 2)
    yhist_25 = digit(quantile(yBolum_hist()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    yhist_50 = digit(quantile(yBolum_hist()$YERKULLANILANBASARISIRASI, 0.5), 0)
    yhist_75 = digit(quantile(yBolum_hist()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    yhist_90 = digit(quantile(yBolum_hist()$YERKULLANILANBASARISIRASI, 0.9), 0)
    yhist_95 = digit(quantile(yBolum_hist()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    yhist_son = digit(quantile(yBolum_hist()$YERKULLANILANBASARISIRASI, 1), 0)
    yhist_ab = nrow(subset(yBolum_hist(), ANADOLUBURS == "AB"))
    return(
      c(
        yhist_ilk_bin,
        yhist_ilk_5bin,
        yhist_ilk_10bin,
        yhist_ilk_20bin,
        yhist_ilk_50bin,
        yhist_ilk_100bin,
        yhist_ilk_200bin,
        yhist_ust_50bin,
        yhist_ust_100bin,
        yhist_25,
        yhist_50,
        yhist_75,
        yhist_90,
        yhist_95,
        yhist_son,
        yhist_ab
      )
    )
  })
  mava_ist <- reactive({
    ymava_Rows = nrow(yBolum_mava())
    ymava_ilk_bin = digit(nrow(subset(
      yBolum_mava(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ymava_Rows, 2)
    ymava_ilk_5bin = digit(nrow(subset(
      yBolum_mava(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ymava_Rows, 2)
    ymava_ilk_10bin = digit(nrow(subset(
      yBolum_mava(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ymava_Rows, 2)
    ymava_ilk_20bin = digit(nrow(subset(
      yBolum_mava(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ymava_Rows, 2)
    ymava_ilk_50bin = digit(nrow(subset(
      yBolum_mava(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ymava_Rows, 2)
    ymava_ilk_100bin = digit(nrow(subset(
      yBolum_mava(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ymava_Rows, 2)
    ymava_ilk_200bin = digit(nrow(subset(
      yBolum_mava(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ymava_Rows, 2)
    ymava_ust_50bin = digit(nrow(subset(
      yBolum_mava(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ymava_Rows, 2)
    ymava_ust_100bin = digit(nrow(subset(
      yBolum_mava(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ymava_Rows, 2)
    ymava_25 = digit(quantile(yBolum_mava()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    ymava_50 = digit(quantile(yBolum_mava()$YERKULLANILANBASARISIRASI, 0.5), 0)
    ymava_75 = digit(quantile(yBolum_mava()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    ymava_90 = digit(quantile(yBolum_mava()$YERKULLANILANBASARISIRASI, 0.9), 0)
    ymava_95 = digit(quantile(yBolum_mava()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    ymava_son = digit(quantile(yBolum_mava()$YERKULLANILANBASARISIRASI, 1), 0)
    ymava_ab = nrow(subset(yBolum_mava(), ANADOLUBURS == "AB"))
    return(
      c(
        ymava_ilk_bin,
        ymava_ilk_5bin,
        ymava_ilk_10bin,
        ymava_ilk_20bin,
        ymava_ilk_50bin,
        ymava_ilk_100bin,
        ymava_ilk_200bin,
        ymava_ust_50bin,
        ymava_ust_100bin,
        ymava_25,
        ymava_50,
        ymava_75,
        ymava_90,
        ymava_95,
        ymava_son,
        ymava_ab
      )
    )
  })
  phil_ist <- reactive({
    yphil_Rows = nrow(yBolum_phil())
    yphil_ilk_bin = digit(nrow(subset(
      yBolum_phil(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / yphil_Rows, 2)
    yphil_ilk_5bin = digit(nrow(subset(
      yBolum_phil(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / yphil_Rows, 2)
    yphil_ilk_10bin = digit(nrow(subset(
      yBolum_phil(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / yphil_Rows, 2)
    yphil_ilk_20bin = digit(nrow(subset(
      yBolum_phil(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / yphil_Rows, 2)
    yphil_ilk_50bin = digit(nrow(subset(
      yBolum_phil(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / yphil_Rows, 2)
    yphil_ilk_100bin = digit(nrow(subset(
      yBolum_phil(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / yphil_Rows, 2)
    yphil_ilk_200bin = digit(nrow(subset(
      yBolum_phil(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / yphil_Rows, 2)
    yphil_ust_50bin = digit(nrow(subset(
      yBolum_phil(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / yphil_Rows, 2)
    yphil_ust_100bin = digit(nrow(subset(
      yBolum_phil(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / yphil_Rows, 2)
    yphil_25 = digit(quantile(yBolum_phil()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    yphil_50 = digit(quantile(yBolum_phil()$YERKULLANILANBASARISIRASI, 0.5), 0)
    yphil_75 = digit(quantile(yBolum_phil()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    yphil_90 = digit(quantile(yBolum_phil()$YERKULLANILANBASARISIRASI, 0.9), 0)
    yphil_95 = digit(quantile(yBolum_phil()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    yphil_son = digit(quantile(yBolum_phil()$YERKULLANILANBASARISIRASI, 1), 0)
    yphil_ab = nrow(subset(yBolum_phil(), ANADOLUBURS == "AB"))
    return(
      c(
        yphil_ilk_bin,
        yphil_ilk_5bin,
        yphil_ilk_10bin,
        yphil_ilk_20bin,
        yphil_ilk_50bin,
        yphil_ilk_100bin,
        yphil_ilk_200bin,
        yphil_ust_50bin,
        yphil_ust_100bin,
        yphil_25,
        yphil_50,
        yphil_75,
        yphil_90,
        yphil_95,
        yphil_son,
        yphil_ab
      )
    )
  })
  psyc_ist <- reactive({
    ypsyc_Rows = nrow(yBolum_psyc())
    ypsyc_ilk_bin = digit(nrow(subset(
      yBolum_psyc(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ypsyc_Rows, 2)
    ypsyc_ilk_5bin = digit(nrow(subset(
      yBolum_psyc(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ypsyc_Rows, 2)
    ypsyc_ilk_10bin = digit(nrow(subset(
      yBolum_psyc(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ypsyc_Rows, 2)
    ypsyc_ilk_20bin = digit(nrow(subset(
      yBolum_psyc(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ypsyc_Rows, 2)
    ypsyc_ilk_50bin = digit(nrow(subset(
      yBolum_psyc(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ypsyc_Rows, 2)
    ypsyc_ilk_100bin = digit(nrow(subset(
      yBolum_psyc(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ypsyc_Rows, 2)
    ypsyc_ilk_200bin = digit(nrow(subset(
      yBolum_psyc(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ypsyc_Rows, 2)
    ypsyc_ust_50bin = digit(nrow(subset(
      yBolum_psyc(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ypsyc_Rows, 2)
    ypsyc_ust_100bin = digit(nrow(subset(
      yBolum_psyc(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ypsyc_Rows, 2)
    ypsyc_25 = digit(quantile(yBolum_psyc()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    ypsyc_50 = digit(quantile(yBolum_psyc()$YERKULLANILANBASARISIRASI, 0.5), 0)
    ypsyc_75 = digit(quantile(yBolum_psyc()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    ypsyc_90 = digit(quantile(yBolum_psyc()$YERKULLANILANBASARISIRASI, 0.9), 0)
    ypsyc_95 = digit(quantile(yBolum_psyc()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    ypsyc_son = digit(quantile(yBolum_psyc()$YERKULLANILANBASARISIRASI, 1), 0)
    ypsyc_ab = nrow(subset(yBolum_psyc(), ANADOLUBURS == "AB"))
    return(
      c(
        ypsyc_ilk_bin,
        ypsyc_ilk_5bin,
        ypsyc_ilk_10bin,
        ypsyc_ilk_20bin,
        ypsyc_ilk_50bin,
        ypsyc_ilk_100bin,
        ypsyc_ilk_200bin,
        ypsyc_ust_50bin,
        ypsyc_ust_100bin,
        ypsyc_25,
        ypsyc_50,
        ypsyc_75,
        ypsyc_90,
        ypsyc_95,
        ypsyc_son,
        ypsyc_ab
      )
    )
  })
  soci_ist <- reactive({
    ysoci_Rows = nrow(yBolum_soci())
    ysoci_ilk_bin = digit(nrow(subset(
      yBolum_soci(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ysoci_Rows, 2)
    ysoci_ilk_5bin = digit(nrow(subset(
      yBolum_soci(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ysoci_Rows, 2)
    ysoci_ilk_10bin = digit(nrow(subset(
      yBolum_soci(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ysoci_Rows, 2)
    ysoci_ilk_20bin = digit(nrow(subset(
      yBolum_soci(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ysoci_Rows, 2)
    ysoci_ilk_50bin = digit(nrow(subset(
      yBolum_soci(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ysoci_Rows, 2)
    ysoci_ilk_100bin = digit(nrow(subset(
      yBolum_soci(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ysoci_Rows, 2)
    ysoci_ilk_200bin = digit(nrow(subset(
      yBolum_soci(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ysoci_Rows, 2)
    ysoci_ust_50bin = digit(nrow(subset(
      yBolum_soci(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ysoci_Rows, 2)
    ysoci_ust_100bin = digit(nrow(subset(
      yBolum_soci(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ysoci_Rows, 2)
    ysoci_25 = digit(quantile(yBolum_soci()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    ysoci_50 = digit(quantile(yBolum_soci()$YERKULLANILANBASARISIRASI, 0.5), 0)
    ysoci_75 = digit(quantile(yBolum_soci()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    ysoci_90 = digit(quantile(yBolum_soci()$YERKULLANILANBASARISIRASI, 0.9), 0)
    ysoci_95 = digit(quantile(yBolum_soci()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    ysoci_son = digit(quantile(yBolum_soci()$YERKULLANILANBASARISIRASI, 1), 0)
    ysoci_ab = nrow(subset(yBolum_soci(), ANADOLUBURS == "AB"))
    return(
      c(
        ysoci_ilk_bin,
        ysoci_ilk_5bin,
        ysoci_ilk_10bin,
        ysoci_ilk_20bin,
        ysoci_ilk_50bin,
        ysoci_ilk_100bin,
        ysoci_ilk_200bin,
        ysoci_ust_50bin,
        ysoci_ust_100bin,
        ysoci_25,
        ysoci_50,
        ysoci_75,
        ysoci_90,
        ysoci_95,
        ysoci_son,
        ysoci_ab
      )
    )
  })
  law_ist <- reactive({
    ylaw_Rows = nrow(yBolum_law())
    ylaw_ilk_bin = digit(nrow(subset(
      yBolum_law(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ylaw_Rows, 2)
    ylaw_ilk_5bin = digit(nrow(subset(
      yBolum_law(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ylaw_Rows, 2)
    ylaw_ilk_10bin = digit(nrow(subset(
      yBolum_law(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ylaw_Rows, 2)
    ylaw_ilk_20bin = digit(nrow(subset(
      yBolum_law(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ylaw_Rows, 2)
    ylaw_ilk_50bin = digit(nrow(subset(
      yBolum_law(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ylaw_Rows, 2)
    ylaw_ilk_100bin = digit(nrow(subset(
      yBolum_law(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ylaw_Rows, 2)
    ylaw_ilk_200bin = digit(nrow(subset(
      yBolum_law(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ylaw_Rows, 2)
    ylaw_ust_50bin = digit(nrow(subset(
      yBolum_law(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ylaw_Rows, 2)
    ylaw_ust_100bin = digit(nrow(subset(
      yBolum_law(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ylaw_Rows, 2)
    ylaw_25 = digit(quantile(yBolum_law()$YERKULLANILANBASARISIRASI, 0.25), 0)
    ylaw_50 = digit(quantile(yBolum_law()$YERKULLANILANBASARISIRASI, 0.5), 0)
    ylaw_75 = digit(quantile(yBolum_law()$YERKULLANILANBASARISIRASI, 0.75), 0)
    ylaw_90 = digit(quantile(yBolum_law()$YERKULLANILANBASARISIRASI, 0.9), 0)
    ylaw_95 = digit(quantile(yBolum_law()$YERKULLANILANBASARISIRASI, 0.95), 0)
    ylaw_son = digit(quantile(yBolum_law()$YERKULLANILANBASARISIRASI, 1), 0)
    ylaw_ab = nrow(subset(yBolum_law(), ANADOLUBURS == "AB"))
    return(
      c(
        ylaw_ilk_bin,
        ylaw_ilk_5bin,
        ylaw_ilk_10bin,
        ylaw_ilk_20bin,
        ylaw_ilk_50bin,
        ylaw_ilk_100bin,
        ylaw_ilk_200bin,
        ylaw_ust_50bin,
        ylaw_ust_100bin,
        ylaw_25,
        ylaw_50,
        ylaw_75,
        ylaw_90,
        ylaw_95,
        ylaw_son,
        ylaw_ab
      )
    )
  })
  medi_ist <- reactive({
    ymedi_Rows = nrow(yBolum_medi())
    ymedi_ilk_bin = digit(nrow(subset(
      yBolum_medi(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ymedi_Rows, 2)
    ymedi_ilk_5bin = digit(nrow(subset(
      yBolum_medi(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ymedi_Rows, 2)
    ymedi_ilk_10bin = digit(nrow(subset(
      yBolum_medi(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ymedi_Rows, 2)
    ymedi_ilk_20bin = digit(nrow(subset(
      yBolum_medi(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ymedi_Rows, 2)
    ymedi_ilk_50bin = digit(nrow(subset(
      yBolum_medi(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ymedi_Rows, 2)
    ymedi_ilk_100bin = digit(nrow(subset(
      yBolum_medi(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ymedi_Rows, 2)
    ymedi_ilk_200bin = digit(nrow(subset(
      yBolum_medi(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ymedi_Rows, 2)
    ymedi_ust_50bin = digit(nrow(subset(
      yBolum_medi(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ymedi_Rows, 2)
    ymedi_ust_100bin = digit(nrow(subset(
      yBolum_medi(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ymedi_Rows, 2)
    ymedi_25 = digit(quantile(yBolum_medi()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    ymedi_50 = digit(quantile(yBolum_medi()$YERKULLANILANBASARISIRASI, 0.5), 0)
    ymedi_75 = digit(quantile(yBolum_medi()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    ymedi_90 = digit(quantile(yBolum_medi()$YERKULLANILANBASARISIRASI, 0.9), 0)
    ymedi_95 = digit(quantile(yBolum_medi()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    ymedi_son = digit(quantile(yBolum_medi()$YERKULLANILANBASARISIRASI, 1), 0)
    ymedi_ab = nrow(subset(yBolum_medi(), ANADOLUBURS == "AB"))
    return(
      c(
        ymedi_ilk_bin,
        ymedi_ilk_5bin,
        ymedi_ilk_10bin,
        ymedi_ilk_20bin,
        ymedi_ilk_50bin,
        ymedi_ilk_100bin,
        ymedi_ilk_200bin,
        ymedi_ust_50bin,
        ymedi_ust_100bin,
        ymedi_25,
        ymedi_50,
        ymedi_75,
        ymedi_90,
        ymedi_95,
        ymedi_son,
        ymedi_ab
      )
    )
  })
  nurs_ist <- reactive({
    ynurs_Rows = nrow(yBolum_nurs())
    ynurs_ilk_bin = digit(nrow(subset(
      yBolum_nurs(), YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ynurs_Rows, 2)
    ynurs_ilk_5bin = digit(nrow(subset(
      yBolum_nurs(), YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ynurs_Rows, 2)
    ynurs_ilk_10bin = digit(nrow(subset(
      yBolum_nurs(), YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ynurs_Rows, 2)
    ynurs_ilk_20bin = digit(nrow(subset(
      yBolum_nurs(), YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ynurs_Rows, 2)
    ynurs_ilk_50bin = digit(nrow(subset(
      yBolum_nurs(), YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ynurs_Rows, 2)
    ynurs_ilk_100bin = digit(nrow(subset(
      yBolum_nurs(), YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ynurs_Rows, 2)
    ynurs_ilk_200bin = digit(nrow(subset(
      yBolum_nurs(), YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ynurs_Rows, 2)
    ynurs_ust_50bin = digit(nrow(subset(
      yBolum_nurs(), YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ynurs_Rows, 2)
    ynurs_ust_100bin = digit(nrow(subset(
      yBolum_nurs(), YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ynurs_Rows, 2)
    ynurs_25 = digit(quantile(yBolum_nurs()$YERKULLANILANBASARISIRASI, 0.25),
                     0)
    ynurs_50 = digit(quantile(yBolum_nurs()$YERKULLANILANBASARISIRASI, 0.5), 0)
    ynurs_75 = digit(quantile(yBolum_nurs()$YERKULLANILANBASARISIRASI, 0.75),
                     0)
    ynurs_90 = digit(quantile(yBolum_nurs()$YERKULLANILANBASARISIRASI, 0.9), 0)
    ynurs_95 = digit(quantile(yBolum_nurs()$YERKULLANILANBASARISIRASI, 0.95),
                     0)
    ynurs_son = digit(quantile(yBolum_nurs()$YERKULLANILANBASARISIRASI, 1), 0)
    ynurs_ab = nrow(subset(yBolum_nurs(), ANADOLUBURS == "AB"))
    return(
      c(
        ynurs_ilk_bin,
        ynurs_ilk_5bin,
        ynurs_ilk_10bin,
        ynurs_ilk_20bin,
        ynurs_ilk_50bin,
        ynurs_ilk_100bin,
        ynurs_ilk_200bin,
        ynurs_ust_50bin,
        ynurs_ust_100bin,
        ynurs_25,
        ynurs_50,
        ynurs_75,
        ynurs_90,
        ynurs_95,
        ynurs_son,
        ynurs_ab
      )
    )
  })
  case_ist <- reactive({
    yBolum_case = subset(yResult(), KUSISCOLL == "CASE")
    ycase_Rows = nrow(yBolum_case)
    ycase_ilk_bin = digit(nrow(subset(
      yBolum_case, YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ycase_Rows, 2)
    ycase_ilk_5bin = digit(nrow(subset(
      yBolum_case, YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ycase_Rows, 2)
    ycase_ilk_10bin = digit(nrow(subset(
      yBolum_case, YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ycase_Rows, 2)
    ycase_ilk_20bin = digit(nrow(subset(
      yBolum_case, YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ycase_Rows, 2)
    ycase_ilk_50bin = digit(nrow(subset(
      yBolum_case, YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ycase_Rows, 2)
    ycase_ilk_100bin = digit(nrow(subset(
      yBolum_case, YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ycase_Rows, 2)
    ycase_ilk_200bin = digit(nrow(subset(
      yBolum_case, YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ycase_Rows, 2)
    ycase_ust_50bin = digit(nrow(subset(
      yBolum_case, YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ycase_Rows, 2)
    ycase_ust_100bin = digit(nrow(subset(
      yBolum_case, YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ycase_Rows, 2)
    ycase_25 = digit(quantile(yBolum_case$YERKULLANILANBASARISIRASI, 0.25), 0)
    ycase_50 = digit(quantile(yBolum_case$YERKULLANILANBASARISIRASI, 0.5), 0)
    ycase_75 = digit(quantile(yBolum_case$YERKULLANILANBASARISIRASI, 0.75), 0)
    ycase_90 = digit(quantile(yBolum_case$YERKULLANILANBASARISIRASI, 0.9), 0)
    ycase_95 = digit(quantile(yBolum_case$YERKULLANILANBASARISIRASI, 0.95), 0)
    ycase_son = digit(quantile(yBolum_case$YERKULLANILANBASARISIRASI, 1), 0)
    ycase_ab = nrow(subset(yBolum_case, ANADOLUBURS == "AB"))
    return(
      c(
        ycase_ilk_bin,
        ycase_ilk_5bin,
        ycase_ilk_10bin,
        ycase_ilk_20bin,
        ycase_ilk_50bin,
        ycase_ilk_100bin,
        ycase_ilk_200bin,
        ycase_ust_50bin,
        ycase_ust_100bin,
        ycase_25,
        ycase_50,
        ycase_75,
        ycase_90,
        ycase_95,
        ycase_son,
        ycase_ab
      )
    )
  })
  ce_ist <- reactive({
    yBolum_ce = subset(yResult(), KUSISCOLL == "CE")
    yce_Rows = nrow(yBolum_ce)
    yce_ilk_bin = digit(nrow(subset(
      yBolum_ce, YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / yce_Rows, 2)
    yce_ilk_5bin = digit(nrow(subset(
      yBolum_ce, YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / yce_Rows, 2)
    yce_ilk_10bin = digit(nrow(subset(
      yBolum_ce, YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / yce_Rows, 2)
    yce_ilk_20bin = digit(nrow(subset(
      yBolum_ce, YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / yce_Rows, 2)
    yce_ilk_50bin = digit(nrow(subset(
      yBolum_ce, YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / yce_Rows, 2)
    yce_ilk_100bin = digit(nrow(subset(
      yBolum_ce, YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / yce_Rows, 2)
    yce_ilk_200bin = digit(nrow(subset(
      yBolum_ce, YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / yce_Rows, 2)
    yce_ust_50bin = digit(nrow(subset(
      yBolum_ce, YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / yce_Rows, 2)
    yce_ust_100bin = digit(nrow(subset(
      yBolum_ce, YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / yce_Rows, 2)
    yce_25 = digit(quantile(yBolum_ce$YERKULLANILANBASARISIRASI, 0.25), 0)
    yce_50 = digit(quantile(yBolum_ce$YERKULLANILANBASARISIRASI, 0.5), 0)
    yce_75 = digit(quantile(yBolum_ce$YERKULLANILANBASARISIRASI, 0.75), 0)
    yce_90 = digit(quantile(yBolum_ce$YERKULLANILANBASARISIRASI, 0.9), 0)
    yce_95 = digit(quantile(yBolum_ce$YERKULLANILANBASARISIRASI, 0.95), 0)
    yce_son = digit(quantile(yBolum_ce$YERKULLANILANBASARISIRASI, 1), 0)
    yce_ab = nrow(subset(yBolum_ce, ANADOLUBURS == "AB"))
    return(
      c(
        yce_ilk_bin,
        yce_ilk_5bin,
        yce_ilk_10bin,
        yce_ilk_20bin,
        yce_ilk_50bin,
        yce_ilk_100bin,
        yce_ilk_200bin,
        yce_ust_50bin,
        yce_ust_100bin,
        yce_25,
        yce_50,
        yce_75,
        yce_90,
        yce_95,
        yce_son,
        yce_ab
      )
    )
  })
  cs_ist <- reactive({
    yBolum_cs = subset(yResult(), KUSISCOLL == "CS")
    ycs_Rows = nrow(yBolum_cs)
    ycs_ilk_bin = digit(nrow(subset(
      yBolum_cs, YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ycs_Rows, 2)
    ycs_ilk_5bin = digit(nrow(subset(
      yBolum_cs, YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ycs_Rows, 2)
    ycs_ilk_10bin = digit(nrow(subset(
      yBolum_cs, YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ycs_Rows, 2)
    ycs_ilk_20bin = digit(nrow(subset(
      yBolum_cs, YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ycs_Rows, 2)
    ycs_ilk_50bin = digit(nrow(subset(
      yBolum_cs, YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ycs_Rows, 2)
    ycs_ilk_100bin = digit(nrow(subset(
      yBolum_cs, YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ycs_Rows, 2)
    ycs_ilk_200bin = digit(nrow(subset(
      yBolum_cs, YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ycs_Rows, 2)
    ycs_ust_50bin = digit(nrow(subset(
      yBolum_cs, YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ycs_Rows, 2)
    ycs_ust_100bin = digit(nrow(subset(
      yBolum_cs, YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ycs_Rows, 2)
    ycs_25 = digit(quantile(yBolum_cs$YERKULLANILANBASARISIRASI, 0.25), 0)
    ycs_50 = digit(quantile(yBolum_cs$YERKULLANILANBASARISIRASI, 0.5), 0)
    ycs_75 = digit(quantile(yBolum_cs$YERKULLANILANBASARISIRASI, 0.75), 0)
    ycs_90 = digit(quantile(yBolum_cs$YERKULLANILANBASARISIRASI, 0.9), 0)
    ycs_95 = digit(quantile(yBolum_cs$YERKULLANILANBASARISIRASI, 0.95), 0)
    ycs_son = digit(quantile(yBolum_cs$YERKULLANILANBASARISIRASI, 1), 0)
    ycs_ab = nrow(subset(yBolum_cs, ANADOLUBURS == "AB"))
    return(
      c(
        ycs_ilk_bin,
        ycs_ilk_5bin,
        ycs_ilk_10bin,
        ycs_ilk_20bin,
        ycs_ilk_50bin,
        ycs_ilk_100bin,
        ycs_ilk_200bin,
        ycs_ust_50bin,
        ycs_ust_100bin,
        ycs_25,
        ycs_50,
        ycs_75,
        ycs_90,
        ycs_95,
        ycs_son,
        ycs_ab
      )
    )
  })
  cssh_ist <- reactive({
    yBolum_cssh = subset(yResult(), KUSISCOLL == "CSSH")
    ycssh_Rows = nrow(yBolum_cssh)
    ycssh_ilk_bin = digit(nrow(subset(
      yBolum_cssh, YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ycssh_Rows, 2)
    ycssh_ilk_5bin = digit(nrow(subset(
      yBolum_cssh, YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ycssh_Rows, 2)
    ycssh_ilk_10bin = digit(nrow(subset(
      yBolum_cssh, YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ycssh_Rows, 2)
    ycssh_ilk_20bin = digit(nrow(subset(
      yBolum_cssh, YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ycssh_Rows, 2)
    ycssh_ilk_50bin = digit(nrow(subset(
      yBolum_cssh, YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ycssh_Rows, 2)
    ycssh_ilk_100bin = digit(nrow(subset(
      yBolum_cssh, YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ycssh_Rows, 2)
    ycssh_ilk_200bin = digit(nrow(subset(
      yBolum_cssh, YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ycssh_Rows, 2)
    ycssh_ust_50bin = digit(nrow(subset(
      yBolum_cssh, YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ycssh_Rows, 2)
    ycssh_ust_100bin = digit(nrow(subset(
      yBolum_cssh, YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ycssh_Rows, 2)
    ycssh_25 = digit(quantile(yBolum_cssh$YERKULLANILANBASARISIRASI, 0.25), 0)
    ycssh_50 = digit(quantile(yBolum_cssh$YERKULLANILANBASARISIRASI, 0.5), 0)
    ycssh_75 = digit(quantile(yBolum_cssh$YERKULLANILANBASARISIRASI, 0.75), 0)
    ycssh_90 = digit(quantile(yBolum_cssh$YERKULLANILANBASARISIRASI, 0.9), 0)
    ycssh_95 = digit(quantile(yBolum_cssh$YERKULLANILANBASARISIRASI, 0.95), 0)
    ycssh_son = digit(quantile(yBolum_cssh$YERKULLANILANBASARISIRASI, 1), 0)
    ycssh_ab = nrow(subset(yBolum_cssh, ANADOLUBURS == "AB"))
    return(
      c(
        ycssh_ilk_bin,
        ycssh_ilk_5bin,
        ycssh_ilk_10bin,
        ycssh_ilk_20bin,
        ycssh_ilk_50bin,
        ycssh_ilk_100bin,
        ycssh_ilk_200bin,
        ycssh_ust_50bin,
        ycssh_ust_100bin,
        ycssh_25,
        ycssh_50,
        ycssh_75,
        ycssh_90,
        ycssh_95,
        ycssh_son,
        ycssh_ab
      )
    )
  })
  ea_ist <- reactive({
    yBolum_ea = subset(yResult(), YERLESTIGIPUANTURU == "EA")
    yea_Rows = nrow(yBolum_ea)
    yea_ilk_bin = digit(nrow(subset(
      yBolum_ea, YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / yea_Rows, 2)
    yea_ilk_5bin = digit(nrow(subset(
      yBolum_ea, YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / yea_Rows, 2)
    yea_ilk_10bin = digit(nrow(subset(
      yBolum_ea, YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / yea_Rows, 2)
    yea_ilk_20bin = digit(nrow(subset(
      yBolum_ea, YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / yea_Rows, 2)
    yea_ilk_50bin = digit(nrow(subset(
      yBolum_ea, YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / yea_Rows, 2)
    yea_ilk_100bin = digit(nrow(subset(
      yBolum_ea, YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / yea_Rows, 2)
    yea_ilk_200bin = digit(nrow(subset(
      yBolum_ea, YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / yea_Rows, 2)
    yea_ust_50bin = digit(nrow(subset(
      yBolum_ea, YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / yea_Rows, 2)
    yea_ust_100bin = digit(nrow(subset(
      yBolum_ea, YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / yea_Rows, 2)
    yea_25 = digit(quantile(yBolum_ea$YERKULLANILANBASARISIRASI, 0.25), 0)
    yea_50 = digit(quantile(yBolum_ea$YERKULLANILANBASARISIRASI, 0.5), 0)
    yea_75 = digit(quantile(yBolum_ea$YERKULLANILANBASARISIRASI, 0.75), 0)
    yea_90 = digit(quantile(yBolum_ea$YERKULLANILANBASARISIRASI, 0.9), 0)
    yea_95 = digit(quantile(yBolum_ea$YERKULLANILANBASARISIRASI, 0.95), 0)
    yea_son = digit(quantile(yBolum_ea$YERKULLANILANBASARISIRASI, 1), 0)
    yea_ab = nrow(subset(yBolum_ea, ANADOLUBURS == "AB"))
    return(
      c(
        yea_ilk_bin,
        yea_ilk_5bin,
        yea_ilk_10bin,
        yea_ilk_20bin,
        yea_ilk_50bin,
        yea_ilk_100bin,
        yea_ilk_200bin,
        yea_ust_50bin,
        yea_ust_100bin,
        yea_25,
        yea_50,
        yea_75,
        yea_90,
        yea_95,
        yea_son,
        yea_ab
      )
    )
  })
  say_ist <- reactive({
    yBolum_say = subset(yResult(), YERLESTIGIPUANTURU == "SAY")
    ysay_Rows = nrow(yBolum_say)
    ysay_ilk_bin = digit(nrow(subset(
      yBolum_say, YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ysay_Rows, 2)
    ysay_ilk_5bin = digit(nrow(subset(
      yBolum_say, YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ysay_Rows, 2)
    ysay_ilk_10bin = digit(nrow(subset(
      yBolum_say, YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ysay_Rows, 2)
    ysay_ilk_20bin = digit(nrow(subset(
      yBolum_say, YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ysay_Rows, 2)
    ysay_ilk_50bin = digit(nrow(subset(
      yBolum_say, YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ysay_Rows, 2)
    ysay_ilk_100bin = digit(nrow(subset(
      yBolum_say, YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ysay_Rows, 2)
    ysay_ilk_200bin = digit(nrow(subset(
      yBolum_say, YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ysay_Rows, 2)
    ysay_ust_50bin = digit(nrow(subset(
      yBolum_say, YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ysay_Rows, 2)
    ysay_ust_100bin = digit(nrow(subset(
      yBolum_say, YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ysay_Rows, 2)
    ysay_25 = digit(quantile(yBolum_say$YERKULLANILANBASARISIRASI, 0.25), 0)
    ysay_50 = digit(quantile(yBolum_say$YERKULLANILANBASARISIRASI, 0.5), 0)
    ysay_75 = digit(quantile(yBolum_say$YERKULLANILANBASARISIRASI, 0.75), 0)
    ysay_90 = digit(quantile(yBolum_say$YERKULLANILANBASARISIRASI, 0.9), 0)
    ysay_95 = digit(quantile(yBolum_say$YERKULLANILANBASARISIRASI, 0.95), 0)
    ysay_son = digit(quantile(yBolum_say$YERKULLANILANBASARISIRASI, 1), 0)
    ysay_ab = nrow(subset(yBolum_say, ANADOLUBURS == "AB"))
    return(
      c(
        ysay_ilk_bin,
        ysay_ilk_5bin,
        ysay_ilk_10bin,
        ysay_ilk_20bin,
        ysay_ilk_50bin,
        ysay_ilk_100bin,
        ysay_ilk_200bin,
        ysay_ust_50bin,
        ysay_ust_100bin,
        ysay_25,
        ysay_50,
        ysay_75,
        ysay_90,
        ysay_95,
        ysay_son,
        ysay_ab
      )
    )
  })
  soz_ist <- reactive({
    yBolum_soz = subset(yResult(), YERLESTIGIPUANTURU == "SOZ")
    ysoz_Rows = nrow(yBolum_soz)
    ysoz_ilk_bin = digit(nrow(subset(
      yBolum_soz, YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ysoz_Rows, 2)
    ysoz_ilk_5bin = digit(nrow(subset(
      yBolum_soz, YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ysoz_Rows, 2)
    ysoz_ilk_10bin = digit(nrow(subset(
      yBolum_soz, YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ysoz_Rows, 2)
    ysoz_ilk_20bin = digit(nrow(subset(
      yBolum_soz, YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ysoz_Rows, 2)
    ysoz_ilk_50bin = digit(nrow(subset(
      yBolum_soz, YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ysoz_Rows, 2)
    ysoz_ilk_100bin = digit(nrow(subset(
      yBolum_soz, YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ysoz_Rows, 2)
    ysoz_ilk_200bin = digit(nrow(subset(
      yBolum_soz, YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ysoz_Rows, 2)
    ysoz_ust_50bin = digit(nrow(subset(
      yBolum_soz, YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ysoz_Rows, 2)
    ysoz_ust_100bin = digit(nrow(subset(
      yBolum_soz, YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ysoz_Rows, 2)
    ysoz_25 = digit(quantile(yBolum_soz$YERKULLANILANBASARISIRASI, 0.25), 0)
    ysoz_50 = digit(quantile(yBolum_soz$YERKULLANILANBASARISIRASI, 0.5), 0)
    ysoz_75 = digit(quantile(yBolum_soz$YERKULLANILANBASARISIRASI, 0.75), 0)
    ysoz_90 = digit(quantile(yBolum_soz$YERKULLANILANBASARISIRASI, 0.9), 0)
    ysoz_95 = digit(quantile(yBolum_soz$YERKULLANILANBASARISIRASI, 0.95), 0)
    ysoz_son = digit(quantile(yBolum_soz$YERKULLANILANBASARISIRASI, 1), 0)
    ysoz_ab = nrow(subset(yBolum_soz, ANADOLUBURS == "AB"))
    return(
      c(
        ysoz_ilk_bin,
        ysoz_ilk_5bin,
        ysoz_ilk_10bin,
        ysoz_ilk_20bin,
        ysoz_ilk_50bin,
        ysoz_ilk_100bin,
        ysoz_ilk_200bin,
        ysoz_ust_50bin,
        ysoz_ust_100bin,
        ysoz_25,
        ysoz_50,
        ysoz_75,
        ysoz_90,
        ysoz_95,
        ysoz_son,
        ysoz_ab
      )
    )
  })
  dil_ist <- reactive({
    yBolum_dil = subset(yResult(), YERLESTIGIPUANTURU == "DIL")
    ydil_Rows = nrow(yBolum_dil)
    ydil_ilk_bin = digit(nrow(subset(
      yBolum_dil, YERKULLANILANBASARISIRASI <= 1000
    )) * 100 / ydil_Rows, 2)
    ydil_ilk_5bin = digit(nrow(subset(
      yBolum_dil, YERKULLANILANBASARISIRASI <= 5000
    )) * 100 / ydil_Rows, 2)
    ydil_ilk_10bin = digit(nrow(subset(
      yBolum_dil, YERKULLANILANBASARISIRASI <= 10000
    )) * 100 / ydil_Rows, 2)
    ydil_ilk_20bin = digit(nrow(subset(
      yBolum_dil, YERKULLANILANBASARISIRASI <= 20000
    )) * 100 / ydil_Rows, 2)
    ydil_ilk_50bin = digit(nrow(subset(
      yBolum_dil, YERKULLANILANBASARISIRASI <= 50000
    )) * 100 / ydil_Rows, 2)
    ydil_ilk_100bin = digit(nrow(subset(
      yBolum_dil, YERKULLANILANBASARISIRASI <= 100000
    )) * 100 / ydil_Rows, 2)
    ydil_ilk_200bin = digit(nrow(subset(
      yBolum_dil, YERKULLANILANBASARISIRASI <= 200000
    )) * 100 / ydil_Rows, 2)
    ydil_ust_50bin = digit(nrow(subset(
      yBolum_dil, YERKULLANILANBASARISIRASI >= 50000
    )) * 100 / ydil_Rows, 2)
    ydil_ust_100bin = digit(nrow(subset(
      yBolum_dil, YERKULLANILANBASARISIRASI >= 100000
    )) * 100 / ydil_Rows, 2)
    ydil_25 = digit(quantile(yBolum_dil$YERKULLANILANBASARISIRASI, 0.25), 0)
    ydil_50 = digit(quantile(yBolum_dil$YERKULLANILANBASARISIRASI, 0.5), 0)
    ydil_75 = digit(quantile(yBolum_dil$YERKULLANILANBASARISIRASI, 0.75), 0)
    ydil_90 = digit(quantile(yBolum_dil$YERKULLANILANBASARISIRASI, 0.9), 0)
    ydil_95 = digit(quantile(yBolum_dil$YERKULLANILANBASARISIRASI, 0.95), 0)
    ydil_son = digit(quantile(yBolum_dil$YERKULLANILANBASARISIRASI, 1), 0)
    ydil_ab = digit(nrow(subset(yBolum_dil, ANADOLUBURS == "AB")), 0)
    return(
      c(
        ydil_ilk_bin,
        ydil_ilk_5bin,
        ydil_ilk_10bin,
        ydil_ilk_20bin,
        ydil_ilk_50bin,
        ydil_ilk_100bin,
        ydil_ilk_200bin,
        ydil_ust_50bin,
        ydil_ust_100bin,
        ydil_25,
        ydil_50,
        ydil_75,
        ydil_90,
        ydil_95,
        ydil_son,
        ydil_ab
      )
    )
  })
  
  
  
  Bolum_names = c(
    "BUSAD",
    "ECON",
    "INTL",
    "CHBI",
    "COMP",
    "ELEC",
    "INDR",
    "MECH",
    "CHEM",
    "MATH",
    "MBGE",
    "PHYS",
    "ARHA",
    "ENCL",
    "HIST",
    "MAVA",
    "PHIL",
    "PSYC",
    "SOCI",
    "LAW",
    "MEDI",
    "NURS"
  )
  Fakulte_names = c("CASE", "CE", "CS", "CSSH")
  Puan_names = c("EA", "SAY", "SOZ", "DIL")
  All_ist <- reactive({
    All_ist <-
      rbind(
        KU_ist(),
        ea_ist(),
        say_ist(),
        soz_ist(),
        dil_ist(),
        case_ist(),
        ce_ist(),
        cs_ist(),
        cssh_ist(),
        busad_ist(),
        econ_ist(),
        intl_ist(),
        chbi_ist(),
        comp_ist(),
        elec_ist(),
        indr_ist(),
        mech_ist(),
        chem_ist(),
        math_ist(),
        mbge_ist(),
        phys_ist(),
        arha_ist(),
        encl_ist(),
        hist_ist(),
        mava_ist(),
        phil_ist(),
        psyc_ist(),
        soci_ist(),
        law_ist(),
        medi_ist(),
        nurs_ist()
      )
    colnames(All_ist) = c(
      "ilk bin",
      "ilk 5 bin",
      "ilk 10 bin",
      "ilk 20 bin",
      "ilk 50 bin",
      "ilk 100 bin",
      "ilk 200 bin",
      "below 50 bin",
      "below 100 bin",
      "25%",
      "median",
      "75%",
      "90%",
      "95%",
      "max",
      "AB"
    )
    rownames(All_ist) = c("KU", Puan_names, Fakulte_names, Bolum_names)
    return(All_ist)
  })
  
  output$statisticsTable <- renderTable({
    All_ist()
  }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = FALSE)
  
  
  # # # Data Visualization Start
  graphDataset <- reactive({
    df <- read.csv(
      input$file2$datapath,
      sep = ",",
      header = TRUE,
      check.names = FALSE
    )
    df <- as.data.frame(df)
    df <- df[order(df$YERKULLANILANBASARISIRASI), ]
    
    return(df)
  })
  Burs_100 <- Burs_50 <- Burs_25 <- Burs_0 <- list()
  Bolum <- Fakulte <- Puan <- list()
  bRows <- fRows <- pRows <- list()
  output$graph <- renderPlot({
    graphPlot()
  }, height = 1000, width = 1000)
  
  graphPlot <- reactive({
    helpText("Graph")
    if (is.null(input$file2)) {
      return()
    }
    
    Rows <- nrow(graphDataset())
    
    for (i in 1:22) {
      Bolum[[i]] = subset(graphDataset(), KUSISDEPT == Bolum_names[i])
      bRows[[i]] = nrow(Bolum[[i]])
    }
    
    for (i in 1:4) {
      Fakulte[[i]] = subset(graphDataset(), KUSISCOLL == Fakulte_names[i])
      fRows[[i]] = nrow(Fakulte[[i]])
    }
    
    for (i in 1:4) {
      Puan[[i]] = subset(graphDataset(), YERLESTIGIPUANTURU == Puan_names[i])
      pRows[[i]] = nrow(Puan[[i]])
    }
    
    if (input$dataType == "KU") {
      plot(
        1:Rows,
        sort(graphDataset()$YERKULLANILANBASARISIRASI),
        main = "KU",
        xlab = "nt Student",
        ylab = "RANKING"
      )
    }
    
    if (input$dataType == "Score Type") {
      for (i in 1:4) {
        if (input$scoreType == Puan_names[[i]])
          plot(
            1:pRows[[i]],
            sort(Puan[[i]]$YERKULLANILANBASARISIRASI),
            main = Puan_names[[i]],
            xlab = "nt Student",
            ylab = "RANKING"
          )
      }
    }
    if (input$dataType == "College") {
      for (i in 1:4) {
        if (input$college == Fakulte_names[[i]])
          plot(
            1:fRows[[i]],
            sort(Fakulte[[i]]$YERKULLANILANBASARISIRASI),
            main = Fakulte_names[[i]],
            xlab = "nt Student",
            ylab = "RANKING"
          )
      }
    }
    if (input$dataType == "Department") {
      for (i in 1:22) {
        if (input$department == Bolum_names[[i]]) {
          Burs_100[[i]] = Bolum[[i]][which(Bolum[[i]]$BURS == 1), 3]
          Burs_50[[i]] = Bolum[[i]][which(Bolum[[i]]$BURS == 0.5), 3]
          Burs_25[[i]] = Bolum[[i]][which(Bolum[[i]]$BURS == 0.25), 3]
          Burs_0[[i]] = Bolum[[i]][which(Bolum[[i]]$BURS == 0), 3]
          plot(
            1:length(Burs_100[[i]]),
            Burs_100[[i]],
            main = Bolum_names[[i]],
            xlim = c(0, nrow(Bolum[[i]])) ,
            ylim = c(0, max(
              Bolum[[i]]$YERKULLANILANBASARISIRASI
            )),
            xlab = "nt Student",
            ylab = "RANKING"
          )
          points(which(Bolum[[i]]$BURS == 0.5), Burs_50[[i]] , col = "red")
          points(which(Bolum[[i]]$BURS == 0.25), Burs_25[[i]], col = "blue")
          points(which(Bolum[[i]]$BURS == 0), Burs_0[[i]], col = "green")
          # points( which(Bolum[[i]]$ANADOLUBURS == "AB") , rep(-5, length(which(Bolum[[i]]$ANADOLUBURS == "AB"))), col = "yellow")
          points(which(Bolum[[i]]$ANADOLUBURS == "AB") , Bolum[[i]][which(Bolum[[i]]$ANADOLUBURS == "AB"), 3], pch = 4)
          legend(
            "topleft",
            legend = c("100%", "50%", "25%", "0%", "AB"),
            col = c("black", "red", "blue", "green"),
            pch = c(rep(1 , 4), 4),
            cex = 0.8
          )
        }
      }
    }
    
    
  })
  
  output$yealyGraphDownload <- downloadHandler(
    filename = "yearlyComparisonGraph.pdf",
    content = function(file) {
      pdf(file)
      plot(
        0,
        0,
        main = "Yearly Ranking Comparison Graph",
        xlab = "nt Student",
        ylab = "RANKING",
        xlim = c(-1, 1500),
        ylim = c(-1, 500000)
      )
      if (99 %in% input$inCheckbox && !is.null(yResult())) {
        points(1:nrow(yResult()),
               sort(yResult()$YERKULLANILANBASARISIRASI))
      }
      
      if (!is.null(input$file4)) {
        df <- read.csv(
          input$file4$datapath,
          sep = ",",
          header = TRUE,
          check.names = FALSE
        )
        
        distinctVector <- unique(df[, 1])
        
        yearGroupList <-
          list()  # placement data stored in list according to their years
        color <-
          c(
            "red",
            "blue",
            "green",
            "cyan",
            "azure",
            "deeppink",
            "darkgrey",
            "blueviolet",
            "darkorange"
          )
        
        for (i in (1:length(distinctVector))) {
          yearGroupList[[i]] = as.data.frame(df[which(df[, 1] == distinctVector[i]), ])
          if (i %in% input$inCheckbox) {
            lines(1:nrow(yearGroupList[[i]]), yearGroupList[[i]][order(as.numeric(as.character(yearGroupList[[i]][, 4]))), 4], col = color[i])
          }
        }
      }
      legend("topleft",
             legend = distinctVector,
             col = color,
             pch = 19)
      dev.off()
    }
  )
  
  output$graphDownload <- downloadHandler(
    filename = function() {
      if (input$dataType == "KU") {
        return("KU.pdf")
      }
      else if (input$dataType == "Score Type") {
        return(paste(input$scoreType, ".pdf", sep = ""))
      }
      else if (input$dataType == "College") {
        return(paste(input$college, ".pdf", sep = ""))
      }
      else if (input$dataType == "Department") {
        return(paste(input$department, ".pdf", sep = ""))
      }
    },
    content = function(file) {
      if (is.null(input$file2)) {
        return()
      }
      pdf(file)
      Rows <- nrow(graphDataset())
      
      for (i in 1:22) {
        Bolum[[i]] = subset(graphDataset(), KUSISDEPT == Bolum_names[i])
        bRows[[i]] = nrow(Bolum[[i]])
      }
      
      for (i in 1:4) {
        Fakulte[[i]] = subset(graphDataset(), KUSISCOLL == Fakulte_names[i])
        fRows[[i]] = nrow(Fakulte[[i]])
      }
      
      for (i in 1:4) {
        Puan[[i]] = subset(graphDataset(), YERLESTIGIPUANTURU == Puan_names[i])
        pRows[[i]] = nrow(Puan[[i]])
      }
      
      if (input$dataType == "KU") {
        plot(
          1:Rows,
          sort(graphDataset()$YERKULLANILANBASARISIRASI),
          main = "KU",
          xlab = "nt Student",
          ylab = "RANKING"
        )
      }
      
      if (input$dataType == "Score Type") {
        for (i in 1:4) {
          if (input$scoreType == Puan_names[[i]])
            plot(
              1:pRows[[i]],
              sort(Puan[[i]]$YERKULLANILANBASARISIRASI),
              main = Puan_names[[i]],
              xlab = "nt Student",
              ylab = "RANKING"
            )
        }
      }
      if (input$dataType == "College") {
        for (i in 1:4) {
          if (input$college == Fakulte_names[[i]])
            plot(
              1:fRows[[i]],
              sort(Fakulte[[i]]$YERKULLANILANBASARISIRASI),
              main = Fakulte_names[[i]],
              xlab = "nt Student",
              ylab = "RANKING"
            )
        }
      }
      if (input$dataType == "Department") {
        for (i in 1:22) {
          if (input$department == Bolum_names[[i]]) {
            Burs_100[[i]] = Bolum[[i]][which(Bolum[[i]]$BURS == 1), 3]
            Burs_50[[i]] = Bolum[[i]][which(Bolum[[i]]$BURS == 0.5), 3]
            Burs_25[[i]] = Bolum[[i]][which(Bolum[[i]]$BURS == 0.25), 3]
            Burs_0[[i]] = Bolum[[i]][which(Bolum[[i]]$BURS == 0), 3]
            plot(
              1:length(Burs_100[[i]]),
              Burs_100[[i]],
              main = Bolum_names[[i]],
              xlim = c(0, nrow(Bolum[[i]])) ,
              ylim = c(0, max(
                Bolum[[i]]$YERKULLANILANBASARISIRASI
              )),
              xlab = "nt Student",
              ylab = "RANKING"
            )
            points(which(Bolum[[i]]$BURS == 0.5), Burs_50[[i]] , col = "red")
            points(which(Bolum[[i]]$BURS == 0.25), Burs_25[[i]], col = "blue")
            points(which(Bolum[[i]]$BURS == 0), Burs_0[[i]], col = "green")
            # points( which(Bolum[[i]]$ANADOLUBURS == "AB") , rep(-5, length(which(Bolum[[i]]$ANADOLUBURS == "AB"))), col = "yellow")
            points(which(Bolum[[i]]$ANADOLUBURS == "AB") , Bolum[[i]][which(Bolum[[i]]$ANADOLUBURS == "AB"), 3], pch = 4)
            legend(
              "topleft",
              legend = c("100%", "50%", "25%", "0%", "AB"),
              col = c("black", "red", "blue", "green"),
              pch = c(rep(1 , 4), 4),
              cex = 0.8
            )
          }
        }
      }
      dev.off()
    }
  )
  # # # Data Visualization End
  
  # # # Yerlestirme Start
  
  yerlestirmeDataset <- reactive({
    df <- read.csv(
      input$file3$datapath,
      sep = ",",
      header = TRUE,
      check.names = FALSE
    )
    
    return(df)
  })
  
  output$yerlestirmeStatisticsDownload <- downloadHandler(
    filename = function() {
      "yerlestirme_statistic.csv"
    },
    content = function(file) {
      write.csv(yerlestirmeStatistics(), file)
    }
  )
  
  output$yerlestirmeStatistics <- renderTable({
    yerlestirmeStatistics()
  }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = FALSE)
  
  yerlestirmeStatistics <- reactive({
    req(input$file3)
    
    dataset = yerlestirmeDataset()
    
    Bolum <- Fakulte <- Puan <- list()
    bRows <-
      nBolum_ilk_100 <-
      nBolum_ilk_250 <-
      nBolum_ilk_500 <-
      nBolum_ilk_bin <-
      nBolum_ilk_5bin <-
      nBolum_ilk_10bin <-
      nBolum_ilk_20bin <-
      nBolum_ilk_50bin <-
      nBolum_ilk_100bin <-
      nBolum_ilk_200bin <-
      nBolum_ust_50bin <- nBolum_ust_100bin <- vector()
    Bolum_ilk_100 <-
      Bolum_ilk_250 <-
      Bolum_ilk_500 <-
      Bolum_ilk_bin <-
      Bolum_ilk_5bin <-
      Bolum_ilk_10bin <-
      Bolum_ilk_20bin <-
      Bolum_ilk_50bin <-
      Bolum_ilk_100bin <-
      Bolum_ilk_200bin <- Bolum_ust_50bin <- Bolum_ust_100bin <- vector()
    bq25 <-
      bmedian <- bq75 <- bq90 <- bq95 <- bmin <- bmax <- vector()
    
    fRows <-
      nFakulte_ilk_100 <-
      nFakulte_ilk_250 <-
      nFakulte_ilk_500 <-
      nFakulte_ilk_bin <-
      nFakulte_ilk_5bin <-
      nFakulte_ilk_10bin <-
      nFakulte_ilk_20bin <-
      nFakulte_ilk_50bin <-
      nFakulte_ilk_100bin <-
      nFakulte_ilk_200bin <-
      nFakulte_ust_50bin <- nFakulte_ust_100bin <- vector()
    Fakulte_ilk_100 <-
      Fakulte_ilk_250 <-
      Fakulte_ilk_500 <-
      Fakulte_ilk_bin <-
      Fakulte_ilk_5bin <-
      Fakulte_ilk_10bin <-
      Fakulte_ilk_20bin <-
      Fakulte_ilk_50bin <-
      Fakulte_ilk_100bin <-
      Fakulte_ilk_200bin <-
      Fakulte_ust_50bin <- Fakulte_ust_100bin <- vector()
    fq25 <-
      fmedian <- fq75 <- fq90 <- fq95 <- fmin <- fmax <- vector()
    
    pRows <-
      nPuan_ilk_100 <-
      nPuan_ilk_250 <-
      nPuan_ilk_500 <-
      nPuan_ilk_bin <-
      nPuan_ilk_5bin <-
      nPuan_ilk_10bin <-
      nPuan_ilk_20bin <-
      nPuan_ilk_50bin <-
      nPuan_ilk_100bin <-
      nPuan_ilk_200bin <- nPuan_ust_50bin <- nPuan_ust_100bin <- vector()
    Puan_ilk_100 <-
      Puan_ilk_250 <-
      Puan_ilk_500 <-
      Puan_ilk_bin <-
      Puan_ilk_5bin <-
      Puan_ilk_10bin <-
      Puan_ilk_20bin <-
      Puan_ilk_50bin <-
      Puan_ilk_100bin <-
      Puan_ilk_200bin <- Puan_ust_50bin <- Puan_ust_100bin <- vector()
    pq25 <-
      pmedian <- pq75 <- pq90 <- pq95 <- pmin <- pmax <- vector()
    
    
    # Her bolum icin data tanimlanmasi for loop'un icinde:
    for (i in 1:22) {
      Bolum[[i]] = subset(dataset, KUSISDEPT == Bolum_names[i])
      bRows[[i]] = nrow(Bolum[[i]])
      nBolum_ilk_100[[i]] = nrow(subset(Bolum[[i]], YERKULLANILANBASARISIRASI <= 100))
      nBolum_ilk_250[[i]] = nrow(subset(Bolum[[i]], YERKULLANILANBASARISIRASI <= 250))
      nBolum_ilk_500[[i]] = nrow(subset(Bolum[[i]], YERKULLANILANBASARISIRASI <= 500))
      nBolum_ilk_bin[[i]] = nrow(subset(Bolum[[i]], YERKULLANILANBASARISIRASI <= 1000))
      nBolum_ilk_5bin[[i]] = nrow(subset(Bolum[[i]], YERKULLANILANBASARISIRASI <= 5000))
      nBolum_ilk_10bin[[i]] = nrow(subset(Bolum[[i]], YERKULLANILANBASARISIRASI <= 10000))
      nBolum_ilk_20bin[[i]] = nrow(subset(Bolum[[i]], YERKULLANILANBASARISIRASI <= 20000))
      nBolum_ilk_50bin[[i]] = nrow(subset(Bolum[[i]], YERKULLANILANBASARISIRASI <= 50000))
      nBolum_ilk_100bin[[i]] = nrow(subset(Bolum[[i]], YERKULLANILANBASARISIRASI <= 100000))
      nBolum_ilk_200bin[[i]] = nrow(subset(Bolum[[i]], YERKULLANILANBASARISIRASI <= 200000))
      nBolum_ust_50bin[[i]] = nrow(subset(Bolum[[i]], YERKULLANILANBASARISIRASI >= 50000))
      nBolum_ust_100bin[[i]] = nrow(subset(Bolum[[i]], YERKULLANILANBASARISIRASI >= 100000))
      Bolum_ilk_100[[i]] = nBolum_ilk_100[[i]] / bRows[[i]]
      Bolum_ilk_250[[i]] = nBolum_ilk_250[[i]] / bRows[[i]]
      Bolum_ilk_500[[i]] = nBolum_ilk_500[[i]] / bRows[[i]]
      Bolum_ilk_bin[[i]] = nBolum_ilk_bin[[i]] / bRows[[i]]
      Bolum_ilk_5bin[[i]] = nBolum_ilk_5bin[[i]] / bRows[[i]]
      Bolum_ilk_10bin[[i]] = nBolum_ilk_10bin[[i]] / bRows[[i]]
      Bolum_ilk_20bin[[i]] = nBolum_ilk_20bin[[i]] / bRows[[i]]
      Bolum_ilk_50bin[[i]] = nBolum_ilk_50bin[[i]] / bRows[[i]]
      Bolum_ilk_100bin[[i]] = nBolum_ilk_100bin[[i]] / bRows[[i]]
      Bolum_ilk_200bin[[i]] = nBolum_ilk_200bin[[i]] / bRows[[i]]
      Bolum_ust_50bin[[i]] = nBolum_ust_50bin[[i]] / bRows[[i]]
      Bolum_ust_100bin[[i]] = nBolum_ust_100bin[[i]] / bRows[[i]]
      bq25[[i]]    = quantile(Bolum[[i]]$YERKULLANILANBASARISIRASI, 0.25)
      bmedian[[i]] = quantile(Bolum[[i]]$YERKULLANILANBASARISIRASI, 0.5)
      bq75[[i]]    = quantile(Bolum[[i]]$YERKULLANILANBASARISIRASI, 0.75)
      bq90[[i]]    = quantile(Bolum[[i]]$YERKULLANILANBASARISIRASI, 0.90)
      bq95[[i]]    = quantile(Bolum[[i]]$YERKULLANILANBASARISIRASI, 0.95)
      bmin[[i]]    = min(Bolum[[i]]$YERKULLANILANBASARISIRASI)
      bmax[[i]]    = max(Bolum[[i]]$YERKULLANILANBASARISIRASI)
    }
    
    for (i in 1:4) {
      Fakulte[[i]] = subset(dataset, KUSISCOLL == Fakulte_names[i])
      fRows[[i]] = nrow(Fakulte[[i]])
      nFakulte_ilk_100[[i]] = nrow(subset(Fakulte[[i]], YERKULLANILANBASARISIRASI <= 100))
      nFakulte_ilk_250[[i]] = nrow(subset(Fakulte[[i]], YERKULLANILANBASARISIRASI <= 250))
      nFakulte_ilk_500[[i]] = nrow(subset(Fakulte[[i]], YERKULLANILANBASARISIRASI <= 500))
      nFakulte_ilk_bin[[i]] = nrow(subset(Fakulte[[i]], YERKULLANILANBASARISIRASI <= 1000))
      nFakulte_ilk_5bin[[i]] = nrow(subset(Fakulte[[i]], YERKULLANILANBASARISIRASI <= 5000))
      nFakulte_ilk_10bin[[i]] = nrow(subset(Fakulte[[i]], YERKULLANILANBASARISIRASI <= 10000))
      nFakulte_ilk_20bin[[i]] = nrow(subset(Fakulte[[i]], YERKULLANILANBASARISIRASI <= 20000))
      nFakulte_ilk_50bin[[i]] = nrow(subset(Fakulte[[i]], YERKULLANILANBASARISIRASI <= 50000))
      nFakulte_ilk_100bin[[i]] = nrow(subset(Fakulte[[i]], YERKULLANILANBASARISIRASI <= 100000))
      nFakulte_ilk_200bin[[i]] = nrow(subset(Fakulte[[i]], YERKULLANILANBASARISIRASI <= 200000))
      nFakulte_ust_50bin[[i]] = nrow(subset(Fakulte[[i]], YERKULLANILANBASARISIRASI >= 50000))
      nFakulte_ust_100bin[[i]] = nrow(subset(Fakulte[[i]], YERKULLANILANBASARISIRASI >= 100000))
      Fakulte_ilk_100[[i]] = nFakulte_ilk_100[[i]] / fRows[[i]]
      Fakulte_ilk_250[[i]] = nFakulte_ilk_250[[i]] / fRows[[i]]
      Fakulte_ilk_500[[i]] = nFakulte_ilk_500[[i]] / fRows[[i]]
      Fakulte_ilk_bin[[i]] = nFakulte_ilk_bin[[i]] / fRows[[i]]
      Fakulte_ilk_5bin[[i]] = nFakulte_ilk_5bin[[i]] / fRows[[i]]
      Fakulte_ilk_10bin[[i]] = nFakulte_ilk_10bin[[i]] / fRows[[i]]
      Fakulte_ilk_20bin[[i]] = nFakulte_ilk_20bin[[i]] / fRows[[i]]
      Fakulte_ilk_50bin[[i]] = nFakulte_ilk_50bin[[i]] / fRows[[i]]
      Fakulte_ilk_100bin[[i]] = nFakulte_ilk_100bin[[i]] / fRows[[i]]
      Fakulte_ilk_200bin[[i]] = nFakulte_ilk_200bin[[i]] / fRows[[i]]
      Fakulte_ust_50bin[[i]] = nFakulte_ust_50bin[[i]] / fRows[[i]]
      Fakulte_ust_100bin[[i]] = nFakulte_ust_100bin[[i]] / fRows[[i]]
      fq25[[i]]    = quantile(Fakulte[[i]]$YERKULLANILANBASARISIRASI, 0.25)
      fmedian[[i]] = quantile(Fakulte[[i]]$YERKULLANILANBASARISIRASI, 0.5)
      fq75[[i]]    = quantile(Fakulte[[i]]$YERKULLANILANBASARISIRASI, 0.75)
      fq90[[i]]    = quantile(Fakulte[[i]]$YERKULLANILANBASARISIRASI, 0.90)
      fq95[[i]]    = quantile(Fakulte[[i]]$YERKULLANILANBASARISIRASI, 0.95)
      fmin[[i]]    = min(Fakulte[[i]]$YERKULLANILANBASARISIRASI)
      fmax[[i]]    = max(Fakulte[[i]]$YERKULLANILANBASARISIRASI)
    }
    
    # Puan turleri icin dagilim hesaplari
    for (i in 1:4) {
      Puan[[i]] = subset(dataset, YERLESTIGIPUANTURU == Puan_names[i])
      pRows[[i]] = nrow(Puan[[i]])
      nPuan_ilk_100[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 100))
      nPuan_ilk_250[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 250))
      nPuan_ilk_500[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 500))
      nPuan_ilk_bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 1000))
      nPuan_ilk_5bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 5000))
      nPuan_ilk_10bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 10000))
      nPuan_ilk_20bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 20000))
      nPuan_ilk_50bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 50000))
      nPuan_ilk_100bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 100000))
      nPuan_ilk_200bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 200000))
      nPuan_ust_50bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI >= 50000))
      nPuan_ust_100bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI >= 100000))
      Puan_ilk_100[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 100)) / pRows[[i]]
      Puan_ilk_250[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 250)) / pRows[[i]]
      Puan_ilk_500[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 500)) / pRows[[i]]
      Puan_ilk_bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 1000)) / pRows[[i]]
      Puan_ilk_5bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 5000)) / pRows[[i]]
      Puan_ilk_10bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 10000)) / pRows[[i]]
      Puan_ilk_20bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 20000)) / pRows[[i]]
      Puan_ilk_50bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 50000)) / pRows[[i]]
      Puan_ilk_100bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 100000)) / pRows[[i]]
      Puan_ilk_200bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI <= 200000)) / pRows[[i]]
      Puan_ust_50bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI >= 50000)) / pRows[[i]]
      Puan_ust_100bin[[i]] = nrow(subset(Puan[[i]], YERKULLANILANBASARISIRASI >= 100000)) / pRows[[i]]
      pq25[[i]]    = quantile(Puan[[i]]$YERKULLANILANBASARISIRASI, 0.25)
      pmedian[[i]] = quantile(Puan[[i]]$YERKULLANILANBASARISIRASI, 0.5)
      pq75[[i]] = quantile(Puan[[i]]$YERKULLANILANBASARISIRASI, 0.75)
      pq90[[i]] = quantile(Puan[[i]]$YERKULLANILANBASARISIRASI, 0.9)
      pq95[[i]] = quantile(Puan[[i]]$YERKULLANILANBASARISIRASI, 0.95)
      pmin[[i]] = min(Puan[[i]]$YERKULLANILANBASARISIRASI)
      pmax[[i]] = max(Puan[[i]]$YERKULLANILANBASARISIRASI)
    }
    
    Rows = nrow(dataset)
    nKU_ilk_100 = nrow(subset(dataset, YERKULLANILANBASARISIRASI <= 100))
    nKU_ilk_250 = nrow(subset(dataset, YERKULLANILANBASARISIRASI <= 250))
    nKU_ilk_500 = nrow(subset(dataset, YERKULLANILANBASARISIRASI <= 500))
    nKU_ilk_bin = nrow(subset(dataset, YERKULLANILANBASARISIRASI <= 1000))
    nKU_ilk_5bin = nrow(subset(dataset, YERKULLANILANBASARISIRASI <= 5000))
    nKU_ilk_10bin = nrow(subset(dataset, YERKULLANILANBASARISIRASI <= 10000))
    nKU_ilk_20bin = nrow(subset(dataset, YERKULLANILANBASARISIRASI <= 20000))
    nKU_ilk_50bin = nrow(subset(dataset, YERKULLANILANBASARISIRASI <= 50000))
    nKU_ilk_100bin = nrow(subset(dataset, YERKULLANILANBASARISIRASI <= 100000))
    nKU_ilk_200bin = nrow(subset(dataset, YERKULLANILANBASARISIRASI <= 200000))
    nKU_ust_50bin = nrow(subset(dataset, YERKULLANILANBASARISIRASI >= 50000))
    nKU_ust_100bin = nrow(subset(dataset, YERKULLANILANBASARISIRASI >= 100000))
    KU_ilk_100 = nKU_ilk_100 / Rows
    KU_ilk_250 = nKU_ilk_250 / Rows
    KU_ilk_500 = nKU_ilk_500 / Rows
    KU_ilk_bin = nKU_ilk_bin / Rows
    KU_ilk_5bin = nKU_ilk_5bin / Rows
    KU_ilk_10bin = nKU_ilk_10bin / Rows
    KU_ilk_20bin = nKU_ilk_20bin / Rows
    KU_ilk_50bin = nKU_ilk_50bin  / Rows
    KU_ilk_100bin = nKU_ilk_100bin / Rows
    KU_ilk_200bin = nKU_ilk_200bin / Rows
    KU_ust_50bin = nKU_ust_50bin  / Rows
    KU_ust_100bin = nKU_ust_100bin / Rows
    q25    = quantile(dataset$YERKULLANILANBASARISIRASI, 0.25)
    median = quantile(dataset$YERKULLANILANBASARISIRASI, 0.5)
    q75    = quantile(dataset$YERKULLANILANBASARISIRASI, 0.75)
    q90    = quantile(dataset$YERKULLANILANBASARISIRASI, 0.90)
    q95    = quantile(dataset$YERKULLANILANBASARISIRASI, 0.95)
    min    = min(dataset$YERKULLANILANBASARISIRASI)
    max    = max(dataset$YERKULLANILANBASARISIRASI)
    
    KU_ist = c(
      nKU_ilk_100,
      nKU_ilk_250,
      nKU_ilk_500,
      nKU_ilk_bin,
      nKU_ilk_5bin,
      nKU_ilk_10bin,
      nKU_ilk_20bin,
      nKU_ilk_50bin,
      nKU_ilk_100bin,
      nKU_ilk_200bin,
      nKU_ust_50bin,
      nKU_ust_100bin,
      KU_ilk_100,
      KU_ilk_250,
      KU_ilk_500,
      KU_ilk_bin,
      KU_ilk_5bin,
      KU_ilk_10bin,
      KU_ilk_20bin,
      KU_ilk_50bin,
      KU_ilk_100bin,
      KU_ilk_200bin,
      KU_ust_50bin,
      KU_ust_100bin,
      q25,
      median,
      q75,
      q90,
      q95,
      min,
      max
    )
    Fakulte_ist = cbind(
      nFakulte_ilk_100,
      nFakulte_ilk_250,
      nFakulte_ilk_500,
      nFakulte_ilk_bin,
      nFakulte_ilk_5bin,
      nFakulte_ilk_10bin,
      nFakulte_ilk_20bin,
      nFakulte_ilk_50bin,
      nFakulte_ilk_100bin,
      nFakulte_ilk_200bin,
      nFakulte_ust_50bin,
      nFakulte_ust_100bin,
      Fakulte_ilk_100,
      Fakulte_ilk_250,
      Fakulte_ilk_500,
      Fakulte_ilk_bin,
      Fakulte_ilk_5bin,
      Fakulte_ilk_10bin,
      Fakulte_ilk_20bin,
      Fakulte_ilk_50bin,
      Fakulte_ilk_100bin,
      Fakulte_ilk_200bin,
      Fakulte_ust_50bin,
      Fakulte_ust_100bin,
      fq25,
      fmedian,
      fq75,
      fq90,
      fq95,
      fmin,
      fmax
    )
    Bolum_ist   = cbind(
      nBolum_ilk_100,
      nBolum_ilk_250,
      nBolum_ilk_500,
      nBolum_ilk_bin,
      nBolum_ilk_5bin,
      nBolum_ilk_10bin,
      nBolum_ilk_20bin,
      nBolum_ilk_50bin,
      nBolum_ilk_100bin,
      nBolum_ilk_200bin,
      nBolum_ust_50bin,
      nBolum_ust_100bin,
      Bolum_ilk_100,
      Bolum_ilk_250,
      Bolum_ilk_500,
      Bolum_ilk_bin,
      Bolum_ilk_5bin,
      Bolum_ilk_10bin,
      Bolum_ilk_20bin,
      Bolum_ilk_50bin,
      Bolum_ilk_100bin,
      Bolum_ilk_200bin,
      Bolum_ust_50bin,
      Bolum_ust_100bin,
      bq25,
      bmedian,
      bq75,
      bq90,
      bq95,
      bmin,
      bmax
    )
    Puan_ist    = cbind(
      nPuan_ilk_100,
      nPuan_ilk_250,
      nPuan_ilk_500,
      nPuan_ilk_bin,
      nPuan_ilk_5bin,
      nPuan_ilk_10bin,
      nPuan_ilk_20bin,
      nPuan_ilk_50bin,
      nPuan_ilk_100bin,
      nPuan_ilk_200bin,
      nPuan_ust_50bin,
      nPuan_ust_100bin,
      Puan_ilk_100,
      Puan_ilk_250,
      Puan_ilk_500,
      Puan_ilk_bin,
      Puan_ilk_5bin,
      Puan_ilk_10bin,
      Puan_ilk_20bin,
      Puan_ilk_50bin,
      Puan_ilk_100bin,
      Puan_ilk_200bin,
      Puan_ust_50bin,
      Puan_ust_100bin,
      pq25,
      pmedian,
      pq75,
      pq90,
      pq95,
      pmin,
      pmax
    )
    All_ist = rbind(KU_ist, Puan_ist, Fakulte_ist, Bolum_ist)
    
    colnames(All_ist) = c(
      "ilk 100",
      "ilk 250",
      "ilk 500",
      "ilk bin",
      "ilk 5 bin",
      "ilk 10 bin",
      "ilk 20 bin",
      "ilk 50 bin",
      "ilk 100 bin",
      "ilk 200 bin",
      "ust 50 bin",
      "ust 100 bin",
      "% ilk 100",
      "% ilk 250",
      "% ilk 500",
      "% ilk bin",
      "% ilk 5 bin",
      "% ilk 10 bin",
      "% ilk 20 bin",
      "% ilk 50 bin",
      "% ilk 100 bin",
      "% ilk 200 bin",
      "% ust 50 bin",
      "% ust 100 bin",
      "25%",
      "median",
      "75%",
      "90%",
      "95%",
      "min",
      "max"
    )
    rownames(All_ist) = c("KU", Puan_names, Fakulte_names, Bolum_names)
    
    return(All_ist)
  })
  
} # SERVER CLOSE

shinyApp(ui = ui, server = server)
