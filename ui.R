shinyUI(pageWithSidebar(
    headerPanel("Shift between Races by County, 2020 Election"),
    sidebarPanel(
        width = 2,
        splitLayout(
            actionButton("submit", "Submit",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            textInput("district", "District", value = "")
        ),
        splitLayout(
            textInput("fpop", "Min pop(k)", value = "0"),
            textInput("fcounty", "County", value = "")
        ),
        selectInput("state2", "States abbr",
                    choices = c("AZ","CA","FL","GA","IA","KY","ME","MI","MT","NC",
                                "PA","SC","TX"),
                    selected = c("AZ"),
                    multiple = FALSE),
        selectInput("measure", "Measure",
                    choices = c("Shift","Percent change","Percent ratio"),
                    selected = "Shift",
                    multiple = FALSE),
        selectInput("party", "Party",
                    choices = c("Democrat","Republican","Margin","Total"),
                    selected = "Margin",
                    multiple = FALSE),
        selectInput("units", "Units",
                    choices = c("Count","Percent"),
                    selected = "Percent",
                    multiple = FALSE),
        selectInput("racex", "Races, x & y axes",
                    choices = c("President","Senate","House","Governor","Registered"),
                    selected = "Senate",
                    multiple = FALSE),
        selectInput("racey", NULL,
                    choices = c("President","Senate","House","Governor","Registered"),
                    selected = "President",
                    multiple = FALSE),
        splitLayout(
            numericInput("yearx", "Year, x-axis", 2020),
            numericInput("yeary", "Year, y-axis", 2020)
        ),
        selectInput("distype", "District type",
                    choices = c("2-party","1 & 2-party","Non-zero","All"),
                    selected = "2-party",
                    multiple = FALSE),
        checkboxInput("showdist","Show district",value = FALSE),
        checkboxInput("dronly","Dem+Rep Only",value = FALSE),
        checkboxInput("createfiles","Create Data Files",value = FALSE)
    ),
    mainPanel(
        tabsetPanel(id = "tabs",
            tabPanel("Output",
                mainPanel(
                    width = 9,
                    verbatimTextOutput("myText")
                )
            ),
            tabPanel("Plot",
                sidebarPanel(
                    width = 3,
                    checkboxInput("showrow","Show row",value = FALSE),
                    textInput("pos1", "Position above", value = ""),
                    textInput("pos3", "Position below", value = ""),
                    textInput("xscale", "X From,To,Step,Tick", value = ""),
                    textInput("yscale", "Y From,To,Step,Tick", value = ""),
                    textInput("xlimit","Limit",value = "-9,-3,3,9"),
                    textInput("xcolor","Color",value = "red3,orange,green3,violet,blue3"),
                    textInput("xparty","Party",value = "1_Solid R,2_Leans R,3_Toss-Up,4_Leans D,5_Solid D"),
                    selectInput("noparty", "No-party",
                                choices = c("Count as Dem","Split 50/50","Split by Ratio","Count as Rep"),
                                selected = "Split by Ratio",
                                multiple = FALSE),
                    textInput("vlimit","Vote Limit (1000s)",value = "1,10,100,1000"),
                    textInput("vshape","Vote Shape",value = "1,10,16,17,15"),
                    textInput("vdesc","Vote Desc",value = "< 1k,>=    1k,>=   10k,>=  100k,>= 1000k"),
                    splitLayout(
                        numericInput("plotload", "Load", 1),
                        actionButton("plotsave", "Save")
                    )
                ),
                mainPanel(
                    width = 9,
                    plotOutput("myPlot")
                )
            ),
            tabPanel(
                "Map",
                sidebarPanel(
                    width = 3,
                    splitLayout(
                        numericInput("minpop", "Min Pop", value = 200000, step = 1000),
                        numericInput("longoff", "Long Offset", value = 0.5)
                    ),
                    textInput("skipcity", "Cities to Skip", value = "Oakland,Long Beach"),
                    textInput("showcity", "Cities to Show", value = "Santa Barbara,Santa Rosa,San Rafael,Santa Cruz"),
                    selectInput("maplimitset", "Map Limits",
                                choices = c("Use value(s) below",
                                            "Auto set to min,max",
                                            "Auto set balanced"),
                                selected = "Auto set balanced",
                                multiple = FALSE),
                    textInput("maplimits", NULL, value = "-100,100"),
                    selectInput("mapvar", "Map Variable",
                                choices = c("DEM1","REP1","MARGIN1","TOTAL1",
                                            "DEM2","REP2","MARGIN2","TOTAL2",
                                            "DEM_SH","REP_SH","MAR_SH","TOT_SH",
                                            "DEM1_N","REP1_N","MAR1_N","TOT1_N"),
                                selected = "MAR_SH",
                                multiple = FALSE),
                    numericInput("mapyear", "Map Year", 2020),
                    textInput("mapcolors", "Map Colors", value = "red,orange,yellow,green,green3"),
                    splitLayout(
                        numericInput("mapload", "Load", 1),
                        actionButton("mapsave", "Save")
                    )
                ),
                mainPanel(
                    width = 9,
                    imageOutput("myggMap")
                )
            ),
            tabPanel("Data",
                sidebarPanel(
                    width = 3,
                    splitLayout(
                        numericInput("year_first", "First Year", 2020),
                        numericInput("year_last", "Last Year", 2016)
                    ),
                    numericInput("year_step", "Step Year", -2)
                ),
                mainPanel(
                    width = 9,
                    verbatimTextOutput("myVoteData")
                )
            ),
            tabPanel("Usage",
                     htmlOutput(outputId = "myUsage")
            )
        ),
        mainPanel(
            
        )
    )
))