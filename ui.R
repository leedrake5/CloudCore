library(DT)
library(dplyr)
library(shinythemes)
library(shiny)
library(rhandsontable)
library(shinyWidgets)



shinyUI(navbarPage("Core Analysis", id="nav", theme = shinytheme("united"),
tabPanel("Spectrum",
div(class="outer",
headerPanel("X-Ray Fluorescence Spectrum Viewer"),
sidebarLayout(
sidebarPanel(



actionButton("actionprocess", label = "Process Data"),
actionButton("actionprocessdepth", label = "Enter Depths"),
actionButton("actionplot", label = "Plot Spectrum"),
downloadButton('downloadPlot', "Plot"),


tags$hr(),

fileInput('file1', 'Light Element Spectra', multiple=TRUE,
accept=c('.csv', '.zip')),

fileInput('file2', 'Trace Element Spectra', multiple=TRUE,
accept=c('.csv', '.zip')),

#checkboxInput('useall', "Match Light and Trace scans", value=TRUE),

selectInput("filetype", label=NULL, c("Spectra", "Net", "Artax Excel", "Spreadsheet"), selected="Spectra"),

numericInput("aggregationfactor", label="Aggregation", value=5, min=1, max=100),

tags$hr(),


textInput('projectname', label="Project Name", value=""),

tags$hr(),


element <- selectInput(
"element", "Element:",
c("(Ne) Neon" = "Ne.table",
"(Na) Sodium" = "Na.table",
"(Mg) Magnesium" = "Mg.table",
"(Al) Aluminum" = "Al.table",
"(Si) Silicon" = "Si.table",
"(P)  Phosphorous" = "P.table",
"(S)  Sulfur" = "S.table",
"(Cl) Chlorine" = "Cl.table",
"(Ar) Argon" = "Ar.table",
"(K)  Potassium" = "K.table",
"(Ca) Calcium" = "Ca.table",
"(Sc) Scandium" = "Sc.table",
"(Ti) Titanium" = "Ti.table",
"(V)  Vanadium" = "V.table",
"(Cr) Chromium" = "Cr.table",
"(Mn) Manganese" = "Mn.table",
"(Fe) Iron" = "Fe.table",
"(Co) Cobalt" = "Co.table",
"(Ni) Nickel" = "Ni.table",
"(Cu) Copper" = "Cu.table",
"(Zn) Zinc"= "Zn.table",
"(Ga) Gallium" = "Ga.table",
"(Ge) Germanium" = "Ge.table",
"(As) Arsenic" = "As.table",
"(Se) Selenium" = "Se.table",
"(Br) Bromium" = "Br.table",
"(Kr) Krypton" = "Kr.table",
"(Rb) Rubidium" = "Rb.table",
"(Sr) Strontium" = "Sr.table",
"(Y)  Yttrium" = "Y.table",
"(Zr) Zirconium" = "Zr.table",
"(Nb) Niobium" = "Nb.table",
"(Mo) Molybdenum" = "Mo.table",
"(Tc) Technicium" = "Tc.table",
"(Ru) Ruthenium" = "Ru.table",
"(Rh) Rhodium" = "Rh.table",
"(Pd) Paladium" = "Pd.table",
"(Ag) Silver" = "Ag.table",
"(Cd) Cadmium" = "Cd.table",
"(In) Indium" = "In.table",
"(Sn) Tin" = "Sn.table",
"(Sb) Antimony" = "Sb.table",
"(Te) Tellerium" = "Te.table",
"(I) Iodine" = "I.table",
"(Xe) Xenon" = "Xe.table",
"(Cs) Cesium" = "Cs.table",
"(Bs) Barium" = "Ba.table",
"(Ce) Cerium" = "Ce.table",
"(Pr) Praeseodymeum" = "Pr.table",
"(Nd) Neodymeum" = "Nd.table",
"(Pr) Promethium" = "Pr.table",
"(Sm) Samarium" = "Sm.table",
"(Eu) Europium" = "Eu.table",
"(Gd) Gadolinium" = "Gd.table",
"(Tb) Terbium" = "Tb.table",
"(Dy) Dysprosium" = "Dy.table",
"(Ho) Holmium" = "Ho.table",
"(Er) Erbium" = "Er.table",
"(Tm) Thullium" = "Tm.table",
"(Yb) Ytterbium" = "Yb.table",
"(Lu) Lutetium" = "Lu.table",
"(Hf) Halfnium" = "Hf.table",
"(Ta) Tantalum" = "Ta.table",
"(W)  Tungsten" = "W.table",
"(Re) Rhenium" = "Re.table",
"(Os) Osmium" = "Os.table",
"(Ir) Irridium" = "Ir.table",
"(Pt) Platinum" = "Pt.table",
"(Au) Gold" = "Au.table",
"(Hg) Mercury" = "Hg.table",
"(Tl) Thallium" = "Tl.table",
"(Pb) Lead" = "Pb.table",
"(Bi) Bismuth" = "Bi.table",
"(Po) Polonium" = "Po.table",
"(At) Astatine" = "At.table",
"(Rn) Radon" = "Rn.table",
"(Fr) Francium" = "Fr.table",
"(Ra) Radium" = "Ra.table",
"(Ac) Actinum" = "Ac.table",
"(Th) Thorium" = "Th.table",
"(Pa) Proactinum" = "Pa.table",
"(U)  Uranium" = "U.table"),
selected="Fe.table"),


#checkboxInput('backgroundsubtract', "Background Subtract"),




tags$hr(),
tags$hr(),
tags$hr(),


fileInput('calfileinput1', 'Load Light Element Cal File', accept='.quant', multiple=FALSE),
fileInput('calfileinput2', 'Load Trace Element Cal File', accept='.quant', multiple=FALSE),

checkboxInput('usecalfile', "Use Cal File"),
downloadButton('downloadFullData', "Full Table"),
downloadButton('downloadlight', "light"),
downloadButton('downloadtrace', "trace")




),



mainPanel(
tabsetPanel(
tabPanel('Light Depth',
rHandsontableOutput('depthtablelight')),
tabPanel('Trace Depth',
rHandsontableOutput('depthtabletrace')),
tabPanel('Spectrum',
column(width = 11, class = "well",
plotOutput("distPlot", width = 400, height = 455,
dblclick = "plot1_dblclick",
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
))))



))
))
),


tabPanel("Age Model",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(

actionButton('hotableprocess2', "Enter Dates"),
actionButton('hotableprocess3', "Run Age Model"),
downloadButton('ageresults', "Age Results Table"),

tags$hr(),
checkboxInput("ageon", label="Age Model On", FALSE),
checkboxInput("constrainage", label="Constrain Chronology", FALSE),
fileInput('agemodelfile', 'Load Age Model', accept='.txt', multiple=FALSE),


selectInput('curvetype', "Choose Calibration", choices=c("intcal13", "marine13", "shcal13", "normal"), selected="intcal13")



),

mainPanel(
tabsetPanel(
tabPanel('Age Input', rHandsontableOutput('hotage')),
tabPanel('Age Curve', plotOutput('agemodcurve'), height = 1200),
tabPanel('Age Table', tableOutput('allagemodel'))
))


))



)),



tabPanel("Counts",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(




actionButton('hotableprocess', "Enter Values"),
downloadButton('downloadData', "Table"),
downloadButton('thanksforallthefishtable', "MCL"),

tags$hr(),

checkboxInput('zeroout', "Eliminate Negative Values", value=TRUE),

conditionalPanel(
condition='input.dataset === myData()',
checkboxInput('clusterlearn', "Machine Learn Cluster", value=FALSE),
uiOutput('nvariablesui'),
uiOutput('defaultlines')
)),




mainPanel(
tabsetPanel(
tabPanel('Spectral Lines', dataTableOutput('mytable1')),
tabPanel('Add Categories', rHandsontableOutput('hot')),
tabPanel("Machine Determined Clusers", DT::dataTableOutput('thanksforallthefish'))
))
)

)



)),

















tabPanel("PCA",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(

uiOutput('knumui'),

selectInput("pcacolour", "Colour", choices=c(
"Black"="Black",
"Cluster"="Cluster",
"Climate"="Climate",
"Qualitative"="Qualitative",
"Quantitative"="Quantitative"),
selected="Cluster"),

sliderInput("spotsize", label = "Point Size", value=2, min=2, max=15),

checkboxInput('elipseplot1', "Elipse"),

tags$hr(),


downloadButton('downloadPlot2', "Plot"),
downloadButton('downloadPcaTable', "Results"),
downloadButton('wsstable', "WSS")

),



mainPanel(
tabsetPanel(
tabPanel('PCA Plot',

# this is an extra div used ONLY to create positioned ancestor for tooltip
# we don't change its position
div(
style = "position:relative",
plotOutput("xrfpcaplot", height = 700,
hover = hoverOpts("plot_hoverpca", delay = 100, delayType = "debounce"),
dblclick = "plot_pca_dblclick",
brush = brushOpts(id = "plot_pca_brush", resetOnNew = TRUE)),
uiOutput("hover_infopca")
)


),

tabPanel("Optimal Clusters",
div(
style = "position:relative",
plotOutput('optimalkplot',
hover = hoverOpts("plot_hoveroptimalk", delay = 100, delayType = "debounce")),
uiOutput("hover_infooptimalk"))
),

tabPanel("Table", DT::dataTableOutput('xrfpcatable'))


))

))

)),

tabPanel("Timeseries",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(

p("Create Plot"),
actionButton('timeseriesact1', "1"),
actionButton('timeseriesact2', "2"),
actionButton('timeseriesact3', "3"),
actionButton('timeseriesact4', "4"),
actionButton('timeseriesact5', "5"),

tags$hr(),

downloadButton('downloadPlot3a', "1"),
downloadButton('downloadPlot3b', "2"),
downloadButton('downloadPlot3c', "3"),
downloadButton('downloadPlot3d', "4"),
downloadButton('downloadPlot3e', "5"),


tags$hr(),

uiOutput('inelementtrend'),
uiOutput('inelementnorm'),

selectInput(
'timecolour', "Time Series Type",
c(
"Black" = "Black",
"Smooth" = "Smooth",
"Ramp" = "Selected",
"Cluster" = "Cluster",
"Climate" = "Climate",
"Qualitative Point" = "QualitativePoint",
"Qualitative Line" = "QualitativeLine",
"Depth" = "Depth",
"Area" = "Area")
),


tags$hr(),

dropdownButton(
tags$h3("Line Options"), icon = icon("gear"),
sliderInput('smoothing', label = "Smoothed Mean Average", value=1, min=1, max=50),
sliderInput('linesize', label = "Line Size", value=1, min=1, max=15),
sliderInput('pointsize', label = "Point Size", value=5, min=1, max=15),
tooltip = tooltipOptions(title = "Click for line options")
),


dropdownButton(
tags$h3("X Axis Customization"), icon = icon("gear"),
#uiOutput('inxlimrange'),
selectInput('xaxistype', label="X Axis", c("Depth", "Age", "Custom"), selected="Depth"),
textInput('customxaxis', label="Custom X Axis"),
checkboxInput('flipx', label="Flip X Axis", value=FALSE),
radioButtons('lengthunit', label=NULL, c("mm", "cm", "m", "inches", "feet"), selected="mm"),
numericInput('startmm', label = "Start Point (mm)", value=0),
radioButtons('timetype', label=NULL, c("AD", "BC", "BC/AD", "BP"), selected="BP"),
tooltip = tooltipOptions(title = "Click for X-axis options")
),

dropdownButton(
tags$h3("Y Axis Customization"), icon = icon("gear"),
#uiOutput('inxlimrange'),
checkboxInput('usecustumyaxis', label="Use Custom Y Axis", value=FALSE),
textInput('customyaxis', label="Custom Y Axis"),
numericInput('ymultiply', label="Y Axis Unit Shift", min=.000001, max=1000000, value=1),
tooltip = tooltipOptions(title = "Click for Y-axis options")
)


),

mainPanel(
tabsetPanel(
tabPanel('Time Series 1',
div(
style = "position:relative",
plotOutput('timeseriesplot1',
hover = hoverOpts("plot_hover3a", delay = 100, delayType = "debounce"),
dblclick = "plot_3a_dblclick",
brush = brushOpts(id = "plot_3a_brush", resetOnNew = TRUE),
height = 700),
uiOutput("hover_info3a")
)
),

tabPanel('Time Series 2',
div(
style = "position:relative",
plotOutput('timeseriesplot2',
hover = hoverOpts("plot_hover3b", delay = 100, delayType = "debounce"),
dblclick = "plot_3b_dblclick",
brush = brushOpts(id = "plot_3b_brush", resetOnNew = TRUE),
height = 700),
uiOutput("hover_info3b")
)
),

tabPanel('Time Series 3',
div(
style = "position:relative",
plotOutput('timeseriesplot3',
hover = hoverOpts("plot_hover3c", delay = 100, delayType = "debounce"),
dblclick = "plot_3c_dblclick",
brush = brushOpts(id = "plot_3c_brush", resetOnNew = TRUE),
height = 700),
uiOutput("hover_info3c")
)
),

tabPanel('Time Series 4',
div(
style = "position:relative",
plotOutput('timeseriesplot4',
hover = hoverOpts("plot_hover3d", delay = 100, delayType = "debounce"),
dblclick = "plot_3d_dblclick",
brush = brushOpts(id = "plot_3d_brush", resetOnNew = TRUE),
height = 700),
uiOutput("hover_info3d")
)
),

tabPanel('Time Series 5',
div(
style = "position:relative",
plotOutput('timeseriesplot5',
hover = hoverOpts("plot_hover3e", delay = 100, delayType = "debounce"),
dblclick = "plot_3e_dblclick",
brush = brushOpts(id = "plot_3e_brush", resetOnNew = TRUE),
height = 700),
uiOutput("hover_info3e")
)
)



))

))

)),

tabPanel("Ternary Diagram",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(


selectInput("ternarycolour", "Colour", choices=c(
"Black"="black",
"Cluster"="Cluster",
"Climate"="Climate",
"Age"="Age",
"Depth"="Depth",
"Qualitative" = "Qualitative"),
selected="Cluster"),


tags$hr(),


uiOutput('inaxisa'),
uiOutput('inaxisb'),
uiOutput('inaxisc'),

checkboxInput('terndensityplot', "Density Contour"),
checkboxInput('ternnormplot', "Normalize"),


tags$hr(),

sliderInput("ternpointsize", label = "Point Size", value=5, min=2, max=15),

tags$hr(),

downloadButton('downloadPlot5', "Plot")

),

mainPanel(
plotOutput('ternaryplot',
dblclick = "plot1_dblclick", height = 700,
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
)))


))

)),

tabPanel("Elemental Ratios",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(

selectInput(
"ratiocolour", "Ratio Plot Type",
c(
"Black" = "Black",
"Cluster" = "Cluster",
"Climate"="Climate",
"Age"="Age",
"Depth"="Depth",
"Qualitative" = "Qualitative"
), selected="Cluster"),

tags$hr(),


uiOutput('inelementratioa'),
uiOutput('inelementratiob'),

uiOutput('inelementratioc'),
uiOutput('inelementratiod'),

tags$hr(),

sliderInput("spotsize2", label = "Point Size", value=5, min=2, max=15),


checkboxInput('elipseplot2', "Elipse"),



tags$hr(),


downloadButton('downloadPlot4', "Plot")



),

mainPanel(
tabPanel("Element Ratios",
div(
style = "position:relative",
plotOutput("elementratiotimeseries", height = 700,
hover = hoverOpts("plot_hoverratio", delay = 100, delayType = "debounce"),
dblclick = "plot_ratio_dblclick",
brush = brushOpts(id = "plot_ratio_brush", resetOnNew = TRUE)),
uiOutput("hover_inforatio")
)
)

)
)

))


),


tabPanel("Timeseries Equations",
div(class="outer",


fluidRow(
sidebarLayout(

sidebarPanel(

p("Create Plot"),
actionButton('timeserieseq1', "1"),
actionButton('timeserieseq2', "2"),
actionButton('timeserieseq3', "3"),
actionButton('timeserieseq4', "4"),
actionButton('timeserieseq5', "5"),

tags$hr(),

downloadButton('downloadPlot6a', "1"),
downloadButton('downloadPlot6b', "2"),
downloadButton('downloadPlot6c', "3"),
downloadButton('downloadPlot6d', "4"),
downloadButton('downloadPlot6e', "5"),


tags$hr(),

dropdownButton(
tags$h3("Equation"), icon = icon("code"),
uiOutput('inelementnum1'),
selectInput("transform1", label=NULL, c("None", "+", "-", "*", "/"), selected="None"),
uiOutput('inelementnum2'),
selectInput("transform2", label=NULL, c("None", "+", "-", "*", "/"), selected="None"),
uiOutput('inelementnum3'),

uiOutput('inelementden1'),
selectInput("transform3", label=NULL, c("None", "+", "-", "*", "/"), selected="None"),
uiOutput('inelementden2'),
selectInput("transform4", label=NULL, c("None", "+", "-", "*", "/"), selected="None"),
uiOutput('inelementden3'),
tooltip = tooltipOptions(title = "Define Mathematical Treatments")
),

tags$hr(),

textInput('yaxistype', label="Y Axis Label", value="Index"),

tags$hr(),


selectInput(
'timecoloureq', "Time Series Type",
c(
"Black" = "Black",
"Smooth" = "Smooth",
"Ramp" = "Selected",
"Cluster" = "Cluster",
"Climate" = "Climate",
"Qualitative Point" = "QualitativePoint",
"Qualitative Line" = "QualitativeLine",
"Depth" = "Depth",
"Area" = "Area")
),


tags$hr(),



dropdownButton(
tags$h3("Line Options"), icon = icon("gear"),
sliderInput('smoothingeq', label = "Smoothed Mean Average", value=1, min=1, max=50),
sliderInput('linesizeeq', label = "Line Size", value=1, min=1, max=15),
sliderInput('pointsizeeq', label = "Point Size", value=5, min=1, max=15),
tooltip = tooltipOptions(title = "Click for line options")
),


dropdownButton(
tags$h3("X Axis Customization"), icon = icon("gear"),
#uiOutput('inxlimrange'),
selectInput('xaxistypeeq', label="X Axis", c("Depth", "Age", "Custom"), selected="Depth"),
textInput('customxaxiseq', label="Custom X Axis"),
checkboxInput('flipxeq', label="Flip X Axis", value=FALSE),
radioButtons('lengthuniteq', label=NULL, c("mm", "cm", "m", "inches", "feet"), selected="mm"),
numericInput('startmmeq', label = "Start Point (mm)", value=0),
radioButtons('timetypeeq', label=NULL, c("AD", "BC", "BC/AD", "BP"), selected="BP"),
tooltip = tooltipOptions(title = "Click for X-axis options")
),



checkboxInput('transformnorm', label="Normalize", FALSE)






),

mainPanel(
tabsetPanel(
tabPanel('Time Series 1',
div(
style = "position:relative",
plotOutput('timeserieseqplot1',
hover = hoverOpts("plot_hover6a", delay = 100, delayType = "debounce"),
dblclick = "plot_6a_dblclick",
brush = brushOpts(id = "plot_6a_brush", resetOnNew = TRUE),
height = 700
),
uiOutput("hover_info6a")
)
),

tabPanel('Time Series 2',
div(
style = "position:relative",
plotOutput('timeserieseqplot2',
hover = hoverOpts("plot_hover6b", delay = 100, delayType = "debounce"),
dblclick = "plot_6b_dblclick",
brush = brushOpts(id = "plot_6b_brush", resetOnNew = TRUE),
height = 700),
uiOutput("hover_info6b")
)
),

tabPanel('Time Series 3',
div(
style = "position:relative",
plotOutput('timeserieseqplot3',
hover = hoverOpts("plot_hover6c", delay = 100, delayType = "debounce"),
dblclick = "plot_6c_dblclick",
brush = brushOpts(id = "plot_6c_brush", resetOnNew = TRUE),
height = 700),
uiOutput("hover_info6c")
)
),

tabPanel('Time Series 4',
div(
style = "position:relative",
plotOutput('timeserieseqplot4',
hover = hoverOpts("plot_hover6d", delay = 100, delayType = "debounce"),
dblclick = "plot_6d_dblclick",
brush = brushOpts(id = "plot_6d_brush", resetOnNew = TRUE),
height = 700),
uiOutput("hover_info6d")
)
),

tabPanel('Time Series 5',
div(
style = "position:relative",
plotOutput('timeserieseqplot5',
hover = hoverOpts("plot_hover6e", delay = 100, delayType = "debounce"),
dblclick = "plot_6e_dblclick",
brush = brushOpts(id = "plot_6e_brush", resetOnNew = TRUE),
height = 700),
uiOutput("hover_info6e")
)
)



))


))))


#tabPanel("Equation Ratios",
#div(class="outer",


#fluidRow(
#sidebarLayout(

#sidebarPanel(

#selectInput(
#"ratiocolour", "Ratio Plot Type",
#c(
#"Black" = "Black",
#"Cluster" = "Cluster",
#"Climate"="Climate",
#"Age"="Age",
#"Depth"="Depth",
#"Qualitative" = "Qualitative"
#), selected="Cluster"),

#tags$hr(),

#textInput("xaxisdef", label="Custom X Axis", value="X axis Index"),

#uiOutput('inelementx1'),
#selectInput("xtransform1", label=NULL, c("None", "+", "-", "*", "/"), selected="None"),
#uiOutput('inelementx2'),
#selectInput("xtransform2", label=NULL, c("None", "+", "-", "*", "/"), selected="None"),
#uiOutput('inelementx3'),

#tags$hr(),

#textInput("yaxisdef", label="Custom Y Axis", value="Y Axis Index"),

#uiOutput('inelementy1'),
#selectInput("ytransform1", label=NULL, c("None", "+", "-", "*", "/"), selected="None"),
#uiOutput('inelementy2'),
#selectInput("ytransform1", label=NULL, c("None", "+", "-", "*", "/"), selected="None"),
#uiOutput('inelementy3'),


#tags$hr(),

#sliderInput("spotsize3", label = "Point Size", value=5, min=2, max=15),


#checkboxInput('elipseplot3', "Elipse"),



#tags$hr(),


#downloadButton('downloadPlot7', "Plot")



#),

#mainPanel(
#tabPanel('Element Ratios', plotOutput('elementratiotequation',
#dblclick = "plot1_dblclick", height = 700,
#brush = brushOpts(
#id = "plot1_brush",
#resetOnNew = TRUE
#)))



#)

#))

#))


))










