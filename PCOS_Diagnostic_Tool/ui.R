
# Define UI for application that takes patients symptoms and blood test results 
#to determine the probability of Poly cystic Ovarian Syndrome. 

shinyUI(fixedPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),

    # Application title
    titlePanel("PCOS Diagnostic Tool"),
  
      navlistPanel(
        "Sample Data",
        tabPanel("Probability Plot", plotlyOutput("PCOS_sigmoid")), 
        "Patient Compared to Sample",
        tabPanel("Menstrual Cycle", plotlyOutput("PCOS_Cycle_Hist")),
        tabPanel("Luteinizing Hormone", plotlyOutput("LH_Hist")),
        tabPanel("Left Ovary Follicle Count", plotlyOutput("Follicle_Left")),
        tabPanel("Right Ovary Follicle Count", plotlyOutput("Follicle_Right")), 
        tabPanel("Weight Gain", plotlyOutput("Weight_Gain")), 
        tabPanel("Hair Growth", plotlyOutput("Hair_Growth")), 
        tabPanel("Skin Darkening", plotlyOutput("Skin_Darkening"))
        
    ),
  
    hr(), 
      
      fixedRow(
        
        column(12, h4(strong(textOutput("PCOS_prob")), width = 12, align = "center"))),
        
        fixedRow(
          column(6,
        
        radioButtons("weight_gain", 
                     label = p("Has the patient experienced weight gain in the past 6 months?"),
                     choices = list("Yes" = 1, "No" = 0), selected = 0),
        
        radioButtons("hair_growth", label = p("Has the patient experienced unwanted hair growth in the past 6 months?"),
                     choices = list("Yes" = 1, "No" = 0), selected = 0),
        
        radioButtons("skin_darkening", label = p("Has the patient experienced skin darkening in the past 6 months?"),
                     choices = list("Yes" = 1, "No" = 0), selected = 0),
        
        sliderInput("Cycle_Length", label = p("Last Menstrual Cycle Length in Days"), 
                    min = 0, max = 15, value = 5, step = 1),
        
        br(),
        
        ),
        
        column(6, 
        
        sliderInput("LH", label = p("What is the patient's Luteinizing Hormone (LH) IU/mL?"), 
                    min = 0, max = 60, value = 30, step = .1),
        
        sliderInput("Follicle_L", label = p("In the last ultrasound, how many follicles were found in the left ovary?"), 
                    min = 0, max = 30, value = 15, step = 1),
        
        sliderInput("Follicle_R", label = p("In the last ultrasound, how many follicles were found in the right ovary?"), 
                    min = 0, max = 30, value = 15, step = 1), 
        
        actionButton('show_about', "About")
        
        )
        
      )
        
      )
    
    )
  
