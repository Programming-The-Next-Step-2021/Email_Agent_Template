library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "Demo shinydashboard package - add sub menu items to sidebar - an example", titleWidth = 800),
    dashboardSidebar(
      sidebarMenu(id = 'sidebarmenu',
                  # first menu item
                  menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
                  
                  # second menu item with 2 sub menus
                  menuItem('type',
                           icon = icon('clipboard'),
                           menuSubItem('first response',
                                       tabName = 'chart1',
                                       icon = icon('clipboard-check')),
                           menuSubItem('reply',
                                       tabName = 'chart2',
                                       icon = icon('clipboard-check'))),
                  
                  menuItem('topic',
                           icon = icon('clipboard'),
                           menuSubItem('damage',
                                       tabName = 'damage1',
                                       icon = icon('clipboard-check')),
                           menuSubItem('reply',
                                       tabName = 'damage2',
                                       icon = icon('clipboard-check')))
                  )),
    
    
    dashboardBody(
      tabItems(
        tabItem("Dashboard", h4("this is the Dashboard tab page")),
        tabItem("chart1", h4("Thank you for reaching out to us")),
        tabItem("chart2", h4("Thank you for your reply")),
        tabItem("damage1",h4("ghj")),
        tabItem("damage2",h4("ghj"))
      )
    )
  )
  
  server<- function(input, output, session){
    output$chart1<-renderText({input$chart1})
    output$damage11<-renderText({input$damage1})
  }


shinyApp(ui=ui,server = server)