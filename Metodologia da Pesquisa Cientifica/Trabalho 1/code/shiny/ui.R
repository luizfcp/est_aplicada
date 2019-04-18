
# user-interface

dashboardPage(
    dashboardHeader(title = "Twittadas Presidenciais", titleWidth = 250),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Fernando Collor", tabName = "collor"),
            menuItem("Luiz Inacio Lula da Silva", tabName = "lula"),
            menuItem("Dilma Rousseff", tabName = "dilma"),
            menuItem("Michel Temer", tabName = "temer"),
            menuItem("Jair Bolsonaro", tabName = "bolsonaro")
        )
    ),
    dashboardBody(
        # fluidRow(
        #     valueBoxOutput(""),
        #     valueBoxOutput(""),
        #     valueBoxOutput("")
        # ),
        tabItems(
            tabItem("collor",
                    fluidRow(
                        box(
                            width = 3, status = "primary", height = 300,
                            img(src='collor.jpg', height = '280px', 
                                style="display: block; margin-left: auto; margin-right: auto;")
                        ),
                        box(
                            width = 9, title = "Tweets", status = "info", solidHeader = T,
                            verbatimTextOutput("tweets_neg_collor"),
                            verbatimTextOutput("tweets_pos_collor"), height = 300 
                        )
                    ),
                    fluidRow(
                        box(
                            width = 6, plotOutput("wc_collor"), height = 450
                        ),
                        box(
                            width = 6, plotOutput("bc_collor"), height = 450
                        ),
                        box(
                            width = 9, plotOutput("grafo_collor")
                        )
                    )
            ),
            
            tabItem("lula",
                    fluidRow(
                        box(
                            width = 3, status = "primary", height = 300, 
                            img(src='lula.jpg', height = '280px', 
                                style="display: block; margin-left: auto; margin-right: auto;")
                        ),
                        box(
                            width = 9, title = "Tweets", status = "info", solidHeader = T,
                            verbatimTextOutput("tweets_neg_lula"),
                            verbatimTextOutput("tweets_pos_lula"), height = 300 
                        )
                    ),
                    fluidRow(
                        box(
                            width = 6, plotOutput("wc_lula"), height = 450
                        ),
                        box(
                            width = 6, plotOutput("bc_lula"), height = 450
                        ),
                        box(
                            width = 9, plotOutput("grafo_lula")
                        )
                    )
            ),
            
            tabItem("dilma",
                    fluidRow(
                        box(
                            width = 3, status = "primary", height = 300, 
                            img(src='dilma.jpg', height = '280px', 
                                style="display: block; margin-left: auto; margin-right: auto;")
                        ),
                        box(
                            width = 9, title = "Tweets", status = "info", solidHeader = T,
                            verbatimTextOutput("tweets_neg_dilma"),
                            verbatimTextOutput("tweets_pos_dilma"), height = 300 
                        )
                    ),
                    fluidRow(
                        box(
                            width = 6, plotOutput("wc_dilma"), height = 450
                        ),
                        box(
                            width = 6, plotOutput("bc_dilma"), height = 450
                        ),
                        box(
                            width = 9, plotOutput("grafo_dilma")
                        )
                    )
            ),
            
            tabItem("temer",
                    fluidRow(
                        box(
                            width = 3, status = "primary", height = 300, 
                            img(src='temer.jpg', height = '280px', 
                                style="display: block; margin-left: auto; margin-right: auto;")
                        ),
                        box(
                            width = 9, title = "Tweets", status = "info", solidHeader = T,
                            verbatimTextOutput("tweets_neg_temer"),
                            verbatimTextOutput("tweets_pos_temer"), height = 300 
                        )
                    ),
                    fluidRow(
                        box(
                            width = 6, plotOutput("wc_temer"), height = 450
                        ),
                        box(
                            width = 6, plotOutput("bc_temer"), height = 450
                        ),
                        box(
                            width = 9, plotOutput("grafo_temer")
                        )
                    )
            ),
            
            tabItem("bolsonaro",
                    fluidRow(
                        box(
                            width = 3, status = "primary", height = 300, 
                            img(src='bolsonaro.jpg', height = '280px', 
                                style="display: block; margin-left: auto; margin-right: auto;")
                        ),
                        box(
                            width = 9, title = "Tweets", status = "info", solidHeader = T,
                            verbatimTextOutput("tweets_neg_bolsonaro"),
                            verbatimTextOutput("tweets_pos_bolsonaro"), height = 300 
                        )
                    ),
                    fluidRow(
                        box(
                            width = 6, plotOutput("wc_bolsonaro"), height = 450
                        ),
                        box(
                            width = 6, plotOutput("bc_bolsonaro"), height = 450
                        ),
                        box(
                            width = 9, plotOutput("grafo_bolsonaro")
                        )
                    )
            )
        )
    )
)

