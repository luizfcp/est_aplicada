
# user-interface

dashboardPage(
    dashboardHeader(title = "Presidentes"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dilma Rousseff", tabName = "dilma"),
            menuItem("Fernando Collor", tabName = "collor"),
            menuItem("Jair Bolsonaro", tabName = "bolsonaro"),
            menuItem("Luiz Inacio Lula da Silva", tabName = "lula"),
            menuItem("Michel Temer", tabName = "temer")
        )
    ),
    dashboardBody(
        # fluidRow(
        #     valueBoxOutput(""),
        #     valueBoxOutput(""),
        #     valueBoxOutput("")
        # ),
        tabItems(
            tabItem("dilma",
                    fluidRow(
                        box(
                            width = 3, status = "primary", 
                            img(src='', height = '500px'), height = 300
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
            tabItem("collor",
                    fluidRow(
                        box(
                            width = 3, status = "primary", 
                            img(src='', height = '500px'), height = 300
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
            tabItem("bolsonaro",
                    fluidRow(
                        box(
                            width = 3, status = "primary", 
                            img(src='', height = '500px'), height = 300
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
            ),
            tabItem("lula",
                    fluidRow(
                        box(
                            width = 3, status = "primary", 
                            img(src='', height = '500px'), height = 300
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
            tabItem("temer",
                    fluidRow(
                        box(
                            width = 3, status = "primary", 
                            img(src='', height = '500px'), height = 300
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
            )
        )
    )
)

