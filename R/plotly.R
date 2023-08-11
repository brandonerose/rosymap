
plotly_bar<-function(df,x_col,y_col,name){
  fig <- plotly::plot_ly(
    df,
    x = x_col,
    y = y_col,
    type = 'bar',
    name = name,
    text=x_col,
    textposition="inside",
    insidetextanchor="middle",
    insidetextfont=list(color="black")
  ) %>% plotly::layout(
    xaxis = list(
      title = list(
        # text=paste0(name," (",prettyNum(sum(b$n_applicants),","),")"),
        font=list(
          size=12,
          color="black"
        )),
      tickfont=list(
        size=12,
        color="black"
      )),
    yaxis = list(title = '',
                 tickfont=list(
                   size=10,
                   color="black"
                 )
    ),
    barmode = 'stack',
    annotations = list(
      x = x_col,
      y = y_col,
      xanchor="left",
      xref="x",
      yref="y",
      text = paste0((x_col/sum(x_col)*100) %>% round(1),"%"),
      showarrow = F,
      arrowhead = NULL,
      arrowsize = NULL,
      font=list(size=12,
                color="black"),
      textangle=0
    )
  ) %>%
    plotly::config(scrollZoom=F, displaylogo = F,
                   modeBarButtonsToRemove = c(
                     "zoom2d",
                     "pan2d",
                     "select2d",
                     "lasso2d",
                     # "zoomIn2d",
                     # "zoomOut2d",
                     "autoScale2d",
                     # "resetScale2d",
                     "hoverclosest",
                     "hoverCompareCartesian",
                     "toggleHover"
                   )) %>%
    plotly::layout(showlegend = F,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T)
    ) %>% plotly::style(hoverinfo = 'none')
  fig
}

plotify<-function(GG){
  GG %>% plotly::ggplotly(tooltip = "text") %>%
    plotly::config(scrollZoom=F, displaylogo = F,
                   modeBarButtonsToRemove = c(
                     "zoom2d",
                     "pan2d",
                     "select2d",
                     "lasso2d",
                     # "zoomIn2d",
                     # "zoomOut2d",
                     "autoScale2d",
                     # "resetScale2d",
                     "hoverClosestCartesian",
                     "hoverCompareCartesian"
                   )) %>%
    plotly::layout(hoverlabel = list(align = "left"),
                   xaxis = list(
                     tickfont=list(
                       size=10,
                       color="black"
                     )),
                   yaxis = list(
                     tickfont=list(
                       size=10,
                       color="black"
                     ))
    )
}
