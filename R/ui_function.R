
# launcher UI function


valueBoxOutput_custom <- function (outputId, width = 4, style = NULL)
{
    shiny::uiOutput(outputId, class = paste0("col-sm-", width), style = style)
}

infoBoxOutput_custom <- function (outputId, width = 4, style = NULL)
{
    shiny::uiOutput(outputId, class = paste0("col-sm-", width), style = style)
}
