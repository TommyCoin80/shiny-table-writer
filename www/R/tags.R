tagList(
  useShinyjs(),
  tags$script(HTML("$('#expandRight').attr('data-toggle','control-sidebar')")),
  tags$script(src = 'JS/aaScript.js', type='text/javascript', language='javascript'),
  tags$head(
    tags$script(src="https://ajax.googleapis.com/ajax/libs/webfont/1.6.26/webfont.js"),
    tags$script(HTML("WebFont.load({google: {families: ['Play']}});")),
    tags$link(rel = "stylesheet", type = "text/css", href = "AA_STANDARD/CSS/aaStyle.css")
  )
)
