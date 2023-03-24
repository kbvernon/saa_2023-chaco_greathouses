
# for understanding font size and dpi in ggplot, see
# https://www.christophenicault.com/post/understand_size_dimension_ggplot2/
sysfonts::font_add_google("Source Sans Pro", "plot-font")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

ggplot2::theme_set(ggplot2::theme_minimal(14))
ggplot2::theme_update(text = ggplot2::element_text(family = "plot-font"))

d <- function(x){
  
  switch(
    deparse(substitute(x)),
    "h" = 1080,
    "w" = 1920,
    "a" = 1080/1920
  )
  
}

# set year interval for time series
year_interval <- 25

# wildly overengineered way of specifying start and end dates systematically
# so, peppered with latin to make it extra pedantic
# with tidy style lazy eval
rng <- function(x){
  
  switch(
    deparse(substitute(x)),
    "origin" = 650, 
    "terminus" = 1450,
    stop("x must be one of 'origin' or 'terminus'.", call. = FALSE)
  )
  
}

ghc <- function(...){
  
  q <- vapply(
    as.list((match.call()[-1])), 
    deparse, 
    FUN.VALUE = character(1)
  )
  
  r <- list(
      gray1 = "#f6f6f6", 
      gray2 = "#e2e2e2", 
      gray3 = "#8b8b8b", 
      gray4 = "#6f6f6f", 
      gray5 = "#3e3e3e", 
      gray6 = "#222222", 
      
      red1 = "#fff8f6", 
      red2 = "#ffddd8", 
      red3 = "#ff4647", 
      red4 = "#e0002b", 
      red5 = "#830014", 
      red6 = "#530003", 
      
      cinnamon1 = "#fff8f3", 
      cinnamon2 = "#ffdfc6", 
      cinnamon3 = "#d57300", 
      cinnamon4 = "#ac5c00", 
      cinnamon5 = "#633300", 
      cinnamon6 = "#371d00", 
      
      yellow1 = "#fff9e5", 
      yellow2 = "#ffe53e", 
      yellow3 = "#9c8b00", 
      yellow4 = "#7d6f00", 
      yellow5 = "#463d00", 
      yellow6 = "#292300", 
      
      powder1 = "#dafaff", 
      powder2 = "#8df0ff", 
      powder3 = "#0098a9", 
      powder4 = "#007987", 
      powder5 = "#004048", 
      powder6 = "#002227", 
      
      cerulean1 = "#e8f6ff", 
      cerulean2 = "#b9e3ff", 
      cerulean3 = "#0092c5", 
      cerulean4 = "#00749d", 
      cerulean5 = "#003c54", 
      cerulean6 = "#001d2a", 
      
      blue1 = "#f0f4ff", 
      blue2 = "#d4e0ff", 
      blue3 = "#0089fc", 
      blue4 = "#006dca", 
      blue5 = "#00386d", 
      blue6 = "#001a39"
  )
  
  unname(unlist(r[q]))
  
}
