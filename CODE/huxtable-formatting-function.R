hux_format <- function(x) {
  hux(x) %>%
    set_number_format(everywhere, 1, NA)  %>%
    set_bottom_border(brdr(1, "solid", "blue")) %>%
    set_bottom_border(1, everywhere, brdr(2, "solid", "black")) %>%
    set_bold(1, everywhere) %>%
    set_background_color(evens, everywhere, "grey95")
}
