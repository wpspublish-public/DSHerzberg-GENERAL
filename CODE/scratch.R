temp1 <- input[as.name("General Live Webinar Experience"):as.name("Usefulness of Content")]


input[c(
  match("General Live Webinar Experience", names(input)):match("Usefulness of Content", names(input))
)] <- named_super_sub_r_cols

