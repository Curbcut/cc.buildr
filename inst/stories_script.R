## BUILD STORIES ###############################################################

build_stories <- function() {

  # Build empty table -------------------------------------------------------

  stories <- stories_empty_table()

  # Add every story ---------------------------------------------------------

  stories <-
    stories |>
    stories_add_story(...) |>
    stories_add_story(...)


  # Create images and mapping -----------------------------------------------

  stories_mapping <- stories_atlas_mapping(stories = stories)


  # Knit all stories Rmds ---------------------------------------------------

  stories_knit_all()


  # Return ------------------------------------------------------------------

  return(list(stories = stories,
              stories_mapping = stories_mapping))

}
