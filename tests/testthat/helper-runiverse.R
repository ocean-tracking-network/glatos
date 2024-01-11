skip_on_Runiverse <- function() {
  skip_if(Sys.getenv("MY_UNIVERSE") != "", "On R-universe.")
}