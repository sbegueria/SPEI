"SPEINews" <- function() {
    file <- file.path(system.file(package="SPEI"), "NEWS")
    file.show(file)
}