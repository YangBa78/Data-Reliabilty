#large letters
letterwrap <- function(n, depth = 1) {
  args <- lapply(1:depth, FUN = function(x) return(LETTERS))
  x <- do.call(expand.grid, args = list(args, stringsAsFactors = F))
  x <- x[, rev(names(x)), drop = F]
  x <- do.call(paste0, x)
  if (n <= length(x)) return(x[1:n])
  return(c(x, letterwrap(n - length(x), depth = depth + 1)))
}

letterwrap(40)


# small letter
myLetters <- function(length.out) {
  a <- rep(letters, length.out = length.out)
  grp <- cumsum(a == "a")
  vapply(seq_along(a), 
         function(x) paste(rep(a[x], grp[x]), collapse = ""),
         character(1L))
}
myLetters(60)