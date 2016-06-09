\name{form}
\alias{Perl6.Form-package}
\alias{Perl6.Form}
\alias{form}
\title{
  Format a String with Perl6::Form
}
\description{

  A wrapper around the Perl function \code{form} provided by Damian
  Conway's Perl6::Form module.

}
\usage{
form(..., cat = TRUE, perl = "perl", form.options)
}
\arguments{
  \item{\dots}{
    character and numeric vectors. See Examples.
}
  \item{cat}{
    logical: should result be printed?
}
  \item{perl}{
    name and path of the perl programme
}
\item{form.options}{
  optional: a list of named options; see Examples.
}
}
\details{
  A simple wrapper, implemented via a call to \code{\link{system2}}.
}
\value{
  character
}
\references{
   See the extensive help of the Perl function:
  \url{http://search.cpan.org/~dconway/Perl6-Form/lib/Perl6/Form.pm}  
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{system2}}
}
\examples{

nums <- c(2.1711119999, 1000.1, 0.999)
form("* {]]]]]].[} *", nums)

##  ... results in
##  *       2.17 *
##  *    1000.1  *
##  *       1.00 * 


## use decimal separator as specified by locale
form("{]]]]]],[}", nums,
     form.options = list(locale = 1))



Names <- c("Al", "Bob", "Carl")
Values <- c(0.257, 100.1, 98)

form("+--------+--------+",
     "|     Id |  Value |",
     "+========+========+",
     "| {]][[} | {]].[} |",
          Names,    Values,
     "+--------+--------+")

##  ... results in
##  +--------+--------+
##  |     Id |  Value |
##  +========+========+
##  |   Al   |   0.26 |
##  |  Bob   | 100.1  |
##  |  Carl  |  98.0  |
##  +--------+--------+ 

}
