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
form(..., cat = TRUE, perl = "perl")
}
%- maybe also 'usage' for other objects documented here.
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
