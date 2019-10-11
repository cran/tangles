\name{tangles}
\docType{methods}
\alias{tangles}
\title{Anonymize spatial point patterns and raster objects}
\description{Spatial coordinates are anonymized by using three possible techniques of altering locations without loss of the spatial relationships between data. The three techniques are vertical, horizontal and rotational shifts. User defines how many steps to use in the anonymization. The actual steps taken are randomized and saved for later use.}

\usage{tangles(data=NULL, depth=3, rasterdata = FALSE, raster_object = FALSE, 
saveTangles = FALSE, path = NULL)}

\arguments{
  \item{data}{Can be either a 2 column \code{matrix} of spatial coordinates or a \code{raster} object.} 
  \item{depth}{numeric; the number of anonymization steps to take (levels of abstraction). Default is 3.}     
  \item{rasterdata}{logical; If \code{TRUE} the rotational degrees will be taken as either 90, 180 or 270 degrees rather than randomly selected. This is to preserve the properties of a raster if raster data is to be anonymized.}
  \item{raster_object}{logical; If \code{TRUE} the data to be anonymized is a raster object. This means the data associated with the raster are carried through to the outputs.}
  \item{saveTangles}{logical; Save function output to file. Default is \code{FALSE}, but it is recommended to always save outputs.}
  \item{path}{character; Path to where outputs are saved.}
}

\value{A \code{list} object that contains: 1. The transformed coordinates or transformed raster object (dependent on the inputs). 2. A separate list object to be used for untangling the transformed coordinates. The outputs can be written to file to a specified directory with file stub names of \code{tangledXY} and \code{detangler} respectively.These files have a common hash key as part of their file name.The hash key is generated from the detangler object using the sha256 hash algorithm}


\note{Raster data can in practice be rotated for any given angle, yet the linear horizontal and vertical properties of the raster will be lost, meaning that no software will be able to recognize it as a raster. This why there is the \code{rasterdata} parameter exists, and can be used for both raster and non-raster data. It just constrains the possible degree values that can be used in the anonymization. If raster data is being anonymized both \code{rasterdata} and \code{raster_object} will be set to \code{TRUE} in most cases.

The most important output to come from this function is the detangler object (saved to file with unique hash key), as this encodes the anonymization steps that can be used for re-identification, or anonymization of associated data. This enables parties to share pertinent information without the requirement to share actual spatial coordinates.

\code{tangles} is coordinate reference system agnostic. Anonymization will proceed regardless of whether data is in geographic or projected space. Anonymized coordinates will not exist in reality, but the spatial properties of the original data will be maintained.}

\author{Brendan Malone}

\references{
\itemize{
\item CM O`Keefe, S Otorepec, M Elliot, E Mackey, and K O`Hara (2017) \href{https://www.data61.csiro.au/en/Our-Work/Safety-and-Security/Privacy-Preservation/De-identification-Decision-Making-Framework}{The De-Identification Decision Making Framework}. CSIRO Reports EP173122 and EP175702.}}

\examples{
## POINT DATA
library(digest)
data("HV_subsoilpH")
str(HV_subsoilpH)
dat.xy<- HV_subsoilpH[,1:2]
xyData<- as.matrix(dat.xy)

# anonymize with 5 levels of abstraction
tangles.out<- tangles(data = xyData, 
depth = 5, 
rasterdata = FALSE, 
raster_object = FALSE, 
saveTangles = FALSE)

str(tangles.out)  
head(tangles.out[[1]])

## RASTER DATA
library(raster)
data("hunterCovariates_sub")
str(hunterCovariates_sub)
raster_object<- hunterCovariates_sub

tangles.out<- tangles(data = hunterCovariates_sub, 
depth = 5, 
rasterdata = TRUE, 
raster_object = TRUE,
saveTangles = FALSE)

str(tangles.out)


}
\keyword{methods}
