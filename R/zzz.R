# "`-''-/").___..--''"`-._
#  (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
#  (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#    _ ..`--'_..-_/  /--'_.' ,'
#  (il),-''  (li),'  ((!.-'
#
# Author: Weiming Hu <weiming@psu.edu>
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University

.onLoad <- function(lib, pkg) {
  version <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
  message <- paste0("MoreParallelR\nversion: ", version,
                    "\nAuthor: Weiming Hu",
                    "\nWebsite: https://weiming-hu.github.io/MoreParallelR")
  packageStartupMessage(message)
  invisible()
}
