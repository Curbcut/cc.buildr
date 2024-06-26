% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variables_table.R
\name{add_variable}
\alias{add_variable}
\title{Add a new variable to the variables table}
\usage{
add_variable(
  variables,
  var_code,
  type,
  var_title,
  var_short = as.character(var_title),
  explanation,
  explanation_nodet = gsub("^the ", "", explanation),
  exp_q5,
  parent_vec,
  group_name = NA,
  group_diff = list(),
  theme,
  private,
  pe_include = FALSE,
  dates,
  avail_scale,
  source,
  interpolated,
  rankings_chr = c("exceptionally low", "unusually low", "just about average",
    "unusually high", "exceptionally high"),
  rank_name = NULL,
  rank_name_short = NULL,
  var_measurement = data.frame(scale = avail_scale, measurement = rep("scalar",
    length(avail_scale))),
  breaks_q5 = NULL,
  allow_title_duplicate = FALSE,
  classification
)
}
\arguments{
\item{variables}{<\code{data.frame}> The \code{variables} data.frame to which add a
new row.}

\item{var_code}{<\code{character}> The code used to refer to the variables, e.g.
\code{alp}}

\item{type}{<\code{character}> The variable type. One of \code{"ind"}, \code{"pct"},
\code{"avg"}, \code{"median"}, \code{"per1k"}, \code{"sqkm"} or \code{"count"}.}

\item{var_title}{<\code{character}> The variable title}

\item{var_short}{<\code{character}> A short variable title used in graphs or where
space is limited. Preferably ~ <12 characters.}

\item{explanation}{<\code{character}> Variable explanation. Starts with a determinant.
e.g. the percentage of private dwellings occupied by tenants}

\item{explanation_nodet}{<\code{character}> Usually the same as \code{explanation}
without the determinant it starts with. It defaults to \code{explanation} with the \verb{^the }
it starts with removed. Used in the compare panel following \verb{a higher x}.
e.g. a higher 'percentage of private dwellings occupied by tenants'.}

\item{exp_q5}{<\code{character}> String used for the explore panel explaining the
variable. Depends on the \code{type}. The rules are:
\itemize{
\item{"pct"}{Percentage. starts with a verb and follows the absolute count of the parent
string. the string will read e.g. '50 households (3\%) are tenants' where
\verb{are tenants} is the definition value.}
\item{"count"}{Count. starts with a verb and follows the count string (with the
'parent vector' which in this case is just a string). The string will
read e.g. '50 households are tenants' where \verb{are tenants} is the definition value,
and \code{households} is the parent vector string (at the parent_vec argument).}
\item{"dollar"}{Currency (dollar). starts with a subject and ends with a verb. It's assumed it
will be followed by a dollar number. e.g. 'the average rent is 800$' where
\verb{the average rent is} is the definition value.}
\item{"ind"}{Index. starts with a verb and uses a place holder written \verb{_X_} which
would translates to, e.g. 'medium to high'. example: '50 households are living
in areas with low potential for active living' where the definition would
be: \verb{are living in areas with _X_ potential for active living}}
\item{"avg"}{Average. starts with a subject and uses a place holder written \verb{_X_} which
would translates to a number. example: 'the average resident has access to 30
grocery stores within 15 minutes by walk' where the definition would be:
\verb{the average resident has access to _X_ grocery stores within 15 minutes by walk}}
\item{"sqkm"}{X per square kilometres. starts with a determinant and uses a
placeholder written \verb{_X_} which translates to a number. example: 'the density
of green alleys is 2.28 square  metres per square kilometres' where the definition would be:
\verb{the density of green alleys is _X_ square metres per square kilometres}}
\item{"per1k"}{X per 1,000 residents. starts with a determinant and uses a
placeholder written \verb{_X_} which translates to a number. example: 'the density
of green alleys is 28.7 square metres per 1000 residents' where the definition would be:
\verb{the density of green alleys is _X_ square metres per 1000 residents}}
\item{"ppo"}{People per object. Can be a single word of the object.
example: 'there are 4 people for every tree' where the definition of 'exp_q5'
would be \code{tree}}
}}

\item{parent_vec}{<\code{character}> Parent vector of the variable. Used for
the explore panel. Must be another entry in the variable table. E.g. for
Tenant households (\%), the parent variable would be the number of
private households (denominator of the percentage) : \code{private_households}.}

\item{group_name}{<\code{character}> The name of the larger group to which the
variable belongs. e.g. for the variable accessibility to public schools by bike,
the group_name would be \code{"Accessibility to schools"}}

\item{group_diff}{<\verb{named list}> A named list is used to represent a variable
that is part of a larger group. For example, when considering accessibility
to public schools by bike, the larger group is Accessibility to schools, and
the mode of transport (bike) differentiates the subgroups. The list can be
constructed as follows: list("Mode of transport" = "By bike", "Public/Private" =
"Public") This list may contain multiple named vectors, each representing a
different subgroup. By default, these groups will be displayed in a dropdown
menu. If you prefer a slider, you should include the 'slider' class to the
corresponding list element value. The element should be a factor with properly
ordered levels. For example, let's say you are working with the shelter cost
to income ratio. Create a named list with a 'Shelter cost' key and a factor
value of '>30\%', with the following levels (which is the order you want
displayed on the slider): c('>0\%', '>30\%', '>50\%', '>80\%'). Add the 'slider'
class to the value, so it is displayed as a slider. Example:
list("Gender" = "Female",
"Shelter cost to income ratio)" = structure(
factor(">0\%", levels = c(">0\%", ">30\%", ">50\%", ">80\%")),
class = "slider"))}

\item{theme}{<\code{character}> The theme to which the variable belongs, e.g. "Housing",
"Urban life", ... This will inform the dropdowns title (within opened dropdown)
and place explorer themes.}

\item{private}{<\code{logical}> If we have permissions to make the variable available
for public download.}

\item{pe_include}{<\code{logical}> Should this variable be included in the place
explorer? Defaults to \code{TRUE}. Variables with are TRUE here will also be the ones
picked to be placed as 'compare' variables within pages.}

\item{dates}{<\verb{character vector}> A vector of dates for which the data is available.}

\item{avail_scale}{<\code{list}> All the combinations of region and scales
at which the data is available, e.g. \code{c("CMA_CSD", "CMA_CT", ...)}}

\item{source}{<\code{character}> The source where the data comes from, e.g.
"McGill Geo-Social Determinants of Health Research Group"}

\item{interpolated}{<\code{data.frame}> A data.frame indicating from which scale
the region/scale comination has been interpolated. The non-interpolated data
is populated with \code{"FALSE"}. The interpolation scale must be one of the
scale code in the \code{scales_dictionary}.}

\item{rankings_chr}{<\verb{character vector}> Vector of character that will be used
in the explore text to inform how a location ranks within the region.
The attach character will be its \code{q5} break. \code{ranking_chr} must be in order of
lower to higher. \verb{The variable score is 90\% which is unusually low for Montreal}.
Defaults to \code{c("exceptionally low", "unusually low", "just about average", "unusually high", "exceptionally high")}}

\item{rank_name}{<\verb{character vector}> Vector of character to characterise
every 5 bins (breaks). Only used in \code{ind} types. Defaults to NULL, which will
be converted into Very low (1st bin), Low (2nd), Moderate, High, Very high.}

\item{rank_name_short}{<\verb{character vector}> Same as \code{rank_name} but shorter
to fit easily on the legend.}

\item{var_measurement}{<\verb{character list}> Data.frame where every row is an available
scale (\code{aval_scale}) and the second column is the measurement type. These will impact
both the explore panel text and the explore panel graph. Options are:
\itemize{
\item{"scalar"}{The default. If \code{var_measurement} is not supplied, the row will
be automatically filled with this measurement. Represents numeric data with both
meaningful order and consistent distance between values. Example: age, temperature,
height, weight.}
\item{"ordinal"}{Represents rank or order within a dataset. It has meaningful
order or sequence, but the distance between values is not consistent. It is
the one used for the climate risk page at the grid scale where
1 = Insignificant vulnerability, 2 = Minore, 3 = Moderate, ...
Other example: survey responses with options like 'Strongly disagree', 'Disagree', ...}
\item{"nominal"}{Represents categories without inherent order. No meaningful
order or ranking is possible. Qualitative variables. Example: animal types,
such as 'Mammal', 'Bird', 'Reptile', 'Fish'.}
}}

\item{breaks_q5}{<\verb{character/numeric vector}> If the q5 breaks should be
hardcoded, insert them here. Defaults to NULL.}

\item{allow_title_duplicate}{<\code{logical}> It is necessary to not have variable title
duplicates in the same dropdown menu, as the title is what is displayed to the
user. If there is a duplicate, the user will be selecting two variable codes
when selection a unique dropdown choice, and it will crash the app. Defaults
to \code{FALSE}. If you are certain the duplicated variable title will never be
used in a dropdown (ex. it is a parent variable), then switch to \code{TRUE} at
your own risks.}

\item{classification}{<\code{character}> A classification that will be used for
comparison purposes. We want compare variables to be decided programatically,
and never want socio-demographic variables to be compared with each other.
Socio-demo will never be compared with socio-demo, but with physical
environment. While physical environmentcan be compared with other physical
variables. Possible options here are: \code{sociodemo}, \code{physical} or \code{other}. Other
will be consider as a passe-partout, and can be used to be compared with any other
variable.}
}
\value{
The same \code{variables} data.frame fed, with the added row.
}
\description{
Add a new variable to the variables table
}
