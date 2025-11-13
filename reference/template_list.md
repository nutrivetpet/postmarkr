# List Templates

Retrieves a list of templates from the Postmark API. Templates can be
filtered by type and paginated using count and offset parameters.

## Usage

``` r
template_list(count, type = "all")
```

## Arguments

- count:

  An integer specifying the number of templates to retrieve.

- type:

  A string specifying the template type to filter by: "all", "standard",
  or "layout". Defaults to "all".

## Value

A data frame (or tibble if tibble is installed) containing the templates
information. The returned data includes template details from the
Postmark API.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the first 10 templates
templates <- template_list(count = 10)

# Get only layout templates
layouts <- template_list(count = 50, type = "layout")

# Get all templates with pagination
templates <- template_list(count = 100)
} # }
```
