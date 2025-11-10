# Add leading zeros to numbers while preserving other text

This function is created to fix problems caused by a serious bug in
Excel. Editing csv files in that program causes leading zeros to
disappear.

## Usage

``` r
AddLeadingZeros(
  codes,
  places,
  warningText = NULL,
  viaFactor = TRUE,
  nWarning = 6,
  removeLeadingTrailingWhitespace = TRUE
)
```

## Arguments

- codes:

  Character vector

- places:

  Number of places for positive numbers. Minus sign is extra

- warningText:

  When non-NULL, warning will be produced

- viaFactor:

  When TRUE, the algorithm uses factor coding internally.

- nWarning:

  Number of elements to be written before ... in warnings.

- removeLeadingTrailingWhitespace:

  Remove leading and trailing whitespace

## Value

Character vector

## Author

Øyvind Langsrud

## Examples

``` r
AddLeadingZeros(c("1", "ABC", "12345", " 23", "-8", "45 ", " -9", " Agent ", "007", 
                  "7 James Bond "), 10)
#>  [1] "0000000001"   "ABC"          "0000012345"   "0000000023"   "-0000000008" 
#>  [6] "0000000045"   "-0000000009"  "Agent"        "007"          "7 James Bond"
AddLeadingZeros(c("1", "ABC", "12345", " 23", "-8", "45 ", " -9", " Agent ", "007", 
                  "7 James Bond "), 4)
#>  [1] "0001"         "ABC"          "12345"        "0023"         "-0008"       
#>  [6] "0045"         "-0009"        "Agent"        "007"          "7 James Bond"
AddLeadingZeros(c("1", "ABC", "12345", " 23", "-8", "45 ", " -9", " Agent ", "007", 
                  "7 James Bond "), 4, removeLeadingTrailingWhitespace = FALSE)
#>  [1] "0001"          "ABC"           "12345"         " 23"          
#>  [5] "-0008"         "45 "           " -9"           " Agent "      
#>  [9] "007"           "7 James Bond "
AddLeadingZeros(c("1", "ABC", "12345", " 23", "-8", "45 ", " -9", " Agent ", "007", 
                  "7 James Bond "), 4, warningText = "string changes")
#> Warning: Whitespace removed: string changes:  -9,  23,  Agent , 45 , 7 James Bond 
#> Warning: string changes: -0008, -0009, 0001, 0023, 0045
#>  [1] "0001"         "ABC"          "12345"        "0023"         "-0008"       
#>  [6] "0045"         "-0009"        "Agent"        "007"          "7 James Bond"
AddLeadingZeros(c("1", "ABC", "12345", " 23", "-8", "45 ", " -9", " Agent ", "007", 
                  "7 James Bond "), 4, warningText = "", nWarning = 2)
#> Warning: Whitespace removed: :  -9,  23, ..., 7 James Bond 
#> Warning: : -0008, -0009, ..., 0045
#>  [1] "0001"         "ABC"          "12345"        "0023"         "-0008"       
#>  [6] "0045"         "-0009"        "Agent"        "007"          "7 James Bond"
```
