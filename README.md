
## Streamlined Quality Control for Ion Chromatography

Tools for ion chromatography (IC) quality control and sign-off. A
resource for the National Ice Core Facility.

Author: Matt Harris, Earth Sciences New Zealand  
Contact: <m.harris@gns.cri.nz>

Install the package using the following code, assuming it is public.
Building is unnecessary as it contains no non-R code.

``` r
devtools::install_github('GNS-NICRF/ICQC', build = FALSE)
```

### Dependencies

Refer to the package Description file. To render automated reports,
tinytex is required. Install it with the following code.

``` r
tinytex::install_tinytex()
```

### Using the package

This package does one thing at present:

1.  Produce automated reports from IC calibration sequence .xls files to
    allow for quick visualisation and lab sign-off (go/no go).

### To add

Wishlist:

- Supply a folder to calibration_report(), and automatically detect &
  generate reports for all calibration .xls files therein.
- Determine optimum calibration point \# from a series of supplied ion
  calibrations.
- QC tracking through time, within and between calibrations and QC
  versions.
- Adaptive export of calibrated sample sequence data based on
  calibration tweaking (e.g., curve optimisation at low concentrations).
