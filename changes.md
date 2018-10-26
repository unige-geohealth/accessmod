## List of changes in AccessMod

- 5.2.2 - 2018-10-06
    - Bug fixes
        - Solve issue #202 [ 2018-10-05 - 2018-10-06 ]. Population coverage estimation in zonal statistics module could have been wrong by a little less than a minute : instead of showing population coverage according to the selected travel time value, the table included statistics for isochrone lower than value plus one, instead of lower than or equal to the value. This was occurring only when the maximum travel time value in the input layer was greater than the selected value in the input field of the module. Example: if the user selected a 120 minutes travel time and launched a zonal statistic on 60 minutes, the population coverage would have included population within an isochronal area up to 60.99 minutes instead of 60 minutes. 
- 5.2.1 - 2018-09-21
    - New feature
        - It's now possible to set speed of 0 Km/h in the scenario table. See #197 [ 2018-09-15 - 2018-09-21 ]. Speed of zero is considered as barrier and will be act like so. Selection of facilities located on such 0 Km/h cells are not allowed.
- 5.2.0 - 2018-08-24
    - New feature
        - Correct for population on barrier. This tool will remove population data located on land cover merged barriers and redistribute it within administrative zones. 
- 5.1.18 - 2018-07-09
    - Improvements
        - An estimation of disk space and memory needed to compute a cycle of travel time analysis is now visible in the validation panel of all accessibility based analysis. This is an estimation and not an absolute value. This is a complement to 5.1.16, where the estimation was used just before the analysis to prevent data corruption or memory shortage. 
        - Archive list, changed alphabetical order to date. Issue #192 [ 2018-06-28 - 2018-07-09 ].
    - Bug fixes 
        - Issue #193 [ 2018-06-28 – 2018-07-09 ] The export of a dataset containing infinite travel time caused by faulty DEM produced an error when exporting the file. A new limit in possible travel time set to 2^1024 seconds will prevent the issue, as inf number will be ignored, while keeping a large possible output values still possible.
- 5.1.17 - 2018-06-29
    - Improvements
        - In settings panel, added a tool to visualize disk space used and available in the volume where AccessMod stores its data.
    - Bug fixes
        - Some hidden Temporary files were not removed properly. Added a function to force removal. Related to issues #183, #140, #175
- 5.1.16 - 2018-06-29
    - Improvements
        - Write logs in isotropic and anisotropic analysis : disk and memory expected values and what is available at start.
        - Stop analysis before launching a process that would have took too much ressources
    - Bug fixes
        - Solved issue #191 [ 2018-06-27 - 2018-06-29 ], #182 [ 2017-11-06 - 2018-06-29 ] when processes were killed after memory outage. Lower memory attribution to processes.

- 5.1.15 - 2018-06-11
    - Improvements
        - Module data : added a checkbox button to filter datasets from the latest analysis.It will be displayed only after an anlysis.
    - Bug fixes 
        - Re-enabled logs modules

- 5.1.14 - 2018-06-10
    - Improvements 
        - Added rewritten filters system in the data manager. UI did not change.
        - New way to select data computed after an analysis. The previous method had some issue: new computed data was sometimes selected unexpectedly after a filtering process

- 5.1.13 - 2018-06-08
    - Improvements
        - Removed server side filters (replaced by the quick selection bar above each table)
        - Observer isolation : all observers are paused until the corresponding tab is loaded;
        - Added prefix option for archives file name creation 
        - Added a button to delete archive
        - New toolbox tab with "merge landover" tool and "raster preview"; minor changes; 
    - Bug fixes
        - Solved issue #188 [ 2018-03-10 - 2018-06-08 ]: an error was raised when the same id was present in both "from" and "to" facility selection set, resulting in lost facilities referral evaluation 

- 5.1.12 - 2018-05-02
    - Bug fixes
        - Solved selector list creation issue in handsontable when undefined values / null values found in array.

- 5.1.11 - 2018-04-06
    - Bug fixes
        - Solved selection of multiple data to export 

- 5.1.10 - 2018-02-26
    - Improvements
        - Added basic tool to simplify facilities selection in tables, client side.
