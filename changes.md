## List of changes in AccessMod

- 5.1.14
    - Added rewritten filters system in the data manager. UI did not change.
    - New way to select data computed after an analysis. The previous method had some issue: new computed data was sometimes selected unexpectedly after a filtering process

- 5.1.13 
    - Solved issue #188: an error was raised when the same id was present in both "from" and "to" facility selection set, resulting in lost facilities referral evaluation 
    - Removed server side filters (replaced by the quick selection bar above each table)
    - Observer isolation : all observers are paused until the corresponding tab is loaded;
    - Added prefix option for archives file name creation 
    - Delete archive button
    - New toolbox tab with "merge landover" tool and "raster preview"; minor changes; 

- 5.1.12 
  - Solved selector list creation issue in handsontable when undefined values / null values found in array.

- 5.1.11
  - Solved selection of multiple data to export 

- 5.1.8 -> 5.1.10 
  - Added basic tool to simplify facilities selection in tables, client side.
  - Minor changes

