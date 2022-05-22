# SOFIA 2.0.0 (2022-05-22)

* Dropped legacy support for 'stocks.combined' in addDriors() and addEffort().

* Added errors and warnings if stock names in catch/effort/priors don't match.

* plotCat() treats the first two columns as 'stock' and 'year'.




# SOFIA 1.2.1 (2022-05-21)

* Renamed argument 'stocks.combined' to 'same.priors' in addDriors().

* Renamed argument 'stocks.combined' to 'same.effort' in addEffort().

* The old argument name 'stocks.combined' is still supported in both addDriors()
  and addEffort() for backward compatibility. The new argument names
  'same.priors' and 'same.effort' should be used in new SOFIA scripts.




# SOFIA 1.2.0 (2022-02-25)

* Added function convertData() to convert primary data to combined data.

* Improved groupData() so it exits gracefully when no CSV files are found.
  Furthermore, it always creates a new set of empty subdirectories for the
  output before copying files.




# SOFIA 1.1.0 (2022-02-20)

* Added function groupData() to group primary data in subdirectories.




# SOFIA 1.0.3 (2022-02-14)

* Improved addDriors() so effort data are passed to format_driors().




# SOFIA 1.0.2 (2022-01-24)

* Initial release.
