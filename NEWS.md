# redcap 0.2.0

* records are fetched in batches by default with `fetch_records()`
  
# redcap 0.1.0.9000

* `project_logging()` now returns column `timestamp` as class POSIXct by
default. New logical argument `timestamp_to_posix` to control.

* Implemented additional API methods in new functions:
  * `meta_arms()`
  * `project_dags()`
  * `project_info()`
  * `project_users()`
  * `project_users_dags()`

# redcap 0.1.0

* Initial release
