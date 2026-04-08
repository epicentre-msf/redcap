# Changelog

## redcap 0.2.0

- records are fetched in batches by default with
  [`fetch_records()`](https://epicentre-msf.github.io/redcap/reference/fetch_records.md)

## redcap 0.1.0.9000

- [`project_logging()`](https://epicentre-msf.github.io/redcap/reference/project_logging.md)
  now returns column `timestamp` as class POSIXct by default. New
  logical argument `timestamp_to_posix` to control.

- Implemented additional API methods in new functions:

  - [`meta_arms()`](https://epicentre-msf.github.io/redcap/reference/meta_arms.md)
  - [`project_dags()`](https://epicentre-msf.github.io/redcap/reference/project_dags.md)
  - [`project_info()`](https://epicentre-msf.github.io/redcap/reference/project_info.md)
  - [`project_users()`](https://epicentre-msf.github.io/redcap/reference/project_users.md)
  - [`project_users_dags()`](https://epicentre-msf.github.io/redcap/reference/project_users_dags.md)

## redcap 0.1.0

- Initial release
