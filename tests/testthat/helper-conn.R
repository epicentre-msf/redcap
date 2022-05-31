
# define connections
conn_test <- rconn(
  url = Sys.getenv("REDCAP_API_URL"),
  token = Sys.getenv("REDCAP_PKG")
)


conn_fake <- rconn(
  url = Sys.getenv("REDCAP_API_URL"),
  token = "AAAAAAAAA" # fake token
)
