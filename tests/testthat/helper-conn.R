
# define connections
conn_test <- rconn(
  url = "https://www.research.epicentre.msf.org/api/",
  token = Sys.getenv("REDCAP_PKG")
)


conn_fake <- rconn(
  url = "https://www.research.epicentre.msf.org/api/",
  token = "AAAAAAAAA" # fake token
)

