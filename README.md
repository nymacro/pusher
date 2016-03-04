# Pusher

Pusher is a small[ish] backend service and API for doing browser Push.


## Limitations
* Only PostgreSQL is currently "supported" (other database can be added, but currently
  PostgreSQL is the only `persistent` backend provided)

* Push notifications can not have content. This will be added sometime soon.


## Configuration

### Example configuration

```
server {
  port = 8080

}

database {
  username = "postgres"
  password = "postgres"
  name     = "denim"
  host     = "localhost"
  port     = 5432
}

push {
  title    = "Push'd!"
  message  = "You got a push notification!"
}
```

### EKG Monitoring

Live monitoring of the running process can be enabled by adding the `monitor` configuration
into your `pusher.cfg`, and providing a `port` number.

```
server {
  monitor {
    port = 6969
  }
}
```

### TLS

TLS can be enabled by adding the following to your `pusher.cfg`. TLS options are currently
limited.

```
server {
  tls {
    port = 4443
    certificate = "default.crt"
    key = "default.key"
  }
}
```
