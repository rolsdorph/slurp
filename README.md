# Slurp

Slurp is a data collection platform. It fetches data from various third parties (*sources*) and forwards it
to other third party services (*sinks*).

Currently, it supports two types of sources (the Philips Hue APIs and simple JSON) and one type of sink (Influx).

## Components
Slurp is deployed as four Haskell binaries that communicate using RabbitMQ. The user interface is provided by
an Elm web application.

### Collector
The collector scrapes all registered sources and forwards the collected data to the data queue. When it collects
a source it also publishes a message to the notification queue.

### Influxpusher
The Influx pusher listens for data on the data queue and forwards it to the correct Influx sinks. When
it pushes to a sink it also publishes a message to the notification queue.

### Notifier
The notifier allows user to connect to a WebSocket and to authenticate by providing a token.
It listens for messages on the notification queue and forwards them to the appropriate WebSockets.

### API
The api module provides the REST APIs used by the web application for user/sink/source registration.

## Dependencies
Building Slurp should be quite straight-forward, and requires:
- A Haskell toolchain for building the backend pieces
- The libpq library (needed by `HDBC-postgresql`)
- An Elm toolchain for building the frontend application

The minimal dependencies for running Slurp are:
- A Postgres database
- A RabbitMQ instance with two queues (one for transmitting the collected data, one for user notifications)

For local development, `docker-compose.yml` provides both of these.

In order to provide the full functionality, you'll also need:
- A Google OAuth client (for the Sign in with Google button)
- A Philips Hue developer account (for collecting Hue homes)

## Local development quickstart

1) Change the Postgres/RabbitMQ credentials in `.env` to whatever you'd like
2) `docker-compose up` to start Postgres/RabbitMQ
3) `./server.sh start` to start all components
4) At this point a working application should be available at `http://localhost:8080`

The Google login won't work locally, but the `POST /insecureAuth` endpoint provides a semi-quick way of obtaining a token for
local development. After obtaining an insecure token and injecting it into the app (`localStorage.authToken="(...)"`)
you should be ready to add sources and sinks at using the web interface at `http://localhost:8080`.

The E2E test at `e2e/Spec.hs` demonstrates the full API for a simple JSON source and an Influx sink.

### Providing secrets
The applications read their data store config from environment variables, which in turn are read from the `.env` file if the Makefiles are used to run the components.
Providing postgres and RabbitMQ configuration in that file (or directly in the relevant environment variables) is enough
to get the application up and running.

Additionally, a Philips hue client id and secret (see [developers.meethue.com](https://developers.meethue.com/) for details)
must be provided in `api/.env` (or directly in the corresponding environment variables) in order to ingest Philips Hue home data.

For Google Sign-In to work, a Google OAuth client id and secret must be provided in `.api/env` (see [Get your Google API client ID](https://developers.google.com/identity/gsi/web/guides/get-google-api-clientid))
for details). Additionally, the client ID must be provided in the `<meta name="google-signin-client_id" (...)` tag in `api/login-landing.html`.
