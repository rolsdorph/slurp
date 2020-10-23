SHELL := /bin/bash

vimux-run-command:
	./fetchCerts.sh && source loadCreds.sh && stack run hue-metrics-server-exe
vimux-build-command:
	stack build
