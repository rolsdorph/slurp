SHELL := /bin/bash

vimux-run-command:
	source loadCreds.sh && stack run hue-metrics-server-exe
vimux-build-command:
	stack build
vimux-test-command:
	stack test
