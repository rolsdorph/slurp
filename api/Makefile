SHELL := /bin/bash

include ../.env
include .env

.EXPORT_ALL_VARIABLES:

vimux-run-command:
	./fetchCerts.sh && stack run slurp-exe
vimux-build-command:
	stack build
