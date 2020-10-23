SHELL := /bin/bash

deploy-all:
	stack build && (make -C website & make -C notifier & make -C collector)
vimux-build-command:
	stack build
vimux-test-command:
	stack test
