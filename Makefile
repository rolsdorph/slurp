SHELL := /bin/bash

vimux-run-command:
	stack build && (make -C frontend & make -C website & make -C notifier & make -C collector & make -C influxpusher)
vimux-build-command:
	stack build
vimux-test-command:
	stack test
