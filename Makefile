SHELL := /bin/bash

vimux-run-command:
	./server.sh start
vimux-build-command:
	stack build
vimux-test-command:
	stack test
