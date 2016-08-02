.PHONY: deps build_and_run build run clean clean_deps clean_all

build_and_run: build run

all: clean_all deps build_and_run

deps:
	@typings install

build:
	@tsc

run:
	@./life

clean:
	@rm life.js

clean_deps:
	@rm -rf ./typings

clean_all: clean clean_deps
