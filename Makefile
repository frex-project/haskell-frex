all: build

build:
	cabal build

test: build
	dist/build/test/test

bench: build
	dist/build/bench/bench

clean:
	cabal clean

docker-image:
	docker build -t partially-static .

docker-bench:
	docker run -it --rm -v $$(pwd):/haskell  partially-static sh -c 'cd /haskell && make bench'

.PHONY: all build test bench clean
