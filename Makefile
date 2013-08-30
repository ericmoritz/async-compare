EXAMPLES=python/mp python/gevent python/threaded node erlang-plists haskell
.PHONY: $(EXAMPLES)

build: node erlang-plists haskell python

node:
	@make -C node build

erlang-plists:
	@make -C erlang-plists build

haskell:
	@make -C haskell build

python:
	@make -C python build

run:
	@mkdir -p data/
	@./runall.sh $(EXAMPLES) | tee data/10times.txt

