EXAMPLES=node erlang-plists haskell
.PHONY: $(EXAMPLES)

build: node erlang-plists haskell

node:
	@make -C node build

erlang-plists:
	@make -C erlang-plists build

haskell:
	@make -C haskell build

run:
	@mkdir -p data/
	@./runall.sh $(EXAMPLES) | tee data/10times.txt

