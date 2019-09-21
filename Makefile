.SILENT:

.PHONY: all
all: build graph stat-complexity

.PHONY: build
build:
	echo 'stack build'; \
	stack build

.PHONY: graph
graph:
	echo 'exporting dependency graph to ./data/modules.pdf and ./data/modules.png'; \
	find src -name "*.hs" | xargs graphmod -q | dot -Tpdf > data/modules.pdf; \
	find src -name "*.hs" | xargs graphmod -q | dot -Tpng -Gdpi=300 > data/modules.png

.PHONY: stat-complexity
stat-complexity:
	echo '🍅 Complexity analysis stats:\n'; \
	homplexity-cli app/Main.hs src/ && echo '\n'

.PHONY: run
run:
	echo 'running...'; \
	stack exec stock-data-server-exe
