profiling-build:
	stack install --library-profiling --executable-profiling

profiling-run: profiling-build
	stack exec -- hs-websocket-server +RTS -hc -P -p

build:
	stack install

run: build
	stack exec hs-websocket-server
