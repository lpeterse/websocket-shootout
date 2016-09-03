profiling-build:
	stack install --library-profiling --executable-profiling

profiling-run: profiling-build
	stack exec -- hs-websocket-server +RTS -p

build:
	stack install

run:
	stack exec hs-websocket-server
