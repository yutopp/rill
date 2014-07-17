cd build

cmake ../. -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DBOOST_ROOT=/usr

make && \
    ./tools/compiler/rillc --rill-rt-lib-path rill-rt/lib/rill-rt/librill-rt.a ../tools/compiler/samples/rec.rill && \
    ./a.out
