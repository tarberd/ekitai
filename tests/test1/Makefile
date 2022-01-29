
run: all
	./build/hello

all: out.o
	cmake -B build
	cmake --build build

out.ll: goo.eki
	cargo run goo.eki

out.o: out.ll
	clang -c out.ll

clean:
	rm out.ll out.o
	rm -rf build
