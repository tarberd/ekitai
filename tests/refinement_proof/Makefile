
run: all
	./build/bool

all: out.o
	cmake -B build
	cmake --build build

out.ll: bool.eki
	cargo run bool.eki

out.o: out.ll
	clang -c out.ll

clean:
	rm out.ll out.o
	rm -rf build
