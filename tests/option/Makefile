
run: all
	./build/option

all: out.o
	cmake -B build
	cmake --build build

out.ll: option.eki
	cargo run option.eki

out.o: out.ll
	clang -c out.ll

clean:
	rm out.ll out.o
	rm -rf build
