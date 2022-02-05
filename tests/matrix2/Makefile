
run: all
	./build/matrix2

all: out.o
	cmake -B build
	cmake --build build

out.ll: matrix2.eki
	cargo run matrix2.eki

out.o: out.ll
	clang -c out.ll

clean:
	rm out.ll out.o
	rm -rf build
