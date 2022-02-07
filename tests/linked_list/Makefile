
run: all
	./build/linked_list

all: out.o
	cmake -B build
	cmake --build build

out.ll: linked_list.eki
	cargo run linked_list.eki

out.o: out.ll
	clang -c out.ll

clean:
	rm out.ll out.o
	rm -rf build
