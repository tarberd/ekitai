
run: all
	./build/linked_list

all: out.o
	cmake -B build
	cmake --build build

out.ll: linked_list.eki
	cargo run linked_list.eki --noliquid

out.o: out.ll
	clang -c out.ll -std=c++20

clean:
	rm out.ll out.o
	rm -rf build
