all: setup shared static included precompiled dynamic optimized

setup:
	@ mkdir -p bin lib
	
shared: setup
	@ echo -e "\e[35mUsing shared library.\e[m"
	@ gcc -fPIC -shared -o lib/libhello.so src/libhello.c
	@ gcc -o bin/shared_hello src/hello.c -Llib -lhello
	@ LD_LIBRARY_PATH=lib bin/shared_hello

static: setup
	@ echo -e "\e[35mUisng static library.\e[m"
	@ gcc -c -o lib/libhello.o src/libhello.c
	@ ar rcs lib/libhello.a lib/libhello.o
	@ gcc -o bin/static_hello src/hello.c lib/libhello.a
	@ bin/static_hello

included: setup
	@ echo -e "\e[35mUsing included library.\e[m"
	@ gcc -o bin/included_hello src/included_hello.c
	@ bin/included_hello

precompiled: setup
	@ echo -e "\e[35mUsing precompiled header and code.\e[m"
	@ gcc -x c-header src/libhello.h -o lib/libhello.h.gch
	@ gcc -c src/libhello.c -o lib/libhello.o
	@ gcc -o bin/precompiled_hello src/hello.c lib/libhello.o -include src/libhello.h
	@ bin/precompiled_hello

dynamic: setup
	@ echo -e "\e[35mUsing dynamic linking.\e[m"
	@ gcc -fPIC -shared -o lib/libhello.so src/libhello.c
	@ gcc -o bin/dynamic_hello src/dynamic_hello.c -ldl
	@ bin/dynamic_hello

optimized: setup
	@ echo -e "\e[35mUsing link time optimization.\e[m"
	@ gcc -c -O2 -flto src/libhello.c -o lib/libhello.o
	@ gcc -c -O2 -flto src/hello.c -o lib/hello.o
	@ gcc -O2 -flto lib/hello.o lib/libhello.o -o bin/optimized_hello
	@ bin/optimized_hello
