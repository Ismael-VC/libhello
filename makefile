all: setup shared static included precompiled dynamic optimized fortran \
	julia python

setup:
	@ mkdir -p bin lib build
	
shared: setup
	@ echo -e "\e[35mUsing shared library.\e[m"
	@ gcc -fPIC -shared -o lib/libhello.so src/libhello.c -Iinclude
	@ gcc -o bin/shared_hello src/hello.c -Llib -lhello -Iinclude
	@ LD_LIBRARY_PATH=lib bin/shared_hello

static: setup
	@ echo -e "\e[35mUisng static library.\e[m"
	@ gcc -c -o build/libhello.o src/libhello.c -Iinclude
	@ ar rcs lib/libhello.a build/libhello.o
	@ gcc -o bin/static_hello src/hello.c lib/libhello.a -Iinclude
	@ bin/static_hello

included: setup
	@ echo -e "\e[35mUsing included library.\e[m"
	@ gcc -o bin/included_hello src/included_hello.c -Iinclude
	@ bin/included_hello

precompiled: setup
	@ echo -e "\e[35mUsing precompiled header and code.\e[m"
	@ gcc -x c-header include/libhello.h -o build/libhello.h.gch
	@ gcc -c src/libhello.c -o build/libhello.o -Iinclude
	@ gcc -o bin/precompiled_hello src/hello.c build/libhello.o -include include/libhello.h -Iinclude
	@ bin/precompiled_hello

dynamic: setup
	@ echo -e "\e[35mUsing dynamic linking.\e[m"
	@ gcc -fPIC -shared -o lib/libhello.so src/libhello.c -Iinclude
	@ gcc -o bin/dynamic_hello src/dynamic_hello.c -ldl -Iinclude
	@ bin/dynamic_hello

optimized: setup
	@ echo -e "\e[35mUsing link time optimization.\e[m"
	@ gcc -c -O2 -flto src/libhello.c -o build/libhello.o -Iinclude
	@ gcc -c -O2 -flto src/hello.c -o build/hello.o -Iinclude
	@ gcc -O2 -flto build/hello.o build/libhello.o -o bin/optimized_hello -Iinclude
	@ bin/optimized_hello

fortran: setup
	@ echo -e "\e[35mUsing Fortran bindings.\e[m"
	@ gcc -fPIC -shared -o lib/libhello.so src/libhello.c -Iinclude
	@ gfortran -c -o build/hello_f90.o src/hello.f90 -Jbuild -Iinclude
	@ gfortran -o bin/fortran_hello build/hello_f90.o -Llib -lhello -Jbuild
	@ LD_LIBRARY_PATH=lib bin/fortran_hello

cobol: setup
	@ echo -e "\e[35mUsing Cobol bindings.\e[m"
	@ gcc -fPIC -shared -o lib/libhello.so src/libhello.c -Iinclude
	@ cobc -O2 -o bin/cobol_hello src/hello.cbl -Llib -lhello
	@ LD_LIBRARY_PATH=lib bin/cobol_hello

julia: setup
	@ echo -e "\e[35mUsing Julia bindings.\e[m"
	@ gcc -fPIC -shared -o lib/libhello.so src/libhello.c -Iinclude
	@ julia src/hello.jl

python: setup
	@ echo -e "\e[35mUsing Python bindings.\e[m"
	@ gcc -fPIC -shared -o lib/libhello.so src/libhello.c -Iinclude
	@ python src/hello.py
	
clean:
	@ rm -rf build bin lib
