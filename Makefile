all: shared static included precompiled dynamic optimized fortran freebasic go \
	julia python r cpp luajit zig rust ruby ada gforth clisp       
	# cobol

setup:
	@ mkdir -p bin lib build

libhello.so: setup
	@ echo -e "\e[31m\nBuilding shared library.\e[m"
	@ time -p sh -c "gcc -fPIC -shared -o lib/libhello.so src/libhello.c -Iinclude"

libhello.a: setup
	@ echo -e "\e[31m\nBuilding static library.\e[m"
	@ time -p sh -c "gcc -c -o build/libhello.o src/libhello.c -Iinclude && \
		ar rcs lib/libhello.a build/libhello.o"

shared: libhello.so
	@ echo -e "\e[35m\nUsing shared library.\e[m"
	@ time -p sh -c "gcc -o bin/shared_hello src/hello.c -Llib -lhello -Iinclude  && \
		LD_LIBRARY_PATH=lib bin/shared_hello"

static: libhello.a
	@ echo -e "\e[35m\nUisng static library.\e[m"
	@ time -p sh -c "gcc -o bin/static_hello src/hello.c lib/libhello.a -Iinclude && \
		bin/static_hello"

included: setup
	@ echo -e "\e[35m\nUsing included library.\e[m"
	@ time -p sh -c "gcc -o bin/included_hello src/included_hello.c -Iinclude && \
		bin/included_hello"

precompiled: setup
	@ echo -e "\e[35m\nUsing precompiled header and code.\e[m"
	@ time -p sh -c "gcc -x c-header include/libhello.h -o build/libhello.h.gch && \
		gcc -c src/libhello.c -o build/libhello.o -Iinclude && \
		gcc -o bin/precompiled_hello src/hello.c build/libhello.o -include include/libhello.h -Iinclude && \
		bin/precompiled_hello"

dynamic: libhello.so
	@ echo -e "\e[35m\nUsing dynamic linking.\e[m"
	@ time -p sh -c "gcc -o bin/dynamic_hello src/dynamic_hello.c -ldl -Iinclude && \
		bin/dynamic_hello"

optimized: setup
	@ echo -e "\e[35m\nUsing link time optimization.\e[m"
	@ time -p sh -c "gcc -c -O2 -flto src/libhello.c -o build/libhello.o -Iinclude && \
		gcc -c -O2 -flto src/hello.c -o build/hello.o -Iinclude && \
		gcc -O2 -flto build/hello.o build/libhello.o -o bin/optimized_hello -Iinclude && \
		bin/optimized_hello"

fortran: libhello.so
	@ echo -e "\e[35m\nUsing Fortran bindings.\e[m"
	@ time -p sh -c "gfortran -c -o build/hello_f90.o src/hello.f90 -Jbuild -Iinclude && \
		gfortran -o bin/fortran_hello build/hello_f90.o -Llib -lhello -Jbuild && \
		LD_LIBRARY_PATH=lib bin/fortran_hello"

cobol: libhello.so
	@ echo -e "\e[35m\nUsing Cobol bindings.\e[m"
	@ time -p -sh -c "cobc -O2 -o bin/cobol_hello src/hello.cbl -Llib -lhello && \
		LD_LIBRARY_PATH=lib bin/cobol_hello"

julia: libhello.so
	@ echo -e "\e[35m\nUsing Julia bindings.\e[m"
	@ time -p sh -c "julia src/hello.jl"

python: libhello.so
	@ echo -e "\e[35m\nUsing Python bindings.\e[m"
	@ time -p sh -c "python src/hello.py"

r: libhello.so
	@ echo -e "\e[35m\nUsing R bindings.\e[m"
	@ time -p sh -c "Rscript src/hello.R"

cpp: libhello.so
	@ echo -e "\e[35m\nUsing C++ bindings.\e[m"
	@ time -p sh -c "g++ -o bin/cpp_hello src/hello.cpp -Llib -lhello -Iinclude && \
		LD_LIBRARY_PATH=lib bin/cpp_hello"

luajit: libhello.so
	@ echo -e "\e[35m\nUsing LuaJIT bindings.\e[m"
	@ time -p sh -c "luajit src/hello.lua"

zig: libhello.so
	@ echo -e "\e[35m\nUsing Zig bindings.\e[m"
	@ time -p sh -c "zig build-obj src/hello.zig -Llib -lhello -lc -femit-bin=build/zig_hello.o > /dev/null 2>&1 && \
		zig build-exe build/zig_hello.o -Llib -lhello -lc -femit-bin=bin/zig_hello > /dev/null 2>&1 && \
		LD_LIBRARY_PATH=lib bin/zig_hello"

rust: libhello.so
	@ echo -e "\e[35m\nUsing Rust bindings.\e[m"
	@ time -p sh -c "cd src/rust_hello && cargo build --quiet --release && \
		LD_LIBRARY_PATH=../../lib target/release/rust_hello"

clisp: libhello.so
	@ echo -e "\e[35m\nUsing CLisp bindings.\e[m"
	@ time -p sh -c "clisp src/hello.lisp"

ruby: libhello.so
	@ echo -e "\e[35m\nUsing Ruby bindings.\e[m"
	@ time -p sh -c "ruby src/hello.rb"

ada: libhello.so
	@ echo -e "\e[35m\nUsing Ada bindings.\e[m"
	@ time -p sh -c "gnatmake -q -c -Iinclude -o build/hello.o src/hello.adb && \
		mv hello.ali hello.o libhello.ali libhello.o build && \
		gnatbind -x build/hello.ali && \
		gnatlink build/hello.ali -Llib -lhello -o bin/ada_hello && \
		LD_LIBRARY_PATH=lib bin/ada_hello"

go: libhello.so
	@ echo -e "\e[35m\nUsing Go bindings.\e[m"
	@ time -p sh -c "go build -o bin/go_hello src/hello.go && \
		LD_LIBRARY_PATH=lib bin/go_hello"

freebasic: libhello.a
	@ echo -e "\e[35m\nUsing FreeBASIC bindings.\e[m"
	@ time -p sh -c "cp lib/libhello.a liblibhello.a && \
		fbc -a lib/libhello.a src/hello.bas -x bin/freebasic_hello && \
		rm liblibhello.a && bin/freebasic_hello"

gforth: libhello.so
	@ echo -e "\e[35m\nUsing GForth bindings.\e[m"
	@ time -p sh -c "C_INCLUDE_PATH=include LIBRARY_PATH=lib LD_LIBRARY_PATH=lib gforth src/hello.fth"

clean:
	@ rm -rf build bin lib src/rust_hello/target
