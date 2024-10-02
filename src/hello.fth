c-library hello
	s" libhello" add-lib
	\c #include "libhello.h"
	c-function hello hello -- void
end-c-library

hello bye
