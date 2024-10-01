from ctypes import CDLL as dlopen

libhello = dlopen('lib/libhello.so') 

libhello.hello.argtypes = []
libhello.hello.restype = None

def hello():
    libhello.hello()

if __name__ == "__main__":
    hello()
