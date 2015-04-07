# Usage (only first time):
# 1. make clean
# 2. make sbvClone
# 3. make init
# 4. make

# Usage (regular)
# 1. make

all: build

sbvClone:
	git clone https://github.com/LeventErkok/sbv.git ../sbv

init:
	cabal sandbox init
	cabal sandbox add-source ../sbv
	cabal install --only-dependencies

build:
	cabal build

clean:
	cabal clean
	cabal sandbox delete
