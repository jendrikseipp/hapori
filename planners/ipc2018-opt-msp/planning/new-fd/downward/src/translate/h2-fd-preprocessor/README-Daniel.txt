mkdir -p builds/release32
cd builds/release32
cmake ../../
make all

Raquel: modify option in Macros.cmake to compile for 64 bits

mkdir -p builds/release64
cd builds/release64
cmake ../../
make all -j4


