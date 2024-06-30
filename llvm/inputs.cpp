#include <bit>
#include <cstdint>
#include <iostream>

int main() {
   uint64_t x = 0x001345F532A8470EULL;
   uint64_t y = 0x7FECBA0ACD57B8F1ULL;
   std::cout << std::hexfloat << " x: " << std::bit_cast<double>(x) << "\n  y: "
             << std::bit_cast<double>(y) << std::endl;
}
