#include <cstdint>
#include <fmt/core.h>
#include <fmt/format.h>
#include <string>
#include <variant>

using std::string;

extern "C" {
  std::int64_t abs_liquid(std::int64_t);
}

int main() {
  fmt::print("abs(15): {}\n", abs_liquid(15));
  fmt::print("abs(-15): {}\n", abs_liquid(-15));
  return 0;
}
