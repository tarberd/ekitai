#include <cstdint>
#include <fmt/core.h>
#include <fmt/format.h>
#include <string>
#include <variant>

using std::string;

extern "C" {
  std::int64_t id(std::int64_t);
}

int main() {
  fmt::print("id(15): {}\n", id(15));
  fmt::print("id(-15): {}\n", id(-15));
  return 0;
}
