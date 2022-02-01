#include <cstdint>
#include <fmt/core.h>
#include <fmt/format.h>
#include <string>
#include <variant>

using std::string;

struct Option {
  enum class Tag : int64_t {
    None,
    Some,
  };

  struct Some_body {
    int64_t _0;
  };

  Tag tag;
  union {
    Some_body some;
  };
};

template <> struct fmt::formatter<Option::Some_body> : formatter<string> {
  // parse is inherited from formatter<string_view>.
  template <typename FormatContext>
  auto format(Option::Some_body opt, FormatContext &ctx) {
    return formatter<string>::format(fmt::format("{}", opt._0), ctx);
  }
};

template <> struct fmt::formatter<Option> : formatter<string> {
  // parse is inherited from formatter<string_view>.
  template <typename FormatContext>
  auto format(Option opt, FormatContext &ctx) {
    string variant;
    using Tag = Option::Tag;
    switch (opt.tag) {
    case Tag::Some:
      variant = fmt::format("Some({})", opt.some);
      break;
    case Tag::None:
      variant = fmt::format("False()");
      break;
    default:
      variant = fmt::format("<unknown:{}>", static_cast<int64_t>(opt.tag));
    }
    variant = fmt::format("Option::{}", variant);

    return formatter<string>::format(variant, ctx);
  }
};

extern "C" {
int64_t get_or_zero(Option);
Option from(int64_t);
Option map_double(Option);
Option multiply(Option, Option);
}

int main() {
  using Tag = Option::Tag;
  using Some_body = Option::Some_body;
  Option some15{Tag::Some, Some_body{15}};
  Option some10{Tag::Some, Some_body{10}};
  Option none{Tag::None};
  fmt::print("get_or_zero(Some(15)) : {}\n", get_or_zero(some15));
  fmt::print("get_or_zero(None): {}\n", get_or_zero(none));
  fmt::print("from({}): {}\n", 16, from(16));
  fmt::print("map_double({}): {}\n", some15, map_double(some15));
  fmt::print("multiply({}, {}): {}\n", some15, some10,
             multiply(some15, some10));
  return 0;
}
