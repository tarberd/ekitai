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

  //   Option(Some_body some) : tag{Tag::Some}, some{some} {}
};

template <> struct fmt::formatter<Option::Some_body> : formatter<string> {
  template <typename FormatContext>
  auto format(Option::Some_body opt, FormatContext &ctx) {
    string formatted = fmt::format("{}", opt._0);
    return formatter<string>::format(formatted, ctx);
  }
};

template <> struct fmt::formatter<Option> : formatter<string> {
  // parse is inherited from formatter<string_view>.
  template <typename FormatContext>
  auto format(Option opt, FormatContext &ctx) {
    string variant;
    using Tag = Option::Tag;
    switch (opt.tag) {
    case Tag::None:
      variant = fmt::format("None()");
      break;
    case Tag::Some:
      variant = fmt::format("Some({})", opt.some);
      break;
    default:
      variant = fmt::format("<unknown>");
    }
    variant = fmt::format("Option::{}", variant);

    return formatter<string>::format(variant, ctx);
  }
};

extern "C" {
Option match_option(Option input);
}

int main() {
  Option opt{Option::Tag::Some, Option::Some_body{15}};
  fmt::print("{}\n", match_option(opt));
  return 0;
}
