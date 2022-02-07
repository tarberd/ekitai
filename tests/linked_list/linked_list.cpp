#include <cstdint>
#include <fmt/core.h>
#include <fmt/format.h>
#include <string>
#include <variant>

using std::string;

struct List {
  enum class Tag : int64_t {
    Nil,
    Cons,
  };

  struct Cons_body {
    int64_t _0;
    List *_1;
  };

  Tag tag;
  union {
    Cons_body cons;
  };
};

template <> struct fmt::formatter<List::Cons_body> : formatter<string> {
  // parse is inherited from formatter<string_view>.
  template <typename FormatContext>
  auto format(List::Cons_body opt, FormatContext &ctx) {
    return formatter<string>::format(fmt::format("{}, {}", opt._0, *opt._1),
                                     ctx);
  }
};

template <> struct fmt::formatter<List> : formatter<string> {
  // parse is inherited from formatter<string_view>.
  template <typename FormatContext> auto format(List opt, FormatContext &ctx) {
    string variant;
    using Tag = List::Tag;
    switch (opt.tag) {
    case Tag::Nil:
      variant = fmt::format("Nil()");
      break;
    case Tag::Cons:
      variant = fmt::format("Cons({})", opt.cons);
      break;
    default:
      variant = fmt::format("<unknown:{}>", static_cast<int64_t>(opt.tag));
    }
    variant = fmt::format("List::{}", variant);

    return formatter<string>::format(variant, ctx);
  }
};

extern "C" {
  List new_empty();
}

int main() {
  using Tag = List::Tag;
  using Cons_body = List::Cons_body;
  List empty = new_empty();
  fmt::print("empty : {}\n", empty);
  return 0;
}
