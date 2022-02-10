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
List new_single(int64_t);
List new_double(int64_t, int64_t);

List* new_empty_ptr();
List* new_single_ptr(int64_t);
List* new_double_ptr(int64_t, int64_t);

List* add_cons(int64_t, List*);
int64_t accumulate(int64_t, List*);
List* map_square(List*);
}

int main() {
  using Tag = List::Tag;
  using Cons_body = List::Cons_body;
  auto empty = new_empty();
  fmt::print("empty : {}\n", empty);
  auto single = new_single(5);
  fmt::print("single : {}\n", single);
  auto ndouble = new_double(14, 5);
  fmt::print("double : {}\n", ndouble);
  auto empty_ptr = new_empty_ptr();
  fmt::print("empty_ptr : {}\n", *empty_ptr);
  auto single_ptr = new_single_ptr(5);
  fmt::print("single_ptr : {}\n", *single_ptr);
  auto ndouble_ptr = new_double_ptr(14, 5);
  fmt::print("double_ptr : {}\n", *ndouble_ptr);
  auto added_5 = add_cons(5, ndouble_ptr);
  fmt::print("add_cons(5, double_ptr) : {}\n", *added_5);
  auto acc = accumulate(0, added_5);
  fmt::print("accumulate(0, added_5) : {}\n", acc);
  auto map = map_square(added_5);
  fmt::print("map_square(added_5) : {}\n", *map);
  return 0;
}
