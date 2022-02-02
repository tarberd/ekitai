#include <cstdint>
#include <fmt/core.h>
#include <fmt/format.h>
#include <string>
#include <variant>

using std::string;

struct Bool {
  enum class Tag : int64_t {
    True,
    False,
  };

  Tag tag;
};

template <> struct fmt::formatter<Bool> : formatter<string> {
  // parse is inherited from formatter<string_view>.
  template <typename FormatContext> auto format(Bool opt, FormatContext &ctx) {
    string variant;
    using Tag = Bool::Tag;
    switch (opt.tag) {
    case Tag::True:
      variant = fmt::format("True()");
      break;
    case Tag::False:
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
Bool bool_not(Bool);
Bool bool_and(Bool, Bool);
Bool bool_or(Bool, Bool);
bool into_native(Bool);
Bool from_native(bool);
Bool and_with_native(Bool, Bool);
}

int main() {
  Bool bfalse{Bool::Tag::False};
  Bool btrue{Bool::Tag::True};
  fmt::print("not false: {}\n", bool_not(bfalse));
  fmt::print("not true: {}\n", bool_not(btrue));
  fmt::print("not false and true: {}\n", bool_and(bool_not(bfalse), btrue));
  fmt::print("not false or true: {}\n", bool_or(bool_not(bfalse), btrue));
  fmt::print("into_native(True): {}\n", into_native(btrue));
  fmt::print("into_native(False): {}\n", into_native(bfalse));
  fmt::print("from_native(true): {}\n", from_native(true));
  fmt::print("from_native(false): {}\n", from_native(false));
  fmt::print("and_with_native({}, {}): {}\n", btrue, bfalse,
             and_with_native(btrue, bfalse));
  fmt::print("and_with_native({}, {}): {}\n", btrue, btrue,
             and_with_native(btrue, btrue));
  return 0;
}
