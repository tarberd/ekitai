#include <cstdint>
#include <fmt/core.h>
#include <fmt/format.h>
#include <string>
#include <variant>

using std::string;

struct Mat {
  enum class Tag : int64_t {
    Mat,
    Some,
  };

  struct Mat_body {
    int64_t _0;
  };

  Tag tag;
  union {
    Mat_body some;
  };
};

template <> struct fmt::formatter<Mat::Mat_body> : formatter<string> {
  // parse is inherited from formatter<string_view>.
  template <typename FormatContext>
  auto format(Mat::Mat_body opt, FormatContext &ctx) {
    return formatter<string>::format(fmt::format("{}", opt._0), ctx);
  }
};

template <> struct fmt::formatter<Mat> : formatter<string> {
  // parse is inherited from formatter<string_view>.
  template <typename FormatContext> auto format(Mat opt, FormatContext &ctx) {
    string variant;
    using Tag = Mat::Tag;
    switch (opt.tag) {
    case Tag::Some:
      variant = fmt::format("Some({})", opt.some);
      break;
    case Tag::Mat:
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
int64_t get_or_zero(Mat);
Mat from(int64_t);
Mat map_double(Mat);
Mat multiply(Mat, Mat);
Mat fibonacci(Mat);
int64_t fibonacci_native(int64_t);
}

int main() {
  using Tag = Mat::Tag;
  using Some_body = Mat::Mat_body;
  Mat some15{Tag::Some, Some_body{15}};
  Mat some10{Tag::Some, Some_body{10}};
  Mat some1{Tag::Some, Some_body{6}};
  Mat none{Tag::Mat};
  fmt::print("get_or_zero(Some(15)) : {}\n", get_or_zero(some15));
  fmt::print("get_or_zero(None): {}\n", get_or_zero(none));
  fmt::print("from({}): {}\n", 16, from(16));
  fmt::print("map_double({}): {}\n", some15, map_double(some15));
  fmt::print("multiply({}, {}): {}\n", some15, some10,
             multiply(some15, some10));
  fmt::print("fibonacci({}): {}\n", some1, fibonacci(some1));
  fmt::print("fibonacci_native({}): {}\n", 6, fibonacci_native(6));
  return 0;
}
