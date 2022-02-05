#include <cstdint>
#include <fmt/core.h>
#include <fmt/format.h>
#include <string>
#include <variant>

using std::string;

struct Vec {
  enum class Tag : int64_t {
    Vec,
  };

  struct Vec_body {
    int64_t _0;
    int64_t _1;
  };

  Tag tag;
  union {
    Vec_body vec;
  };
};

template <> struct fmt::formatter<Vec::Vec_body> : formatter<string> {
  // parse is inherited from formatter<string_view>.
  template <typename FormatContext>
  auto format(Vec::Vec_body opt, FormatContext &ctx) {
    return formatter<string>::format(fmt::format("{}, {}", opt._0, opt._1),
                                     ctx);
  }
};

template <> struct fmt::formatter<Vec> : formatter<string> {
  // parse is inherited from formatter<string_view>.
  template <typename FormatContext> auto format(Vec opt, FormatContext &ctx) {
    string variant;
    using Tag = Vec::Tag;
    switch (opt.tag) {
    case Tag::Vec:
      variant = fmt::format("Vec({})", opt.vec);
      break;
    default:
      variant = fmt::format("<unknown:{}>", static_cast<int64_t>(opt.tag));
    }
    variant = fmt::format("Vec::{}", variant);

    return formatter<string>::format(variant, ctx);
  }
};

struct Mat {
  enum class Tag : int64_t {
    Mat,
  };

  struct Mat_body {
    Vec _0;
    Vec _1;
  };

  Tag tag;
  union {
    Mat_body mat;
  };
};

template <> struct fmt::formatter<Mat::Mat_body> : formatter<string> {
  // parse is inherited from formatter<string_view>.
  template <typename FormatContext>
  auto format(Mat::Mat_body opt, FormatContext &ctx) {
    return formatter<string>::format(fmt::format("{}, {}", opt._0, opt._1),
                                     ctx);
  }
};

template <> struct fmt::formatter<Mat> : formatter<string> {
  // parse is inherited from formatter<string_view>.
  template <typename FormatContext> auto format(Mat opt, FormatContext &ctx) {
    string variant;
    using Tag = Mat::Tag;
    switch (opt.tag) {
    case Tag::Mat:
      variant = fmt::format("Mat({})", opt.mat);
      break;
    default:
      variant = fmt::format("<unknown:{}>", static_cast<int64_t>(opt.tag));
    }
    variant = fmt::format("Mat::{}", variant);

    return formatter<string>::format(variant, ctx);
  }
};

extern "C" {
int64_t get_or_zero(Mat);
Mat new_mat(int64_t, int64_t, int64_t, int64_t);
Vec multiply_mat_vec(Mat, Vec);
}

int main() {
  using Tag = Mat::Tag;
  using Mat_body = Mat::Mat_body;
  Mat test_mat = new_mat(1, 2, 3, 4);
  Vec test_vec = Vec{Vec::Tag::Vec, Vec::Vec_body{2, 1}};
  fmt::print("mat : {}\n", test_mat);
  fmt::print("vec : {}\n", test_vec);
  fmt::print("mat * vec : {}\n", multiply_mat_vec(test_mat, test_vec));
  return 0;
}
