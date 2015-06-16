#include <string>
#include <boost/algorithm/string.hpp>

using std::string;

namespace bob {
  bool is_shouting(const string &input) {
    auto upper = boost::to_upper_copy(input);
    auto lower = boost::to_lower_copy(input);
    return input == upper && input != lower;
  }

  string hey(const string &input) {
    if (is_shouting(input)) {
      return "Whoa, chill out!";
    }
    else {
      return "Whatever.";
    }
  }
}
