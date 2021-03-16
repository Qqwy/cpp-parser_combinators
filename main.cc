#include <variant>
#include <functional>

#include <string>
#include <istream>

struct ErrorMessage {
  std::string message;
};


// // General template
// template<typename... Ts>
// struct AutoTuple : public std::tuple<Ts...> {
//   using std::tuple<Ts...>::tuple;
// };

// // combining autotuples
// template<typename... As, typename... Bs> AutoTuple<As..., Bs...> operator+(AutoTuple<As...> lhs, AutoTuple<Bs...> rhs) {
//   return std::tuple_cat(lhs, rhs);
// }

// template<typename A, typename... Bs> AutoTuple<A, Bs...> operator+(A lhs, AutoTuple<Bs...> rhs) {
//   return std::tuple_cat(std::make_tuple(lhs), rhs);
// }

// template<typename... As, typename B> AutoTuple<As..., B> operator+(AutoTuple<As...> lhs, B rhs) {
//   return std::tuple_cat(lhs, std::make_tuple(rhs));
// }

template<typename A, typename B>
std::tuple<A, B> super_tuple_cat(A lhs, B rhs) {
  return std::make_tuple(lhs, rhs);
}

template<typename...As, typename ...Bs>
std::tuple<As..., Bs...> super_tuple_cat(std::tuple<As...> lhs, std::tuple<Bs...> rhs) {
  return std::tuple_cat(lhs, rhs);
}
template<typename A, typename ...Bs>
std::tuple<A, Bs...> super_tuple_cat(A lhs, std::tuple<Bs...> rhs) {
  return std::tuple_cat(std::make_tuple(lhs), rhs);
}

template<typename... As, typename B>
std::tuple<As..., B> super_tuple_cat(std::tuple<As...> lhs, B rhs) {
  return std::tuple_cat(lhs, std::make_tuple(rhs));
}


template<typename T>
struct Result : public std::variant<T, ErrorMessage> {
  using std::variant<T, ErrorMessage>::variant;

  typedef T value_type;

  operator bool() const {
    return !std::holds_alternative<ErrorMessage>(*this);
  }
};

template<typename A, typename B> auto operator +(Result<A> lhs, Result<B> rhs) -> Result<decltype(super_tuple_cat(std::get<A>(lhs), std::get<B>(rhs)))> {
  if(!lhs) {
    return std::get<ErrorMessage>(lhs);
  }

  if(!rhs) {
    return std::get<ErrorMessage>(rhs);
  }

  return super_tuple_cat(std::get<A>(lhs), std::get<B>(rhs));
};

template<typename A> auto operator |(Result<A> lhs, Result<A> rhs) {
  if(lhs) {
    return lhs;
  }

  return rhs;
}

// template<typename ...As, typename B> Result<std::tuple<As..., B>> operator+(Result<std::tuple<As...> lhs, Result<B> rhs) {
//   if(!lhs) {
//     return lhs;
//   }

//   if(!rhs) {
//     return rhs;
//   }

//   return std::make_tuple(std::get<std::tuple<As...>>(lhs), std::get<B>(rhs));
// }

// template<typename A, typename ...Bs> Result<std::tuple<A, Bs...>> operator+(Result<std::tuple<As...> lhs, Result<B> rhs) {
//   if(!lhs) {
//     return lhs;
//   }

//   if(!rhs) {
//     return rhs;
//   }

//   return std::make_tuple(std::get<std::tuple<As...>>(lhs), std::get<B>(rhs));
// }


template<typename T>
struct Parser : public std::function<Result<T>(std::istream &)> {
  using std::function<Result<T>(std::istream &)>::function;
};

template<typename T> Parser<T> action_(std::function<T()> fun) {
  return [=](std::istream &) {
    return fun();
  };
}

Parser<char> char_(char target) {
  return [=](std::istream &input) -> Result<char> {
    if(input.peek() == target) {
      input.get();
      return target;
    } else {
      return ErrorMessage{&target};
    }
  };
}

template<typename A, typename B> Parser<B> map(Parser<A> parser, std::function<B(Result<A>)> fun) {
  return [=](std::istream &input) {
    return fun(parser(input));
  };
}

template<typename A, typename B>
Parser<typename decltype(Result<A>() + Result<B>())::value_type>
operator >>(Parser<A> p1, Parser<B> p2) {
  return [=](std::istream &input) -> decltype(Result<A>() + Result<B>()) {
    Result<A> res1 = p1(input);
    if(!res1) {
      return std::get<ErrorMessage>(res1);
    } else {
      Result<B> res2 = p2(input);
      return res1 + res2;
    }
  };
}

template<typename A>
Parser<A> operator| (Parser<A> lhs, Parser<A> rhs) {
  return [=](std::istream &input) -> Result<A> {
    Result<A> res1 = lhs(input);
    if (res1) {
      return res1;
    }
    Result<A> res2 = rhs(input);
    if (res2) {
      return res2;
    }

    // return ErrorMessage{"Expected one of the following but found neither: " + std::get<ErrorMessage>(res1) + std::get<ErrorMessage>(res2)};
    return ErrorMessage{std::get<ErrorMessage>(res1).message + " or " + std::get<ErrorMessage>(res2).message};
  };
}

auto myparser = char_('x') >> char_('y') >> char_('z')
              | char_('a') >> char_('b') >> char_('c')
              ;


#include <iostream>
int main() {
  std::cout << "Hello, world!\n";
  int x = 42;
  std::string foo = "foo";
  // AutoTuple res = AutoTuple<int>(x) + foo;
  Result<int> y = x;
  Result<std::string> bar = foo;
  int baz = y + bar;

  Result<std::tuple<char, char, char>> result = myparser(std::cin);
  if(!result) {
    std::cout << "Syntax error: Expected " << std::get<ErrorMessage>(result).message << '\n';
  } else {
    std::cout << "Parse success!";
  }
}
