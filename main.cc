#include <variant>
#include <functional>

#include <string>
#include <istream>

struct ErrorMessage {
  std::string message;
};

/// Wrap `A` and `B` into a tuple.
/// Overloaded to create a flat tuple even if `A` or `B` themselves are already tuples
template<typename   A , typename    B > std::tuple<A    , B    > wrap_in_tuple(A                 lhs, B                 rhs) { return std::make_tuple(lhs, rhs); }
template<typename...As, typename ...Bs> std::tuple<As..., Bs...> wrap_in_tuple(std::tuple<As...> lhs, std::tuple<Bs...> rhs) { return std::tuple_cat(lhs, rhs);  }
template<typename   A , typename ...Bs> std::tuple<A    , Bs...> wrap_in_tuple(A                 lhs, std::tuple<Bs...> rhs) { return std::tuple_cat(std::make_tuple(lhs), rhs); }
template<typename...As, typename     B> std::tuple<As..., B    > wrap_in_tuple(std::tuple<As...> lhs, B                 rhs) { return std::tuple_cat(lhs, std::make_tuple(rhs)); }

template <typename T, typename... Ts>
T tuple_to_container(const std::tuple<Ts...>& t)
{
  return std::apply([](auto... cs){ return T{cs...}; }, t);
}

/// The result of parsing.
/// If parsing was successful, will contain `T`.
/// If there was a problem, will contain `ErrorMessage`.
template<typename T>
struct Result : public std::variant<T, ErrorMessage> {
  /// Inherit constructors from std::variant
  using std::variant<T, ErrorMessage>::variant;

  /// To help other TMP functions
  typedef T value_type;

  /// Shorthand to check whether the result holds a T or an ErrorMessage
  operator bool() const {
    return !std::holds_alternative<ErrorMessage>(*this);
  }

  /// Converts a T into a Container<T> iff it is possible to construct this.
  template<template<typename ...> typename Container>
  operator Result<Container<T>>() const {
    if(*this) {
      return Container(std::get<T>(*this));
    } else {
      return std::get<ErrorMessage>(*this);
    }
  }

  /// Converts tuples where all elements have the same type
  /// into a container (like a `std::vector` or a `std::string`)
  // Note the extra template parameter, it constrains us to only container-like types.
  // this is required to not conflict with the overload below
  template<typename Container>
  operator Result<Container> () const {
    if(*this){
      return tuple_to_container<Container>(std::get<T>(*this));
    } else {
      return std::get<ErrorMessage>(*this);
    }
  }
};

template<typename A, typename B> auto operator +(Result<A> lhs, Result<B> rhs) -> Result<decltype(wrap_in_tuple(std::get<A>(lhs), std::get<B>(rhs)))> {
  if(!lhs) {
    return std::get<ErrorMessage>(lhs);
  }

  if(!rhs) {
    return std::get<ErrorMessage>(rhs);
  }

  return wrap_in_tuple(std::get<A>(lhs), std::get<B>(rhs));
};

template<typename A> auto operator |(Result<A> lhs, Result<A> rhs) {
  if(lhs) {
    return lhs;
  }

  return rhs;
}

template<typename T>
struct Parser : public std::function<Result<T>(std::istream &)> {
  using std::function<Result<T>(std::istream &)>::function;
};

template<typename DefaultConstructible>
Parser<DefaultConstructible> epsilon_() {
  return [=](std::istream &) {
    return DefaultConstructible{};
  };
}

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
      return ErrorMessage{std::string{1, target}};
    }
  };
}

Parser<std::string> string_(std::string const &target) {
  return [=](std::istream &input) -> Result<std::string> {
    for(size_t index = 0; index < target.size(); ++index) {
      if(input.peek() == target[index]) {
        input.get();
      } else {
        // Restore input:
        while(index--) {
          input.unget();
        }
        return ErrorMessage{target};
      }
    }
    return target;
  };
}

template<typename A, typename B> Parser<B> map(Parser<A> parser, std::function<B(A)> fun) {
  return [=](std::istream &input) -> Result<B> {
    // return fun(parser(input));
    Result<A> res = parser(input);
    if(!res){
      return std::get<ErrorMessage>(res);
    }
    return fun(res);
  };
}

template<typename F, typename Tuple> auto myapply(Parser<Tuple> const &parser, F fun) {
  return [=](std::istream &input) -> Result<decltype(std::apply(fun, std::declval<Tuple>()))> {
    auto res = parser(input);
    if(!res) {
      return std::get<ErrorMessage>(res);
    }
    return std::apply(fun, std::get<Tuple>(res));
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
Parser<A> operator | (Parser<A> lhs, Parser<A> rhs) {
  return [=](std::istream &input) -> Result<A> {
    Result<A> res1 = lhs(input);
    if (res1) {
      return res1;
    }
    Result<A> res2 = rhs(input);
    if (res2) {
      return res2;
    }

    return ErrorMessage{std::get<ErrorMessage>(res1).message + " or " + std::get<ErrorMessage>(res2).message};
  };
}

template<typename Container>
Parser<Container> many(Parser<typename Container::value_type> element_parser);

template<typename Container>
Parser<Container> some(Parser<typename Container::value_type> element_parser);

template<typename Container>
Parser<Container> many(Parser<typename Container::value_type> element_parser) {
  // Wrapping is necessary to make this 'lazy'.
  // otherwise we'd stackoverflow on construction
  return [=](std::istream &input) {
    return (some<Container>(element_parser) | epsilon_<Container>())(input);
  };
}

#include <iostream>
#include <vector>

template<typename Container>
Container prependElement(typename Container::value_type const &first, Container const &rest) {
  Container result{};
  result.push_back(first);
  result.insert(result.end(), rest.begin(), rest.end());
  return result;
}

template<typename Container>
Parser<Container> some(Parser<typename Container::value_type> element_parser) {
  auto combined_parser = element_parser >> many<Container>(element_parser);
  return myapply(combined_parser, prependElement<Container>);
}

auto myparser = char_('x') >> char_('y') >> char_('z')
              | char_('a') >> char_('b') >> char_('c')
              ;

auto parser2 = string_("foo") | string_("faa");

class Person {
  std::string d_first_name;
  std::string d_last_name;

public:
  Person(std::string first_name, std::string last_name)
    : d_first_name(first_name),
      d_last_name(last_name)
  {};
};

Parser<Person> parser3 = string_("john") >> string_("snow");

Parser<std::string> parser4 = some<std::string>(char_('z'));

Parser<std::string> parser5 = myapply(char_('x') >> string_("asdf"), [](auto lhs, auto rhs) {
  return lhs + rhs;
 });


template<typename T>
Result<T> runParser(Parser<T> parser) {
  Result<T> result = parser(std::cin);
  if(!result) {
    std::cout << "Syntax error: Expected " << std::get<ErrorMessage>(result).message << '\n';
  } else {
    std::cout << "Parse success!\n";
  }

  return result;
}



template <>
Result<std::string> runParser(Parser<std::string> parser) {
  Result<std::string> result = parser(std::cin);
  if(!result) {
    std::cout << "Syntax error: Expected " << std::get<ErrorMessage>(result).message << '\n';
  } else {
    std::cout << "Parse success:" << std::get<std::string>(result) << "!\n";
  }

  return result;
}

int main() {
  std::cout << "Hello, world!\n";
  int x = 42;
  std::string foo = "foo";
  // AutoTuple res = AutoTuple<int>(x) + foo;
  Result<int> y = x;
  Result<std::string> bar = foo;
  // int baz = y + bar;

  // runParser(myparser);
  // runParser(parser2);
  // runParser(parser3);
  runParser(parser4);
  // runParser(parser5);

  // Result<std::tuple<char, char, char>> result = myparser(std::cin);
  // Result<std::string> result = myparser(std::cin);
  // if(!result) {
  //   std::cout << "Syntax error: Expected " << std::get<ErrorMessage>(result).message << '\n';
  // } else {
  //   std::cout << "Parse success:" << std::get<std::string>(result) << "!\n";
  // }

  // Result<std::string> result2 = parser2(std::cin);
  // if(!result2) {
  //   std::cout << "Syntax error: Expected " << std::get<ErrorMessage>(result2).message << '\n';
  // } else {
  //   std::cout << "Parse success!\n";
  // }

  // Result<Person> result3 = parser3(std::cin);
  // if(!result3) {
  //   std::cout << "Syntax error: Expected " << std::get<ErrorMessage>(result3).message << '\n';
  // } else {
  //   std::cout << "Parse success!\n";
  // }
}
