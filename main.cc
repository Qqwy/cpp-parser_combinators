#include <variant>
#include <functional>

#include <string>
#include <istream>

#include <iostream>
#include <vector>

struct ErrorMessage {
  std::string message;
};

/// Wrap `A` and `B` into a tuple.
/// Overloaded to create a flat tuple even if `A` or `B` themselves are already tuples
template<typename   A , typename    B > std::tuple<A    , B    > wrapInTuple(A                 const &lhs, B                 const &rhs) { return std::make_tuple(lhs, rhs); }
template<typename...As, typename ...Bs> std::tuple<As..., Bs...> wrapInTuple(std::tuple<As...> const &lhs, std::tuple<Bs...> const &rhs) { return std::tuple_cat(lhs, rhs);  }
template<typename   A , typename ...Bs> std::tuple<A    , Bs...> wrapInTuple(A                 const &lhs, std::tuple<Bs...> const &rhs) { return std::tuple_cat(std::make_tuple(lhs), rhs); }
template<typename...As, typename     B> std::tuple<As..., B    > wrapInTuple(std::tuple<As...> const &lhs, B                 const &rhs) { return std::tuple_cat(lhs, std::make_tuple(rhs)); }

template <typename C, typename... Ts>
C constructFromTuple(std::tuple<Ts...> const &tuple)
{
  return std::apply([](auto... elements){ return C{elements...}; }, tuple);
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
  explicit operator bool() const {
    return !std::holds_alternative<ErrorMessage>(*this);
  }

  /// Converts tuples where all elements have the same type
  /// into a container (like a `std::vector` or a `std::string`)
  /// or any other type
  /// which can be constructed from the tuple's elements
  template<typename C>
  operator Result<C> () const {
    if(*this){
      return constructFromTuple<C>(std::get<T>(*this));
    } else {
      return std::get<ErrorMessage>(*this);
    }
  }
};

template<typename A, typename B> auto operator +(Result<A> const &lhs, Result<B> const &rhs) -> Result<decltype(wrapInTuple(std::get<A>(lhs), std::get<B>(rhs)))> {
  if(!static_cast<bool>(lhs)) {
    return std::get<ErrorMessage>(lhs);
  }

  if(!static_cast<bool>(rhs)) {
    return std::get<ErrorMessage>(rhs);
  }

  return wrapInTuple(std::get<A>(lhs), std::get<B>(rhs));
};

template<typename A> auto operator |(Result<A> const &lhs, Result<A> const &rhs) {
  if(static_cast<bool>(lhs)) {
    return lhs;
  }

  return rhs;
}

/// A parser is in essence nothing more
/// than a function that turns an `istream` into either a T or an ErrorMessage.
template<typename T>
struct Parser : public std::function<Result<T>(std::istream &)> {
  using std::function<Result<T>(std::istream &)>::function;


  /// Fun sugar to allow parsers that take no arguments
  /// to be written without `()`.
  /// e.g. writing `digit` instead of `digit()`.
  Parser(Parser<T> (*ptr)()) :
    Parser(ptr())
  {}
};

/// Run any function regardless of current input
template<typename F> auto action_(F &&fun) {
  return [=](std::istream &) {
    return fun();
  };
}

/// Unconditionally construct something, regardless of current input.
template<typename DefaultConstructible>
Parser<DefaultConstructible> epsilon_() {
  return action_([]{ return DefaultConstructible{}; });
}

/// ignore result of `parser`
/// and just return an empty tuple
/// (which will disappear when this parser is used in sequence with other parsers)
template<typename T>
Parser<std::tuple<>> ignore(Parser<T> const &parser) {
  return [=](std::istream &input) {
    parser(input);
    return std::make_tuple();
  };
}

/// Syntactic sugar to be able to call ignore also without parentheses for parameter-less parsers. :-)
template<typename T>
Parser<std::tuple<>> ignore(Parser<T> (*parser)()) {
  return ignore(parser());
}

/// Parse exactly the character `target`.
Parser<char> char_(char target) {
  return [=](std::istream &input) -> Result<char> {
    char val = input.get();
    if(val == target) {
      return val;
    } else {
      input.unget();
      return ErrorMessage{std::string{1, target}};
    }
  };
}

/// Parse and return any character satisfying `checking_fun`.
/// examples: `satisfy(isspace)`, `satisfy(isdigit)` etc.
Parser<char> satisfy(std::function<bool(char)> checking_fun, const char *name) {
  return [=](std::istream &input) -> Result<char> {
    char val = input.get();
    if(checking_fun(val)) {
      return val;
    } else {
      input.unget();
      return ErrorMessage{std::string{"any `"} + name + '`'};
    }
  };
}

/// Parse any decimal digit
Parser<char> digit() {
  return satisfy(isdigit, "digit");
}

/// Parse exactly the string `target`.
/// Note that this implementation is inefficient.
/// A more efficient implementation is left as an exercise for the reader ;-)
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

/// Transform the result of a parser into another type of result
/// by running a function `fun` on it.
///
/// (Note: The templated arguments allow C++ to do more automatic type-inference
/// than if we'd use `std::function` instead.)
template<typename F, typename A> auto map(Parser<A> const &parser, F &&fun) -> Parser<decltype(fun(std::declval<A>()))> {
  return [=](std::istream &input) -> Result<decltype(fun(std::declval<A>()))> {
    Result<A> res = parser(input);
    if(!static_cast<bool>(res)) {
      return std::get<ErrorMessage>(res);
    }

    return fun(std::get<A>(res));
  };
}

/// Transform the result of a Parser<Tuple> into another type of result
/// by running a function `fun` on it.
/// `fun` should be overloaded exactly for the number and types of the elements in `Tuple`.
///
/// (Note: The templated arguments allow C++ to do more automatic type-inference
/// than if we'd use `std::function` instead.)
template<typename F, typename Tuple> auto mapApply(Parser<Tuple> const &parser, F &&fun) {
  return [=](std::istream &input) -> Result<decltype(std::apply(fun, std::declval<Tuple>()))> {
    auto res = parser(input);
    if(!static_cast<bool>(res)) {
      return std::get<ErrorMessage>(res);
    }
    return std::apply(fun, std::get<Tuple>(res));
  };
}

/// Compose two parsers in sequence: Run `p1` followed by `p2`.
/// Fails when either one fails, only succeeds if both suceed.
template<typename A, typename B>
Parser<typename decltype(Result<A>() + Result<B>())::value_type>
operator >>(Parser<A> const &p1, Parser<B> const &p2) {
  return [=](std::istream &input) -> decltype(Result<A>() + Result<B>()) {
    Result<A> res1 = p1(input);
    if(!static_cast<bool>(res1)) {
      return std::get<ErrorMessage>(res1);
    } else {
      Result<B> res2 = p2(input);
      return res1 + res2;
    }
  };
}

/// Compose two parsers as alternatives: Run `p1` or alternatively `p2`.
/// Fails when both fail, succeeds when either one succeeds.
template<typename A>
Parser<A> operator |(Parser<A> const &lhs, Parser<A> const &rhs) {
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

/// Helper function for `many`/`some`.
/// Given a T `first` and a C<T> `rest`, creates a new C<T> with `first` at the front.
/// Note: This implementation is simple but inefficient.
/// Optimizing it is left a an exercise to the reader ;-)
template<typename Container>
Container prependElement(typename Container::value_type const &first, Container const &rest) {
  Container result{};
  result.push_back(first);
  for(auto const &elem : rest) {
    result.push_back(elem);
  }
  return result;
}

/// Note: `many` and `some` are mutually recursive.

/// Note that `lazy` needs to be defined as a macro,
/// because we only want to evaluate the parameter `parser`
/// when called (rather than before). That's the whole point.
#define lazy(parser) [=](std::istream &input) { return (parser)(input); }

/// Zero or more repetitions of `element_parser`.
template<typename Container>
Parser<Container> many(Parser<typename Container::value_type> element_parser);

/// One or more repetitions of `element_parser`.
template<typename Container>
Parser<Container> some(Parser<typename Container::value_type> element_parser);

template<typename Container>
Parser<Container> many(Parser<typename Container::value_type> element_parser) {
  // Wrapping is necessary to make this 'lazy'.
  // otherwise we'd stackoverflow on construction by infinite recursion
  return lazy(some<Container>(element_parser) | epsilon_<Container>());
}

template<typename Container>
Parser<Container> some(Parser<typename Container::value_type> element_parser) {
  auto res = element_parser >> many<Container>(element_parser);

  return mapApply(res, prependElement<Container>);
}

Parser<char> space() {
  return satisfy(isspace, "space");
}

/// Zero or more spaces
Parser<std::string> whitespace() {
  return many<std::string>(space);
}

/// parses `parser` followed by possible whitespace which is ignored.
template<typename T>
Parser<T> lex(Parser<T> parser) {
  return parser >> ignore(whitespace);
}

class Person {
  std::string d_first_name;
  std::string d_last_name;

public:
  Person(std::string first_name, std::string last_name)
    : d_first_name(first_name),
      d_last_name(last_name)
  {};

  friend std::ostream &operator<<(std::ostream &out, Person const &person);
};

std::ostream &operator<<(std::ostream &out, Person const &person) {
  out << "Person{ " << "first_name = " << person.d_first_name << ", last_name = " << person.d_last_name << " }";
  return out;
}

// auto myparser = char_('x') >> char_('y') >> char_('z')
//   | char_('a') >> char_('b') >> char_('c')
//   ;

auto parser2 = string_("foo") | string_("faa");


// Parser<Person> parser3 = string_("john") >> whitespace() >> string_("snow") >> whitespace();
Parser<Person> parser3 = lex(string_("john")) >> lex(string_("snow"));

Parser<std::string> parser4 = some<std::string>(char_('z'));

Parser<std::string> parser5 = mapApply(char_('x') >> string_("asdf"), [](auto lhs, auto rhs) {
  return lhs + rhs;
 });

template<typename T>
Parser<T> maybe(Parser<T> elem) {
  return elem | epsilon_<T>();
}

Parser<std::string> digits = some<std::string>(digit);
Parser<size_t> uint_ = map(digits, [](std::string const &str){ return std::stoul(str.c_str()); });
  // return [=](std::istream &input) -> Result<size_t> {
  //   auto str_res = some<std::string>(digit)(input);
  //   if(!str_res) {
  //     return std::get<ErrorMessage>(str_res);
  //   }
  //   auto res = std::stoul(std::get<std::string>(str_res).c_str());
  //   std::cout << "res: " << res << '\n';
  //   return res;
  // };
// }
// Parser<int> int_parser = map(maybe>(char_('-')) >> some<std::string>(digit), [](std::string const &str) { return std::atoi(str.c_str()); });

Parser<std::string> maybe_sign = string_("+") | string_("-") | string_("");
Parser<std::string> signed_digits = mapApply(maybe_sign >> digits, [](auto lhs, auto rhs) { return lhs + rhs; });
Parser<long int> int_ = map(signed_digits, [](std::string const &str){ return std::stol(str.c_str()); });

template<typename T>
Result<T> runParser(Parser<T> parser) {
  Result<T> result = parser(std::cin);
  if(!static_cast<bool>(result)) {
    std::cout << "Syntax error: Expected " << std::get<ErrorMessage>(result).message << '\n';
  } else {
    // std::cout << "Parse success! " << '\n';
    T val = std::get<T>(result);
    std::cout << "Parse success! " << val << '\n';
  }

  return result;
}
// template<>
// Result<size_t> runParser(Parser<size_t> parser) {
//   Result<size_t> result = parser(std::cin);
//   if(!result) {
//     std::cout << "Syntax error: Expected " << std::get<ErrorMessage>(result).message << '\n';
//   } else {
//     // std::cout << "Parse success! " << '\n';
//     size_t val = std::get<size_t>(result);
//     std::cout << "Parse success! " << val << '\n';
//   }

//   return result;
// }




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

  runParser(parser2);
  runParser(parser3);
  // runParser(parser4);
  // runParser(parser5);
  runParser(int_);

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
