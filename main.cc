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
template<typename   A , typename    B > std::tuple<A    , B    > wrapInTuple(A                 lhs, B                 rhs) { return std::make_tuple(lhs, rhs); }
template<typename...As, typename ...Bs> std::tuple<As..., Bs...> wrapInTuple(std::tuple<As...> lhs, std::tuple<Bs...> rhs) { return std::tuple_cat(lhs, rhs);  }
template<typename   A , typename ...Bs> std::tuple<A    , Bs...> wrapInTuple(A                 lhs, std::tuple<Bs...> rhs) { return std::tuple_cat(std::make_tuple(lhs), rhs); }
template<typename...As, typename     B> std::tuple<As..., B    > wrapInTuple(std::tuple<As...> lhs, B                 rhs) { return std::tuple_cat(lhs, std::make_tuple(rhs)); }

template <typename C, typename... Ts>
C tupleToContainer(const std::tuple<Ts...>& t)
{
  return std::apply([](auto... elements){ return C{elements...}; }, t);
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

  /// Converts tuples where all elements have the same type
  /// into a container (like a `std::vector` or a `std::string`)
  /// or any other type
  /// which can be constructed from the tuple's elements
  template<typename C>
  operator Result<C> () const {
    if(*this){
      return tupleToContainer<C>(std::get<T>(*this));
    } else {
      return std::get<ErrorMessage>(*this);
    }
  }
};

template<typename A, typename B> auto operator +(Result<A> lhs, Result<B> rhs) -> Result<decltype(wrapInTuple(std::get<A>(lhs), std::get<B>(rhs)))> {
  if(!lhs) {
    return std::get<ErrorMessage>(lhs);
  }

  if(!rhs) {
    return std::get<ErrorMessage>(rhs);
  }

  return wrapInTuple(std::get<A>(lhs), std::get<B>(rhs));
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

// template<typename DefaultConstructible>
// Parser<DefaultConstructible> epsilon_() {
//   return [=](std::istream &) {
//     return DefaultConstructible{};
//   };
// }

template<typename F> auto action_(F &&fun) {
  return [=](std::istream &) {
    return fun();
  };
}

template<typename DefaultConstructible>
Parser<DefaultConstructible> epsilon_() {
  return action_([]{ return DefaultConstructible{}; });
}

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

Parser<char> satisfy(std::function<bool(char)> checking_fun, const char *name) {
  return [=](std::istream &input) -> Result<char> {
    char val = input.get();
    if(checking_fun(val)) {
      return val;
    } else {
      input.unget();
      return ErrorMessage{std::string{"any char satifying `"} + name + '`'};
    }
  };
}

Parser<char> digit = satisfy(isdigit, "isdigit");

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

template<typename F, typename A> auto map(Parser<A> parser, F fun) {
  return [=](std::istream &input) -> Result<decltype(fun(std::declval<A>()))> {
    // return fun(parser(input));
    Result<A> res = parser(input);
    if(!res){
      return std::get<ErrorMessage>(res);
    }
    return fun(std::get<A>(res));
  };
}

template<typename F, typename Tuple> auto mapApply(Parser<Tuple> const &parser, F fun) {
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

/// Note that this needs to be defined as a macro,
/// because we only want to evaluate the expression `parser`
/// once called. (That's the whole point.)
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
  // otherwise we'd stackoverflow on construction
  return lazy(some<Container>(element_parser) | epsilon_<Container>());
}

/// Given a T `first` and a C<T> `rest`, creates a new C<T> with `first` at the front.
/// This implementation is simple but inefficient.
/// Optimizing it is left a an exercise to the reader ;-)
template<typename Container>
Container prependElement(typename Container::value_type const &first, Container const &rest) {
  Container result{};
  std::cout << "foo\n";
  std::cout << first;
  result.push_back(first);
  // result.insert(result.end(), rest.begin(), rest.end());
  for(auto &elem : rest) {
    result.push_back(elem);
  }
  return result;
}

template<typename Container>
Parser<Container> some(Parser<typename Container::value_type> element_parser) {
  auto res = element_parser >> many<Container>(element_parser);
  return mapApply(res, prependElement<Container>);
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

Parser<std::string> parser5 = mapApply(char_('x') >> string_("asdf"), [](auto lhs, auto rhs) {
  return lhs + rhs;
 });

template<typename T>
Parser<T> maybe(Parser<T> elem) {
  return elem | epsilon_<T>();
}

Parser<size_t> uint_parser = map(some<std::string>(digit), [](std::string const &str) { return std::atoi(str.c_str()); });
// Parser<int> int_parser = map(maybe>(char_('-')) >> some<std::string>(digit), [](std::string const &str) { return std::atoi(str.c_str()); });

template<typename T>
Result<T> runParser(Parser<T> parser) {
  Result<T> result = parser(std::cin);
  if(!result) {
    std::cout << "Syntax error: Expected " << std::get<ErrorMessage>(result).message << '\n';
  } else {
    std::cout << "Parse success! " << std::get<T>(result) << '\n';
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
  // runParser(parser4);
  runParser(parser5);
  // runParser(uint_parser);

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
