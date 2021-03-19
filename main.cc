#include <variant>
#include <functional>

#include <string>
#include <istream>

#include <iostream>
#include <vector>

/// Error messages are just strings
/// but you could envision e.g. also keeping track of line and column information
/// et cetera.
struct ErrorMessage {
  std::string message;
};

template <class T, template <class...> class Template>
struct is_specialization : std::false_type {};

template <template <class...> class Template, class... Args>
struct is_specialization<Template<Args...>, Template> : std::true_type {};

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

  // Result(Result<T> const &) = default;
  // Result(Result<T> &&) = default;

  /// To help other TMP functions
  typedef T value_type;

  /// Shorthand to check whether the result holds a T or an ErrorMessage
  explicit operator bool() const {
    return std::holds_alternative<T>(*this);
  }

  /// Converts tuples where all elements have the same type
  /// into a container (like a `std::vector` or a `std::string`)
  /// or any other type
  /// which can be constructed from the tuple's elements
  template <typename C>
  operator Result<C>() const {
    if(bool(*this)) {
      if constexpr (is_specialization<T, std::tuple>{}) {
        return constructFromTuple<C>(std::get<T>(*this));
      } else {
        return std::get<T>(*this);
      }
    } else {
      return std::get<ErrorMessage>(*this);
    }
  }
};

template<typename A, typename B> auto operator +(Result<A> const &lhs, Result<B> const &rhs) -> Result<decltype(wrapInTuple(std::get<A>(lhs), std::get<B>(rhs)))> {
  if(!bool(lhs)) {
    return std::get<ErrorMessage>(lhs);
  }

  if(!bool(rhs)) {
    return std::get<ErrorMessage>(rhs);
  }

  return wrapInTuple(std::get<A>(lhs), std::get<B>(rhs));
};

template<typename A> auto operator |(Result<A> const &lhs, Result<A> const &rhs) {
  if(bool(lhs)) {
    return lhs;
  }

  return rhs;
}

/// A parser is in essence nothing more
/// than a function that turns an `istream` into either a T or an ErrorMessage.
template<typename T>
struct Parser : public std::function<Result<T>(std::istream &)> {
  using std::function<Result<T>(std::istream &)>::function;

  typedef T value_type;

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

template<typename T>
Parser<T> constant(T &&val){
  return action_([=]{ return val; });
}

template<typename DefaultConstructible>
Parser<DefaultConstructible> epsilon_() {
  return constant(DefaultConstructible{});
}

/// ignore result of `parser`
/// and just return an empty tuple
/// (which will disappear when this parser is used in sequence with other parsers)
template<typename T>
Parser<std::tuple<>> ignore(Parser<T> const &parser) {
  return [=](std::istream &input) -> Result<std::tuple<>> {
    auto res = parser(input);
    if(!bool(res)){ return std::get<ErrorMessage>(res); }

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
    if(!bool(res)) { return std::get<ErrorMessage>(res); }

    return fun(std::get<A>(res));
  };
}

template<typename F, typename A> auto mapError(Parser<A> const &parser, F &&fun) -> Parser<A> {
  return [=](std::istream &input) -> Result<A> {
    Result<A> res = parser(input);
    if (!bool(res)) {
      return ErrorMessage{fun(std::get<ErrorMessage>(res))};
    }

    return res;
  };
}

Parser<std::tuple<>> eof() {
  return mapError(ignore(char_(std::char_traits<char>::eof())),
                  [](auto &&) { return "<end of input>"; });
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
    if(!bool(res)) {
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
    if(!bool(res1)) {
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
    if (bool(res1)) {
      return res1;
    }

    Result<A> res2 = rhs(input);
    if (bool(res2)) {
      return res2;
    }

    return ErrorMessage{std::get<ErrorMessage>(res1).message + " or " + std::get<ErrorMessage>(res2).message};
  };
}

template<typename A>
Parser<A> operator +(Parser<A> const &lhs, Parser<A> const &rhs) {
  return mapApply(lhs >> rhs, std::plus());
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
#define lazy(parser)                                            \
  Parser<typename std::decay_t<decltype(parser)>::value_type> { \
    [=](std::istream &input) { return (parser)(input); }        \
      }

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
  return lazy(some<Container>(element_parser) | constant(Container{}));
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

template<typename T>
Parser<T> lex(Parser<T> (*parser)()) {
  return lex(parser());
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
  return elem | constant(T{});
}

Parser<std::string> digits = some<std::string>(digit);
Parser<size_t> uint_ = map(digits, [](std::string const &str){ return std::stoul(str.c_str()); });

Parser<std::string> maybe_sign = string_("+") | string_("-") | string_("");
Parser<std::string> signed_digits = maybe_sign + digits;
Parser<long int> int_ = map(signed_digits, [](std::string const &str){ return std::stol(str.c_str()); });

Parser<double> double_() {
  auto vals = digits + maybe(string_(".") + digits) + maybe(string_("e") + signed_digits);
  // auto vals = signed_digits + maybe(string_(".") + digits) +
  //             maybe(string_("e") + signed_digits);
  return map(vals, [](std::string const &val) {
    return std::atof(val.c_str());
  });
}

template<typename A>
Parser<A> chainl1(Parser<A> elem, Parser<std::function<A(A, A)>> binop) {
  using F = std::function<A(A, A)>;

  return [=](std::istream &input) -> Result<A> {
    Result<A> lhs = elem(input);
    if(!bool(lhs)){ return lhs; }

    Result<A> res = lhs;

    while(true) {
      Result<F> binop_res = binop(input);
      if(!bool(binop_res)){ return res; }

      Result<A> rhs = elem(input);
      if(!bool(rhs)){ return res; }

      res = std::get<F>(binop_res)(std::get<A>(res), std::get<A>(rhs));
    }
  };
}

template<typename A>
Parser<A> chainl(Parser<A> elem, Parser<std::function<A(A, A)>> binop, A default_val) {
  return chainl1 | constant(default_val);
}

template<typename A>
Parser<A> chainl(Parser<A> elem, Parser<std::function<A(A, A)>> binop) {
  return chainl1 | constant(A{});
}

template <typename T>
Parser<T> choice(Parser<T> parser) {
  return parser;
}

template <typename T, typename... Ts>
Parser<T> choice(Parser<T> first, Ts... rest) {
  return first | choice(rest...);
}

template<typename A>
Parser<A> chainr1(Parser<A> elem, Parser<std::function<A(A, A)>> binop) {
  using F = std::function<A(A, A)>;

  return [=](std::istream &input) -> Result<A> {
    Result<A> lhs = elem(input);
    if(!bool(lhs)){ return lhs; }

    Result<A> res = lhs;

    Result<F> binop_res = binop(input);
    if(!bool(binop_res)){ return res; }

    Result<A> rhs = chainr1(elem, binop)(input);
    if(!bool(rhs)){ return res; }

    return std::get<F>(binop_res)(std::get<A>(res), std::get<A>(rhs));
  };
}

template<typename A>
Parser<A> chainr(Parser<A> elem, Parser<std::function<A(A, A)>> binop, A default_val) {
  return chainr1 | constant(default_val);
}

template<typename A>
Parser<A> chainr(Parser<A> elem, Parser<std::function<A(A, A)>> binop) {
  return chainr1 | constant(A{});
}

template <typename A>
class Precedence {
public:
  virtual ~Precedence() = default;

  virtual Parser<A> toParser(Parser<A> const &inner) const = 0;

  operator std::reference_wrapper<Precedence<A>>() {
    return std::ref(*this);
  }
};

template <typename A>
class Unaryop : public Precedence<A> {
  using F = std::function<A(A)>;

  Parser<F> d_unaryops_parser;

public:
  template <typename... Fs>
  Unaryop(Parser<F> const &first, Fs... rest)
    : d_unaryops_parser(choice(first, rest...)){};

  Parser<A> toParser(Parser<A> const &inner) const final {
    return toParserImpl(inner, d_unaryops_parser) | inner;
  };

  virtual Parser<A> toParserImpl(Parser<A> const &inner,
                                 Parser<F> const &unaryop_parser) const = 0;
};

template <typename A>
class UnaryopPrefix : public Unaryop<A> {
  using F = std::function<A(A)>;
  using Unaryop<A>::Unaryop;

private:
  Parser<A> toParserImpl(Parser<A> const &inner,
                         Parser<F> const &unaryop_parser) const final {
    return [=](std::istream &input) -> Result<A> {
      Result<F> fun_res = unaryop_parser(input);
      if(!bool(fun_res)) { return std::get<ErrorMessage>(fun_res); }

      Result<A> inner_res = inner(input);
      if (!bool(inner_res)) { return std::get<ErrorMessage>(inner_res); }

      return std::get<F>(fun_res)(std::get<A>(inner_res));
    };
  };
};

template <typename A> class UnaryopPostfix : public Unaryop<A> {
  using F = std::function<A(A)>;
  using Unaryop<A>::Unaryop;

private:
  Parser<A> toParserImpl(Parser<A> const &inner,
                         Parser<F> const &unaryop_parser) const final {
    return [=](std::istream &input) -> Result<A> {
      Result<A> inner_res = inner(input);
      if (!bool(inner_res)) {
        return std::get<ErrorMessage>(inner_res);
      }

      Result<F> fun_res = unaryop_parser(input);
      if (!bool(fun_res)) {
        return std::get<ErrorMessage>(fun_res);
      }

      return std::get<F>(fun_res)(std::get<A>(inner_res));
    };
  };
};

template <typename A> class Binop : public Precedence<A> {
  using F = std::function<A(A, A)>;

  Parser<F> d_binops_parser;

public:
  template <typename... Fs>
  Binop(Parser<F> const &first, Fs... rest)
    : d_binops_parser(choice(first, Parser<F>(rest)...)){};

  Parser<A> toParser(Parser<A> const &inner) const final {
    return toParserImpl(inner, d_binops_parser);
  }

private:
  virtual Parser<A> toParserImpl(Parser<A> const &inner,
                                 Parser<F> const &binop_parser) const = 0;
};

template<typename A>
class BinaryopLeft : public Binop<A> {
  using F = std::function<A(A, A)>;
  using Binop<A>::Binop;



private:
  Parser<A> toParserImpl(Parser<A> const &inner,
                         Parser<F> const &binop_parser) const final
  {
    return chainl1(inner, binop_parser);
  };
};

template <typename A>
class BinaryopRight : public Binop<A> {
  using F = std::function<A(A, A)>;
  using Binop<A>::Binop;

private:
  Parser<A> toParserImpl(Parser<A> const &inner,
                         Parser<F> const &binop_parser) const final
  {
    return chainr1(inner, binop_parser);
  };
};

template <typename A>
Parser<A> makeExpressioParser(Parser<A> const &inner) {
  return inner;
}

template <typename A, typename... As>
Parser<A> makeExpressioParser(Parser<A> const &inner, Precedence<A> const &next, As...rest) {
  Parser<A> result = next.toParser(inner);
  return makeExpressioParser(result, rest...);
}

template <typename A, typename... As>
Parser<A> makeExpressioParser(Parser<A> (*inner)(), As... rest) {
  return makeExpressioParser(inner(), rest...);
}

// #include <memory>
// template <typename A>
// Parser<A> makeExpressioParser(
//                                         Parser<A> const &inner,
//                                         std::vector < std::reference_wrapper<Precedence<A>>> const &table) {

//   Parser<A> result = inner;
//   for(auto &&row : table) {
//     result = row.get().toParser(result);
//   };
//   return result;
// };

// template <typename A>
// Parser<A> makeExpressioParser(
//                                         Parser<A> (*inner)(),
//                                         std::vector<std::reference_wrapper<Precedence<A>>> const &table) {
//   return makeExpressioParser(inner(), table);
// }

// template <typename A>
// BinaryopLeft<A> binary_left(Parser<std::function<A(A, A)>> const &binop_parser) {
//   return BinaryopLeft<A>(binop_parser);
// }

template <typename A, typename F, typename... Fs>
BinaryopLeft<A> binary_left(
                            F const &first_binop_parser, Fs... other_binop_parsers) {
  Parser<std::function<A(A, A)>> foo = first_binop_parser;
  return BinaryopLeft<A>(foo,
                         other_binop_parsers...);
}

// template <typename A>
// BinaryopLeft<A> binary_left(
//                             Parser<std::function<A(A, A)>>(*binop_parser)()) {
//   return BinaryopLeft<A>(binop_parser());
// }

// template <typename A, typename... Fs>
// BinaryopLeft<A> binary_left(
//                             Parser<std::function<A(A, A)>>(*first_binop_parser)(), Fs... other_binop_parsers) {
//   return BinaryopLeft<A>(first_binop_parser(),
//                          other_binop_parsers()...);
// }

template <typename A>
BinaryopRight<A> binary_right(Parser<std::function<A(A, A)>> const &binop_parser) {
  return BinaryopRight<A>(binop_parser);
}

template <typename A, typename... Fs>
BinaryopRight<A> binary_right(
                              Parser<std::function<A(A, A)>> const &first_binop_parser, Fs... other_binop_parsers) {
  return BinaryopRight<A>(first_binop_parser,
                          other_binop_parsers...);
}

template <typename A>
BinaryopRight<A> binary_right(
                              Parser<std::function<A(A, A)>>(*binop_parser)()) {
  return BinaryopRight<A>(binop_parser());
}

template <typename A, typename... Fs>
BinaryopRight<A> binary_right(
                              Parser<std::function<A(A, A)>>(*first_binop_parser)(), Fs... other_binop_parsers) {
  return BinaryopRight<A>(first_binop_parser(),
                          other_binop_parsers()...);
}

template <typename A>
UnaryopPostfix<A> postfix(Parser<std::function<A(A)>> const &unaryop_parser) {
  return UnaryopPostfix<A>(unaryop_parser);
}

template <typename A, typename... Fs>
UnaryopPostfix<A> postfix(Parser<std::function<A(A)>> const &first_unaryop_parser,
                          Fs... other_unaryop_parsers) {
  return UnaryopPostfix<A>(first_unaryop_parser, other_unaryop_parsers...);
}

template <typename A>
UnaryopPrefix<A> prefix(Parser<std::function<A(A)>> const &unaryop_parser) {
  return UnaryopPrefix<A>(unaryop_parser);
}

// template <typename A>
// UnaryopPrefix<A> prefix(Parser<std::function<A(A)>> (*unaryop_parser)()) {
//   return UnaryopPrefix<A>(unaryop_parser());
// }

// template <typename A, typename... Fs>
// UnaryopPrefix<A> prefix(
//                         Parser<std::function<A(A)>> const &first_unaryop_parser, Fs... other_unaryop_parsers) {
//   return UnaryopPrefix<A>(first_unaryop_parser,
//                           other_unaryop_parsers...);
// }

// template<typename A>
// Parser<std::function<A(A, A)>> plus() {
//   return ignore(lex(string_("+"))) >> constant(std::plus<A>());
// }

// template<typename A>
// Parser<std::function<A(A, A)>> minus() {
//   return ignore(lex(string_("-"))) >> constant(std::minus<A>());
// }

// template <typename A> Parser<std::function<A(A, A)>> mul() {
//   return ignore(lex(string_("*"))) >> constant(std::multiplies<A>());
// }

// template <typename A> Parser<std::function<A(A, A)>> div() {
//   return ignore(lex(string_("/"))) >> constant(std::divides<A>());
// }

// #include <cmath>
// template <typename A> Parser<std::function<A(A, A)>> exp() {
//   return ignore(lex(string_("^"))) >> constant([](auto &&left, auto
//   &&right) { return std::pow(left, right); });
// }

// template <typename A> Parser<std::function<A(A)>> neg() {
//   return ignore(lex(string_("-"))) >> constant(std::negate<A>());
// }
Parser<decltype(std::plus())> plus() {
  return ignore(lex(string_("+"))) >> constant(std::plus<void>());
}

Parser<decltype(std::minus<void>())> minus() {
  return ignore(lex(string_("-"))) >> constant(std::minus());
}

Parser<std::function<double(double, double)>> mul() {
  return ignore(lex(string_("*"))) >> constant(std::multiplies());
}

Parser<std::function<double(double, double)>> divide() {
  return ignore(lex(string_("/"))) >> constant(std::divides());
}

#include <cmath>
Parser<std::function<double(double, double)>> exp() {
  return ignore(lex(string_("^"))) >> constant([](auto &&left, auto &&right) {
    return std::pow(left, right);
  });
}

// template <typename F = std::function<double(double)>>
// template <typename F = std::function<double(double)>> Parser<F> neg() {
template <typename F> Parser<F> neg() {
  // Parser<std::function<double(double)>> neg() {
  return ignore(lex(string_("-"))) >> constant(std::negate());
}

// template<typename A>
// Parser<std::function<A(A, A)>> addOp() {
//   return minus<A>() | plus<A>();
// }

// Parser<size_t> uint_expression() {
//   return chainl1(lex(uint_), lex(addOp<size_t>()));
// };

// Parser<double> double_expression() {
//   return chainl1(lex(double_),
//                  Parser<std::function<double(double, double)>>{plus()} |
//                  Parser<std::function<double(double, double)>>{minus()});
// };

template<typename A, typename B, typename C>
Parser<A> between(Parser<B> const &lhs, Parser<C> const &rhs, Parser<A> const &middle) {
  return ignore(lhs) >> middle >> ignore(rhs);
}

template <typename A>
Parser<A> parenthesized(Parser<A> const &inner) {
  return between(lex(char_('(')), lex(char_(')')), lazy(inner));
}

template <typename A>
Parser<A>
    parenthesized(Parser<A> (*inner)()) {
  return parenthesized(inner());
}

Parser<double> term();

Parser<double> expression() {
  return makeExpressioParser(
          term,
          // prefix(neg()),
          // binary_right(exp),
          // binary_left(mul, divide),
          binary_left<double>(plus(), minus())
        );
}

Parser<double> term() {
  return lex(double_) | parenthesized(expression);
}

Parser<char> newline() {
  return char_('\n');
}

Parser<char> non_newline() {
  return satisfy([](char val){ return val != '\n'; }, "non-newline");
}

Parser<std::tuple<>> comment() {
  return ignore(string_("//") >> many<std::string>(non_newline()) >> whitespace());
}

template <typename T>
Result<T> parsePartial(Parser<T> parser, std::istream &in) {
  Result<T> result = parser(in);
  if(!bool(result)) {
    std::cout << "Syntax error: Expected " << std::get<ErrorMessage>(result).message << '\n';
  } else {
    std::cout << "Parse success! " << std::get<T>(result) << '\n';
  }

  return result;
}

template <typename T> Result<T> parsePartial(Parser<T>(*parser)(), std::istream &in) {
  return parsePartial(parser(), in);
}

template <typename T>
Result<T> parse(Parser<T> parser, std::istream &in) {
  return parsePartial<T>(parser >> eof(), in);
}

template <typename T> Result<T> parse(Parser<T> (*parser)(), std::istream &in) {
  return parse(parser(), in);
}

int main() {
  // runParser(parser2);
  // runParser(parser3);
  // runParser(parser4);
  // runParser(parser5);
  // runParser(int_);
  // runParser(double_());
  // runParser(double_expression());
  parse(expression, std::cin);
}
