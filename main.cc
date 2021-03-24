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

/// Wrap `A` and `B` into a tuple.
/// Overloaded to create a flat tuple even if `A` or `B` themselves are already tuples
template<typename   A , typename    B > std::tuple<A    , B    > constexpr wrapInTuple(A                 const &lhs, B                 const &rhs) { return std::make_tuple(lhs, rhs); }
template<typename...As, typename ...Bs> std::tuple<As..., Bs...> constexpr wrapInTuple(std::tuple<As...> const &lhs, std::tuple<Bs...> const &rhs) { return std::tuple_cat(lhs, rhs);  }
template<typename   A , typename ...Bs> std::tuple<A    , Bs...> constexpr wrapInTuple(A                 const &lhs, std::tuple<Bs...> const &rhs) { return std::tuple_cat(std::make_tuple(lhs), rhs); }
template<typename...As, typename     B> std::tuple<As..., B    > constexpr wrapInTuple(std::tuple<As...> const &lhs, B                 const &rhs) { return std::tuple_cat(lhs, std::make_tuple(rhs)); }

template <typename C, typename... Ts>
constexpr C constructFromTuple(std::tuple<Ts...> const &tuple)
{
  return std::apply([](auto... elements){ return C{elements...}; }, tuple);
}


template <typename R, typename... Ts>
R sumAll(R default_elem, Ts... elements) {
  return (default_elem + ... + elements);
}

template <typename... Ts>
std::string constructFromTuple(std::tuple<std::string, Ts...> const &tuple) {
  return std::apply([](auto... elements) { return sumAll(std::string{}, elements...); }, tuple);
}

template<typename T, typename C>
C convert(T const &val) {
  return constructFromTuple<C>(val);
}

template<> std::string convert(std::vector<char> const &val) {
  std::string res;
  std::copy(std::begin(val), std::end(val), std::back_inserter(res));
  return res;
}

template <> std::vector<char> convert(std::string const &val) {
  std::vector<char> res;
  std::copy(std::begin(val), std::end(val), std::back_inserter(res));
  return res;
}

template <> std::string convert(char const &val) {
  return {val};
}

template<> std::string convert(std::tuple<char, std::string> const &val) {
    char first;
    std::string rest;
    std::tie(first, rest) = val;
    return std::string{first} + rest;
}

template<> std::string convert(std::tuple<std::string, char> const &val) {
    std::string first;
    char rest;
    std::tie(first, rest) = val;
    return first + std::string{rest};
}

template<> std::string convert(std::tuple<char, char> const &val) {
    char first;
    char rest;
    std::tie(first, rest) = val;
    return std::string{first} + std::string{rest};
}


template <typename T> std::vector<T> convert(T const &val) { return {val}; }



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
  explicit constexpr operator bool() const {
    return std::holds_alternative<T>(*this);
  }

  /// Converts tuples where all elements have the same type
  /// into a container (like a `std::vector` or a `std::string`)
  /// or any other type
  /// which can be constructed from the tuple's elements
  template <typename C> operator Result<C>() const {
    if (bool(*this)) {
      return convert<T, C>(std::get<T>(*this));
    } else {
      return std::get<ErrorMessage>(*this);
    }
  }
};

template <typename A, typename B>
auto constexpr operator +(Result<A> const &lhs, Result<B> const &rhs) -> Result<decltype(wrapInTuple(std::get<A>(lhs), std::get<B>(rhs)))> {
  if(!bool(lhs)) {
    return std::get<ErrorMessage>(lhs);
  }

  if(!bool(rhs)) {
    return std::get<ErrorMessage>(rhs);
  }

  return wrapInTuple(std::get<A>(lhs), std::get<B>(rhs));
};

template <typename A>
auto constexpr operator |(Result<A> const &lhs, Result<A> const &rhs) {
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
  constexpr Parser(Parser<T> (*ptr)()) :
    Parser(ptr())
  {}
};

// /// Run any function regardless of current input
// /// Using this directly is not often useful, except maybe for debugging.
// template<typename F> auto constexpr action(F fun) {
//   return [=](std::istream &) {
//     return fun();
//   };
// }

/// Unconditionally construct many1thing, regardless of current input.
template<typename T>
constexpr Parser<T> constant(T const &val){
  return [=](std::istream &) {
    return val;
  };
}

template<typename DefaultConstructible>
constexpr Parser<DefaultConstructible> constant() {
  return constant(DefaultConstructible{});
}

/// ignore result of `parser`
/// and just return an empty tuple
/// (which will disappear when this parser is used in sequence with other parsers)
template<typename T>
Parser<std::tuple<>> ignore(Parser<T> const &parser) {
  return transform(parser, [](T const &) { return std::make_tuple(); });
}

/// Syntactic sugar to be able to call ignore also without parentheses for parameter-less parsers. :-)
template<typename T>
Parser<std::tuple<>> ignore(Parser<T> (*parser)()) {
  return ignore(parser());
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

#if false
/// Parse exactly the character `target`.
Parser<char> char_(char target) {
  return [=](std::istream &input) -> Result<char> {
    char val = input.get();
    if (val == target) {
      return val;
    } else {
      input.unget();
      return ErrorMessage{std::string{1, target}};
    }
  };
}

#else
// Or alternatively:
Parser<char> char_(char target) {
  return satisfy([target](char val) { return val == target; }, std::string{target}.c_str());
}
#endif

/// Parse any decimal digit
Parser<char> digit() {
  return satisfy(isdigit, "digit");
}

/// Parse exactly the string `target`.
/// Note that this implementation allows for backtracking and is inefficient.
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


/// Applicative Functor 'apply', also known as `<*>`.
/// Runs the function 'fun' on input 'val'
/// assuming that val_parser and fun_parser (in that order)
/// are both successful.
/// I did _not_ cover this during the presentation as it is a rather meta-heavy higher-order function.
template <typename F, typename A>
auto ap(Parser<A> const &val_parser, Parser<F> const &fun_parser) {
  return [=](std::istream &input) -> Result<std::invoke_result_t<F, A const &>> {
    Result<A> val_res = val_parser(input);
    if (!bool(val_res)) {
      return std::get<ErrorMessage>(val_res);
    }

    Result<F> fun_res = fun_parser(input);
    if (!bool(fun_res)) {
      return std::get<ErrorMessage>(fun_res);
    }

    return std::invoke(std::get<F>(fun_res), (std::get<A>(val_res)));
  };
}

#if false
/// Transform the result of a parser into another type of result
/// by running a function `fun` on it.
///
/// (Note: The templated arguments allow C++ to do more automatic type-inference
/// than if we'd use `std::function` instead.)
template<typename F, typename A> auto transform(Parser<A> const &parser, F const &fun) {

  return [=](std::istream &input) -> Result<std::invoke_result_t<F, A const &>> {
    Result<A> res = parser(input);
    if(!bool(res)) { return std::get<ErrorMessage>(res); }

    return fun(std::get<A>(res));
  };
}

#else

template <typename F, typename A>
auto transform(Parser<A> const &parser, F const &fun) {
  // Indirection which is necessary to keep the compiler happy
  // since lambdas cannot directly be stored in `Result`'s:
  using Res = std::invoke_result_t<F, A const &>;
  std::function<Res(A const &)> wrapped_fun = fun;

  return ap(parser, constant(wrapped_fun));
}

#endif

template <typename F, typename A>
auto transformError(Parser<A> const &parser, F &&fun)->Parser<A> {
  return [=](std::istream &input) -> Result<A> {
    Result<A> res = parser(input);
    if (!bool(res)) {
      return ErrorMessage{fun(std::get<ErrorMessage>(res))};
    }

    return res;
  };
}

  Parser<std::tuple<>> eof() {
    return transformError(ignore(char_(std::char_traits<char>::eof())),
                          [](auto &&) { return "<end of input>"; });
  }

  /// Transform the result of a Parser<Tuple> into another type of result
  /// by running a function `fun` on it.
  /// `fun` should be overloaded exactly for the number and types of the
  /// elements in `Tuple`.
  ///
  /// (Note: The templated arguments allow C++ to do more automatic
  /// type-inference than if we'd use `std::function` instead.)
  template <typename F, typename Tuple>
  auto transformApply(Parser<Tuple> const &parser, F const &fun) {
    return transform(parser, [=](auto &&res) { return std::apply(fun, res); });
  }

  /// Compose two parsers in sequence: Run `p1` followed by `p2`.
  /// Fails when either one fails, only succeeds if both suceed.
  template <typename A, typename B>
  Parser<typename decltype(Result<A>() + Result<B>())::value_type> operator>>(
      Parser<A> const &p1, Parser<B> const &p2) {
    return [=](std::istream &input) -> decltype(Result<A>() + Result<B>()) {
      Result<A> res1 = p1(input);
      if (!bool(res1)) {
        return std::get<ErrorMessage>(res1);
      } else {
        Result<B> res2 = p2(input);
        return res1 + res2;
      }
    };
  }

  /// Compose two parsers as alternatives: Run `p1` or alternatively `p2`.
  /// Fails when both fail, succeeds when either one succeeds.
  template <typename A>
  Parser<A> operator|(Parser<A> const &lhs, Parser<A> const &rhs) {
    return [=](std::istream &input) -> Result<A> {
      Result<A> res1 = lhs(input);
      if (bool(res1)) {
        return res1;
      }

      Result<A> res2 = rhs(input);
      if (bool(res2)) {
        return res2;
      }

      return ErrorMessage{std::get<ErrorMessage>(res1).message + " or " +
                          std::get<ErrorMessage>(res2).message};
    };
  }

  template <typename A>
  Parser<A> operator+(Parser<A> const &lhs, Parser<A> const &rhs) {
    return transformApply(lhs >> rhs, std::plus());
  }

/// Note: `many` and `many1` are mutually recursive.

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
Parser<Container> many1(Parser<typename Container::value_type> element_parser);

#if false
template<typename Container>
Parser<Container> many(Parser<typename Container::value_type> element_parser) {
  // Wrapping is necessary to make this 'lazy'.
  // otherwise we'd stackoverflow on construction by infinite recursion
  return lazy(many1<Container>(element_parser) | constant(Container{}));
}

template<typename Container>
Parser<Container> many1(Parser<typename Container::value_type> element_parser) {
  return element_parser >> many<Container>(element_parser);
}

#else
template <typename Container>
Parser<Container> many(Parser<typename Container::value_type> element_parser) {
  return many1<Container>(element_parser) | constant(Container{});
}

template <typename Container>
Parser<Container> many1(Parser<typename Container::value_type> element_parser) {
  using T = typename Container::value_type;
  return [=](std::istream &input) -> Result<Container> {
    auto first = element_parser(input);
    if (!bool(first)) {
      return std::get<ErrorMessage>(first);
    }

    Container container;
    container.push_back(std::get<T>(first));

    while (true) {
      auto next = element_parser(input);
      if (!bool(next)) {
        return container;
      }

      container.push_back(std::get<T>(next));
    }
  };
}

#endif

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

Parser<std::string> parser4 = many1<std::string>(char_('z'));

Parser<std::string> parser5 = transformApply(char_('x') >> string_("asdf"), [](auto lhs, auto rhs) {
  return lhs + rhs;
 });

template<typename T>
Parser<T> maybe(Parser<T> elem) {
  return elem | constant(T{});
}

Parser<std::string> digits = many1<std::string>(digit);
Parser<size_t> uint_ = transform(digits, [](std::string const &str){ return std::stoul(str.c_str()); });

// Parser<std::string> maybe_sign = string_("+") | string_("-") | string_("");

Parser<std::string> maybe_sign() {
  return maybe(char_('+') | char_('-'));
}

Parser<std::string> signed_digits = maybe_sign() >> digits;
Parser<long int> int_ = transform(signed_digits, [](std::string const &str) { return std::stol(str.c_str()); });

double strtodouble(std::string const &str) { return std::stod(str); }

Parser<double> double_() {
  Parser<std::string> vals =
    digits
    >> maybe<std::string>(char_('.') >> digits)
    >> maybe<std::string>(char_('e') >> signed_digits);

  return transform(vals, strtodouble);
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

template <typename A>
Parser<A> chainl1bhelper(Parser<A> elem, Parser<std::function<A(A, A)>> binop, A lhs) {
  return binop >> elem >> lazy(chainl1bhelper(elem, binop, lhs)) | constant(lhs);
}

template<typename A>
Parser<A> chainl1b(Parser<A> elem, Parser<std::function<A(A, A)>> binop) {
  auto rest = [=](A const &lhs) {
    return chainl1bhelper(elem, binop, lhs);
  };
  return ap(elem, rest);
}

/// chainl1(lhs, op) { return ap(lhs, chainl1tail); }
/// chainl1tail(lhs) { return ap(op >> rhs, [=](op, rhs){ return chainl1tail(op(lhs, rhs)); })} | constant(lhs);

  template <typename A>
  Parser<A> chainl(Parser<A> const &elem,
                   Parser<std::function<A(A, A)>> const &binop, A default_val) {
    return chainl1 | constant(default_val);
  }

  template <typename A>
  Parser<A> chainl(Parser<A> const &elem,
                   Parser<std::function<A(A, A)>> const &binop) {
    return chainl1 | constant(A{});
  }

  template <typename T> Parser<T> choice(Parser<T> const &parser) {
    return parser;
  }

  template <typename T, typename... Ts>
  Parser<T> choice(Parser<T> const &first, Ts... rest) {
    return first | choice(rest...);
  }

  template <typename A>
  Parser<A> chainr1(Parser<A> const &elem,
                    Parser<std::function<A(A, A)>> const &binop) {
    using F = std::function<A(A, A)>;

    return [=](std::istream &input) -> Result<A> {
      Result<A> lhs = elem(input);
      if (!bool(lhs)) {
        return lhs;
      }

      Result<A> res = lhs;

      Result<F> binop_res = binop(input);
      if (!bool(binop_res)) {
        return res;
      }

      Result<A> rhs = chainr1(elem, binop)(input);
      if (!bool(rhs)) {
        return res;
      }

      return std::get<F>(binop_res)(std::get<A>(res), std::get<A>(rhs));
    };
  }

  template <typename A>
  Parser<A> chainr(Parser<A> const &elem,
                   Parser<std::function<A(A, A)>> const &binop,
                   A const &default_val) {
    return chainr1 | constant(default_val);
  }

  template <typename A>
  Parser<A> chainr(Parser<A> const &elem,
                   Parser<std::function<A(A, A)>> const &binop) {
    return chainr1 | constant(A{});
  }

  template <typename A> class Precedence {
  public:
    virtual ~Precedence() = default;

    virtual Parser<A> toParser(Parser<A> const &inner) const = 0;

    operator std::reference_wrapper<Precedence<A>>() { return std::ref(*this); }
  };

  template <typename A> class Unaryop : public Precedence<A> {
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

  template <typename A> class UnaryopPrefix : public Unaryop<A> {
    using F = std::function<A(A)>;
    using Unaryop<A>::Unaryop;

  private:
    Parser<A> toParserImpl(Parser<A> const &inner,
                           Parser<F> const &unaryop_parser) const final {
      return [=](std::istream &input) -> Result<A> {
        Result<F> fun_res = unaryop_parser(input);
        if (!bool(fun_res)) {
          return std::get<ErrorMessage>(fun_res);
        }

        Result<A> inner_res = inner(input);
        if (!bool(inner_res)) {
          return std::get<ErrorMessage>(inner_res);
        }

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
        : d_binops_parser(choice(first, rest...)){};

    Parser<A> toParser(Parser<A> const &inner) const final {
      return toParserImpl(inner, d_binops_parser);
    }

  private:
    virtual Parser<A> toParserImpl(Parser<A> const &inner,
                                   Parser<F> const &binop_parser) const = 0;
  };

  template <typename A> class BinaryopLeft : public Binop<A> {
    using F = std::function<A(A, A)>;
    using Binop<A>::Binop;

  private:
    Parser<A> toParserImpl(Parser<A> const &inner,
                           Parser<F> const &binop_parser) const final {
      return chainl1(inner, binop_parser);
    };
  };

  template <typename A> class BinaryopRight : public Binop<A> {
    using F = std::function<A(A, A)>;
    using Binop<A>::Binop;

  private:
    Parser<A> toParserImpl(Parser<A> const &inner,
                           Parser<F> const &binop_parser) const final {
      return chainr1(inner, binop_parser);
    };
  };

  template <typename A> Parser<A> makeExpressioParser(Parser<A> const &inner) {
    return inner;
  }

  template <typename A, typename... As>
  Parser<A> makeExpressioParser(Parser<A> const &inner,
                                Precedence<A> const &next, As... rest) {
    Parser<A> result = next.toParser(inner);
    return makeExpressioParser(result, rest...);
  }

  template <typename A, typename... As>
  Parser<A> makeExpressioParser(Parser<A>(*inner)(), As... rest) {
    return makeExpressioParser(inner(), rest...);
  }

  // #include <memory>
  // template <typename A>
  // Parser<A> makeExpressioParser(
  //                                         Parser<A> const &inner,
  //                                         std::vector <
  //                                         std::reference_wrapper<Precedence<A>>>
  //                                         const &table) {

  //   Parser<A> result = inner;
  //   for(auto &&row : table) {
  //     result = row.get().toParser(result);
  //   };
  //   return result;
  // };

  // template <typename A>
  // Parser<A> makeExpressioParser(
  //                                         Parser<A> (*inner)(),
  //                                         std::vector<std::reference_wrapper<Precedence<A>>>
  //                                         const &table) {
  //   return makeExpressioParser(inner(), table);
  // }

  template <typename A>
  BinaryopLeft<A> binary_left(
      Parser<std::function<A(A, A)>> const &binop_parser) {
    return BinaryopLeft<A>(binop_parser);
  }

  template <typename A, typename... Fs>
  BinaryopLeft<A> binary_left(
      Parser<std::function<A(A, A)>> const &first_binop_parser,
      Fs... other_binop_parsers) {
    return BinaryopLeft<A>(first_binop_parser, other_binop_parsers...);
  }

  template <typename A>
  BinaryopLeft<A> binary_left(Parser<std::function<A(A, A)>>(*binop_parser)()) {
    return BinaryopLeft<A>(binop_parser());
  }

  template <typename A, typename... Fs>
  BinaryopLeft<A> binary_left(
      Parser<std::function<A(A, A)>>(*first_binop_parser)(),
      Fs... other_binop_parsers) {
    return BinaryopLeft<A>(first_binop_parser(), other_binop_parsers()...);
  }

  template <typename A>
  BinaryopRight<A> binary_right(
      Parser<std::function<A(A, A)>> const &binop_parser) {
    return BinaryopRight<A>(binop_parser);
  }

  template <typename A, typename... Fs>
  BinaryopRight<A> binary_right(
      Parser<std::function<A(A, A)>> const &first_binop_parser,
      Fs... other_binop_parsers) {
    return BinaryopRight<A>(first_binop_parser, other_binop_parsers...);
  }

  template <typename A>
  BinaryopRight<A> binary_right(
      Parser<std::function<A(A, A)>>(*binop_parser)()) {
    return BinaryopRight<A>(binop_parser());
  }

  template <typename A, typename... Fs>
  BinaryopRight<A> binary_right(
      Parser<std::function<A(A, A)>>(*first_binop_parser)(),
      Fs... other_binop_parsers) {
    return BinaryopRight<A>(first_binop_parser(), other_binop_parsers()...);
  }

  template <typename A>
  UnaryopPostfix<A> postfix(Parser<std::function<A(A)>> const &unaryop_parser) {
    return UnaryopPostfix<A>(unaryop_parser);
  }

  template <typename A, typename... Fs>
  UnaryopPostfix<A> postfix(
      Parser<std::function<A(A)>> const &first_unaryop_parser,
      Fs... other_unaryop_parsers) {
    return UnaryopPostfix<A>(first_unaryop_parser, other_unaryop_parsers...);
  }

  template <typename A>
  UnaryopPrefix<A> prefix(Parser<std::function<A(A)>> const &unaryop_parser) {
    return UnaryopPrefix<A>(unaryop_parser);
  }

  template <typename A>
  UnaryopPrefix<A> prefix(Parser<std::function<A(A)>>(*unaryop_parser)()) {
    return UnaryopPrefix<A>(unaryop_parser());
  }

  template <typename A, typename... Fs>
  UnaryopPrefix<A> prefix(
      Parser<std::function<A(A)>> const &first_unaryop_parser,
      Fs... other_unaryop_parsers) {
    return UnaryopPrefix<A>(first_unaryop_parser(), other_unaryop_parsers()...);
  }

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
  Parser<std::function<double(double, double)>> plus() {
    return ignore(lex(string_("+"))) >> constant(std::plus());
  }

  Parser<std::function<double(double, double)>> minus() {
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

Parser<std::function<double(double)>> neg() {
  return ignore(lex(string_("-"))) >> constant(std::negate());
}

// template<typename A>
// Parser<std::function<A(A, A)>> addOp() {
//   return minus<A>() | plus<A>();
// }

// Parser<size_t> uint_expression() {
//   return chainl1(lex(uint_), lex(addOp<size_t>()));
// };

Parser<double> double_expression() {
  return chainl1(lex(double_), plus() | minus());
};

template<typename A, typename B, typename C>
Parser<A> between(Parser<B> const &lhs, Parser<C> const &rhs, Parser<A> const &middle) {
  return ignore(lhs) >> middle >> ignore(rhs);
}

template <typename A>
Parser<A> parenthesized(Parser<A> const &inner) {
  return between(lex(char_('(')), lex(char_(')')), inner);
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
          prefix(neg),
          binary_right(exp),
          binary_left(mul, divide),
          binary_left(plus, minus)
        );
}

Parser<double> term() {
  return lex(double_) | parenthesized(lazy(expression()));
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
