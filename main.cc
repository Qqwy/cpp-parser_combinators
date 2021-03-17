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


/// Concept which requires the type to behave like a container
/// The current implementation is not very nice;
/// it would be better to require exactly the operations that tupleToContainer uses.
#include <concepts>
template <typename Type>
concept HasValueType =
  requires(Type type)
  {
    // { std::remove_reference_t<Type>() }; // require a default constructor
    // { type.push_back(std::declval<std::remove_reference_t<Type>::value_type>()) }; // require it can pushback `Type` elements.
    { *type.begin() } -> std::same_as<typename std::remove_reference_t<Type>::value_type &>;
  };


/// Turns a tuple into a container.
/// Obviously this function template only exists
/// for tuples->containers where all of the tuples' containing types `Ts` are each convertible to the container's `value_type`.
/// `Container` might e.g. be `std::vector` or a `std::string`, etc.
///
/// As example: `std::tuple<char, char, char>` might be turned into a `std::string`.
template<size_t index = 0, HasValueType Container, typename... Ts>
constexpr Container tupleToContainerHelper(Container &&container, std::tuple<Ts...> const &tup) {
  if constexpr (index == sizeof...(Ts)) {
    return container;
  } else {
    container.push_back(std::get<index>(tup));
    return tupleToContainerHelper<index + 1>(container, tup);
  }
}

template<HasValueType Container, typename... Ts>
constexpr Container tupleToContainer(std::tuple<Ts...> const &tup) {
  return tupleToContainerHelper(Container{}, tup);
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
  template<HasValueType Container, typename = typename Container::value_type>
  operator Result<Container> () const {
    if(*this){
      return tupleToContainer<Container>(std::get<T>(*this));
    } else {
      return std::get<ErrorMessage>(*this);
    }
  }


  /// Converts tuples
  /// to a `ConstructableType`
  /// iff it has a constructor matching each of the tuple's types.
  template<typename ConstructableType>
  requires(!HasValueType<ConstructableType>)
  // operator Result<decltype(std::make_from_tuple<ConstructableType>(std::declval<T>()))> () const {
  operator Result<ConstructableType> () const {
    if(*this){
      return std::make_from_tuple<ConstructableType>(std::get<T>(*this));
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

    return ErrorMessage{std::get<ErrorMessage>(res1).message + " or " + std::get<ErrorMessage>(res2).message};
  };
}

template<typename Container>
Parser<Container> many(Parser<typename Container::value_type> element_parser);

template<typename Container>
Parser<Container> some(Parser<typename Container::value_type> element_parser);

template<typename Container>
Parser<Container> many(Parser<typename Container::value_type> element_parser) {
  return some<Container>(element_parser) | epsilon_<Container>();
}

template<typename Container>
Parser<Container> some(Parser<typename Container::value_type> element_parser) {
  Parser<std::tuple<typename Container::value_type, Container>> combined_parser = element_parser >> many<Container>(element_parser);
  return myapply(combined_parser, [](typename Container::value_type element_res, Container many_res){

    Container final_result{};
    final_result.push_back(element_res);
    final_result.insert(final_result.end(), many_res.begin(), many_res.end());
    return final_result;
  });
}

auto myparser = (char_('x') >> char_('y') >> char_('z')
                 | char_('a') >> char_('b') >> char_('c'))
              // | string_("foo")
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

// Parser<std::vector<char>> parser4 = some<std::vector<char>>(char_('z'));

Parser<std::string> parser5 = myapply(char_('x') >> string_("asdf"), [](auto lhs, auto rhs) {
  return lhs + rhs;
 });

#include <iostream>
#include <vector>

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

  runParser(myparser);
  runParser(parser2);
  runParser(parser3);
  // runParser(parser4);
  runParser(parser5);

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
