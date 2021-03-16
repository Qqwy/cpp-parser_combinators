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

// #include <concepts>
// template <class ContainerType>
// concept HasValueType = requires (ContainerType ct) {
//   { *ct.begin() } -> std::same_as<typename ContainerType::value_type>;
// };
#include <concepts>
template <typename Type>
concept HasValueType =
  requires(Type type)
  {
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

    return ErrorMessage{std::get<ErrorMessage>(res1).message + " or " + std::get<ErrorMessage>(res2).message};
  };
}

auto myparser = char_('x') >> char_('y') >> char_('z')
              | char_('a') >> char_('b') >> char_('c')
              // | string_("foo")
              ;

auto parser2 = string_("foo") | string_("faa");


struct Person {
  std::string d_first_name;
  std::string d_last_name;

  Person(std::string const &first_name, std::string const &last_name)
    : d_first_name(first_name),
      d_last_name(last_name) {};
};

auto parser3 = string_("john") >> string_("snow");

#include <iostream>
#include <vector>
int main() {
  std::cout << "Hello, world!\n";
  int x = 42;
  std::string foo = "foo";
  // AutoTuple res = AutoTuple<int>(x) + foo;
  Result<int> y = x;
  Result<std::string> bar = foo;
  // int baz = y + bar;

  // Result<std::tuple<char, char, char>> result = myparser(std::cin);
  Result<std::string> result = myparser(std::cin);
  if(!result) {
    std::cout << "Syntax error: Expected " << std::get<ErrorMessage>(result).message << '\n';
  } else {
    std::cout << "Parse success!\n";
  }

  Result<std::string> result2 = parser2(std::cin);
  if(!result2) {
    std::cout << "Syntax error: Expected " << std::get<ErrorMessage>(result2).message << '\n';
  } else {
    std::cout << "Parse success!\n";
  }

  Result<Person> result3 = parser3(std::cin);
  if(!result3) {
    std::cout << "Syntax error: Expected " << std::get<ErrorMessage>(result3).message << '\n';
  } else {
    std::cout << "Parse success!\n";
  }
}
