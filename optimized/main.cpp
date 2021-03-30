#include <functional>
#include <istream>
#include <iostream>

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

template <> std::string convert(std::tuple<char, char, std::string> const &val) {
  char first;
  char second;
  std::string rest;
  std::tie(first, second, rest) = val;
  return std::string{first} + std::string{second} + rest;
}

template <typename T> std::vector<T> convert(T const &val) { return {val}; }


struct ErrorMessage {
  const char *message;
};

template <typename T> struct Result {
    bool isSuccess = false;
    union {
        T success_val;
        ErrorMessage error_val;
    };

    constexpr Result(T const &val_) : isSuccess(true), success_val(val_) {};
    constexpr Result(ErrorMessage const &error_) : isSuccess(false), error_val(error_) {};
    constexpr Result(Result<T> const &other) {
        this->isSuccess = other.isSuccess;
        if(other.isSuccess) {
            this->success_val = other.success_val;
        } else {
            this->error_val = other.error_val;
        }
    };

    constexpr Result<T> &operator=(Result<T> const &other) {
        this->isSuccess = other.isSuccess;
        if(other.isSuccess) {
            this->success_val = other.success_val;
        } else {
            this->error_val = other.error_val;
        }
        return *this;
    }


    ~Result() {
        if(isSuccess) {
            success_val.~T();
        } else {
            error_val.~ErrorMessage();
        }
    }

    template<typename C>
    operator Result<C> () const {
        if(this->isSuccess) {
          return convert<T, C>(this->success_val);
        } else {
            return this->error_val;
        }
    }
};

template<typename P1, typename F>
consteval auto transform(P1 const &parser, F const &fun);

template<typename P1, typename F, typename Res>
struct Transform;

template<typename Derived, typename Res>
struct Parser {

    typedef Res value_type;
    
    // consteval virtual std::function<Res(std::istream &)> toFun() = 0;
    constexpr auto toFun() const {
        return static_cast<Derived const &>(*this).toFunImpl();
    }

    template<typename C>
    operator Parser<Derived, C> () const {
        return transform(*this, [](Res const &val) {return convert(val); });
    }
};


template<typename F>
struct SatisfyParser : public Parser<SatisfyParser<F>, char> {
    //bool (*fun)(char);
    F fun;

    using Parser<SatisfyParser<F>, char>::Parser;

    constexpr SatisfyParser(F const &fun_) : fun(fun_) {};
    // consteval SatisfyParser(SatisfyParser const &) = default;
    // consteval SatisfyParser(SatisfyParser &&) = default;

    constexpr auto toFunImpl() const {
        auto fun_ = fun;
        return [fun_](std::istream &input) -> Result<char> {
            const char val = input.peek();
            if(fun_(val)) {
                input.ignore();
                return val;
            } else {
                return ErrorMessage{"satisfy"};
            }
        };
    }
};

// Could be altered into a SatisfyParser maybe?
// struct CharParser : public Parser<CharParser, char> {
//     const char target;

//     constexpr CharParser(char target_) : target(target_) {};

//     constexpr auto toFunImpl() const {
//         auto target_ = this->target;
//         return [target_](std::istream &input) -> Result<char> {
//             const char val = input.peek();
//             if(val == target_) {
//                 input.ignore();
//                 return val;
//             } else {
//                 return ErrorMessage{"char"};
//             }
//         };
//     }
// };

template<typename P1, typename P2, typename Res = decltype(wrapInTuple(std::declval<typename P1::value_type>(), std::declval<typename P2::value_type>()))>
struct Combine : public Parser<Combine<P1, P2, Res>, Res> {
    using Parser<Combine<P1, P2, Res>, Res>::Parser;

    const P1 p1;
    const P2 p2;

    constexpr Combine(P1 const &p1_, P2 const &p2_) : p1(p1_), p2(p2_) {};

    constexpr auto toFunImpl() const {
        auto p1_impl = p1.toFun();
        auto p2_impl = p2.toFun();


        return [p1_impl, p2_impl](std::istream &input) -> Result<Res> {
            auto res1 = p1_impl(input);
            if(!res1.isSuccess) {return res1.error_val; }

            auto res2 = p2_impl(input);
            if (!res2.isSuccess) { return res2.error_val; }

            return wrapInTuple(res1.success_val, res2.success_val);
        };
    }
};

template<typename P1, typename P2, typename Res = typename P2::value_type>
struct Parallel : public Parser<Parallel<P1, P2, Res>, Res> {
    using Parser<Parallel<P1, P2, Res>, Res>::Parser;

    P1 p1;
    P2 p2;

    constexpr Parallel(P1 const &p1_, P2 const &p2_) : p1(p1_), p2(p2_) {};

    constexpr auto toFunImpl() const {
        auto p1_impl = p1.toFun();
        auto p2_impl = p2.toFun();


        return [p1_impl, p2_impl](std::istream &input) -> Result<Res> {
            auto res1 = p1_impl(input);
            if(res1.isSuccess) {return res1; }

            return p2_impl(input);
        };
    }
};

template<typename P1, typename F, typename Res = std::invoke_result_t<F, typename P1::value_type> >
// template<typename P1, typename F, typename Res = decltype(F('a')) >
struct Transform : public Parser<Transform<P1, F, Res>, Res> {
    using Parser<Transform<P1, F, Res>, Res>::Parser;

    const P1 p1;
    const F fun;

    constexpr Transform(P1 const &p1_, F const &fun_) : p1(p1_), fun(fun_) {};

    constexpr auto toFunImpl() const {
        auto p1_impl = this->p1.toFun();
        auto fun_ = fun;
        return [p1_impl, fun_](std::istream &input) -> Result<Res> {
            auto res = p1_impl(input);
            if(!res.isSuccess) { return res.error_val; }

            return fun_(res.success_val);
        };
    }
};

template<typename P1, typename Res>
struct Many1 : public Parser<Many1<P1, Res>, Res> {
    using Parser<Many1<P1, Res>, Res>::Parser;

    const P1 p1;

    constexpr Many1(P1 const &p1_) : p1(p1_) {};
    
    constexpr auto toFunImpl() const {
        auto p1_impl = p1.toFun();

        return [p1_impl](std::istream &input) -> Result<Res> {
            auto first = p1_impl(input);
            if(!first.isSuccess) { return first.error_val; }

            Res container = first.success_val;

            while(true) {
                auto next = p1_impl(input);
                if(!next.isSuccess) { return container; }
                Res next_container = next.success_val;

                std::copy(std::begin(next_container), std::end(next_container), std::back_inserter(container));
            }
        };
    }
};

/// Constant needs to take a lambda that will eventually produce a result
/// because otherwise it is impossible to produce non-constexpr types.
template<typename F>
struct Constant : public Parser<Constant<F>, typename std::invoke_result_t<F>> {
    using Res = typename std::invoke_result_t<F>;
    using Parser<Constant<F>, typename std::invoke_result_t<F>>::Parser;

    F res_fun;

    constexpr Constant(F const &res_fun_) : res_fun(res_fun_) {};

    constexpr auto toFunImpl() const {
        const auto res_fun_ = res_fun;
        return [res_fun_](std::istream &) -> Result<Res> {
            return res_fun_();
        };
    }
};


// template<typename P1, typename P2, typename A>
// consteval CombinedParser<P1, P2, A> operator>>(Parser<P1, A> const &lhs, Parser<P2, A> const &rhs) {

// };

template<typename P1, typename P2>
consteval auto operator>>(P1 const &p1, P2 const &p2) {
    return Combine{p1, p2};
}

template<typename P1, typename F>
consteval auto transform(P1 const &parser, F const &fun) {
    return Transform{parser, fun};
}

template<typename F>
consteval auto satisfy(F const &fun) {
    return SatisfyParser{fun};
}

template<typename Res = char>
consteval auto char_(char target) {
    return transform(
        satisfy([target](char const &val) {return val == target; }),
        [](char res) -> Res { return {res}; }
    );
}

template<typename Res>
consteval auto constant(Res const &res) {
    auto lam = [res]() { return res; };
    return Constant<decltype(lam)>{lam};
}

template<typename DefaultConstructable>
consteval auto constant() {
    auto lam = []() {return DefaultConstructable{}; };
    return Constant<decltype(lam)>{lam};
}

template<typename P1, typename P2>
consteval auto operator+(P1 const &p1, P2 const &p2) {
    return transform(Combine{p1, p2}, [](auto tup){
        return std::get<0>(tup) + std::get<1>(tup);
    });
}

template<typename P1, typename P2>
consteval auto operator|(P1 const &p1, P2 const &p2) {
    return Parallel{p1, p2};
}

template<typename P1, typename T = typename P1::value_type>
consteval auto many1(P1 const &parser) {
  return Many1<P1, T>{parser};
}

template<typename P1, typename T>
consteval auto many(P1 const &parser, T const &default_value) {
    return many1(parser) | constant(default_value);
}

template<typename P1>
consteval auto many(P1 const &parser) {
    return many1(parser) | constant<typename P1::value_type>();
}

template<typename P>
consteval auto toParseFun(P const &parser) {
    return parser.toFun();
}

int main() {
    // std::function<char(std::istream &)> fun = SatisfyParser{isdigit}.toFun();
    // auto fun = SatisfyParser{isdigit}.toFun();

    // auto fun = Combine{Combine{satisfy(isdigit), char_('+')}, satisfy(isdigit)}.toFun();
    // auto fun = (
    //             (char_('+') >> char_('-')) | 
    //             (char_('a') >> satisfy(isdigit))
    // constexpr auto parser = many1(char_<std::string>('a'));
  constexpr auto parser = (char_('A') >> char_('B') >> many1(char_<std::string>('B')));
  // constexpr auto parser = char_<std::string>('B');
        //satisfy(isdigit)
//    >> char_('+') >> satisfy(isdigit) >> satisfy(isdigit) | char_('a')
//    >> char_('+') >> satisfy(isspace) >> satisfy(isdigit)
//    >> char_('+') >> satisfy(isdigit) >> satisfy(isdigit)
//    >> char_('+') >> satisfy(isspace) >> satisfy(isdigit)
//    >> char_('+') >> satisfy(isdigit) >> satisfy(isdigit)
//    >> char_('+') >> satisfy(isspace) >> satisfy(isdigit)
//    >> char_('+') >> satisfy(isdigit) >> satisfy(isdigit)
//    >> char_('+') >> satisfy(isspace) >> satisfy(isdigit)
//    >> char_('+') >> satisfy(isdigit) >> satisfy(isdigit)
//     >> char_('+') >> satisfy(isspace) >> satisfy(isdigit)
    //std::function<Result<std::string>(std::istream &)> fun = toParseFun(parser);
    auto fun = toParseFun(parser);

    Result<std::string> res = fun(std::cin);
    if(res.isSuccess) {
        std::cout << "parse success! " << res.success_val << " \n";
    } else {
        std::cout << "parse failure\n";
    }
}
