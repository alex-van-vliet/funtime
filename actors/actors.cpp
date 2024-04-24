#include <experimental/meta>

#include <algorithm>
#include <array>
#include <ranges>
#include <string_view>
#include <iostream>
#include <utility>
#include <functional>
#include <future>
#include <thread>
#include <queue>
#include <mutex>

// Move only function not yet available, a really really simple implem just for this use case
template<typename Ret, typename ...Args>
class MoveOnlyFunction {
    struct Callable {
        virtual Ret call(Args...) = 0;
        virtual ~Callable() = default;
    };
    template<typename T>
    struct Impl: Callable {
        Impl(T&& t): t{std::move(t)}
        {}

        T t;

        Ret call(Args... args) override {
            return t(args...);
        }
    };

public:
    MoveOnlyFunction()
    {}
    MoveOnlyFunction(auto&& fun): fun{new Impl{std::move(fun)}}
    {}

    auto operator()(auto&&... args) {
        return fun->call(std::forward<decltype(args)>(args)...);
    }

    bool empty() {
        return !fun;
    }

private:
    std::unique_ptr<Callable> fun{nullptr};
};

// For demo purposes; there are way better implementations out there
class Actor {
public:
    Actor(): thread{[&]{
        while (!stop) {
            MoveOnlyFunction<void> fun;
            
            {
                std::lock_guard lock{mutex};
                if (!messages.empty()) {
                    fun = std::move(messages.front());
                    messages.pop();
                }
            }

            if (!fun.empty()) {
                fun();
            }
        }
    }} {
    }

    ~Actor() {
        send([&]{
            stop = true;
        });
        thread.join();
    }

    void send(MoveOnlyFunction<void> fun) {
        std::lock_guard lock{mutex};
        messages.push(std::move(fun));
    }

private:
    std::thread thread;
    std::mutex mutex;
    std::queue<MoveOnlyFunction<void>> messages;
    bool stop{false};
};

template<auto Member>
struct Method {
    static auto operator()(auto& self, auto&&... arguments) {
        using return_type = decltype(self.instance.[: Member :](arguments...));

        std::promise<return_type> p;
        auto f = p.get_future();

        // self is not passable by copy, so we mask it and take the instance per reference
        self.actor.send([=, &self = self.instance, p = std::move(p)] mutable {
            if constexpr (std::is_same_v<return_type, void>) {
                p.set_value();
            } else {
                // Not forwarding as copied in lambda
                p.set_value(self.[: Member :](arguments...));
            }
        });

        return f;
    }
};

template<typename T, std::size_t I>
consteval bool is_finished() {
    auto members = members_of(^T);
    return members.size() <= I;
}

template<typename T, std::size_t I = 0>
consteval int add_members(std::vector<std::meta::info>& members) {
    if constexpr (is_finished<T, I>()) {
        return I;
    } else {
        constexpr auto member = members_of(^T)[I];

        if constexpr (std::meta::is_public(member)
            && std::meta::is_function(member)
            && !std::meta::is_special_member(member)) {
        
            members.push_back(std::meta::data_member_spec(^Method<member>, {
                .name=name_of(member), .is_static=true,
            }));
        }

        return add_members<T, I + 1>(members);
    }
}

template<typename Name, typename T>
consteval auto make_actor() {
    std::vector<std::meta::info> members{
        std::meta::data_member_spec(^T, {.name="instance"}),
        std::meta::data_member_spec(^Actor, {.name="actor"}),
    };
    
    add_members<T>(members);
    
    return define_class(^Name, members);
}

class CalculatorImpl {
public:
    int add(int i) {
        std::cout << "Calling add, original value " << value << ", adding " << i << std::endl; 
        value += i;
        return value;
    }

    void reset() {
        std::cout << "Calling reset, original value " << value << std::endl;
        value = 0;
    }

private:
    int value{0};
};

struct Calculator;
using Calculator = [: make_actor<Calculator, CalculatorImpl>() :];

int main() {
    Calculator calc;
    // Only data members are supported, so we have to pass the calculator as an argument
    auto v1 = Calculator::add(calc, 2);
    auto v2 = Calculator::add(calc, 3);
    auto reset = Calculator::reset(calc);

    std::cout << v1.get() << std::endl;
    std::cout << v2.get() << std::endl;
    reset.get();
    std::cout << "Reset" << std::endl;
}
