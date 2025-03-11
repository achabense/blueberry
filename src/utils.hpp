#pragma once

#include <bit>
#include <concepts>
#include <functional>
#include <type_traits>
#include <utility>

struct no_create {
    no_create() = delete;
};

struct no_copy {
    no_copy() = default;
    no_copy(const no_copy&) = delete;
    no_copy(no_copy&&) = delete;
    no_copy& operator=(const no_copy&) = delete;
    no_copy& operator=(no_copy&&) = delete;
};

// ~ `requires requires` makes errors happen at call sites instead of inside the function.
template <class T, class U>
    requires requires(T& t, const U& u) {
        t == u;
        t = u;
    }
inline bool compare_update(T& t, const U& u) {
    if (t == u) {
        return false;
    } else {
        t = u; // -> assert(t == u);
        return true;
    }
}

// Function-ref for const-invocable types; suitable for replacing one-off usage of std::function or invocable auto (to be more IDE friendly).
// References:
// https://stackoverflow.com/questions/36645660/why-cant-i-cast-a-function-pointer-to-void
// https://stackoverflow.com/questions/62644070/differences-between-stdis-convertible-and-stdconvertible-to-in-practice
// https://stackoverflow.com/questions/28033251/can-you-extract-types-from-template-parameter-function-signature
// (Forget C++26 `std::function_ref`.)
// (There used to be many `const std::invocable<...> auto&` in the project. Actually the constraints were incorrect as they applied to T instead of const T, so they required only non-const-invocable.)

// (Using bit-cast as gcc rejects static-cast.)
inline constexpr bool may_store_fp_as_voidp = requires(void (*fp)()) { std::bit_cast<void*>(fp); };

template <class From, class To>
concept implicitly_convertible_to = std::is_convertible_v<From, To>;

// (T -> const T& shouldn't go across function calls.)
template <class From, class To>
concept implicitly_returnable_as =
    implicitly_convertible_to<From, To> && !(!std::is_reference_v<From> && std::is_reference_v<To>);

template <class S>
class func_ref_impl;

// TODO: does `return static_cast<T>(prvalue)` introduce additional copy? (Does guaranteed copy elision apply to this?)
// (The current impl is not performing explicit conversion due to this; can R be &&-ref?)
template <class R, class... Args>
    requires(may_store_fp_as_voidp)
class func_ref_impl<R(Args...)> {
    const void* context;
    R (*thunk)(const void*, Args&&...);

public:
    func_ref_impl(const func_ref_impl&) = default;
    func_ref_impl& operator=(const func_ref_impl&) = delete;

    template <class F>
        requires requires(const F& fn, Args&&... args) {
            { fn(static_cast<Args&&>(args)...) } -> implicitly_returnable_as<R>;
            // { fn(static_cast<Args&&>(args)...) } -> std::same_as<R>;
        }
    func_ref_impl(const F& fn) {
        context = std::bit_cast<const void*>(&fn /*no bother with std::addressof(fn); I hate it*/);
        thunk = [](const void* ctx, Args&&... args) -> R { //
            return (*std::bit_cast<const F*>(ctx))(static_cast<Args&&>(args)...);
        };
    }

    R operator()(Args... args) const { //
        static_assert(std::is_trivially_copyable_v<func_ref_impl>);
        return thunk(context, static_cast<Args&&>(args)...);
    }
};

template <class S>
using func_ref = std::conditional_t<may_store_fp_as_voidp, func_ref_impl<S>, std::function<S>>;
