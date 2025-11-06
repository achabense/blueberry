#pragma once

// #include <bit>
#include <concepts>
// #include <functional>
#include <cassert>
#include <type_traits>
#include <utility>

#define assert_implies(a, b) assert(!(a) || (b))

#ifndef NDEBUG
#define YDEBUG
inline constexpr bool debug_mode = true;
#else
inline constexpr bool debug_mode = false;
#endif

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

// `static_assert(false)` was introduced as a DR in C++23. However, there is no standard way (macro / constexpr bool)
// to detect whether the DR is actually implemented. As a result, the DR cannot be relied upon in C++20...
template <class T>
inline constexpr bool always_false_v = false;

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
// https://stackoverflow.com/questions/6640254/how-to-store-various-types-of-function-pointers-together
// https://stackoverflow.com/questions/62644070/differences-between-stdis-convertible-and-stdconvertible-to-in-practice
// https://stackoverflow.com/questions/28033251/can-you-extract-types-from-template-parameter-function-signature
// (`std::function_ref` doesn't exist in real world.)
// (There used to be many `const std::invocable<...> auto&` in the project. Actually the constraints were incorrect as they applied to T instead of const T, so they required only non-const-invocable.)

// (Used to rely on std::bit_cast.)
// inline constexpr bool may_store_fp_as_voidp = requires(void (*fp)()) { std::bit_cast<void*>(fp); };

union void_ptr {
    const void* op;
    void (*fp)();

    using fp_type = decltype(fp);
};

template <class From, class To>
concept implicitly_convertible_to = std::is_convertible_v<From, To>;

// (T -> const T& shouldn't go across function calls.)
template <class From, class To>
concept implicitly_returnable_as =
    implicitly_convertible_to<From, To> && !(!std::is_reference_v<From> && std::is_reference_v<To>);

template <class S>
class func_ref;

// TODO: does `return static_cast<T>(prvalue)` introduce additional copy? (Does guaranteed copy elision apply to this?)
// (The current impl is not performing explicit conversion due to this; can R be &&-ref?)
template <class R, class... Args>
class func_ref<R(Args...)> {
    void_ptr context;
    R (*thunk)(void_ptr, Args&&...);

public:
    func_ref(const func_ref&) = default;
    func_ref& operator=(const func_ref&) = delete;

    template <class F>
        requires requires(const F& fn, Args&&... args) {
            { fn(static_cast<Args&&>(args)...) } -> implicitly_returnable_as<R>;
            // { fn(static_cast<Args&&>(args)...) } -> std::same_as<R>;
        }
    func_ref(const F& fn) {
        if constexpr (std::is_function_v<F>) {
            static_assert(std::is_same_v<const F&, F&> && std::is_same_v<const F*, F*>);
            context.fp = reinterpret_cast<void_ptr::fp_type>(&fn);
            thunk = [](void_ptr context, Args&&... args) -> R { //
                return (*reinterpret_cast<const F*>(context.fp))(static_cast<Args&&>(args)...);
            };
        } else {
            // (No bother with std::addressof(fn); I hate it.)
            context.op = &fn;
            thunk = [](void_ptr context, Args&&... args) -> R { //
                return (*static_cast<const F*>(context.op))(static_cast<Args&&>(args)...);
            };
        }
    }

    R operator()(Args... args) const {
        static_assert(std::is_trivially_copyable_v<func_ref>);
        return thunk(context, static_cast<Args&&>(args)...);
    }
};
