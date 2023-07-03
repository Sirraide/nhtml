#ifndef NHTML_INTERNAL_EVAL_HH
#define NHTML_INTERNAL_EVAL_HH

#ifndef NHTML_DISABLE_EVAL
#include <nhtml/utils.hh>
#include <nhtml/internal/utils.hh>
#include <nhtml/internal/source_location.hh>
#include <v8.h>

namespace nhtml::detail {
struct parser;
struct eval_impl;
class eval_ctx {
    eval_impl* impl;

public:
    eval_ctx(parser&);
    ~eval_ctx();
    eval_ctx(const eval_ctx&) = delete;
    eval_ctx(eval_ctx&&) = delete;
    eval_ctx operator=(const eval_ctx&) = delete;
    eval_ctx operator=(eval_ctx&&) = delete;

    /// Evaluate a script.
    auto operator()(string_ref script, loc l) -> res<void>;
};
}

#endif

#endif // NHTML_INTERNAL_EVAL_HH
