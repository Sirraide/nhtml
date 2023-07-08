#ifndef NHTML_DISABLE_EVAL
#    include <expected>
#    include <libplatform/libplatform.h>
#    include <mutex>
#    include <nhtml/internal/eval.hh>
#    include <nhtml/internal/parser_impl.hh>

using namespace nhtml::detail;
using namespace v8;

namespace nhtml::detail {
struct eval_impl {
    using eval = eval_impl;
    ArrayBuffer::Allocator* alloc;
    Isolate* isolate;
    parser& p;
    Persistent<ObjectTemplate> globl_tmpl;
    Persistent<ObjectTemplate> element_tmpl;
    Persistent<ObjectTemplate> children_tmpl;
    Persistent<ObjectTemplate> attributes_tmpl;
    Persistent<ObjectTemplate> attributes_it_tmpl;
    Persistent<ObjectTemplate> children_it_tmpl;

    /// Helper to export a global function. Must not be called outside the constructor.
    template <auto cb>
    void export_global(Local<ObjectTemplate>& tm, auto&& name) {
        auto wrapper = [](const FunctionCallbackInfo<Value>& info) {
            HandleScope hs{info.GetIsolate()};
            auto val = cb(info);
            if (val.IsEmpty()) info.GetReturnValue().SetUndefined();
            else info.GetReturnValue().Set(val);
        };

        tm->Set(isolate, name, FunctionTemplate::New(isolate, wrapper));
    };

    /// ===========================================================================
    ///  Object template helpers.
    /// ===========================================================================
    using template_t = Persistent<ObjectTemplate> eval_impl::*;

    /// CRTP helper for types that have a native handle pointer.
    template <typename handle_ty, template_t obj_templ, std::size_t fields = 1>
    requires (not std::is_pointer_v<handle_ty>)
    struct object {
        static auto handle(const auto& info) -> handle_ty* {
            return static_cast<handle_ty*>(
                Local<External>::Cast(info.Holder()->GetInternalField(0))->Value()
            );
        }

        using handle_type = handle_ty;
        static constexpr template_t object_template = obj_templ;
        static constexpr usz field_count = fields;
    };

    /// CRTP index-based iterator template. For internal use in register_template() only.
    template <typename derived, typename handle_type>
    struct iterator_base {
        static void next(const FunctionCallbackInfo<Value>& info) {
            auto I = info.GetIsolate();
            HandleScope hs{I};

            /// Get element.
            auto e = static_cast<handle_type*>(
                Local<External>::Cast(info.Holder()->GetInternalField(0))->Value()
            );

            /// Get index.
            auto i = int(Local<Integer>::Cast(info.Holder()->GetInternalField(1))->Value());

            /// Create return value.
            auto obj = info.Holder();
            auto ctx = I->GetCurrentContext();
            auto ret = Object::New(I);
            if (not derived::done(e, i)) {
                obj->SetInternalField(1, Integer::New(I, i + 1));
                ret->Set(ctx, S(I, "done"), False(I)).Check();
                ret->Set(ctx, S(I, "value"), derived::value(I, e, i)).Check();
            } else {
                ret->Set(ctx, S(I, "done"), True(I)).Check();
            }
            info.GetReturnValue().Set(ret);
        }
    };

    /// CRTP index-based iterator template for iterable types.
    template <typename derived, template_t iterator_obj_templ>
    struct iterable {
        static void create_iterator(const FunctionCallbackInfo<Value>& info) {
            auto I = info.GetIsolate();
            HandleScope hs{I};

            /// Create iterator.
            auto e = derived::handle(info);
            auto iter = instantiate(I, context(I)->*iterator_obj_templ);
            iter->SetInternalField(0, External::New(I, e));
            iter->SetInternalField(1, Integer::New(I, 0));
            info.GetReturnValue().Set(iter);
        }

        static constexpr template_t iterator_object_template = iterator_obj_templ;
    };

    /// ===========================================================================
    ///  Object templates.
    /// ===========================================================================
    /// Accessors for elements.
    struct $element : object<element, &eval::element_tmpl> {
        /// Get attributes of an element.
        static void get_attributes(Local<String>, const PropertyCallbackInfo<Value>& info) {
            auto I = info.GetIsolate();
            HandleScope hs{I};

            /// Create wrapper.
            Local<Object> obj = instantiate(I, context(I)->attributes_tmpl);
            obj->SetInternalField(0, External::New(I, handle(info)));
            info.GetReturnValue().Set(obj);
        }

        /// Get children of an element.
        static void get_children(Local<String>, const PropertyCallbackInfo<Value>& info) {
            auto I = info.GetIsolate();
            HandleScope hs{I};

            /// Create wrapper.
            Local<Object> obj = instantiate(I, context(I)->children_tmpl);
            obj->SetInternalField(0, External::New(I, handle(info)));
            info.GetReturnValue().Set(obj);
        }

        /// Get ID of an element.
        static void get_id(Local<String>, const PropertyCallbackInfo<Value>& info) {
            auto I = info.GetIsolate();
            HandleScope hs{I};
            info.GetReturnValue().Set(S(I, handle(info)->id));
        }

        /// Set attributes of an element.
        static void set_attributes(
            Local<String>,
            Local<Value> new_attrs,
            const PropertyCallbackInfo<void>& info
        ) {
            auto I = info.GetIsolate();
            HandleScope hs{I};

            /// Value must be a key-value map.
            if (not new_attrs->IsObject()) {
                I->ThrowError("attributes must be an object");
                return;
            }

            /// Set attributes.
            auto e = handle(info);
            auto obj = new_attrs->ToObject(I->GetCurrentContext()).ToLocalChecked();
            auto ctx = I->GetCurrentContext();
            auto names = obj->GetOwnPropertyNames(ctx).ToLocalChecked();
            e->attributes.clear();
            for (u32 i = 0, end = names->Length(); i < end; i++) {
                auto name = names->Get(ctx, i).ToLocalChecked();
                auto value = obj->Get(ctx, name).ToLocalChecked();
                String::Utf8Value utf8_name{I, name};
                String::Utf8Value utf8_value{I, value};
                (void) e->attributes.try_emplace(
                    std::string{*utf8_name, size_t(utf8_name.length())},
                    std::string{*utf8_value, size_t(utf8_value.length())}
                );
            }
        }

        /// Set ID of an element.
        static void set_id(Local<String>, Local<Value> s, const PropertyCallbackInfo<void>& info) {
            auto I = info.GetIsolate();
            HandleScope hs{I};
            String::Utf8Value utf8{I, s->ToString(I->GetCurrentContext()).ToLocalChecked()};
            handle(info)->id = trim(std::string{*utf8, size_t(utf8.length())});
        }
    };

    /// Accessors for attributes.
    struct $attributes
        : object<element, &eval::attributes_tmpl>
        , iterable<$attributes, &eval::attributes_it_tmpl> {
        struct iterator {
            static bool done(handle_type* e, int index) { return index >= int(e->attributes.size()); }
            static auto value(Isolate* I, handle_type* e, int index) {
                auto it = std::next(e->attributes.begin(), index);
                auto pair = Array::New(I, 2);
                auto ctx = I->GetCurrentContext();
                pair->Set(ctx, 0, S(I, it->first)).Check();
                pair->Set(ctx, 1, S(I, it->second)).Check();
                return pair;
            }
        };

        /// Access an attribute.
        static void get(Local<Name> name, const PropertyCallbackInfo<Value>& info) {
            auto I = info.GetIsolate();
            HandleScope hs{I};

            String::Utf8Value utf8{I, name};
            auto h = handle(info);
            auto it = h->attributes.find(std::string{*utf8, size_t(utf8.length())});
            if (it != h->attributes.end()) {
                info.GetReturnValue().Set(S(I, it->second));
            }
        }

        /// Set an attribute.
        static void set(Local<Name> name, Local<Value> val, const PropertyCallbackInfo<Value>& info) {
            auto I = info.GetIsolate();
            HandleScope hs{I};

            auto h = handle(info);
            String::Utf8Value utf8{I, name};
            String::Utf8Value utf8_val{I, val->ToString(I->GetCurrentContext()).ToLocalChecked()};
            h->attributes[std::string{*utf8, size_t(utf8.length())}] = std::string{*utf8_val, size_t(utf8_val.length())};
        }

        /// Delete an attribute.
        static void del(Local<Name> name, const PropertyCallbackInfo<Boolean>& info) {
            auto I = info.GetIsolate();
            HandleScope hs{I};

            auto h = handle(info);
            String::Utf8Value utf8{I, name};
            auto it = h->attributes.find(std::string{*utf8, size_t(utf8.length())});
            if (it != h->attributes.end()) {
                h->attributes.erase(it);
                info.GetReturnValue().Set(true);
            } else {
                info.GetReturnValue().Set(false);
            }
        }
    };

    /// Accessors for children of an element.
    struct $children
        : object<element, &eval::children_tmpl>
        , iterable<$children, &eval::children_it_tmpl> {
        struct iterator {
            static bool done(handle_type* e, int index) {
                auto ch = std::get_if<element::vector>(&e->content);
                return not ch or index >= int(ch->size());
            }

            static auto value(Isolate* I, handle_type* e, int index) {
                auto it = std::next(std::get<element::vector>(e->content).begin(), index);
                auto r = context(I)->element_tmpl.Get(I)->NewInstance(I->GetCurrentContext()).ToLocalChecked();
                r->SetInternalField(0, External::New(I, it->get()));
                return r;
            }
        };
    };

    /// ===========================================================================
    ///  Global functions.
    /// ===========================================================================
    /// Query selector.
    static Local<Value> $$(const FunctionCallbackInfo<Value>& info) {
        auto I = info.GetIsolate();

        /// Invalid selector.
        if (info.Length() < 1 or not info[0]->IsString()) return {};

        /// Perform query.
        auto this_ = context(I);
        String::Utf8Value selector{I, info[0]};
        auto res = this_->p.query_selector(std::string_view{*selector, size_t(selector.length())});
        if (not res) return {};

        /// Return result.
        auto r = this_->element_tmpl.Get(I)->NewInstance(I->GetCurrentContext()).ToLocalChecked();
        r->SetInternalField(0, External::New(I, res));
        return r;
    }

    /// Print to stdout.
    static Local<Value> $print(const FunctionCallbackInfo<Value>& info) {
        auto I = info.GetIsolate();
        for (int i = 0, end = info.Length(); i < end; i++) {
            auto value = info[i];
            if (value->IsString()) {
                String::Utf8Value utf8{I, value};
                fmt::print("{}\n", *utf8);
            } else {
                auto json = JSON::Stringify(I->GetCurrentContext(), value);
                String::Utf8Value utf8{I, json.ToLocalChecked()};
                fmt::print("{}\n", *utf8);
            }
        }
        return {};
    }

    /// ===========================================================================
    ///  Initialisation.
    /// ===========================================================================
    eval_impl(parser& _p)
        : p{_p} {
        static std::once_flag flag;
        static std::unique_ptr<Platform> v8_platform;
        std::call_once(flag, [] {
            V8::InitializeICU();
            V8::InitializeExternalStartupData(nullptr);
            v8_platform = platform::NewDefaultPlatform();
            V8::InitializePlatform(v8_platform.get());
            V8::Initialize();
        });

        /// Create isolate.
        Isolate::CreateParams create_params;
        create_params.array_buffer_allocator = alloc = ArrayBuffer::Allocator::NewDefaultAllocator();
        auto I = isolate = Isolate::New(create_params);
        I->SetData(0, this);

        /// Template for global object.
        HandleScope hs{I};
        auto g_tm = ObjectTemplate::New(I);
        globl_tmpl.Reset(I, g_tm);

        /// Set globals.
        export_global<$print>(g_tm, "print");
        export_global<$$>(g_tm, "$");

        /// Template for element wrapper objects.
        auto elem_tm = register_template<$element>();
        register_template<$attributes>();
        register_template<$children>();

        elem_tm->SetAccessor(
            S(I, "attributes"),
            $element::get_attributes,
            $element::set_attributes
        );

        elem_tm->SetAccessor(
            S(I, "children"),
            $element::get_children
        );

        elem_tm->SetAccessor(
            S(I, "id"),
            $element::get_id,
            $element::set_id
        );
    }

    /// ===========================================================================
    ///  Various helpers.
    /// ===========================================================================
    ~eval_impl() {
        isolate->Dispose();
        delete alloc;
    }

    /// Get the parser from the isolate.
    static auto context(Isolate* I) -> eval_impl* { return static_cast<eval_impl*>(I->GetData(0)); }

    /// Create a new JS object from a template.
    static auto instantiate(Isolate* I, Persistent<ObjectTemplate>& templ) -> Local<Object> {
        return templ.Get(I)->NewInstance(I->GetCurrentContext()).ToLocalChecked();
    }

    /// Create an object template.
    template <typename backing_type>
    auto register_template() -> Local<ObjectTemplate> {
        /// Sanity check.
        if (not(this->*backing_type::object_template).IsEmpty()) {
            fmt::print(stderr, "Internal error: template already registered\n");
            fmt::print(stderr, "In: {}\n", __PRETTY_FUNCTION__);
            std::abort();
        }

        /// Create template.
        auto I = isolate;
        auto tm = ObjectTemplate::New(I);
        (this->*backing_type::object_template).Reset(I, tm);
        tm->SetInternalFieldCount(backing_type::field_count);

        /// Iterables.
        if constexpr (requires { backing_type::create_iterator; }) {
            static_assert(backing_type::field_count >= 1, "Iterable types must a backing native handle");
            tm->Set(
                Symbol::GetIterator(I),
                FunctionTemplate::New(I, backing_type::create_iterator)
            );

            /// Iterator type.
            auto iterator_tm = ObjectTemplate::New(I);
            iterator_tm->SetInternalFieldCount(2);
            (this->*backing_type::iterator_object_template).Reset(I, iterator_tm);
            iterator_tm->Set(
                S(I, "next"),
                FunctionTemplate::New(I, iterator_base<typename backing_type::iterator, typename backing_type::handle_type>::next)
            );
        }

        /// Register named property handlers.
        if constexpr (
            requires { backing_type::get; } or
            requires { backing_type::set; } or
            requires { backing_type::query; } or
            requires { backing_type::del; } or
            requires { backing_type::enumerate; } or
            requires { backing_type::define; } or
            requires { backing_type::describe; }
        ) {
            NamedPropertyHandlerConfiguration config{};
            if constexpr (requires { backing_type::get; }) config.getter = backing_type::get;
            if constexpr (requires { backing_type::set; }) config.setter = backing_type::set;
            if constexpr (requires { backing_type::query; }) config.query = backing_type::query;
            if constexpr (requires { backing_type::del; }) config.deleter = backing_type::del;
            if constexpr (requires { backing_type::enumerate; }) config.enumerator = backing_type::enumerate;
            if constexpr (requires { backing_type::define; }) config.definer = backing_type::define;
            if constexpr (requires { backing_type::describe; }) config.descriptor = backing_type::describe;
            tm->SetHandler(config);
        }

        return tm;
    }

    /// Create a new string.
    static Local<String> S(Isolate* I, auto&& str) {
        if constexpr (requires { str.data(); }) {
            auto s = String::NewFromUtf8(
                I,
                std::forward<decltype(str)>(str).data(),
                NewStringType::kNormal,
                int(std::forward<decltype(str)>(str).size())
            );

            return s.ToLocalChecked();
        } else {
            return String::NewFromUtf8(I, std::forward<decltype(str)>(str)).ToLocalChecked();
        }
    }
};

namespace {
using err = std::unexpected<std::string>;
auto exception_to_diag(Isolate* I, TryCatch& tc) -> res<void> {
    String::Utf8Value exception{I, tc.Exception()};
    auto s = std::string_view{*exception, size_t(exception.length())};
    auto m = tc.Message();

    /// No info.
    if (m.IsEmpty()) return err(s);

    /// Get info.
    String::Utf8Value filename{I, m->GetScriptResourceName()};
    auto ctx = I->GetCurrentContext();
    auto line = m->GetLineNumber(ctx).FromJust();
    std::string msg = fmt::format("{}:{}: {}\n", *filename, line, s);

    /// Print line.
    String::Utf8Value sourceline{I, m->GetSourceLine(ctx).ToLocalChecked()};
    msg += fmt::format("{}\n", *sourceline);

    /// Print underline.
    usz st = usz(m->GetStartColumn(ctx).FromJust());
    usz en = usz(m->GetEndColumn(ctx).FromJust());
    NHTML_REPEAT(st)
    msg += " ";
    NHTML_REPEAT(en - st)
    msg += "~";
    msg += "\n";

    /// Stack trace.
    Local<Value> stack_trace;
    if (
        tc.StackTrace(ctx).ToLocal(&stack_trace) and
        stack_trace->IsString() and
        not stack_trace.As<String>().IsEmpty()
    ) {
        String::Utf8Value stack_trace_str{I, stack_trace};
        msg += fmt::format("Stack trace: {}\n", *stack_trace_str);
    }

    return err(msg);
}
} // namespace
} // namespace nhtml::detail

/// ===========================================================================
///  API
/// ===========================================================================
nhtml::detail::eval_ctx::eval_ctx(parser& p)
    : impl(::new eval_impl(p)) {
}

nhtml::detail::eval_ctx::~eval_ctx() {
    delete impl;
}

auto nhtml::detail::eval_ctx::operator()(string_ref script_data, loc where) -> res<void> {
    auto I = impl->isolate;
    auto l = impl->p.seek(where);
    Isolate::Scope is{I};
    HandleScope hs{I};
    TryCatch tc{I};
    ScriptOrigin so{
        I,
        String::NewFromUtf8(I, impl->p.files[where.file].name.string().c_str()).ToLocalChecked(),
        int(l.line - 1),
        int(l.col),
    };
    Local<Context> ctx{Context::New(I, nullptr, impl->globl_tmpl.Get(I))};
    Context::Scope cs{ctx};
    Local<Script> script;

    /// Compile script.
    auto text = String::NewFromUtf8(I, script_data.data(), NewStringType::kNormal, int(script_data.size())).ToLocalChecked();
    if (not Script::Compile(ctx, text, &so).ToLocal(&script)) return exception_to_diag(I, tc);

    /// Execute it.
    if (Local<Value> res; not script->Run(ctx).ToLocal(&res)) return exception_to_diag(I, tc);
    return {};
}

#endif