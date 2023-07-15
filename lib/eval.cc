#ifndef NHTML_DISABLE_EVAL
#    include <expected>
#    include <libplatform/libplatform.h>
#    include <mutex>
#    include <nhtml/internal/eval.hh>
#    include <nhtml/internal/parser_impl.hh>

using namespace nhtml::detail;
using namespace v8;

namespace nhtml::detail {
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

struct function_template {
    Persistent<FunctionTemplate> tmpl;

    /// Create a new instance.
    auto operator()(Isolate* I) -> Local<Object> {
        auto ctx = I->GetCurrentContext();
        return tmpl.Get(I)->GetFunction(ctx).ToLocalChecked()->NewInstance(ctx).ToLocalChecked();
    }

    /// Check if an object is an instance of this template.
    auto is(Isolate* I, Local<Value> obj) -> bool {
        return tmpl.Get(I)->HasInstance(obj);
    }
};

struct eval_impl {
    using eval = eval_impl;
    Isolate* isolate;
    parser& p;
    Persistent<ObjectTemplate> globl_tmpl;
    function_template element_tmpl;
    function_template children_tmpl;
    function_template attributes_tmpl;
    function_template attributes_it_tmpl;
    function_template children_it_tmpl;

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
    using template_t = function_template eval_impl::*;

    /// CRTP helper for types that have a native handle pointer.
    template <typename handle_ty, template_t obj_templ, std::size_t fields = 1>
    requires (not std::is_pointer_v<handle_ty>)
    struct object {
        static auto handle(const auto& info) -> handle_ty* {
            return static_cast<handle_ty*>(
                Local<External>::Cast(info.Holder()->GetInternalField(0))->Value()
            );
        }

        static auto handle(Isolate* I, Local<Value> info) -> handle_ty* {
            return static_cast<handle_ty*>(
                Local<External>::Cast(info->ToObject(I->GetCurrentContext()).ToLocalChecked()->GetInternalField(0))->Value()
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
            auto iter = (context(I)->*iterator_obj_templ)(I);
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
        static void register_interface(Isolate* I, Local<ObjectTemplate> inst, Local<ObjectTemplate> proto) {
            inst->SetAccessor(S(I, "attributes"), get_attributes, set_attributes);
            inst->SetAccessor(S(I, "children"), get_children);
            inst->SetAccessor(S(I, "id"), get_id, set_id);
            proto->Set(S(I, "toString"), FunctionTemplate::New(I, to_string));
        }

        /// Stringify an element.
        static auto stringify(element* e) -> std::string {
            auto s = fmt::format("#Element<{}, {}", fmt::ptr(e), e->tag_name);
            if (not e->id.empty()) s += fmt::format(", #{}>", e->id);
            else s += ">";
            return s;
        }

        /// toString() implementation.
        static void to_string(const FunctionCallbackInfo<Value>& info) {
            auto I = info.GetIsolate();
            HandleScope hs{I};
            info.GetReturnValue().Set(S(I, stringify(handle(info))));
        }

        /// Get attributes of an element.
        static void get_attributes(Local<String>, const PropertyCallbackInfo<Value>& info) {
            auto I = info.GetIsolate();
            HandleScope hs{I};

            /// Create wrapper.
            Local<Object> obj = context(I)->attributes_tmpl(I);
            obj->SetInternalField(0, External::New(I, handle(info)));
            info.GetReturnValue().Set(obj);
        }

        /// Get children of an element.
        static void get_children(Local<String>, const PropertyCallbackInfo<Value>& info) {
            auto I = info.GetIsolate();
            HandleScope hs{I};

            /// Create wrapper.
            Local<Object> obj = context(I)->children_tmpl(I);
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
                auto r = context(I)->element_tmpl(I);
                r->SetInternalField(0, External::New(I, it->get()));
                return r;
            }
        };

        /// Add functions to prototype.
        static void register_interface(Isolate* I, Local<ObjectTemplate> inst, Local<ObjectTemplate> proto) {
            proto->Set(S(I, "push"), FunctionTemplate::New(I, push));
            proto->Set(S(I, "toString"), FunctionTemplate::New(I, $children::to_string));
        }

        /// toString() implementation.
        static void to_string(const FunctionCallbackInfo<Value>& info) {
            auto I = info.GetIsolate();
            HandleScope hs{I};
            auto h = handle(info);
            auto s = fmt::format("#ChildrenOf<{}>", $element::stringify(h));
            info.GetReturnValue().Set(S(I, s));
        }

        /// Add a new child.
        static void push(const FunctionCallbackInfo<Value>& info) {
            auto I = info.GetIsolate();
            auto ctx = I->GetCurrentContext();
            HandleScope hs{I};

            /// Make sure we have an object.
            if (info.Length() < 1 or not info[0]->IsObject()) {
                I->ThrowError("Argument of `push()` must be an element");
                return;
            }

            /// Make sure it’s an element.
            auto e = info[0]->ToObject(ctx).ToLocalChecked();
            if (not context(I)->element_tmpl.is(I, e)) {
                I->ThrowError("Argument of `push()` must be an element");
                return;
            }

            /// Get the children.
            auto h = handle(info);
            auto ch = std::get_if<element::vector>(&h->content);
            if (not ch) {
                h->content = element::vector{};
                ch = &std::get<element::vector>(h->content);
            }

            /// Add the child.
            ch->emplace_back($element::handle(I, e));
        }
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
        auto r = this_->element_tmpl(I);
        r->SetInternalField(0, External::New(I, res));
        return r;
    }

    /// Create an element.
    ///
    /// function element(
    ///     name: string,
    ///     content?: string | element | element[],
    ///     attributes?: object | [string?, string? | string[], object?]
    /// ): element
    ///
    /// The attributes are either an object, in which case they are used as-is,
    /// or a 3-tuple whose first value is the id of the element, the second the
    /// class list, and the third the attributes.
    static Local<Value> $make_element(const FunctionCallbackInfo<Value>& info) {
        auto I = info.GetIsolate();
        auto C = context(I);
        auto ctx = I->GetCurrentContext();

        /// See the JS signature above for a description of the arguments.
        if (info.Length() < 1 or not info[0]->IsString()) {
            I->ThrowError("element() constructor takes at least one string argument");
            return {};
        }

        /// Create the element.
        auto el = element::make();

        /// Set the name.
        String::Utf8Value tag_name{I, info[0]};
        el->tag_name = std::string{*tag_name, size_t(tag_name.length())};

        /// Set the content, if any.
        if (info.Length() >= 2) {
            auto content = info[1];

            /// No content.
            if (content->IsNullOrUndefined()) {
                /// Do nothing.
            }

            /// Text content.
            else if (content->IsString()) {
                String::Utf8Value str_content{I, content};
                el->content = std::string{*str_content, size_t(str_content.length())};
            }

            /// Array of elements.
            else if (content->IsArray()) {
                /// Set content to a vector.
                el->content = element::vector{};

                /// Add the elements.
                auto& els = std::get<element::vector>(el->content);
                auto arr = content->ToObject(ctx).ToLocalChecked().As<Array>();
                for (u32 i = 0, end = arr->Length(); i < end; i++) {
                    /// Make sure it’s an element.
                    auto v = arr->Get(ctx, i).ToLocalChecked();
                    if (not C->element_tmpl.is(I, v)) {
                        I->ThrowError(S(I, fmt::format("Element {} of content array was not an element", i)));
                        return {};
                    }

                    /// Add it.
                    els.emplace_back($element::handle(I, v));
                }
            }

            /// Single element.
            else if (content->IsObject()) {
                if (not C->element_tmpl.is(I, content)) {
                    I->ThrowError("Content argument of element() constructor was not an element");
                    return {};
                }

                el->content = element::vector{$element::handle(I, content)};
            }

            /// Nonsense.
            else {
                I->ThrowError("Content argument of element() constructor must be a string, element, or array of elements");
                return {};
            }
        }

        /// Set id, classes, and attributes, if any.
        if (info.Length() >= 3) {
            auto extra = info[2];

            /// Add attributes from an object.
            const auto add_attributes = [&] [[nodiscard]] (Local<Object> attrs) {
                auto names = attrs->GetOwnPropertyNames(ctx).ToLocalChecked();
                for (u32 i = 0, end = names->Length(); i < end; i++) {
                    /// Skip if the value is null.
                    auto name = names->Get(ctx, i).ToLocalChecked();
                    auto value = attrs->Get(ctx, name).ToLocalChecked();
                    if (value->IsNullOrUndefined()) continue;

                    /// Get the key.
                    String::Utf8Value utf8_name{I, name};
                    auto key = std::string{*utf8_name, size_t(utf8_name.length())};

                    /// Make sure the value is a string or null.
                    if (not value->IsString()) {
                        I->ThrowError(S(I, fmt::format("Attribute value for key '{}' was not a string", key)));
                        return false;
                    }

                    /// Add the attribute.
                    String::Utf8Value utf8_value{I, value};
                    auto val = std::string{*utf8_value, size_t(utf8_value.length())};
                    el->attributes[std::move(key)] = std::move(val);
                }

                return true;
            };

            /// No extra data.
            if (extra->IsNullOrUndefined()) {
                /// Do nothing.
            }

            /// Id, classes, and attributes.
            else if (extra->IsArray()) {
                auto arr = extra->ToObject(ctx).ToLocalChecked().As<Array>();

                /// ID.
                if (arr->Length() >= 1) {
                    auto id = arr->Get(ctx, 0).ToLocalChecked();
                    if (id->IsNullOrUndefined()) { /** No id. **/
                    } else if (id->IsString()) {
                        String::Utf8Value utf8{I, id};
                        el->id = std::string{*utf8, size_t(utf8.length())};
                    } else {
                        I->ThrowError("First element of attributes array must be a string or null");
                        return {};
                    }
                }

                /// Class list.
                if (arr->Length() >= 2) {
                    auto classes = arr->Get(ctx, 1).ToLocalChecked();

                    /// Add a class.
                    const auto add_class = [&] [[nodiscard]] (Local<Value> class_name) {
                        if (class_name->IsNullOrUndefined()) return true;
                        if (class_name->IsString()) {
                            String::Utf8Value utf8{I, class_name};
                            if (not el->attributes["class"].empty()) el->attributes["class"] += " ";
                            el->attributes["class"] += std::string{*utf8, size_t(utf8.length())};
                            return true;
                        } else {
                            I->ThrowError("Class list must be an array of (optionally null) strings");
                            return false;
                        }
                    };

                    /// No classes.
                    if (classes->IsNullOrUndefined()) {
                        /// Nothing to do.
                    }

                    /// Each element of the array is a class.
                    else if (classes->IsArray()) {
                        auto class_arr = classes->ToObject(ctx).ToLocalChecked().As<Array>();
                        for (u32 i = 0, end = class_arr->Length(); i < end; i++) {
                            auto class_name = class_arr->Get(ctx, i).ToLocalChecked();
                            if (not add_class(class_name)) return {};
                        }
                    }

                    /// The element is a single class.
                    else if (classes->IsString()) {
                        if (not add_class(classes)) return {};
                    }
                }

                /// Attributes.
                if (arr->Length() >= 3) {
                    auto attrs = arr->Get(ctx, 2).ToLocalChecked();

                    /// Do nothing.
                    if (attrs->IsNullOrUndefined()) {
                        /// Nothing to do.
                    }

                    /// Attributes must be an object.
                    else if (attrs->IsObject()) {
                        if (not add_attributes(attrs->ToObject(ctx).ToLocalChecked())) return {};
                    }

                    /// Nonsense.
                    else {
                        I->ThrowError("Third element of attributes array must be an object or null");
                        return {};
                    }
                }
            }

            /// Just attributes.
            else if (extra->IsObject()) {
                if (not add_attributes(extra->ToObject(ctx).ToLocalChecked())) return {};
            }

            /// Nonsense.
            else {
                I->ThrowError("Third argument of element() constructor must be an array or object");
                return {};
            }
        }

        /// Save the element as a floating element to make sure it isn’t deleted.
        auto raw = el.get();
        C->p.floating_elements.push_back(std::move(el));

        /// Create the JS wrapper object.
        auto obj = C->element_tmpl(I);
        obj->SetInternalField(0, External::New(I, raw));
        return obj;
    }

    /// Print to stdout.
    static Local<Value> $print(const FunctionCallbackInfo<Value>& info) {
        auto I = info.GetIsolate();
        for (int i = 0, end = info.Length(); i < end; i++) {
            auto value = info[i];
            String::Utf8Value utf8{I, value};
            fmt::print("{}\n", std::string_view{*utf8, size_t(utf8.length())});
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
        create_params.array_buffer_allocator_shared = std::shared_ptr<ArrayBuffer::Allocator>(ArrayBuffer::Allocator::NewDefaultAllocator());
        auto I = isolate = Isolate::New(create_params);
        I->SetData(0, this);

        /// Template for global object.
        HandleScope hs{I};
        auto g_tm = ObjectTemplate::New(I);
        globl_tmpl.Reset(I, g_tm);

        /// Set globals.
        export_global<$print>(g_tm, "print");
        export_global<$$>(g_tm, "$");
        export_global<$make_element>(g_tm, "element");

        /// Template for element wrapper objects.
        register_template<$element>();
        register_template<$children>();
        register_template<$attributes>();
    }

    /// ===========================================================================
    ///  Various helpers.
    /// ===========================================================================
    ~eval_impl() {
        isolate->Dispose();
    }

    /// Get the parser from the isolate.
    static auto context(Isolate* I) -> eval_impl* { return static_cast<eval_impl*>(I->GetData(0)); }

    /// Create an object template.
    template <typename backing_type>
    auto register_template() -> Local<FunctionTemplate> {
        /// Sanity check.
        if (not(this->*backing_type::object_template).tmpl.IsEmpty()) {
            fmt::print(stderr, "Internal error: template already registered\n");
            fmt::print(stderr, "In: {}\n", __PRETTY_FUNCTION__);
            std::abort();
        }

        /// Create template.
        auto I = isolate;
        auto tm = FunctionTemplate::New(I);
        (this->*backing_type::object_template).tmpl.Reset(I, tm);
        tm->InstanceTemplate()->SetInternalFieldCount(backing_type::field_count);

        /// Register additional callbacks.
        if constexpr (requires { backing_type::register_interface; }) {
            backing_type::register_interface(I, tm->InstanceTemplate(), tm->PrototypeTemplate());
        }

        /// Iterables.
        if constexpr (requires { backing_type::create_iterator; }) {
            static_assert(backing_type::field_count >= 1, "Iterable types must a backing native handle");
            tm->PrototypeTemplate()->Set(
                Symbol::GetIterator(I),
                FunctionTemplate::New(I, backing_type::create_iterator)
            );

            /// Iterator type.
            using iter_type = backing_type::iterator;
            auto iterator_tm = FunctionTemplate::New(I);
            iterator_tm->InstanceTemplate()->SetInternalFieldCount(2);
            (this->*backing_type::iterator_object_template).tmpl.Reset(I, iterator_tm);
            iterator_tm->PrototypeTemplate()->Set(
                S(I, "next"),
                FunctionTemplate::New(I, iterator_base<iter_type, typename backing_type::handle_type>::next)
            );

            /// Register additional callbacks.
            if constexpr (requires { iter_type::register_interface; }) {
                iter_type::register_interface(I, tm->PrototypeTemplate(), tm->PrototypeTemplate());
            }
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
            tm->InstanceTemplate()->SetHandler(config);
        }

        return tm;
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
    Isolate::Scope is{I};
    HandleScope hs{I};
    TryCatch tc{I};
    ScriptOrigin so = [&] {
        if (where) {
            auto l = impl->p.seek(where);
            return ScriptOrigin{
                I,
                String::NewFromUtf8(I, impl->p.files[where.file].name.string().c_str()).ToLocalChecked(),
                int(l.line - 1),
                int(l.col),
            };
        } else {
            return ScriptOrigin{I, S(I, "<input>")};
        }
    }();
    Local<Context> ctx{Context::New(I, nullptr, impl->globl_tmpl.Get(I))};
    Context::Scope cs{ctx};
    Local<Script> script;

    /// Update globals.
    if (not impl->p.files.empty())
        ctx->Global()->Set(ctx, S(I, "filename"), S(I, impl->p.files[0].name.string())).Check();

    /// Compile script.
    auto text = String::NewFromUtf8(I, script_data.data(), NewStringType::kNormal, int(script_data.size())).ToLocalChecked();
    if (not Script::Compile(ctx, text, &so).ToLocal(&script)) return exception_to_diag(I, tc);

    /// Execute it.
    if (Local<Value> res; not script->Run(ctx).ToLocal(&res)) return exception_to_diag(I, tc);
    return {};
}

#endif