#include <caml/mlvalues.h>
#include <caml/domain_state.h>
#define CAML_INTERNALS
#include <caml/fiber.h>

value caml_alloc_stack_info() {
    value v = caml_alloc(1, Abstract_tag);
    *((struct stack_info **) Data_abstract_val(v)) = NULL;
    return v;
}

int count = 0;

value caml_stack_info(value target) {
    CAMLparam1(target);
    *((struct stack_info **) Data_abstract_val(target)) = Caml_state->current_stack;
    if (Caml_state->current_stack->magic == 42) {
        Caml_state->current_stack->magic = count;
        count++;
        if (count == 42) {
            count = 43;
        }
    }
    CAMLreturn (Val_unit);
}

value caml_stack_info_handler(value stack_info_ptr) {
    CAMLparam1(stack_info_ptr);
    struct stack_info * v = *((struct stack_info **) Data_abstract_val(stack_info_ptr));
    CAMLreturn (Val_int(v->magic));
}

value caml_stack_info_parent(value stack_info_ptr) {
    CAMLparam1(stack_info_ptr);
    struct stack_info * v = *((struct stack_info **) Data_abstract_val(stack_info_ptr));

    *((struct stack_info **) Data_abstract_val(stack_info_ptr)) = v->handler->parent;

    if (v->handler->parent == NULL) {
        CAMLreturn (Val_false);
    } else {
        CAMLreturn (Val_true);
    }
}