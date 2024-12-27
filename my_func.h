#include <stdio.h>
#include <stdbool.h>

typedef void (*callback_t)(int);

bool register_callback(callback_t cb);