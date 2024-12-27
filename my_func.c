#include "my_func.h"

__declspec(dllexport) bool register_callback(callback_t cb)
{
    if (cb)
    {
        cb(42);      // Call the callback with a test value
        return true; // Indicate success
    }
    return false; // Indicate failure
}