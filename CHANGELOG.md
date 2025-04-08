# Revision history for dap

## Unreleased -- YYYY-mm-dd

* `Adaptor` has an additional type parameter denoting the type of the request
    we are responding to. Crucially, this will be `Request` when responding to a
    DAP request (e.g. in `send***` functions).
    On the other hand, this will be `()` for the `withAdaptor` continuation
    argument of `registerNewDebugSession` which unlifts `Adaptor` to `IO`
    because, when unlifting, we are not replying to any request.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
