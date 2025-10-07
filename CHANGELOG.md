# Revision history for dap

## 0.3.0.0 -- 2025-10-03

### Main library changes

* Adds support for sending a `runInTerminal` reverse request using
  `sendRunInTerminalReverseRequest`.
* And adds support for receiving responses to reverse requests via the new
  argument to `runDAPServerWithLogger` -- a function which receives a
  `ReverseRequestResponse`.

## 0.2.0.0 -- 2025-05-05

### Main library changes

* `Adaptor` has an additional type parameter denoting the type of the request
    we are responding to. Crucially, this will be `Request` when responding to a
    DAP request (e.g. in `send***` functions).
    On the other hand, this will be `()` for the `withAdaptor` continuation
    argument of `registerNewDebugSession` which unlifts `Adaptor` to `IO`
    because, when unlifting, we are not replying to any request.

### Merged MRs
* Create main.yml workflow by @dmjio in https://github.com/haskell-debugger/dap/pull/1
* Remove pedantic by @dmjio in https://github.com/haskell-debugger/dap/pull/2
* Add badge by @dmjio in https://github.com/haskell-debugger/dap/pull/3
* Api unlock custom event by @csabahruska in https://github.com/haskell-debugger/dap/pull/4
* Expose OutputEventCategory and fix JSON rendering by @alt-romes in https://github.com/haskell-debugger/dap/pull/8
* Fix race conditions due to MVar usage by @alt-romes and @mpickering in https://github.com/haskell-debugger/dap/pull/5
* Refactoring to use logging framework by @alt-romes in https://github.com/haskell-debugger/dap/pull/10
* Allow a client to explicitly terminate the server by @alt-romes in https://github.com/haskell-debugger/dap/pull/11
* Allow setBreakpointsArgumentsSourceModified to be null by @alt-romes in https://github.com/haskell-debugger/dap/pull/13

**Full Changelog**: https://github.com/haskell-debugger/dap/commits/0.2.0.0

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
