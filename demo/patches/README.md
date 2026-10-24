# Dependency patches

`@lucid-evolution__utils@0.2.3.patch` goes with the workspace overrides that
resolve `@lucid-evolution/utils`' `@harmoniclabs/uplc` and
`@harmoniclabs/plutus-data` to the 2.x versions the rest of the workspace
uses, instead of the 1.x versions utils declares. UPLC 2 `encodeUPLC` returns
the encoded bytes directly, so the patch drops the 1.x `.toBuffer().buffer`
wrapper from `applyParamsToScript` in both published module formats.
`midgard-core/tests/lucid-parameterization-resolution.test.ts` compares the
parameterized script bytes and hashes with Aiken's. Drop the patch and both
overrides together once a utils release depends on UPLC 2.

The existing `blake2b@2.1.4.patch` remains independently managed.

`postgres@3.4.9.patch` applies the unreleased upstream fix for a reserved
connection waiter that never settles (porsager/postgres#1195, fixed by the open
pull request porsager/postgres#1229). When a pooled connection closes while
`reserve()` waiters are queued, the unpatched pool hands the next waiter to the
reconnect and then drops it at ReadyForQuery, so an `@effect/sql-pg`
transaction waits forever. The patch keeps every reserve waiter in the queue,
removes a rejected waiter from it, and clears per-query state when a connection
closes. The ESM and CommonJS builds are patched; the unused `cf` build is not.
Drop the patch once a released `postgres` version contains the fix.

The same patch makes `query.cancel()` observe the promise of its cancel
request. The request runs on a connection of its own, and the unpatched
`cancel()` drops that promise, so a cancel whose connection is refused or reset
is an unhandled rejection, which ends the node: `@effect/sql-pg` cancels every
running query it interrupts. The patched `cancel()` sends the request once,
handles a rejection itself, and returns the promise, as the open pull request
porsager/postgres#1237 does without the handler. A failed cancel leaves the
query running to its end; its own result and failure path are unchanged.
`midgard-node/tests/database-startup-retry.test.ts` resets a real cancel
connection and checks the pool serves the next query. Drop this part once a
released `postgres` version observes the cancel promise.
