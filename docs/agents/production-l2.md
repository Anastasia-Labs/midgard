# Production L2 Principles

Read before adding a guard, retry, hold, stored record, status, alert, config
knob or layer.

## What production-grade means here

- A validator accepts only valid transitions, whatever inputs, ordering and
  timing an adversary (anyone but us) chooses.
- Every fraud can be proven, every honest party keeps its funds and bond, and
  the protocol keeps moving.
- The same inputs give the same result, on chain and in both codec twins.
- Configuration and migrations of shipped state are explicit.

It does not mean bookkeeping for every imaginable failure of our own
process: extra machinery is itself a risk to the four lines above.

## The test a mechanism has to pass

Write the answers in the change description; a reviewer rejects a mechanism
that lacks one. [review]

1. **Trigger.** Who does what, in a sequence that really occurs. A chain of
   coincidences (a restart at one instant while a peer still holds a
   transaction and a rebuild picks other coins) is not a trigger.
2. **Cost.** A protocol failure as the root `AGENTS.md` defines it. Fees, a
   rebuilt transaction or a repeated read are not a cost.
3. **Nothing else covers it.** Not the ledger rules, a validator, an earlier
   layer, or the chain read again.
4. **Smallest form.** Bound the work under the existing limit; never raise a
   limit or add a knob to make a case fit. [review]

A specification row or review finding that asks for a mechanism failing this
test is wrong: change the row instead of building the mechanism.

## Facts that remove whole classes of handling

- **The chain is the record.** A transaction we sent is in a block or in our
  node's mempool; if it is in neither, we did not send it. Derive progress
  from the ledger plus the mempool on every pass: no journal, remembered
  sends, reservations, reconciliation, or waiting for confirmation before
  building on a pending output. A restart is one more read. [review]
- **Our transaction cannot land script-invalid.** The node rejects it in
  phase 1 unless we set the validity flag false, which we never do. No
  handling, accounting, alert or test for forfeited collateral. [review]

## Observability

A log line, metric, readiness reason or alert exists for a condition that
occurs and that an operator acts on; otherwise delete it. [review]

## Non-Goals

Unless explicitly requested and isolated, do not introduce: [review]

- Benchmark, demo or throughput shortcuts that weaken a guarantee by default
- Silent data rewrites that hide integrity issues
- Backward compatibility for anything unlaunched (formats, state files,
  previous in-repo shapes, alias fields, shims)
- Temporary or diagnostic code left in finished work
