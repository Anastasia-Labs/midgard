# Midgard Agent Directive

All prompts, design choices, and code changes in this repository must be interpreted through one primary lens:

- This codebase is intended to be a **production-grade L2**.

## Decision Standard

When making any implementation or operational decision, prefer options that maximize:

- Protocol correctness and state integrity
- Safety under adversarial conditions
- Determinism and reproducibility
- Operational reliability and failure recovery
- Auditability, traceability, and clear observability
- Explicit, auditable migrations and explicit configuration

## Explicit Non-Goals (Unless Clearly Requested and Isolated)

- Benchmark shortcuts that weaken correctness guarantees
- Silent data rewrites that hide integrity issues
- Demo-only behavior becoming default production behavior
- Throughput optimizations that compromise safety or liveness guarantees
- Backward compatibility for unlaunched runtime formats, local state files,
  scaffold APIs, or previous in-repo shapes
- For `demo/midgard-node`, backward compatibility, compatibility layers,
  legacy identifier support, and migration shims

## No Pre-Launch Legacy Support Rule

Midgard has not launched yet. Agents and review subagents must not request,
implement, or preserve support for older runtime objects, old local state files,
previous scaffold API shapes, legacy UTxO layouts, alias fields, or compatibility
shims unless the user explicitly asks for an isolated migration tool.

When a cleaner production design breaks older in-repo behavior, prefer the
cleaner design. Tests should prove the intended current canonical behavior, not
backward compatibility with abandoned pre-launch shapes.

## Midgard Node Compatibility Rule

For `demo/midgard-node`:

- Backward compatibility is never a goal.
- Do not add compatibility modes, fallback paths, alias lookups, dual-ID
  behavior, legacy-format support, or operator toggles intended to preserve old
  behavior.
- If a cleaner or stricter production design breaks prior behavior, prefer the
  stricter design.

## No Shortcuts Policy

- Shortcuts are not allowed for default behavior.
- Do not cut corners to make code "work" quickly if it weakens correctness, safety, or operability.
- Implementations must follow the right approach for a production L2, even when it is more complex or slower to ship.
- If a temporary workaround is unavoidable, it must be clearly isolated, explicitly documented, and never the default path.
- Temporary/debug-only code, config, and instrumentation may be used during testing and diagnosis, but must be removed or cleaned up before finalizing work.

## Priority Order

When tradeoffs are required, prioritize:

1. Correctness
2. Safety
3. Liveness
4. Performance
5. Convenience

## Practical Rule

If a change improves benchmarks but weakens production-grade guarantees, do not make it the default. Keep strict behavior as default and isolate non-production behavior behind explicit, clearly named controls.

## Local State Reset / On-Chain Redeploy Rule

For any Midgard node environment connected to real or persistent Cardano
network state, never wipe, reset, delete, or recreate local durable state
without also performing a full, explicit on-chain redeploy/reset of the
corresponding protocol state.

This includes, but is not limited to:

- `docker compose down -v`
- deleting or recreating Postgres volumes
- clearing local ledger/MPT databases
- deleting migration, mempool, block, deposit, admission, finalization, or
  confirmed-ledger state
- using any reset script that discards local node state

If local DBs or durable node state are wiped, the on-chain contracts,
reference scripts, scheduler, state queue, hub oracle, operator lists, and any
other protocol UTxOs must be redeployed from a clean genesis/deployment flow
before running deposits, L2 transactions, commitments, merges, benchmarks, or
readiness checks.

Do not combine a clean local database with previously deployed on-chain
protocol state. That creates unauditable split-brain state and can make
deposits, scheduler alignment, commitments, and finalization appear broken for
the wrong reason.

The only exception is a clearly labeled, read-only forensic diagnostic where no
new deposits, L2 transactions, commitments, merges, or state-changing
operations are submitted.

## Transaction Finalization Rule

For transaction finalization in this repository:

- Never set `.complete({ localUPLCEval: false })`.
- Always set local UPLC evaluation to `true`.
- Treat `.complete({ localUPLCEval: true })` as the required default for correctness, determinism, and production-grade validation behavior.

## Cardano Script Context Ordering (SDK / Tx Building)

For Plutus/Aiken redeemer indexing and script-context alignment, follow ledger ordering semantics exactly:

- Transaction `inputs` in script context are ordered lexicographically by `TxOutRef` (`txHash`, then `outputIndex`).
- Transaction `reference_inputs` in script context are ordered lexicographically by `TxOutRef` (`txHash`, then `outputIndex`).
- Redeemer indices for spending/reference-input driven logic must be derived from those lexicographic orders.
- Transaction `outputs` in script context are **not** re-ordered lexicographically by the ledger; output order is preserved exactly as authored in the transaction body.
