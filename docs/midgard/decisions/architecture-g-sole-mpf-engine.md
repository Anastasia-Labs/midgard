# Architecture G is the only MPF engine

Status: Accepted (owner ruling, 2026-09-26).

Supersedes: [Benchmark-gated runtime options](benchmark-gated-runtime-options.md)
(2026-09-07).

## Decision

The node has one ledger MPF engine: Architecture G, the native owner process
(`architecture-g-owner`) that holds the ledger trie and hands commit workers a
fork of its durable root. The `legacy`, `overlay` and `event_flat` engines are
deleted, together with the `MPF_ENGINE` setting and the probes and tests that
existed only for them. No setting selects an engine and no fallback path
remains.

Startup fails closed unless the node is pinned to one owner binary:

- `MPF_NATIVE_OWNER_BINARY_PATH` names the binary (the release image ships it
  at `/app/native/architecture-g-owner`);
- `MPF_NATIVE_OWNER_BINARY_SHA256` is its lowercase 64-hex SHA-256 (the image
  ships the value next to the binary as `architecture-g-owner.sha256`);
- `MPF_NATIVE_OWNER_SIDECAR_PATH` names the owner's durable sidecar; it
  defaults to `<LEDGER_MPF_DB_PATH>.architecture-g.sidecar`.

`SPECULATIVE_COMMIT_BUILD` remains an explicit opt-in that defaults to `false`.
The check that it runs on an "overlay-capable" engine is gone, because every
engine now is Architecture G.

## Why

- Recovery exists only on Architecture G. Correction rewinds, expired-intent
  release and local-finalization recovery all rewind or promote the native
  owner's root. The other engines had no equivalent, so a node running them
  could not recover from those events.
- The tracked default had diverged from every live run. The 2026-09-07 record
  kept `MPF_ENGINE=legacy` as the default, but every live deployment, devnet
  run and acceptance run since then has used `architecture_g`. The default
  described a configuration nobody operated or tested end to end.

## Open mainnet gates

This ruling does not say that Architecture G passed its acceptance gates. The
owner did not rule on them, and none has a retained passing report on the
current revision. These gates stay open and block mainnet:

- the 24-hour live soak on matching identities
  ([soak procedure](../../benchmark-scenarios/phase-3-architecture-g-soak.md));
- the 50k and retained-growth root and candidate gates
  ([operator closure](../../benchmark-scenarios/phase-3-architecture-g-closure.md));
- release-image verification of the shipped owner binary
  (`scripts/phase3-architecture-g-release-image.mjs` and its verifier);
- the final-build differential and crash/recovery coverage and a clean live
  lifecycle, as listed in the operator closure.

Deleting the other engines does not discharge any of these. A failed gate stays
failed; changing defaults, inflating limits or shortening runs cannot
discharge it.

## Carried forward from the superseded record

Speculative building does not authorize submitting children of unconfirmed L1
commits. The [one-hour gate](../../benchmark-scenarios/phase-4-pipelined-one-hour.md)
measures the current pipeline. Unconfirmed chaining needs a separate design for
rollback, journals, provider acceptance, and on-chain state linkage. Multi-block
merge likewise requires an explicit validator and recovery assessment.

DA capacity is a consensus and memory-admission boundary, not an environment
escape hatch. The retired 50k distribution fixture could not be regenerated
within the canonical payload bound, so its former timing results and commands
were removed. Any replacement must use the current payload format and actual
publication measurements; a fixed transaction count alone proves neither fit
nor latency. A future format change follows
[prelaunch replacement rules](prelaunch-format-replacement.md) or the relevant
shipped-version upgrade policy.

## Consequences

- Every node, test harness and benchmark runs the native owner. A fresh stack
  must provide the owner binary's path and SHA-256 before the node starts.
- Acceptance must bind the actual runtime, corpus, applied deployment,
  topology, and producer/consumer evidence.
- The TypeScript MPF store stays, in its `direct` and `overlay` modes, for the
  commit worker's scratch transactions trie, offline replay and the reference
  oracle. It is not a ledger engine.
