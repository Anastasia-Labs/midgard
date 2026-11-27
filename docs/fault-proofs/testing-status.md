# Fault-Proof Testing Status

Status: Active

Last reviewed: 2026-11-23 (evidence boundaries and verification commands).

## What the evidence establishes

[Catalogue status](catalogue-status.md) owns source and watcher-installation
inventory. It does not assert that all tests passed. Avoid test-file counts:
a file can contain skipped, uncollected, or failing scenarios.

| Evidence                                   | Establishes                                                                                          |
| ------------------------------------------ | ---------------------------------------------------------------------------------------------------- |
| Aiken unit/property results                | Predicate and transition behavior for the collected scenarios                                        |
| TypeScript unit/workflow results           | Codecs, evidence, admission, journals, and deterministic workflow behavior                           |
| Real-blueprint Lucid lifecycles            | Applied scripts, complete transaction fit, refusals, recovery, mint/removal for the exercised shapes |
| Independent-process / real-node acceptance | Runtime composition, transport, durable recovery, and correction                                     |
| Preprod acceptance on release identity     | Live challenge behavior for the exercised deployment and scenarios                                   |

The [installed validation workflow](validation-trace-dispute-installed-workflow.md)
and [transition replay](transition-trace-installed-replay.md) identify current
routing and executable acceptance surfaces. The [size plans](size-plans/README.md)
describe how transaction fit is measured. A saved measurement, passing digest
check, or a test that returned early does not establish current-build acceptance.

The [availability challenge](availability-challenge-operations.md) has
signed-publication and contract-lifecycle tests under the pinned mainnet
protocol parameters; its live acceptance is open.

## Verification commands

From the repository root, build the real blueprint using the pinned compiler:

```sh
(cd onchain/aiken && aiken build --env testnet)
```

Use the [Aiken build skill](../../.agents/skills/aiken-contract-build/SKILL.md)
for focused selectors and cache isolation. Selectors must collect a nonzero
number of tests. For package suites, also from the repository root:

```sh
pnpm --dir demo/midgard-fault-proofs run typecheck
pnpm --dir demo/midgard-fault-proofs test
pnpm --dir demo/midgard-core test
pnpm --dir demo/midgard-sdk test
pnpm --dir demo/midgard-validation test
pnpm --dir demo/midgard-watcher test
pnpm --dir demo/midgard-node test
pnpm --dir demo/da-committee-node test
```

The fault-proof, node and watcher suites also run as a fast and a slow tier,
split by each file's CI seconds in its `tests/support/ci-file-durations.json`
(60 s and over is slow). Iterate on the fast tier and run the slow tier, which
holds the maximum-shape proofs and the long lifecycles, before handing over;
CI always runs both:

```sh
pnpm --dir demo/midgard-fault-proofs run test:fast
pnpm --dir demo/midgard-fault-proofs run test:slow
```

Set `MIDGARD_REAL_BLUEPRINT_PATH` to the absolute freshly built blueprint when
running real-contract scenarios. Workspace source conditions resolve sibling
source for typecheck/lint/Vitest; scripts that invoke plain `node` still require
the package builds they document.

Positive publication and lifecycle acceptance must use the shared
`demo/midgard-fault-proofs/tests/support/emulator/protocol-parameters.ts`
limits. Raised size/ExUnit limits are diagnostic, not release acceptance.

Run the complete named emulator gate from the repository root; lower
`MIDGARD_FAULT_PROOF_FORKS` on smaller machines:

```sh
MIDGARD_FAULT_PROOF_FORKS=8 pnpm --dir demo run test:tx-prep:emulator
```

## Release boundary

Use [remaining acceptance](execution-plan.md) for publication, maximum-shape,
public evidence lifetime, economics, independent-process, and preprod closure.
The [public readiness checklist](../public_testnet_readiness.md) owns launch
approval. A source installation or passing subset does not close these gates.
