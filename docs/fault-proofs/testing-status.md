# Fault-Proof Testing Status

Status: Active

Last reviewed: 2026-09-07 (evidence index; no new protocol test receipt).

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
routing and executable acceptance surfaces. The [fit evidence index](size-plans/README.md)
distinguishes current-blueprint gates from historical snapshot consistency tests.
A saved measurement, passing digest check, or a test that returned early does not
establish current-build acceptance.

The [availability challenge](size-plans/availability-challenge.md) now has passing
signed-publication and registered contract-lifecycle gates using the pinned
mainnet protocol-11 cost model. Its operational challenger/watcher and live release
acceptance remain open. Remeasure any release evidence whose source or blueprint
binding changed; saved results do not replace the current-build gates.

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

Set `MIDGARD_REAL_BLUEPRINT_PATH` to the absolute freshly built blueprint when
running real-contract scenarios. Workspace source conditions resolve sibling
source for typecheck/lint/Vitest; scripts that invoke plain `node` still require
the package builds they document.

Positive publication and lifecycle acceptance must use the shared
`demo/midgard-fault-proofs/tests/support/emulator/protocol-parameters.ts`
limits. Raised size/ExUnit limits are diagnostic, not release acceptance.

## Release boundary

Use [remaining acceptance](execution-plan.md) for publication, maximum-shape,
public evidence lifetime, economics, independent-process, and preprod closure.
The [public readiness checklist](../public_testnet_readiness.md) owns launch
approval. A source installation or passing subset does not close these gates.
