# TQ-12 — Classify pins before deriving or changing them

Status: Proposed
Last reviewed: 2026-11-23 (cited sources)

- **Rules**: R5, R6, R9 in [principles](principles.md)

## Problem and boundary

Repeated constants can drift, but exact values can also be the contract under
review. Wire bytes, explicit catalogue IDs, public exports, applied validator
hashes, compiler identity, and reproducible execution measurements can all
justify independent pins. A build hash changing after a source change may be
exactly the signal the release reviewer needs.

Derive incidental loop counts or duplicated configuration where appropriate.
Do not derive both expected and actual from the same production definition
when the purpose is to detect an unintended change to that definition.
Catalogue identity comes from its explicit ID map, not array position.

## Confirmed source facts

- Execution costs are ceilings:
  [the script-budget judge](../../onchain/aiken/scripts/script-budgets.mjs)
  holds each fresh reading at or under its ceiling in
  [`script-budgets.json`](../../demo/midgard-core/src/script-budgets.json),
  and keeps the feasibility basis and the above-basis claims. Changing that
  contract again is outside routine cleanup.
- Exact cost-model values, canonical vectors, and protocol-version checks
  remain independent expectations. Do not treat every compiler literal or
  duplicated cross-language constant as prohibited.

## Focused work

1. Classify proposed pins as normative identity, public API, deployment
   provenance, reproducible measurement, conservative reserve, or incidental
   bookkeeping. Record the consumer and consequence of changing each.
2. Derive incidental counts from declared scenarios; retain an independent
   required-set check so dropping a supported scenario cannot silently pass.
3. Consolidate repeated incidental calculations only when the surviving check
   still detects the same regression. Keep independent cross-language twins.
4. If performance bands are appropriate for a separate benchmark, state their
   purpose and bounds explicitly. They do not replace live verifier inputs or
   the script-budget ceilings.
5. Remove stale repinning history from comments where it obscures current
   intent, without discarding the evidence or its verification contract.

## Acceptance

Each changed pin has a documented classification, source/consumer, and
regression detector. Required artifact verification and cross-language checks
still run. Do not use targets such as zero duplicated constants, zero compiler
literals, or no exact ExUnit equality as measures of success. Any change to
release-evidence semantics must be reviewed explicitly rather than hidden in
this refactor.
