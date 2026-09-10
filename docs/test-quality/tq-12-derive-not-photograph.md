# TQ-12 — Classify pins before deriving or changing them

Status: Proposed
Last reviewed: 2026-09-07 (semantic source review)

- **Audit sections**: §2 and §7.1
- **Rules**: R5, R6, R9 in [principles](principles.md)
- **Coordination**: TQ-01 handles provenance and TQ-15 handles shared fit checks.

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

## Confirmed corrections to the original proposal

- [The execution-ledger verifier](../../onchain/aiken/scripts/exec-ledger-within-basis-v1.mjs)
  checks fresh measurements against the declared feasibility basis and then
  requires exact recorded memory/CPU equality in verification mode. That is
  an evidence-identity contract. **Preserve exactness.** Converting it to a
  ratchet would accept different evidence and is outside routine cleanup;
  it requires an explicit change to the verifier's acceptance contract.
- [The API export snapshot](../../demo/lucid-midgard/tests/api-export-snapshot.test.ts)
  protects an intentional exported set. Do not delete it merely because it
  lists names or because another assertion checks the declaration file exists.
- [Reference-script roster checks](../../demo/midgard-node/tests/reference-scripts.test.ts)
  using `toContain` detect removal of each named target; they do not reject
  unexpected extras. Use exact set equality only if excluding extras is the
  intended contract, and retain the duplicate-name check where required.
- A 14-KiB raw-script target need not be a ledger maximum to be useful: it
  could reserve room for transaction overhead. Trace its rationale and full
  signed-transaction consumer before changing it. A currently failing bound
  is not sufficient reason to delete it.
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
   exact execution-ledger identity checks.
5. Remove stale repinning history from comments where it obscures current
   intent, without discarding the evidence or its verification contract.

## Acceptance

Each changed pin has a documented classification, source/consumer, and
regression detector. Required artifact verification and cross-language checks
still run. Do not use targets such as zero duplicated constants, zero compiler
literals, or no exact ExUnit equality as measures of success. Any change to
release-evidence semantics must be reviewed explicitly rather than hidden in
this refactor.
