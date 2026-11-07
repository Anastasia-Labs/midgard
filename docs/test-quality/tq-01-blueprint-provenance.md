# TQ-01 — Bind current fit claims to their producing build

Status: Proposed
Last reviewed: 2026-09-07 (consumer and source review)

Audit: §1.1, §2, §11. Rules: R1, R5, R6. Related: TQ-08, TQ-12, TQ-15.

## Problem and current evidence

A recorded blueprint digest compared with an independently pinned expected
value can detect artifact substitution. It does not establish freshness against
the current build. These are different contracts; such a comparison is not a
literal self-comparison merely because the two values were recorded together.

The fit ledger JSON files this card first inventoried have been removed; fit is
now measured on the current build at every emulator submission (TQ-15). For any
other saved artifact, a recorded digest that does not match the present
blueprint does not by itself invalidate the artifact's consistency check; it
prevents using that measurement as current-build evidence without
revalidation.

Current consumers include live measurement producers, saved-ledger checks, the
CG1 publication verifier, and machine-readable necessity pins parsed by
`demo/midgard-validation/tests/validation-machine.test.ts`. Preserve those
consumer distinctions before changing pins or schemas.

## Proposed work

1. Map each retained artifact to its producer, consumer, build identity and
   acceptance contract. Identify which ones authorize a current deployment.
2. For current-build gates, compare with the exact blueprint actually loaded,
   including compiler and applied validator identities. Share incidental
   provenance-reading logic while keeping independent expected wire vectors.
3. Rerun producing measurements when their required identity changed. Never
   replace only the recorded digest, compiler, or cost values to make a check
   pass. Preserve a failed fit verdict and its stated resource limit.
4. Review CG1's `--blueprint-optional` lane separately. The flag skips the
   working-tree comparison only when the blueprint is absent; a present
   blueprint is still compared. A current deployment acceptance lane must
   build and require its exact blueprint. A source/index-only consistency lane
   must describe that narrower result explicitly.
5. Make required build prerequisites fail with an actionable message. Inspect
   each CI lane before assuming a gitignored artifact is absent: the node
   workflow builds Aiken before relevant checks.

## Acceptance

- Every current-build claim has an actual producing measurement and exact
  identity binding; historical consistency checks remain labeled accordingly.
- Required lanes reject missing or mismatched build identity.
- Saved artifact integrity, wire vectors and independent compiler/validator
  expectations remain covered; no blanket ban on literal digests is introduced.
- Record producing commands and outcomes, including failures. Do not claim all
  43 artifacts need the same regeneration workflow or schema.

## Limits

This card does not authorize weakening exact acceptance pins, replacing whole
blueprint identity with a per-validator scheme, or deleting live verifier
inputs. Such changes require review of the governing acceptance contract.
