# Canonical V1 lifecycle-repair checkpoint — 2026-07-26

Branch: `codex/tx-validation-capability-checkpoint`
Clean base: `a03fbd18`

This is the hard boundary for the bounded lifecycle-repair milestone. The
formal canonical-V1 consolidation goal remains **blocked** and was neither
replaced nor completed. The successor capability plan also remains incomplete.
No Docker topology, deployment, stress run, semantic-resolver expansion, or
release-evidence digest change occurred.

## Shared header-commit regression

The established transition-trace case and the new validation-dispute case
failed at the same generic `setup.header-commit.complete` stage. Four stale
off-chain assumptions caused the applied current validator to reject:

1. The test harness applied the state-queue mint/spend validators with their
   old parameter lists. The current blueprint requires the DA-attestation
   policy id as the eighth mint parameter and second spend parameter.
2. The setup anchor used genesis protocol version `0`; canonical InitV1 and
   HeaderV1 require protocol version `1`.
3. The state-queue anchor spend used a static redeemer while the active
   operator redeemer was delayed. Canonical input sorting placed fee,
   active-operator, and anchor inputs at indexes 0, 1, and 2, which crossed the
   two spend redeemers. Delaying the anchor redeemer binds it after final input
   ordering.
4. The harness retained a 30 ms operator-bond maturity value instead of the
   canonical seven-day profile duration.

The repair updates only those applications, datum values, and redeemer
construction. The established transition-trace lifecycle is the regression
control.

## Validation-dispute mismatches exposed after commit repair

The complete invalid-forced lifecycle then exposed four independent,
fail-closed wire/time mismatches:

- Lucid unwraps a one-constructor `Data.Enum`; the Open, VerifySource,
  PrepareResolution, and ChallengerTimeout Aiken actions must be represented
  as raw record schemas under `Continue`, not as a second named wrapper.
- The SDK descriptor verdict schema omitted Aiken's leading `Pending`
  constructor. That encoded `Accepted`/`Rejected` at indexes 0/1 instead of
  1/2. The schema now preserves all wire indexes, while semantic conversion
  explicitly rejects a pending terminal descriptor.
- Lucid `validTo` is exclusive, while Aiken normalizes an inclusive upper
  bound. Constructors now use `validTo - 1` and reject an empty range.
- The deterministic validation trace hashed a definite-list context
  (`87…`) while Aiken's canonical `serialiseData` form is an indefinite list
  (`9f…ff`). The trace builder now commits and exposes the exact
  Aiken-canonical bytes; the evidence witness reuses those bytes.
- Recreating a Custom-network Lucid client after advancing emulator time
  generated a new slot epoch and made otherwise current validity ranges appear
  stale. Target-parameter clients now retain the original Custom slot mapping.
- A long lifecycle exhausted the participants' token-free fee inputs. The
  fixture preserves each participant's original funding total but divides a
  bounded portion into pure-Ada fee UTxOs.

Trace-enabled diagnostic blueprints localized each assertion. All diagnostic
Aiken source edits and blueprints are excluded from this checkpoint.

## Lifecycle evidence

Both commands use one Vitest fork and one in-memory emulator. Commands are run
from `demo/`.

Established transition-trace control:

```sh
cd demo

NODE_OPTIONS=--max-old-space-size=4096 \
pnpm --filter @al-ft/midgard-fault-proofs exec vitest run \
  tests/submit-init-emulator.test.ts \
  -t "submits and removes a tail transition-trace fraud proof end to end" \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000
```

Result: **PASS** (1 passed, 10 skipped; lifecycle 2.408 s, total 4.22 s).

Invalid-forced validation dispute:

```sh
curl -sS \
  "https://preprod.koios.rest/api/v1/epoch_params?epoch_no=eq.303" \
  -o /tmp/midgard-preprod-epoch-params-2026-07-26.json

cd demo

NODE_OPTIONS=--max-old-space-size=4096 \
MIDGARD_DIAGNOSTIC_CARDANO_PARAMETERS=/tmp/midgard-preprod-epoch-params-2026-07-26.json \
MIDGARD_PRINT_PROOF_FIT=1 \
pnpm --filter @al-ft/midgard-fault-proofs exec vitest run \
  tests/submit-init-emulator.test.ts \
  -t "opens, bisects, resolves, and awards a validation dispute end to end" \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** through open, source authentication, all generated midpoint
reveals, boundary preparation, selected one-step resolution, semantic
resolution, and award (1 passed, 10 skipped; lifecycle 104.923 s, total
107.41 s). The snapshot loader validates the required numeric fields, all
three cost-model vectors, and the 16,384-byte target before applying it.

## Concrete proof-transaction measurements

The transaction hook measures the complete signed CBOR submitted to the
emulator and sums the execution units encoded in every redeemer. These are the
real applied/parameterized testnet validators from `onchain/aiken/plutus.json`,
with real emulator vkey signatures, inline output datums, concrete inputs,
reference inputs, outputs, redeemers, and attached Plutus V3 scripts.

A public preprod epoch-303 snapshot observed on 2026-07-26 supplied the
diagnostic cost models and transaction parameters:

- block hash
  `f0ce77c9d7ed2f6eb260c985ebd48927f2189aedf1f31e8df40dccc5d76dc70c`;
- `maxTxSize = 16,384`;
- `maxTxExecutionUnits = 16,500,000 memory / 10,000,000,000 steps`;
- downloaded JSON SHA-256
  `d4178364b6c45216bf51d067e0a8360ce8060e8a2bef8213cf4121376380c137`.

Koios is observation evidence, not the trusted activation authority. The
release-evidence gate remains unset.

| Transaction         | Complete signed bytes |    Memory |       Steps | Inputs / refs / outputs | Vkeys / redeemers / witness datums / V3 scripts | Margin to 16,384 | Result   |
| ------------------- | --------------------: | --------: | ----------: | ----------------------- | ----------------------------------------------- | ---------------: | -------- |
| Open publication    |                18,586 | 1,645,470 | 679,310,442 | 2 / 2 / 2               | 1 / 1 / 0 / 1                                   |           -2,202 | **FAIL** |
| Semantic resolution |                11,411 | 1,182,410 | 472,088,839 | 2 / 0 / 2               | 1 / 1 / 0 / 1                                   |           +4,973 | PASS     |
| Settlement/award    |                 7,238 |   245,511 |  86,745,086 | 2 / 0 / 2               | 1 / 3 / 0 / 3                                   |           +9,146 | PASS     |

The zero witness-datum counts are expected: each output datum is inline in its
transaction output and is included in the complete byte count.

The emulator's 65,536-byte maximum is used only long enough to execute the
functional open. A separate fresh Lucid construction using the same ledger,
slot mapping, exact validators, and `maxTxSize = 16,384` rejects before signing:

```text
Max transaction size of 16384 exceeded. Found: 18467
```

The functional signed shape is 18,586 bytes after its vkey witness, so an exact
target-max signed open cannot exist. After the diagnostic open is submitted,
fresh target-parameter Lucid clients with `maxTxSize = 16,384` construct and
submit the remainder of the lifecycle, including the measured resolution and
award.

## Read-only operational-input audit

`demo/midgard-node/.env` has populated provider/network entries and signing
wallet entries. Its contract-deployment and DA-runtime-manifest paths both
exist and contain valid JSON; no credential or wallet value was printed or
used. The two files agree on their deployed manifest identity.

They describe the currently live pre-canonical deployment, not this working
tree's canonical surface:

- contract schema `midgard-deployment-manifest-v2`, 40 contracts and 27
  reference-script roles;
- DA runtime schema `midgard-da-libp2p-runtime-manifest-v2`;
- no transaction-field preimage, field-receipt, immutable CEK-material, or
  validation-trace-dispute contract/reference-script records.

Consequently the current live deployment cannot provide the exact
validation-dispute first-step reference script needed to repair publication
fit. This is evidence for the later semantic-runbook-identity and fresh
deployment gates, not authority to submit or mutate preprod state.

## Phase status

| Phase                              | Previous | Current                                  | Reason                                                                                                                                                                                                       |
| ---------------------------------- | -------- | ---------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| P1 split control plane / proof fit | FAIL     | **FAIL**                                 | Evidence improved from unavailable to a concrete full-transaction failure: open exceeds L1 by 2,202 signed bytes.                                                                                            |
| P4 forced/misclassification        | FAIL     | **FAIL overall; lifecycle subpath PASS** | The invalid-forced accepted-operator/rejected-challenger path now reaches award, but the opposite direction, retained-data construction, preprod, and the matrix's other fund-safety rows remain incomplete. |

Unsupported/incomplete activation therefore still fails closed.

## Remaining blocker and smallest next milestone

The immediate blocker is the attached first-step open validator: the exact
signed publication is 2,202 bytes too large. The current live manifest has no
canonical record for it. The smallest justified next milestone is therefore
to publish that exact applied first-step validator as an authenticated
reference script in the emulator deployment fixture, make open use the
manifest-bound reference input instead of attaching the script, and rerun this
same 16,384-byte construction plus both lifecycle controls. Stop again if the
complete signed open or execution budget does not fit. A fresh canonical
preprod publication/redeployment remains a later, separately authorized gate.

Only after P1 fits should work resume on the remaining P4 matrix rows. Live
preprod submission, broader deployment, P6 stress, trusted-parameter binding,
and release-digest generation remain later gates.
