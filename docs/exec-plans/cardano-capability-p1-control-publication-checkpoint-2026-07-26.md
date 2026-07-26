# Cardano capability P1 control-publication checkpoint — 2026-07-26

Branch: `codex/tx-validation-capability-checkpoint`
Clean base: `4369c41d`

This is the boundary for the bounded P1 emulator-only measurement milestone.
It measures the six canonical validation-dispute control validators after each
received a unique authenticated reference-script role and strict deployment
manifest slot. No resolver expansion, control-submission refactoring, live
target-network submission, Docker topology, P4 work, release-evidence digest,
or stress run occurred.

## Construction and parameter binding

The focused test constructs each publication with:

- the exact applied and parameterized validator selected by
  `buildValidationTraceDisputeFaultProofContracts`;
- the shared timelocked native reference-script auth policy;
- the control's unique canonical role token;
- a concrete plain-Ada funding input and three concrete outputs;
- the reference script in its authenticated output; and
- a completed wallet signature.

Each complete signed CBOR transaction is submitted to one in-memory emulator
before the next control is measured. The emulator has `maxTxSize = 16,384` and
uses the same public preprod epoch-303 parameter snapshot as the preceding
dispute-open checkpoint:

- maximum execution units:
  `16,500,000 memory / 10,000,000,000 steps`;
- snapshot SHA-256:
  `d4178364b6c45216bf51d067e0a8360ce8060e8a2bef8213cf4121376380c137`.

This remains deterministic emulator evidence. It does not satisfy the later
trusted-parameter or actual target-network construction/submission gates.

## Complete signed publication measurements

The standalone-script column is diagnostic only. PASS is based on the
complete signed transaction.

| Control  | Standalone script bytes | Signed bytes | Byte margin | Memory | Steps | Inputs / refs / outputs | Vkeys / native / redeemers / datums / P1 / P2 / P3 scripts | Result |
| -------- | ----------------------: | -----------: | ----------: | -----: | ----: | ----------------------- | ---------------------------------------------------------- | ------ |
| dispute  |                  12,757 |       13,260 |      +3,124 |      0 |     0 | 1 / 0 / 3               | 1 / 1 / 0 / 0 / 0 / 0 / 0                                  | PASS   |
| source   |                  11,573 |       12,072 |      +4,312 |      0 |     0 | 1 / 0 / 3               | 1 / 1 / 0 / 0 / 0 / 0 / 0                                  | PASS   |
| game     |                   4,308 |        4,803 |     +11,581 |      0 |     0 | 1 / 0 / 3               | 1 / 1 / 0 / 0 / 0 / 0 / 0                                  | PASS   |
| boundary |                   5,314 |        5,819 |     +10,565 |      0 |     0 | 1 / 0 / 3               | 1 / 1 / 0 / 0 / 0 / 0 / 0                                  | PASS   |
| timeout  |                   2,100 |        2,603 |     +13,781 |      0 |     0 | 1 / 0 / 3               | 1 / 1 / 0 / 0 / 0 / 0 / 0                                  | PASS   |
| award    |                   1,567 |        2,064 |     +14,320 |      0 |     0 | 1 / 0 / 3               | 1 / 1 / 0 / 0 / 0 / 0 / 0                                  | PASS   |

All six authenticated control-publication transactions fit. `dispute` is the
limiting publication with 3,124 bytes of remaining transaction space.

## Command and result

Run from `demo/midgard-fault-proofs/` with one Vitest fork, one emulator, and a
4 GiB V8 heap ceiling:

```sh
NODE_OPTIONS=--max-old-space-size=4096 \
MIDGARD_DIAGNOSTIC_CARDANO_PARAMETERS=/tmp/midgard-preprod-epoch-params-2026-07-26.json \
MIDGARD_PRINT_PROOF_FIT=1 \
./node_modules/.bin/vitest run \
  tests/submit-init-emulator.test.ts \
  -t "publishes every authenticated validation-dispute control under the exact L1 envelope" \
  --pool=forks --poolOptions.forks.singleFork=true \
  --testTimeout=300000 --hookTimeout=300000 --reporter=verbose
```

Result: **PASS** (1 passed, 11 skipped; measured test 6.494 s, total 8.23 s).

## P1 status and stop boundary

The P1 control-publication gate is now **PASS** for all six canonical controls:
each exact, fully signed Cardano transaction fits the required 16,384-byte
envelope with measured margin. Trusted effective/pending parameters and actual
target-network construction remain separately required by P5; this
emulator-only checkpoint does not satisfy or weaken that later gate.
Unsupported or incomplete activation remains fail closed.

Stop here before resolver publication/expansion, converting control
submissions to consume these references, P4 fault-family work, live
construction/submission, P5 release evidence, deployment, Docker, or stress.
