# Native-script-invalid wrongful rejection: transition and size sketch

The existing five-step chain has a direct terminal (small script and at most
28 address witnesses) and a resumable signer-frontier / pushdown evaluation
route. Extend its state with the canonical verdict subject. Step 01 binds the
accepted source or forced header/root/count/key/source and exact typed
`WitnessNativeScriptFalse { script_index }`; step 02 must authenticate that
exact field-6 item index and bind its content hash. Direct and staged terminal
arms preserve false-script conviction for acceptance and require true-script
contradiction for forced rejection.

The forced signer set must contain only signatures verified against the bound
transaction id. Existing accepted behavior does not establish signature
validity and cannot simply be inverted. Signer batches remain bounded and
checkpointed; reserve measurements determine a payable forced batch. Invalid
signatures must never turn a false script into a satisfied one.

Maximum-shape evidence must include the 64-Branch forced membership path,
maximum field-6 preimage with selected coordinate at its end, raw/certified
field boundaries, maximum signer fields and script node/depth limits. The
field-6 selection cannot consume an unauthenticated provisional item count:
large variable-field selection needs a bounded authenticated prefix/grammar
walk before the exact selected item can enter evaluation. Every publication,
Init, binding, scan, terminal, cancellation/restart and removal transaction is
measured against 15,872 publication bytes, 16,384 ledger bytes, 13,200,000
memory and 8,000,000,000 CPU. No field or script limit is reduced to fit.

This initial sketch is not a measured fit claim. ABI details may change when
maximum selection and signature-batch measurements resolve the route.

## Implemented transition plan

The five registered scripts retain their deployment parameters. Every state
carries the canonical subject. Step 01 has accepted and forced source arms;
the forced arm authenticates the exact header, counted forced root, order key,
compact source and `WitnessNativeScriptFalse` reason. Step 02 checks the exact
reason index before accepting an item. Its bounded route first certifies
all field envelopes in batches of 32, then selects the item through a second
bounded walk; both checkpoints are committed in thread state and can resume
at the same step. Fields with at most 32 items retain the direct/raw selection path; larger raw or inline fields use the same bounded grammar route as certified fields.

Steps 03 and 04 retain 16-witness signer batches. The forced direction verifies
each Ed25519 signature against the authenticated transaction id, excludes
invalid signatures, and commits the resulting sorted, deduplicated signer
frontier. Step 05 retains 16-node pushdown batches and requires a true terminal
verdict for rejection. Accepted terminal behavior remains false. Native script
limits are 32 nodes and depth 16, so applicable script bytes fit the existing
1,024-byte direct bound; the large-field case instead places the selected
native item after authenticated sibling items.

Durable artifacts retain submitted transaction bytes and the complete forced
source separately. Admission re-adjudicates the submitted validity flag before
checking the committed source and reopens header, counted root, MPF membership,
exact reason/index and script truth. The workflow resolves checkpoints from
admitted bytes and the current on-chain datum. Cancellation burns the thread
without minting evidence; a fresh admitted workflow can restart.

The deployment ABI, reference-script role/token map and watcher indexer now
name all five scripts. No additional family script or parameter was added.

## Measured ledger

`native-script-invalid-wrongful-rejection-v1-fit-ledger.json` records eight real
Lucid emulator paths with 372 complete signed transactions, including family
script publications, field publications/certificates, Init, source binding,
all scans, permanent mint, removal publications and block removal. Shapes cover
28/29 signers, 15,148/15,149-byte field carriage boundaries, 318 signers
(32,757 field bytes), a 32,768-byte script field with selected index 65 and a
64-Branch source, maximum native depth, and cancellation/restart at grammar and signer scans. Each shape retains the permanent
proof token after removing the fraudulent header.

Current maxima are 15,872 signed bytes, 12,209,007 execution memory and
5,508,631,646 execution steps. The final verification run pins the exact
blueprint SHA-256 in the ledger. No evaluator, ledger limit or protocol bound
is bypassed. Negative tests skip only the local semantic pre-check explicitly
so the real on-chain verifier must refuse honest false scripts and forged
signatures on both direct and staged routes.


Verification (Node 22, pnpm 9.15.4): family Aiken checks 26/26; four targeted
fault-proof Vitest files 35/35; the dedicated public durable/replay case 1/1;
core deployment identity 12/12, node deployment/identity 32/32, watcher
indexer/identity 41/41. SDK, core, fault-proofs, node and watcher typechecks pass.
The node tests use `MIDGARD_SKIP_DB_TESTS=1` because the selected manifest tests
do not use Postgres. Aiken build/check use pinned testnet via
`flock --close /tmp/midgard-nip-aiken.lock`.

## Shared build evidence

Both direction and five-step deployment commits are integrated. The shared
testnet blueprint digest is
`4293d71d2cd435f44021530aa4a60e0ddfee42839f0cc6fac09aca14bdfed728`.
The shared Van Rossem writer records 372 signed transactions across eight
shapes, and the verifier requires the current blueprint and reproducible
ledger digest. Regenerate from the repository root after the pinned build:

```bash
MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs exec vitest run tests/native-script-invalid-wrongful-rejection-lifecycle.test.ts
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/native-script-invalid-wrongful-rejection-fit-ledger.test.ts
```

The combined native-script/reference-input regression passed 33 tests;
12 core identity, 22 deployment, and 18 watcher indexer tests also passed.
The manifest counts include both the missing-signature forced scripts and
all five native-script-invalid stages. Final combined-tree closure remains open.
