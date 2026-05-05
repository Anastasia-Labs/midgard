# Deposit-Send-And-Withdraw CLI Implementation Plan

Status: implemented

Implementation notes:

- Event ids are the canonical `SDK.OutputReference` CBOR hex used by Midgard
  event NFTs, not raw 32-byte transaction hashes.
- The runbook is now in
  [`DEPOSIT_SEND_AND_WITHDRAW.md`](./DEPOSIT_SEND_AND_WITHDRAW.md).
- Payout lifecycle commands resolve settlement UTxOs and PHAS proofs internally;
  operators do not need to hand-build proof CBOR.
- The optional `commit-once` and `merge-once` wrappers from Task 9 were not
  added; the runbook uses the existing authenticated HTTP admin endpoints.

This plan defines the command and support work needed before a real preprod
`DEPOSIT_SEND_AND_WITHDRAW.md` runbook can be accurate and runnable. The target
flow is:

1. Submit an L1 deposit.
2. Project and spend the deposited L2 value with a Midgard-native transfer.
3. Submit an L1 withdrawal order for a selected L2 UTxO.
4. Commit and merge the withdrawal block.
5. Initialize payout, fund it from reserve, and conclude payout to L1.

The command surface must be production-shaped. It must resolve protocol context
from canonical node state and L1 UTxOs instead of asking operators to assemble
CBOR, PHAS proofs, settlement inputs, or reference scripts by hand.

## Current State

- `submit-deposit`, `project-deposits-once`, `submit-l2-transfer`, `utxos`,
  `listen`, `GET /commit`, and `GET /merge` already exist.
- `demo/midgard-node/src/transactions/submit-withdrawal.ts` has a withdrawal
  transaction builder, but no CLI command.
- `demo/midgard-node/src/transactions/reserve-payout.ts` has submit programs
  for deposit absorption, payout initialization, reserve funding, payout
  conclusion, and invalid-withdrawal refund, but no CLI commands.
- `listen` runs withdrawal fetching automatically, but there is no manual
  one-shot withdrawal fetch command.
- Withdrawal classification currently validates L2 out-ref existence, owner,
  value, and token count before marking `WithdrawalIsValid`; it still needs a
  real signature verification path before this can be treated as a preprod
  withdrawal runbook.

## Implementation Principles

- Commands must use `.complete({ localUPLCEval: true })` through the existing
  builders.
- Commands must resolve reference scripts from the configured reference-script
  address when possible.
- Commands must fail closed on ambiguous UTxO selection. If more than one
  candidate matches and no deterministic canonical choice exists, fail with a
  JSON diagnostic.
- Commands must print machine-readable JSON by default for runbook copy/paste
  and future automation.
- No compatibility shims for old local state, old withdrawal shapes, or legacy
  event identifiers.

## Task 1: Shared CLI Utilities

Add a small command-support module, for example
`demo/midgard-node/src/commands/withdrawal-utils.ts`.

Required functionality:

- Parse event ids as canonical `SDK.OutputReference` CBOR hex.
- Parse L2 out-refs in a single canonical user-facing form:
  `<txHash>#<outputIndex>`.
- Convert parsed out-refs to `SDK.OutputReference` and canonical CBOR.
- Resolve a seed phrase from `--wallet-seed-phrase` or
  `--wallet-seed-phrase-env`, matching the existing `submit-l2-transfer`
  precedence and error style.
- Derive Cardano payment key, public key hash, and Bech32 address from the seed
  phrase for the configured network.
- Convert Bech32 L1 addresses to `SDK.AddressData`.
- Parse optional datum arguments:
  - absent means `NoDatum`,
  - hex means `InlineDatum`,
  - reject non-hex strings.
- Format command results and command errors as stable JSON.

Acceptance criteria:

- Unit tests cover valid and invalid event ids, out-refs, datums, and seed
  phrase resolution.
- Invalid input exits before any L1 provider or database mutation is attempted.
- Error messages name the bad argument and the expected format.

## Task 2: Withdrawal Signature Specification And SDK Utilities

Define one canonical withdrawal signature scheme and implement it in shared code
usable by both the CLI and classifier.

Required design:

- `WithdrawalSignature` is interpreted as `[publicKeyHex, signatureHex]`.
- `publicKeyHex` is a 32-byte Ed25519 public key.
- `signatureHex` is a 64-byte Ed25519 signature.
- `l2_owner` must equal the Cardano payment key hash derived from
  `publicKeyHex`.
- The signed message must be domain separated and deterministic:
  `blake2b_256("MidgardWithdrawalV1" || canonical_cbor(WithdrawalBody))`.
- The canonical body encoding must use `Data.to(body, SDK.WithdrawalBody)`.

Implementation tasks:

- Add SDK or node utility functions:
  - `withdrawalSigningMessage(body)`
  - `signWithdrawalBody(privateKey, body)`
  - `verifyWithdrawalSignature(body, signature, expectedOwnerHash)`
  - `publicKeyHashFromWithdrawalSignature(signature)`
- Use CML or an equivalent audited Ed25519 implementation already available in
  the dependency graph.
- Reject malformed public keys, malformed signatures, non-canonical body data,
  and owner/public-key mismatches.

Acceptance criteria:

- Tests prove a withdrawal signed by the matching seed verifies.
- Tests prove tampering any field of `WithdrawalBody` invalidates the signature.
- Tests prove a valid signature from the wrong key is rejected because the
  public-key hash does not match `l2_owner`.
- Tests prove malformed public key and signature byte lengths are rejected.
- The signature fixture is deterministic and checked into tests.

## Task 3: Enforce Withdrawal Signature Verification In Classification

Update `demo/midgard-node/src/workers/utils/mpt.ts` so withdrawal
classification cannot return `WithdrawalIsValid` unless the withdrawal
signature verifies.

Required behavior:

- Preserve existing validity checks for:
  - missing L2 UTxO,
  - owner mismatch,
  - value mismatch,
  - too many tokens.
- Add signature verification after owner and value checks and before
  `WithdrawalIsValid`.
- If verification fails, classify as `IncorrectWithdrawalSignature`.
- Persist a structured `validity_detail` with enough information to diagnose
  the failure without logging private material.
- The settlement root must use the overridden classified validity, so invalid
  signatures are provable in the withdrawal root and can only take the refund
  path.

Acceptance criteria:

- A withdrawal with a valid signature is classified as `WithdrawalIsValid`.
- A withdrawal with the wrong signature is classified as
  `IncorrectWithdrawalSignature`.
- A withdrawal with a matching signature but mismatched `l2_owner` is not
  accepted.
- Existing deposit and withdrawal PHAS root tests still pass.
- Payout initialization rejects a withdrawal classified as invalid and the
  invalid-withdrawal refund path remains available.

## Task 4: Submit-Withdrawal CLI Command

Wire `submit-withdrawal` into `demo/midgard-node/src/index.ts` and keep the
command implementation in a dedicated module, for example
`demo/midgard-node/src/commands/submit-withdrawal.ts`.

Command shape:

```sh
node dist/index.js submit-withdrawal \
  --wallet-seed-phrase-env USER_WALLET \
  --l2-out-ref "<txHash>#<outputIndex>" \
  --l1-address "$USER_L1_ADDRESS" \
  --endpoint http://127.0.0.1:3000
```

Options:

- `--wallet-seed-phrase <seedPhrase>` optional direct seed phrase.
- `--wallet-seed-phrase-env <envVar>` default `USER_WALLET`.
- `--l2-out-ref <txHash#ix>` required.
- `--l1-address <address>` required payout target.
- `--l1-datum <hex>` optional, default `NoDatum`.
- `--refund-address <address>` optional, default `--l1-address`.
- `--refund-datum <hex>` optional, default `NoDatum`.
- `--order-lovelace <amount>` optional, default builder value.
- `--endpoint <url>` default `MIDGARD_NODE_URL` or `http://127.0.0.1:$PORT`.

Required behavior:

- Fetch the selected L2 UTxO from the node's public `/utxo` or `/utxos`
  endpoint, not from user-supplied value arguments.
- Verify the selected L2 UTxO payment credential matches the wallet key hash.
- Build `SDK.WithdrawalBody` from the resolved L2 UTxO, wallet owner hash,
  UTxO value, L1 target address, and optional L1 datum.
- Sign the body with the wallet payment key using Task 2 utilities.
- Select the same wallet for the L1 withdrawal-order transaction.
- Fetch the `withdrawal minting` reference script from the configured
  reference-script address.
- Call the existing withdrawal builder and submit the transaction.
- Return JSON containing:
  - `txHash`,
  - `withdrawalEventId`,
  - `withdrawalAssetName`,
  - `l2OutRef`,
  - `l2Owner`,
  - `l2Value`,
  - `l1Address`,
  - `refundAddress`.

Implementation note:

- The transaction builder currently exposes only `buildUnsignedWithdrawalTxProgram`.
  Add a production submit program or expose safe metadata from
  `buildUnsignedWithdrawalTxWithMetadataProgram` so the CLI can print the event
  id and asset name without parsing logs.

Acceptance criteria:

- `node dist/index.js submit-withdrawal --help` documents all arguments.
- The command rejects operational node wallets using the same isolation rule as
  `submit-deposit` and `submit-l2-transfer`.
- The command rejects a selected L2 UTxO not owned by the withdrawal signer.
- The command rejects stale or missing L2 out-refs before submitting L1.
- Emulator tests submit a real withdrawal order through this command program.

## Task 5: Manual Withdrawal Fetch Command

Add a manual one-shot command named `fetch-withdrawals-once`.

Command shape:

```sh
node dist/index.js fetch-withdrawals-once
```

Required behavior:

- Run `fetchAndInsertWithdrawalUTxOs`.
- Do not mutate the L2 ledger view directly.
- Print JSON with:
  - visible withdrawal UTxO count,
  - inserted or reconciled count if available,
  - fetch completion time.

Acceptance criteria:

- Running the command twice is idempotent.
- The command makes an eligible withdrawal order visible to later block
  commitment.
- Documentation is explicit that withdrawals are classified during block
  commitment, not projected to the mempool ledger like deposits.

## Task 6: Settlement And PHAS Proof Resolver

Add a resolver module, for example
`demo/midgard-node/src/commands/event-settlement-proof.ts`, used internally by
reserve/payout commands and exposed as a diagnostic CLI command.

Command shape:

```sh
node dist/index.js resolve-event-settlement-proof \
  --kind withdrawal \
  --event-id <event-id-hex>
```

Also support `--kind deposit`.

Required behavior:

- Read the event row from `deposits_utxos` or `withdrawal_utxos`.
- Require `projected_header_hash` to be present.
- Retrieve all event rows for the same header hash in canonical root order.
- Convert each row to the same PHAS key/value pair used during block
  commitment.
- Compute the PHAS proof for the requested event id.
- Fetch the settlement UTxO at the settlement script address whose settlement
  NFT asset name equals the header hash.
- Decode the settlement datum and verify its deposit or withdrawal root equals
  the recomputed root.
- Return JSON with:
  - event kind,
  - event id,
  - header hash,
  - settlement out-ref,
  - root,
  - proof,
  - event status,
  - event validity for withdrawals.

Required database helpers:

- Add retrieval by `projected_header_hash` for deposits.
- Add retrieval by `projected_header_hash` for withdrawals.
- Keep ordering identical to block commitment ordering.

Acceptance criteria:

- Resolver fails if the event has not been assigned to a header.
- Resolver fails if the settlement UTxO is missing or not unique.
- Resolver fails if the recomputed root does not match the settlement datum.
- Resolver output can be fed directly into the absorb and initialize-payout
  builders without manual transformation.

## Task 7: Payout Lifecycle CLI Commands

Add command wrappers for the existing submit programs in
`demo/midgard-node/src/transactions/reserve-payout.ts`.

### 7.1 Absorb Confirmed Deposit To Reserve

Command shape:

```sh
node dist/index.js absorb-confirmed-deposit-to-reserve \
  --deposit-event-id <event-id-hex>
```

Required behavior:

- Resolve deposit row, deposit L1 UTxO, settlement UTxO, and PHAS proof by event
  id using Task 6.
- Resolve membership proof withdrawal witness script and reference scripts.
- Submit `submitAbsorbConfirmedDepositToReserveProgram`.
- Return JSON with `txHash`, `depositEventId`, `reserveOutputRef`, and absorbed
  assets if practical.

Acceptance criteria:

- Fails if the deposit has not been committed and merged into a settlement.
- Fails if the deposit UTxO is already spent.
- Produces a reserve UTxO containing the deposited assets.

### 7.2 Initialize Payout

Command shape:

```sh
node dist/index.js initialize-payout \
  --withdrawal-event-id <event-id-hex>
```

Required behavior:

- Resolve withdrawal row, withdrawal L1 UTxO, settlement UTxO, and PHAS proof by
  event id using Task 6.
- Require withdrawal validity to be `WithdrawalIsValid`.
- Submit `submitInitializePayoutProgram`.
- Return JSON with `txHash`, `withdrawalEventId`, `payoutUnit`, and
  `payoutOutputRef`.

Acceptance criteria:

- Fails closed for invalid withdrawal classifications.
- Fails if the withdrawal event has not been committed and merged.
- Produces exactly one payout NFT whose asset name matches the withdrawal asset
  name.

### 7.3 Add Reserve Funds To Payout

Command shape:

```sh
node dist/index.js add-reserve-funds-to-payout \
  --withdrawal-event-id <event-id-hex>
```

Required behavior:

- Resolve the current payout UTxO by payout NFT asset name.
- Resolve reserve UTxOs at the reserve address.
- Select exactly one reserve UTxO that contributes to the payout's remaining
  target value.
- Submit `submitAddReserveFundsToPayoutProgram`.
- Return JSON with `txHash`, `payoutOutputRef`, `reserveChangeOutputRef`,
  `fundedAssets`, and `remainingAssets`.

Acceptance criteria:

- Fails if no payout UTxO exists.
- Fails if no reserve UTxO contributes to the still-needed value.
- If one reserve UTxO fully funds the payout, `remainingAssets` is empty.
- If partial funding is possible, output clearly says the command must be rerun.
- Does not consume reserve UTxOs that do not contribute to the payout target.

### 7.4 Conclude Payout

Command shape:

```sh
node dist/index.js conclude-payout \
  --withdrawal-event-id <event-id-hex>
```

Required behavior:

- Resolve the current payout UTxO by payout NFT asset name.
- Decode payout datum and verify current payout assets exactly satisfy the
  target value.
- Submit `submitConcludePayoutProgram`.
- Return JSON with `txHash`, `withdrawalEventId`, `l1Address`, and paid assets.

Acceptance criteria:

- Fails if payout is missing.
- Fails if payout is underfunded or overfunded.
- Burns the payout NFT and pays the target L1 address with the datum specified
  by the withdrawal body.

## Task 8: Discovery And Status Commands

### 8.1 Withdrawal Status

Command shape:

```sh
node dist/index.js withdrawal-status --event-id <event-id-hex>
```

Also support lookup by L1 transaction:

```sh
node dist/index.js withdrawal-status --l1-tx-hash <txHash>
```

Required output:

- event id,
- L1 withdrawal order out-ref,
- inclusion time,
- L2 out-ref,
- L2 owner,
- requested value,
- L1 target address,
- refund address,
- status,
- projected header hash,
- validity,
- validity detail,
- settlement out-ref if resolvable,
- payout status if initialized.

Acceptance criteria:

- Works for awaiting, projected, finalized, valid, and invalid withdrawals.
- Does not require the withdrawal order UTxO to still be unspent.
- JSON output is stable enough for the runbook to use with `jq`.

### 8.2 Reserve UTxOs

Command shape:

```sh
node dist/index.js reserve-utxos
```

Required output:

- reserve script address,
- each reserve out-ref,
- assets per reserve UTxO,
- aggregate totals,
- whether each reserve UTxO is spendable according to the provider view.

Acceptance criteria:

- Decodes and rejects unexpected script/datum shapes rather than printing
  untyped raw UTxOs as trusted reserve state.
- Aggregate totals match the sum of listed UTxOs.

### 8.3 Payout Status

Command shape:

```sh
node dist/index.js payout-status --withdrawal-event-id <event-id-hex>
```

Required output:

- withdrawal event id,
- payout NFT unit,
- payout out-ref if present,
- phase: `not_initialized`, `initialized`, `partially_funded`, `funded`, or
  `concluded`,
- target assets,
- current assets,
- remaining assets,
- target L1 address,
- target L1 datum.

Acceptance criteria:

- Correctly reports `not_initialized` before payout initialization.
- Correctly reports `funded` only when the payout value exactly equals the
  target value plus payout NFT handling.
- Correctly reports `concluded` when no payout UTxO exists and the withdrawal
  has been finalized with a concluded payout transaction if that linkage is
  available; otherwise it reports `not_found_after_initialization` with an
  explicit diagnostic.

## Task 9: HTTP Admin Helpers For Runbooks

The current runbook can use `curl` for `GET /commit` and `GET /merge`. Keep
that path, but improve operator ergonomics.

Tasks:

- Document the required `x-midgard-admin-key` header for `/commit` and
  `/merge`.
- Optionally add CLI wrappers:
  - `commit-once`
  - `merge-once`
- Wrappers must call the same internal actions or HTTP endpoint and must not
  bypass admin authorization accidentally.

Acceptance criteria:

- The final runbook can advance commit and merge manually without relying on
  undocumented endpoints.
- Admin-disabled environments fail with an explicit error.

## Task 10: End-To-End Emulator Coverage

Update emulator coverage so the command-level flows are tested, not only the
lower-level transaction builders.

Required tests:

- `submit-withdrawal` command program creates a real withdrawal order for a
  real L2 UTxO.
- Invalid withdrawal signature is committed as
  `IncorrectWithdrawalSignature`.
- `fetch-withdrawals-once` is idempotent.
- `resolve-event-settlement-proof` returns a proof matching the settlement
  root for both deposit and withdrawal events.
- `absorb-confirmed-deposit-to-reserve` succeeds after deposit merge.
- `initialize-payout`, `add-reserve-funds-to-payout`, and `conclude-payout`
  complete the payout lifecycle after withdrawal merge.
- Status commands return expected phases throughout the flow.

Acceptance criteria:

- The existing deposit flow emulator test continues to pass.
- A new command-level emulator test proves the full:
  deposit -> send -> withdrawal -> commit -> merge -> reserve -> payout
  lifecycle.
- Tests assert the final L1 target receives the withdrawn value.
- Tests assert the withdrawn L2 UTxO is removed from the local ledger after
  withdrawal finalization.

## Task 11: Preprod Runbook

After Tasks 1-10 are complete, add:

`demo/midgard-node/docs/DEPOSIT_SEND_AND_WITHDRAW.md`

Required sections:

- Environment and wallet prerequisites.
- Deposit submission.
- Deposit projection verification.
- L2 send transaction.
- Withdrawal submission.
- Withdrawal fetch, commit, and merge.
- Withdrawal status verification.
- Reserve absorption.
- Payout initialization.
- Reserve funding.
- Payout conclusion.
- Final L1 and L2 balance verification.
- Troubleshooting for each command.

Acceptance criteria:

- Every command in the runbook exists and has been tested.
- No step requires manually building CBOR, PHAS proofs, settlement UTxO refs, or
  reference-script refs.
- The runbook includes exact expected JSON fields and `jq` snippets for moving
  values between steps.

## Final Verification Checklist

- `pnpm --dir demo/midgard-node build`
- `NODE_ENV=emulator pnpm --dir demo/midgard-node exec vitest run tests/deposit-flow-emulator.test.ts --reporter=basic --disable-console-intercept`
- Command-level emulator test for the full deposit-send-withdraw-payout flow.
- Unit tests for withdrawal signing and verification.
- Unit tests for event id, out-ref, datum, and command parser failures.
- Manual `--help` check for every new command.
- No `.complete({ localUPLCEval: false })`.
- No compatibility paths for legacy withdrawal records or old command shapes.
