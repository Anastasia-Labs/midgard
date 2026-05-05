# Preprod Deposit, Send, And Withdraw Runbook

This runbook covers the operator-facing preprod flow for:

1. submitting an L1 deposit,
2. spending the projected L2 value with a Midgard-native transfer,
3. committing and merging the block that includes the deposit and transfer,
4. absorbing the confirmed deposit into the reserve,
5. submitting a signed withdrawal order for a selected L2 UTxO,
6. committing and merging the withdrawal block, and
7. initializing, funding, and concluding the L1 payout.

The commands below resolve settlement UTxOs, PHAS proofs, and reference scripts
internally. No step requires hand-built CBOR or manually assembled proofs.

## Prerequisites

Start from `demo/midgard-node/.env.example` and verify the preprod deployment is
configured:

- `NETWORK=Preprod`
- `L1_PROVIDER` and matching provider credentials
- `L1_OPERATOR_SEED_PHRASE`
- `L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX`
- `L1_REFERENCE_SCRIPT_SEED_PHRASE`
- `L1_REFERENCE_SCRIPT_ADDRESS`
- `HUB_ORACLE_ONE_SHOT_TX_HASH`
- `HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX`
- `POSTGRES_*`
- `PORT`
- `ADMIN_API_KEY`

Use user wallets that are distinct from the operator-main, operator-merge, and
reference-script wallets. The withdrawal signer must control the selected L2
UTxO being withdrawn.

The shell snippets assume `jq` is installed.

## 1. Build And Start The Node

Run the node from `demo/midgard-node` and keep it running:

```sh
cd demo/midgard-node
pnpm install
pnpm build
pnpm listen
```

In a second terminal:

```sh
cd demo/midgard-node

export MIDGARD_NODE_URL="${MIDGARD_NODE_URL:-http://127.0.0.1:${PORT:-3000}}"
export USER_WALLET="user deposit seed phrase here"
export DEST_WALLET="destination withdrawal seed phrase here"

export USER_L2_ADDRESS="$(node --input-type=module -e 'import { walletFromSeed } from "@lucid-evolution/lucid"; console.log(walletFromSeed(process.env.USER_WALLET, { network: "Preprod" }).address)')"
export DEST_L2_ADDRESS="$(node --input-type=module -e 'import { walletFromSeed } from "@lucid-evolution/lucid"; console.log(walletFromSeed(process.env.DEST_WALLET, { network: "Preprod" }).address)')"
export DEST_L1_ADDRESS="$DEST_L2_ADDRESS"

printf 'USER_L2_ADDRESS=%s\n' "$USER_L2_ADDRESS"
printf 'DEST_L2_ADDRESS=%s\n' "$DEST_L2_ADDRESS"
```

## 2. Submit The Deposit

```sh
DEPOSIT_JSON="$(node dist/index.js submit-deposit \
  --wallet-seed-phrase-env USER_WALLET \
  --l2-address "$USER_L2_ADDRESS" \
  --lovelace 12000000)"

printf '%s\n' "$DEPOSIT_JSON" | jq .

export DEPOSIT_TX_HASH="$(printf '%s\n' "$DEPOSIT_JSON" | jq -r '.txHash')"
export DEPOSIT_EVENT_ID="$(printf '%s\n' "$DEPOSIT_JSON" | jq -r '.metadata.depositEventId')"
```

Expected fields:

- `txHash`
- `metadata.depositEventId`
- `metadata.depositAssetName`
- `metadata.depositAuthUnit`
- `metadata.inclusionTime`

Wait until the deposit inclusion time has elapsed, then project deposits once:

```sh
node dist/index.js project-deposits-once
```

Verify the deposit is visible in the L2 ledger view:

```sh
node dist/index.js utxos --address "$USER_L2_ADDRESS" | jq .
```

## 3. Submit The L2 Send Transaction

Send part of the deposited value to the destination wallet:

```sh
TRANSFER_JSON="$(node dist/index.js submit-l2-transfer \
  --wallet-seed-phrase-env USER_WALLET \
  --endpoint "$MIDGARD_NODE_URL" \
  --l2-address "$DEST_L2_ADDRESS" \
  --lovelace 5000000)"

printf '%s\n' "$TRANSFER_JSON" | jq .
export TRANSFER_TX_ID="$(printf '%s\n' "$TRANSFER_JSON" | jq -r '.txId')"
```

Check admission status:

```sh
curl -fsS "$MIDGARD_NODE_URL/tx-status?tx_hash=$TRANSFER_TX_ID" | jq .
```

## 4. Commit And Merge The Deposit/Transfer Block

The admin endpoints require a configured `ADMIN_API_KEY`.

```sh
curl -fsS \
  -H "x-midgard-admin-key: $ADMIN_API_KEY" \
  "$MIDGARD_NODE_URL/commit" | jq .
```

Wait until the queued block is eligible for merge, then merge:

```sh
curl -fsS \
  -H "x-midgard-admin-key: $ADMIN_API_KEY" \
  "$MIDGARD_NODE_URL/merge" | jq .
```

Resolve the deposit settlement proof as a diagnostic check:

```sh
node dist/index.js resolve-event-settlement-proof \
  --kind deposit \
  --event-id "$DEPOSIT_EVENT_ID" | jq .
```

The output must include `settlementOutRef`, `root`, and `proofCbor`.

## 5. Absorb The Confirmed Deposit To Reserve

```sh
ABSORB_JSON="$(node dist/index.js absorb-confirmed-deposit-to-reserve \
  --deposit-event-id "$DEPOSIT_EVENT_ID")"

printf '%s\n' "$ABSORB_JSON" | jq .
node dist/index.js reserve-utxos | jq .
```

The reserve output should contain the deposited lovelace and no datum.

## 6. Select A Destination L2 UTxO For Withdrawal

After the transfer block is merged, the destination UTxO is available as a
withdrawal target:

```sh
DEST_UTXOS_JSON="$(node dist/index.js utxos --address "$DEST_L2_ADDRESS")"
printf '%s\n' "$DEST_UTXOS_JSON" | jq .

export WITHDRAW_L2_OUT_REF="$(printf '%s\n' "$DEST_UTXOS_JSON" \
  | jq -r '.utxos
    | sort_by(.txHash, .outputIndex)
    | map(select(((.assets.lovelace // "0") | tonumber) >= 5000000))
    | .[0]
    | "\(.txHash)#\(.outputIndex)"')"

test "$WITHDRAW_L2_OUT_REF" != "null"
printf 'WITHDRAW_L2_OUT_REF=%s\n' "$WITHDRAW_L2_OUT_REF"
```

## 7. Submit The Withdrawal Order

```sh
WITHDRAWAL_JSON="$(node dist/index.js submit-withdrawal \
  --wallet-seed-phrase-env DEST_WALLET \
  --endpoint "$MIDGARD_NODE_URL" \
  --l2-out-ref "$WITHDRAW_L2_OUT_REF" \
  --l1-address "$DEST_L1_ADDRESS")"

printf '%s\n' "$WITHDRAWAL_JSON" | jq .

export WITHDRAWAL_TX_HASH="$(printf '%s\n' "$WITHDRAWAL_JSON" | jq -r '.txHash')"
export WITHDRAWAL_EVENT_ID="$(printf '%s\n' "$WITHDRAWAL_JSON" | jq -r '.withdrawalEventId')"
```

`withdrawalEventId` is the canonical OutputReference CBOR for the L1 nonce
input spent by the withdrawal order transaction. Use this value for withdrawal
fetching, settlement proof resolution, and payout lifecycle commands.

Expected fields:

- `txHash`
- `withdrawalEventId`
- `withdrawalAssetName`
- `l2OutRef`
- `l2Owner`
- `l2Value`
- `l1Address`
- `refundAddress`
- `nonceInput`
- `validTo`
- `inclusionTime`

## 8. Fetch, Commit, And Merge The Withdrawal

After the withdrawal order inclusion time has elapsed:

```sh
node dist/index.js fetch-withdrawals-once | jq .

node dist/index.js withdrawal-status \
  --event-id "$WITHDRAWAL_EVENT_ID" | jq .
```

Commit the withdrawal block:

```sh
curl -fsS \
  -H "x-midgard-admin-key: $ADMIN_API_KEY" \
  "$MIDGARD_NODE_URL/commit" | jq .
```

Wait until it is eligible, then merge:

```sh
curl -fsS \
  -H "x-midgard-admin-key: $ADMIN_API_KEY" \
  "$MIDGARD_NODE_URL/merge" | jq .
```

Verify the withdrawal is finalized and valid:

```sh
node dist/index.js withdrawal-status \
  --event-id "$WITHDRAWAL_EVENT_ID" | jq .

node dist/index.js resolve-event-settlement-proof \
  --kind withdrawal \
  --event-id "$WITHDRAWAL_EVENT_ID" | jq .
```

Expected withdrawal status fields:

- `status` is `finalized`
- `validity` is `WithdrawalIsValid`
- `settlementOutRef` is non-null
- `payoutUtxoCount` is `0` before initialization

## 9. Initialize The Payout

```sh
INIT_PAYOUT_JSON="$(node dist/index.js initialize-payout \
  --withdrawal-event-id "$WITHDRAWAL_EVENT_ID")"

printf '%s\n' "$INIT_PAYOUT_JSON" | jq .

node dist/index.js payout-status \
  --withdrawal-event-id "$WITHDRAWAL_EVENT_ID" | jq .
```

The payout phase should be `initialized` unless the initial payout UTxO already
contains part of the target value.

## 10. Fund The Payout From Reserve

Run reserve funding until `payout-status.phase` becomes `funded`:

```sh
while true; do
  PAYOUT_STATUS_JSON="$(node dist/index.js payout-status \
    --withdrawal-event-id "$WITHDRAWAL_EVENT_ID")"
  printf '%s\n' "$PAYOUT_STATUS_JSON" | jq .

  PAYOUT_PHASE="$(printf '%s\n' "$PAYOUT_STATUS_JSON" | jq -r '.phase')"
  test "$PAYOUT_PHASE" = "funded" && break

  node dist/index.js add-reserve-funds-to-payout \
    --withdrawal-event-id "$WITHDRAWAL_EVENT_ID" | jq .
done
```

If no reserve UTxO contributes to the remaining target value, the funding
command fails closed and prints a diagnostic instead of consuming unrelated
reserve state.

## 11. Conclude The Payout

```sh
CONCLUDE_JSON="$(node dist/index.js conclude-payout \
  --withdrawal-event-id "$WITHDRAWAL_EVENT_ID")"

printf '%s\n' "$CONCLUDE_JSON" | jq .

node dist/index.js payout-status \
  --withdrawal-event-id "$WITHDRAWAL_EVENT_ID" | jq .
```

After conclusion, `payout-status.phase` should be `concluded`.

## 12. Final Balance Checks

Verify the withdrawn L2 UTxO was removed:

```sh
node dist/index.js utxos --address "$DEST_L2_ADDRESS" | jq .
```

Verify the L1 payout target received the withdrawn value:

```sh
node dist/index.js l1-utxos --address "$DEST_L1_ADDRESS" | jq .
```

At minimum, check that the L1 UTxO list includes the concluded payout
transaction hash from `CONCLUDE_JSON.txHash` and the expected lovelace value.

## Troubleshooting

### Admin Endpoints Return 403

`ADMIN_API_KEY` is empty or not configured on the running node. Set it in the
node environment and restart `pnpm listen`.

### Admin Endpoints Return 401

The `x-midgard-admin-key` header does not match the running node's
`ADMIN_API_KEY`.

### `project-deposits-once` Or `fetch-withdrawals-once` Finds Nothing

The L1 event may not have reached its inclusion time. Wait and rerun the
one-shot command. These commands are idempotent.

### `submit-withdrawal` Rejects The L2 Out-Ref

The selected out-ref is missing from the node's `/utxos?by-outrefs` view, is
stale, or is not controlled by the withdrawal signer derived from
`DEST_WALLET`.

### Withdrawal Status Is Invalid

Do not initialize payout for invalid withdrawals. Inspect `validity` and
`validityDetail`. Invalid withdrawals are committed into the withdrawal root
and must use the invalid-withdrawal refund path instead of the payout path.

### `initialize-payout` Cannot Resolve Settlement Proof

The withdrawal block has not been merged yet, the event id is not canonical
`SDK.OutputReference` CBOR hex, or the settlement UTxO for the projected header
is missing from L1.

### Payout Funding Is Underfunded

Run `reserve-utxos` and compare its aggregate totals with
`payout-status.remainingAssets`. The funding command only consumes reserve UTxOs
that contribute to the remaining target value.
