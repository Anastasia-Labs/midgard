# Preprod Deposit, Send, And Withdraw Runbook

This runbook covers the operator-facing preprod flow for:

1. submitting an L1 deposit,
2. committing and merging the deposit block, then spending its L2 value,
3. committing and merging the later transfer block,
4. absorbing the confirmed deposit into the reserve,
5. submitting a signed withdrawal order for a selected L2 UTxO,
6. committing and merging the withdrawal block, and
7. initializing, funding, and concluding the L1 payout.

The commands below resolve settlement UTxOs, PHAS proofs, and reference scripts
internally. No step requires hand-built CBOR or manually assembled proofs.

**The manual `/commit` and `/merge` calls in sections 2, 4 and 8 are for a
local devnet only.** They bypass the node's commit fiber and automatic merge
fiber, so a run that uses them is not fresh-deploy acceptance evidence. For
acceptance, follow
[live-acceptance.md](../../../.agents/skills/midgard-e2e-acceptance/references/live-acceptance.md):
skip every manual `/commit` and `/merge` call here, and instead wait for the
running node to commit and to merge automatically, as its
`await-automatic-merge` step does. The deposit, withdrawal and payout commands
in this runbook are the same in both cases.

## Prerequisites

Start from `demo/midgard-node/.env.example` and verify the preprod deployment is
configured:

- `NETWORK=Preprod`
- `L1_PROVIDER=Kupmios` with healthy local Kupo and Ogmios endpoints
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
pnpm db:migrate
pnpm listen
```

In a second terminal:

```sh
cd demo/midgard-node

export MIDGARD_NODE_URL="${MIDGARD_NODE_URL:-http://127.0.0.1:${PORT:-3000}}"
export USER_SEED_PHRASE="user deposit seed phrase here"
export DEST_WALLET="destination withdrawal seed phrase here"

export USER_L2_ADDRESS="$(node --input-type=module -e 'import { walletFromSeed } from "@lucid-evolution/lucid"; console.log(walletFromSeed(process.env.USER_SEED_PHRASE, { network: "Preprod" }).address)')"
export DEST_L2_ADDRESS="$(node --input-type=module -e 'import { walletFromSeed } from "@lucid-evolution/lucid"; console.log(walletFromSeed(process.env.DEST_WALLET, { network: "Preprod" }).address)')"
export DEST_L1_ADDRESS="$DEST_L2_ADDRESS"

printf 'USER_L2_ADDRESS=%s\n' "$USER_L2_ADDRESS"
printf 'DEST_L2_ADDRESS=%s\n' "$DEST_L2_ADDRESS"
```

Verify the live deployment before submitting value:

```sh
node dist/index.js deployment-status | jq .
```

Expected deployment fields:

- `complete` is `true`
- `missingComponents` is empty

Fresh `init` registers the canonical PHAS membership reward account in the
atomic initialization transaction. `deployment-status` intentionally does not
query provider-specific reward-account state. The ledger proves missing
registration when a membership-proof withdraw-zero transaction is submitted.

If an existing deployment is otherwise complete but the PHAS reward account is
known missing, or a reserve/payout transaction fails with
`WithdrawalsNotInRewardsCERTS`, run the explicit deployment repair command and
check status again:

```sh
node dist/index.js register-phas-membership-reward-account
node dist/index.js deployment-status | jq .
```

## 2. Submit The Deposit

Reuse the same `DEPOSIT_SUBMISSION_ID` if you retry an interrupted
submission; a new deposit needs a new ID.

```sh
export DEPOSIT_SUBMISSION_ID="deposit-$(node -p 'crypto.randomUUID()')"
DEPOSIT_JSON="$(node dist/index.js submit-deposit \
  --submission-id "$DEPOSIT_SUBMISSION_ID" \
  --wallet-seed-phrase-env USER_SEED_PHRASE \
  --l2-address "$USER_L2_ADDRESS" \
  --lovelace 12000000)"

printf '%s\n' "$DEPOSIT_JSON" | jq .

export DEPOSIT_TX_HASH="$(printf '%s\n' "$DEPOSIT_JSON" | jq -r '.txHash')"
export DEPOSIT_EVENT_ID="$(printf '%s\n' "$DEPOSIT_JSON" | jq -r '.metadata.depositEventId // empty')"
test -n "$DEPOSIT_EVENT_ID"
```

Expected fields:

- `txHash`
- `metadata.depositEventId`
- `metadata.depositAssetName`
- `metadata.depositAuthUnit`
- `metadata.inclusionTime`

The running node records and projects the deposit itself. The deposit is due
at `metadata.inclusionTime`, the transaction's validity upper bound plus the
profile's `event_wait_ms` (300 s on the testing profiles). Wait at most 20
minutes for `/deposit-status` to report it projected:

```sh
DEPOSIT_STATUS=""
for attempt in $(seq 1 80); do
  DEPOSIT_STATUS="$(curl -fsS \
    "$MIDGARD_NODE_URL/deposit-status?eventId=$DEPOSIT_EVENT_ID" \
    | jq -r '.status')" || DEPOSIT_STATUS=""
  case "$DEPOSIT_STATUS" in projected | consumed) break ;; esac
  sleep 15
done
printf 'DEPOSIT_STATUS=%s\n' "$DEPOSIT_STATUS"
case "$DEPOSIT_STATUS" in projected | consumed) ;; *) false ;; esac
```

The last line fails unless `DEPOSIT_STATUS` is `projected` or `consumed`. A
`404` or `awaiting` before the deposit is due is not a failure.

Inspect the spendable L2 ledger view (the new deposit remains hidden until
its header is confirmed):

```sh
node dist/index.js utxos --address "$USER_L2_ADDRESS" | jq .
```

Before spending this output, this runbook requires committing the deposit block
and waiting for its confirmed merge. The runtime exposes the projected output
once confirmation assigns its header; the runbook waits for settlement as an
additional sequencing check. Deposits are applied after transactions within a block; projection into
the local ledger does not make a same-block deposit spend valid.

Devnet only (see the note at the top; for acceptance, wait for the commit
fiber instead):

```sh
curl -fsS \
  -H "x-midgard-admin-key: $ADMIN_API_KEY" \
  "$MIDGARD_NODE_URL/commit" | jq .
```

Wait for L1 confirmation, committee DA attestation, and maturity, then merge (devnet only; for acceptance, wait for the automatic
merge fiber instead):

```sh
curl -fsS \
  -H "x-midgard-admin-key: $ADMIN_API_KEY" \
  "$MIDGARD_NODE_URL/merge" | jq .

node dist/index.js resolve-event-settlement-proof \
  --kind deposit \
  --event-id "$DEPOSIT_EVENT_ID" | jq .
```

Proceed only once the deposit settlement proof resolves. A skipped merge is not
confirmation of that deposit; inspect its reported blocker and allow the normal
confirmation/attestation lifecycle to finish.

## 3. Submit The L2 Send Transaction

Send part of the deposited value to the destination wallet:

```sh
TRANSFER_JSON="$(node dist/index.js submit-l2-transfer \
  --wallet-seed-phrase-env USER_SEED_PHRASE \
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

## 4. Commit And Merge The Transfer Block

The admin endpoints require a configured `ADMIN_API_KEY`.

Devnet only (see the note at the top; for acceptance, wait for the commit
fiber instead):

```sh
curl -fsS \
  -H "x-midgard-admin-key: $ADMIN_API_KEY" \
  "$MIDGARD_NODE_URL/commit" | jq .
```

Wait until the queued block is eligible for merge, then merge (devnet only; for acceptance, wait for the automatic
merge fiber instead):

```sh
curl -fsS \
  -H "x-midgard-admin-key: $ADMIN_API_KEY" \
  "$MIDGARD_NODE_URL/merge" | jq .
```

The merge response should include `result.status == "merged"`. A skipped status
means no state-queue block was folded into confirmed state and the next steps
must wait or resolve the reported blocker.

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
    | if length == 0 then error("no funded withdrawal UTxO")
      else .[0] | "\(.txHash)#\(.outputIndex)" end')"

test -n "$WITHDRAW_L2_OUT_REF"
printf 'WITHDRAW_L2_OUT_REF=%s\n' "$WITHDRAW_L2_OUT_REF"
```

## 7. Submit The Withdrawal Order

Reuse the same `WITHDRAWAL_SUBMISSION_ID` if you retry an interrupted
submission; a new withdrawal needs a new ID.

```sh
export WITHDRAWAL_SUBMISSION_ID="withdrawal-$(node -p 'crypto.randomUUID()')"
WITHDRAWAL_JSON="$(node dist/index.js submit-withdrawal \
  --submission-id "$WITHDRAWAL_SUBMISSION_ID" \
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
status, settlement proof resolution, and payout lifecycle commands.

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

## 8. Wait For, Commit, And Merge The Withdrawal

The running node records the withdrawal order itself. `withdrawal-status`
fails until the order is recorded, so wait at most 20 minutes for it:

```sh
WITHDRAWAL_STATUS_JSON=""
for attempt in $(seq 1 80); do
  WITHDRAWAL_STATUS_JSON="$(node dist/index.js withdrawal-status \
    --event-id "$WITHDRAWAL_EVENT_ID")" && break
  WITHDRAWAL_STATUS_JSON=""
  sleep 15
done
test -n "$WITHDRAWAL_STATUS_JSON"
printf '%s\n' "$WITHDRAWAL_STATUS_JSON" | jq .
```

A recorded order is still `awaiting` and is not due until its `inclusionTime`,
the order's `validTo` plus the profile's `event_wait_ms` (300 s on the testing
profiles). A block committed before then cannot include it, so wait until that
time has passed:

```sh
WITHDRAWAL_INCLUSION_TIME="$(printf '%s\n' "$WITHDRAWAL_STATUS_JSON" \
  | jq -r '.inclusionTime')"
sleep "$(node -e '
  const due = Date.parse(process.argv[1]);
  if (Number.isNaN(due)) throw new Error("invalid inclusionTime");
  console.log(Math.max(0, Math.ceil((due - Date.now()) / 1000)));
' "$WITHDRAWAL_INCLUSION_TIME")"
```

Commit the withdrawal block:

Devnet only (see the note at the top; for acceptance, wait for the commit
fiber instead):

```sh
curl -fsS \
  -H "x-midgard-admin-key: $ADMIN_API_KEY" \
  "$MIDGARD_NODE_URL/commit" | jq .
```

Wait until it is eligible, then merge (devnet only; for acceptance, wait for the automatic
merge fiber instead):

```sh
curl -fsS \
  -H "x-midgard-admin-key: $ADMIN_API_KEY" \
  "$MIDGARD_NODE_URL/merge" | jq .
```

Require `result.status == "merged"` before continuing to payout proof
resolution.

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

The funding command picks the reserve UTxO with the largest lovelace
contribution among those the validators can spend: no datum, no reference
script, and any change left at least the minimum UTxO lovelace. Anyone can
pay to the reserve address, so it skips UTxOs outside that shape. If none
qualifies, it fails closed with a diagnostic. `--reserve-out-ref
<txHash#outputIndex>` names the reserve UTxO to spend instead, and is refused
with the reason when that UTxO cannot fund the payout.

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

### Reserve Or Payout Fails With `WithdrawalsNotInRewardsCERTS`

The PHAS membership withdrawal reward account is not registered on the live
network. Check the canonical address:

```sh
node dist/index.js deployment-status | jq '.phasMembershipRewardAddress, .missingComponents'
```

For a fresh deployment, rerun the clean deployment flow. For an existing
otherwise-complete deployment, use:

```sh
node dist/index.js register-phas-membership-reward-account
```

Reserve and payout commands intentionally do not auto-register this account
while spending protocol state. The repair command submits the canonical
registration transaction and relies on normal transaction confirmation.

### The Deposit Or Withdrawal Does Not Appear

The L1 event may not have reached its inclusion time. Compare its
`inclusionTime` with the current time, then check the node's `/readyz`
reasons, for example `history_owner_not_ready`. Only the running node ingests
L1 events; there is no one-shot ingestion command.

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

Run `reserve-utxos` and compare its `spendableTotals` with
`payout-status.remainingAssets`. `totals` also counts UTxOs marked
`spendable: false` (their `unspendableReason` names the datum or reference
script), which the validators refuse to spend. The funding command only
consumes spendable reserve UTxOs that contribute to the remaining target value.
