# Preprod Deposit, Send, And Withdraw Runbook

This runbook covers the operator-facing preprod flow for:

1. submitting an L1 deposit and waiting for its automatic confirmed merge,
2. spending its L2 value and waiting for the transfer's automatic merge,
3. waiting for the node to absorb the consumed deposit into the reserve,
4. submitting a signed withdrawal order for a selected L2 UTxO, and
5. waiting for automatic withdrawal finalization and the exact L1 payout.

Keep `listen` running throughout. Its commit and merge workers advance the
state queue; its settlement worker absorbs consumed deposits and initializes,
funds, and concludes valid withdrawal payouts. The normal flow below only
submits user actions and observes those workers. Do not call `/commit`, `/merge`,
`absorb-confirmed-deposit-to-reserve`, `initialize-payout`,
`add-reserve-funds-to-payout`, or `conclude-payout` alongside the workers:
they compete for the same protocol UTxOs and bypass the worker's receipts.

A status of `committed` alone means immutable database inclusion, not confirmed
ledger merge. Use the explicit merge evidence below. Settlement proof resolution
is an optional diagnostic while the settlement reference remains unspent;
automatic absorb or payout may already have spent it by the time you inspect it.

For fresh-deploy acceptance and retained evidence, follow
[live-acceptance.md](../../../.agents/skills/midgard-e2e-acceptance/references/live-acceptance.md).
This runbook does not replace the acceptance journey or its failure drills.

## Prerequisites

Start from `demo/midgard-node/.env.example` and verify the preprod deployment is
configured:

- `NETWORK=Preprod`
- `L1_NODE_SOCKET_PATH`, `L1_NODE_CONFIG_PATH` and
  `L1_NODE_TRANSPORT_BINARY_PATH` for a synced local cardano-node
- `L1_OPERATOR_SEED_PHRASE`
- `L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX`
- `L1_REFERENCE_SCRIPT_SEED_PHRASE`
- `L1_REFERENCE_SCRIPT_ADDRESS`
- `L1_SETTLEMENT_SEED_PHRASE`: a funded wallet the settlement worker owns,
  holding fee funds and a separate ADA-only collateral UTxO
- `HUB_ORACLE_ONE_SHOT_TX_HASH`
- `HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX`
- `POSTGRES_*`
- `PORT`

Use user wallets that are distinct from the operator-main, operator-merge, and
reference-script wallets. The withdrawal signer must control the selected L2
UTxO being withdrawn.

Deposits and withdrawals are submitted with the user's own `midgard-user` tool
(`demo/midgard-user-cli`), not with the node. It reads L1 only through the
user's own indexer and records each signed transaction before broadcasting it;
see [its README](../../midgard-user-cli/README.md). It needs:

- `MIDGARD_DEPLOYMENT_MANIFEST`: the deployment manifest the node serves, for
  example `deploymentInfo/contract-deployment-info.json`;
- `L1_ACCESS=kupmios` with `L1_KUPO_URL` and `L1_OGMIOS_URL`, or
  `L1_ACCESS=blockfrost` with `L1_BLOCKFROST_URL` and
  `L1_BLOCKFROST_PROJECT_ID`;
- a Kupo that indexes the user wallet's address and the deployment's deposit
  and withdrawal list addresses: run it with `--match "*"` or with patterns
  covering those addresses, and without `--prune-utxo`. The tool checks the
  patterns before signing and refuses by name when one is missing.

The shell snippets assume `jq` is installed.

## 1. Build And Start The Node

Run the node from `demo/midgard-node` and keep it running:

```sh
cd demo/midgard-node
pnpm install
pnpm build
pnpm --dir ../midgard-user-cli build
pnpm db:migrate
pnpm listen
```

In a second terminal:

```sh
cd demo/midgard-node

export MIDGARD_NODE_URL="${MIDGARD_NODE_URL:-http://127.0.0.1:${PORT:-3000}}"
export USER_SEED_PHRASE="user deposit seed phrase here"
export DEST_WALLET="destination withdrawal seed phrase here"

export MIDGARD_DEPLOYMENT_MANIFEST="$PWD/deploymentInfo/contract-deployment-info.json"
export L1_ACCESS=kupmios
export L1_KUPO_URL="http://127.0.0.1:1442"
export L1_OGMIOS_URL="http://127.0.0.1:1337"
midgard_user() { node ../midgard-user-cli/dist/cli.js "$@"; }

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

If the command exits 75 (pending) or is interrupted, rerun it with the same
`DEPOSIT_SUBMISSION_ID` and arguments: the rerun follows the recorded
transaction and never signs a second one while the first may still land. A new
deposit needs a new ID. `midgard_user status --submission-id
"$DEPOSIT_SUBMISSION_ID" --kind deposit` reads the record without submitting.

```sh
export DEPOSIT_SUBMISSION_ID="deposit-$(node -p 'crypto.randomUUID()')"
DEPOSIT_JSON="$(midgard_user deposit --json \
  --submission-id "$DEPOSIT_SUBMISSION_ID" \
  --wallet-seed-phrase-env USER_SEED_PHRASE \
  --l2-address "$USER_L2_ADDRESS" \
  --lovelace 12000000)"

printf '%s\n' "$DEPOSIT_JSON" | jq .

export DEPOSIT_TX_HASH="$(printf '%s\n' "$DEPOSIT_JSON" | jq -r '.admissionTxHash')"
export DEPOSIT_EVENT_ID="$(printf '%s\n' "$DEPOSIT_JSON" | jq -r '.eventId // empty')"
test -n "$DEPOSIT_EVENT_ID"
```

Expected fields:

- `admissionTxHash`: the L1 transaction that admitted the deposit;
- `eventId`: the deposit's event id;
- `publicationTxHash`: present only when the deposit's data was published in
  a transaction of its own first;
- `slot` and `attempts`.

The running node ingests, projects, commits, and merges the deposit. It is due
at its inclusion time, the admission transaction's validity upper bound plus
the profile's `event_wait_ms` (300 s on the testing profiles). Wait for
`consumed`, in this fresh flow before spending its output:

```sh
DEPOSIT_STATUS_JSON=""
DEPOSIT_DEADLINE=$(( $(date +%s) + 1200 ))
while test "$(date +%s)" -lt "$DEPOSIT_DEADLINE"; do
  DEPOSIT_STATUS_JSON="$(curl --connect-timeout 5 --max-time 10 -fsS \
    "$MIDGARD_NODE_URL/deposit-status?eventId=$DEPOSIT_EVENT_ID")" \
    || DEPOSIT_STATUS_JSON=""
  if printf '%s\n' "$DEPOSIT_STATUS_JSON" | jq -e '.status == "consumed"' >/dev/null; then
    break
  fi
  sleep 15
done
printf '%s\n' "$DEPOSIT_STATUS_JSON" | jq -e '.status == "consumed"'
node dist/index.js utxos --address "$USER_L2_ADDRESS" | jq .
```

The wait has a 20-minute deadline plus the final request/poll interval. A `404`
or `awaiting` before the deposit is due is expected. `projected` alone does not
satisfy this wait. Before spending, confirm the ledger view contains the
expected deposit output (`ledgerTxId` from the status) and value. Deposits are
applied after transactions within a block, so projection does not permit a
same-block deposit spend. A locally spent deposit can also have `consumed` status, so the unspent output
and exact value check are required for this fresh sequence. The deposit ledger ID is not a submitted L2
transaction ID; query `/deposit-status` for its lifecycle.

## 3. Submit The L2 Send Transaction

Send part of the deposited value to the destination wallet. Reuse the same
`TRANSFER_SUBMISSION_ID` and arguments if you retry an interrupted transfer;
the rerun returns or resubmits the transaction signed the first time and never
signs a second one. A new transfer needs a new ID.

```sh
export TRANSFER_SUBMISSION_ID="transfer-$(node -p 'crypto.randomUUID()')"
TRANSFER_JSON="$(node dist/index.js submit-l2-transfer \
  --submission-id "$TRANSFER_SUBMISSION_ID" \
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

## 4. Wait For The Automatic Transfer Merge

Poll the exact transfer ID until both canonical merge fields are affirmative:

```sh
TRANSFER_STATUS_JSON=""
TRANSFER_DEADLINE=$(( $(date +%s) + 1200 ))
while test "$(date +%s)" -lt "$TRANSFER_DEADLINE"; do
  TRANSFER_STATUS_JSON="$(curl --connect-timeout 5 --max-time 10 -fsS \
    "$MIDGARD_NODE_URL/tx-status?tx_hash=$TRANSFER_TX_ID")" \
    || TRANSFER_STATUS_JSON=""
  if printf '%s\n' "$TRANSFER_STATUS_JSON" | jq -e \
    '.confirmedLedgerFinalized == true and .mergeStatus == "finalized"' >/dev/null; then
    break
  fi
  if printf '%s\n' "$TRANSFER_STATUS_JSON" | jq -e '.status == "rejected"' >/dev/null; then
    break
  fi
  sleep 15
done
printf '%s\n' "$TRANSFER_STATUS_JSON" | jq -e \
  '.confirmedLedgerFinalized == true and .mergeStatus == "finalized"'
```

The deadline is 20 minutes plus the final request/poll interval. `committed`,
`accepted`, `pending_commit`, and `awaiting_local_recovery` require further
progress. A rejected transaction needs diagnosis; do not sign a replacement
using an interrupted submission ID.

## 5. Wait For The Deposit To Reach The Reserve

Once a merge folds the deposit into confirmed state, `/deposit-status` reports
it `consumed` and the settlement worker queues its absorb. Wait for that, then
check the reserve:

```sh
curl -fsS "$MIDGARD_NODE_URL/deposit-status?eventId=$DEPOSIT_EVENT_ID" | jq .
node dist/index.js reserve-utxos | jq .
```

The worker waits for authenticated L1 progress and may retry provider or
funding failures. Inspect the reserve and the settlement job for this event;
reserve funding can also spend an absorbed output before a later snapshot. `/readyz` reports the worker's state under
`settlement`.

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

export WITHDRAW_L2_VALUE="$(printf '%s\n' "$DEST_UTXOS_JSON" \
  | jq -c --arg ref "$WITHDRAW_L2_OUT_REF" \
    '.utxos[] | select("\(.txHash)#\(.outputIndex)" == $ref) | .assets')"
```

`WITHDRAW_L2_VALUE` is the value the payout must pay exactly.

## 7. Submit The Withdrawal Order

Record the destination's current L1 out-refs before submission so the final
check can distinguish a new payout from an older output with the same value:

```sh
PAYOUT_BEFORE_JSON="$(node dist/index.js l1-utxos --address "$DEST_L1_ADDRESS")"
```

Reuse the same `WITHDRAWAL_SUBMISSION_ID` and arguments if you retry an
interrupted or pending (exit 75) submission; a new withdrawal needs a new ID.

```sh
export WITHDRAWAL_SUBMISSION_ID="withdrawal-$(node -p 'crypto.randomUUID()')"
WITHDRAWAL_JSON="$(midgard_user withdraw --json \
  --submission-id "$WITHDRAWAL_SUBMISSION_ID" \
  --wallet-seed-phrase-env DEST_WALLET \
  --node-endpoint "$MIDGARD_NODE_URL" \
  --l2-out-ref "$WITHDRAW_L2_OUT_REF" \
  --l1-address "$DEST_L1_ADDRESS")"

printf '%s\n' "$WITHDRAWAL_JSON" | jq .

export WITHDRAWAL_TX_HASH="$(printf '%s\n' "$WITHDRAWAL_JSON" | jq -r '.admissionTxHash')"
export WITHDRAWAL_EVENT_ID="$(printf '%s\n' "$WITHDRAWAL_JSON" | jq -r '.eventId')"
```

`eventId` is the canonical OutputReference CBOR for the L1 nonce input spent
by the withdrawal order transaction. Use this value for withdrawal status,
settlement proof resolution, and payout lifecycle commands.

On its first run the tool asks the node that the out-ref is unspent, that the
transaction producing it is committed and that it belongs to the signing
wallet, then records its value; a rerun uses the record and does not ask the
node again.

## 8. Wait For The Automatic Withdrawal Merge

The node records the order, waits for its `inclusionTime`, commits it, and
merges it after the deployment's confirmation and challenge rules permit it.
Wait up to 80 attempts at 15-second intervals for finalized, valid status:

```sh
WITHDRAWAL_STATUS_JSON=""
for attempt in $(seq 1 80); do
  WITHDRAWAL_STATUS_JSON="$(node dist/index.js withdrawal-status \
    --event-id "$WITHDRAWAL_EVENT_ID")" || WITHDRAWAL_STATUS_JSON=""
  if printf '%s\n' "$WITHDRAWAL_STATUS_JSON" | jq -e \
    '.status == "finalized" and .validity == "WithdrawalIsValid"' >/dev/null; then
    break
  fi
  sleep 15
done
printf '%s\n' "$WITHDRAWAL_STATUS_JSON" | jq -e \
  '.status == "finalized" and .validity == "WithdrawalIsValid"'
```

CLI provider calls have their own timeouts; the attempt limit is not a precise
20-minute wall-clock guarantee. An invalid withdrawal does not enter the valid
payout path. Diagnose `validity` and `validityDetail` instead.

Do not require `payoutUtxoCount == 0` or a live `settlementOutRef` before
continuing: the settlement worker may already have initialized or concluded
this payout. `resolve-event-settlement-proof` can inspect an available
settlement reference, but its absence after automatic consumption is not proof
that merge failed.

## 9. Wait For The Automatic Payout

The settlement worker queues the payout as soon as the withdrawal is finalized
and valid. It initializes the payout, funds it from the reserve with as many
reserve inputs as it needs, and concludes it by paying exactly the withdrawn
L2 value to the recorded L1 address. Poll the payout until it is concluded:

```sh
PAYOUT_PHASE=""
for attempt in $(seq 1 120); do
  PAYOUT_STATUS_JSON="$(node dist/index.js payout-status \
    --withdrawal-event-id "$WITHDRAWAL_EVENT_ID")" || PAYOUT_STATUS_JSON=""
  PAYOUT_PHASE="$(printf '%s\n' "$PAYOUT_STATUS_JSON" | jq -r '.phase // empty')"
  printf 'PAYOUT_PHASE=%s\n' "$PAYOUT_PHASE"
  test "$PAYOUT_PHASE" = "concluded" && break
  sleep 15
done
test "$PAYOUT_PHASE" = "concluded"
```

The phase moves through `not_initialized`, `initialized`, `partially_funded`
and `funded` to `concluded`. The wait is bounded by 120 attempts, plus the CLI calls' own provider
timeouts. Each step follows the deployment's authenticated confirmation policy.
`concluded` alone is not an exact-value payment proof; perform the next check.

## 10. Final Balance Checks

Verify the withdrawn L2 UTxO was removed:

```sh
node dist/index.js utxos --address "$DEST_L2_ADDRESS" | jq .
```

Verify the L1 payout target received the withdrawn value:

```sh
PAYOUT_AFTER_JSON="$(node dist/index.js l1-utxos --address "$DEST_L1_ADDRESS")"
printf '%s\n' "$PAYOUT_AFTER_JSON" | jq .

PAYOUT_BEFORE_JSON="$PAYOUT_BEFORE_JSON" \
PAYOUT_AFTER_JSON="$PAYOUT_AFTER_JSON" \
WITHDRAW_L2_VALUE="$WITHDRAW_L2_VALUE" node --input-type=module <<'JS'
const before = JSON.parse(process.env.PAYOUT_BEFORE_JSON);
const after = JSON.parse(process.env.PAYOUT_AFTER_JSON);
const l2Value = JSON.parse(process.env.WITHDRAW_L2_VALUE);
const outRef = (u) => `${u.txHash}#${u.outputIndex}`;
const old = new Set(before.utxos.map(outRef));
const assets = (value) => JSON.stringify(Object.entries(value)
  .map(([unit, quantity]) => [unit, BigInt(quantity).toString()])
  .filter(([, quantity]) => quantity !== "0")
  .sort(([a], [b]) => a.localeCompare(b)));
const target = assets(l2Value);
const matching = after.utxos.filter((u) => !old.has(outRef(u)) && assets(u.assets) === target);
if (matching.length === 0) throw new Error("No new output pays the exact withdrawal value");
console.log(JSON.stringify({ payoutOutRefs: matching.map(outRef), targetAssets: l2Value }));
JS
```

Compare every asset, including native tokens, rather than the wallet's total
balance or an ADA-only lower bound. Keep the user wallet idle during this
check and retain the new payout transaction/out-ref. Confirm its inclusion on
the intended canonical L1 chain at the deployment's required depth; an
unconfirmed provider snapshot or a pre-existing equal-value output is not
fresh acceptance evidence.

## Troubleshooting

### Reserve Or Payout Fails With `WithdrawalsNotInRewardsCERTS`

The PHAS membership withdrawal reward account is not registered on the live
network. Check the canonical address:

```sh
node dist/index.js deployment-status | jq '.phasMembershipRewardAddress, .missingComponents'
```

For an existing otherwise-complete deployment, use the explicit registration
repair below. Do not reset local durable state to resolve a provider or worker
failure; follow [state reset rules](../../../docs/agents/state-reset.md) before
any separately authorized redeploy:

```sh
node dist/index.js register-phas-membership-reward-account
```

Reserve and payout commands intentionally do not auto-register this account
while spending protocol state. The repair command submits the canonical
registration transaction and relies on normal transaction confirmation.

### The Deposit Or Withdrawal Does Not Appear

The L1 event may not have reached its inclusion time. Compare its
`inclusionTime` with the current time, then check the node's `/readyz`
reasons, for example `l1_follower_catching_up` or
`l1_follower_view_unapplied`. Only the running node ingests
L1 events; there is no one-shot ingestion command.

### `midgard-user withdraw` Refuses The L2 Out-Ref

The selected out-ref is spent or missing from the node's view, the transaction
that produced it is not committed yet, or it is not controlled by the
withdrawal signer derived from `DEST_WALLET`. The refusal names which.

### `midgard-user` Refuses The Indexer

Kupo does not index the wallet's address or the deployment's deposit or
withdrawal list address. Add the missing pattern (or run Kupo with
`--match "*"`), keep `--prune-utxo` off, and rerun with the same submission ID.

### Withdrawal Status Is Invalid

Do not initialize payout for invalid withdrawals. Inspect `validity` and
`validityDetail`. Invalid withdrawals are committed into the withdrawal root
and must use the invalid-withdrawal refund path instead of the payout path.

### The Deposit Absorb Or Payout Does Not Progress

Check `/readyz`: its `settlement` block reports the worker's state and detail.
The worker retries a failed step with backoff and records the reason in the
`settlement_jobs` table's `last_error`, so read that row for the event rather
than rerunning the step by hand. Common causes: the settlement wallet ran out of
fee funds or lost its ADA-only collateral UTxO; the withdrawal block has not been
merged yet, so its settlement UTxO does not exist; or the reserve cannot fund
the payout (next section).

### The Reserve Cannot Fund A Payout

Run `reserve-utxos` and compare its `spendableTotals` with
`payout-status.remainingAssets`. `totals` also counts UTxOs marked
`spendable: false` (their `unspendableReason` names the datum or reference
script), which the validators refuse to spend. Funding only consumes spendable
reserve UTxOs that contribute to the remaining target value, and a reserve UTxO
that holds tokens cannot leave change below the minimum UTxO lovelace. A reserve
whose lovelace sits only in token-bearing UTxOs can therefore hold enough in
total and still fail to fund a lovelace-heavy payout. Paying a pure-ADA,
datum-free UTxO to the reserve address gives the worker an input it can use.
