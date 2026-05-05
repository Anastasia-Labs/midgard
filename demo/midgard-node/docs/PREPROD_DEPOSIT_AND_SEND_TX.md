# Preprod Deposit And Send-Tx Runbook

This runbook covers the full preprod flow for:

1. submitting an L1 deposit into Midgard,
2. projecting that deposit into the local Midgard ledger view,
3. verifying that the deposited value is spendable, and
4. submitting a Midgard-native send transaction that spends part of the deposited amount.

This document assumes the following are already true:

- the preprod contracts are already deployed and your `.env` points at that deployment,
- PostgreSQL is available,
- `midgard-node` can start successfully against preprod, and
- you are using a normal key-controlled user wallet for the deposited L2 funds.

This document does not cover contract deployment or operator lifecycle setup.

## Important Constraints

- Use a user wallet that is distinct from the operator-main, operator-merge, and reference-script wallets.
- For this flow, deposit into a key-controlled L2 address derived from the same user seed phrase that will later sign the send transaction.
- Do not set `--l2-datum` for this flow.
- Do not try to send the entire deposited lovelace balance in one go. Leave room for the Midgard-native fee.

## Required Environment

Start from [`demo/midgard-node/.env.example`](./.env.example) and fill in the preprod-specific values.

At minimum, verify these fields are set correctly:

- `NETWORK=Preprod`
- `L1_PROVIDER` plus the matching provider credentials
- `L1_OPERATOR_SEED_PHRASE`
- `L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX`
- `L1_REFERENCE_SCRIPT_SEED_PHRASE`
- `L1_REFERENCE_SCRIPT_ADDRESS`
- `HUB_ORACLE_ONE_SHOT_TX_HASH`
- `HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX`
- `POSTGRES_*`
- `PORT`

The runbook below also uses:

- `USER_WALLET` for the deposit signer and later L2 sender wallet
- optionally `DEST_WALLET` if you want to derive a second destination address from a seed phrase

## 1. Build And Start The Node

Run the node from `demo/midgard-node` and keep it running in its own terminal:

```sh
cd demo/midgard-node
pnpm install
pnpm build
pnpm listen
```

If you already have the node running against the correct preprod environment, keep using that instance.

## 2. Export The User Wallet And Derive Addresses

In a second terminal:

```sh
cd demo/midgard-node

export USER_WALLET="your user seed phrase here"
export DEST_WALLET="your destination seed phrase here"

export USER_L2_ADDRESS="$(node --input-type=module -e 'import { walletFromSeed } from "@lucid-evolution/lucid"; console.log(walletFromSeed(process.env.USER_WALLET, { network: "Preprod" }).address)')"
export DEST_L2_ADDRESS="$(node --input-type=module -e 'import { walletFromSeed } from "@lucid-evolution/lucid"; console.log(walletFromSeed(process.env.DEST_WALLET, { network: "Preprod" }).address)')"

printf 'USER_L2_ADDRESS=%s\n' "$USER_L2_ADDRESS"
printf 'DEST_L2_ADDRESS=%s\n' "$DEST_L2_ADDRESS"
```

If you already have a destination address, set `DEST_L2_ADDRESS` directly instead of exporting `DEST_WALLET`.

## 3. Submit The L1 Deposit

Build first if needed, then submit the deposit from the user wallet to the user's own L2 address:

```sh
cd demo/midgard-node
pnpm build

DEPOSIT_JSON="$(node dist/index.js submit-deposit \
  --wallet-seed-phrase-env USER_WALLET \
  --l2-address "$USER_L2_ADDRESS" \
  --lovelace 12000000)"

printf '%s\n' "$DEPOSIT_JSON" | jq .
```

Important details:

- `submit-deposit` defaults to `L1_OPERATOR_SEED_PHRASE`; for this flow, override it with `--wallet-seed-phrase-env USER_WALLET`.
- The command signs, submits, waits for L1 confirmation, and then prints JSON
  containing `txHash`, `metadata.depositEventId`, and the deposit auth asset.
- If the user wallet collides with an operational node wallet, the command will fail by design.

## 4. Wait For The Deposit To Become Projectable

Do not expect the deposit to appear in Midgard immediately after the L1 transaction confirms.

The deposit command builds the transaction with:

- a deposit validity upper bound about `60_000ms` in the future, and
- an event wait duration of `60_000ms`.

In practice, the safe rule is:

- wait about 2 minutes from deposit submission time, or
- wait at least about 60 seconds after the deposit command finishes confirming the L1 transaction.

After that, either let the running node ingest the deposit automatically, or force one projection pass manually.

Automatic path:

- `pnpm listen` already runs the deposit-fetch fiber.
- The poll interval is `WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES` from `.env`.

Manual path:

```sh
cd demo/midgard-node
node dist/index.js project-deposits-once
```

If you run `project-deposits-once` too early, it can legitimately find no eligible deposit UTxOs yet.

## 5. Verify That The Deposit Is Spendable In Midgard

Query the local Midgard mempool-ledger view for the deposited address:

```sh
cd demo/midgard-node
node dist/index.js utxos --address "$USER_L2_ADDRESS"
```

You should see JSON with:

- `utxoCount` greater than `0`,
- `totals.lovelace` reflecting the deposited amount, and
- at least one UTxO at `USER_L2_ADDRESS`.

This is the same ledger view that the transfer flow uses to discover spendable Midgard inputs.

## 6. Submit A Midgard Send Transaction Using The Deposited Amount

Send part of the deposited lovelace to another L2 address:

```sh
cd demo/midgard-node

node dist/index.js submit-l2-transfer \
  --wallet-seed-phrase-env USER_WALLET \
  --l2-address "$DEST_L2_ADDRESS" \
  --lovelace 5000000
```

Notes:

- `submit-l2-transfer` also has the alias `submit-tx`.
- The default endpoint is taken from `MIDGARD_NODE_URL`, or falls back to `http://127.0.0.1:$PORT`.
- The command prints JSON including `txId`, `status`, `selectedInputs`, `requestedAssets`, and `changeAssets`.
- With the default API submission path, the immediate response status is typically `queued`.
- The sender wallet for this command must be the same wallet whose address received the deposit in step 3.

If you want to override the node endpoint explicitly:

```sh
node dist/index.js submit-l2-transfer \
  --wallet-seed-phrase-env USER_WALLET \
  --endpoint http://127.0.0.1:3000 \
  --l2-address "$DEST_L2_ADDRESS" \
  --lovelace 5000000
```

## 7. Verify Post-Transfer State

If the transfer response returned `status: "queued"`, give the node a short moment to process the queue.

You can poll the canonical node status with:

```sh
curl "http://127.0.0.1:3000/tx-status?tx_hash=<txid-from-submit-l2-transfer>"
```

Expected statuses after queue processing are:

- `accepted`
- `pending_commit`
- `committed`

Then check both addresses:

```sh
cd demo/midgard-node

node dist/index.js utxos --address "$USER_L2_ADDRESS"
node dist/index.js utxos --address "$DEST_L2_ADDRESS"
```

Expected result:

- the sender now has less lovelace than after the deposit,
- the destination has the transferred lovelace,
- the sender also has explicit change left over after fees.

## Troubleshooting

### `submit-deposit` rejects the wallet as operationally conflicting

Use a separate `USER_WALLET`. The deposit and send-tx flows intentionally reject wallets that match:

- `L1_OPERATOR_SEED_PHRASE`
- `L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX`
- `L1_REFERENCE_SCRIPT_SEED_PHRASE`

### `project-deposits-once` finds no deposits

Most often this means you ran it too early. Wait longer and retry.

The deposit must satisfy both:

- the L1 transaction is confirmed, and
- the deposit `inclusionTime` has been reached.

### `utxos` shows nothing for `USER_L2_ADDRESS`

Check all of the following:

- the deposit used `--l2-address "$USER_L2_ADDRESS"`,
- the address was derived from the same `USER_WALLET` later used for `submit-l2-transfer`,
- enough time elapsed for projection,
- the node is connected to the correct preprod deployment and database.

### `submit-l2-transfer` says no Midgard L2 UTxOs were found

That means the deposit is not yet available in the Midgard ledger view used by the transfer flow, or the transfer signer seed phrase does not match the address that received the deposit.

### `submit-l2-transfer` fails on insufficient funds

Do not attempt to transfer the full deposited lovelace amount. Leave headroom for the Midgard-native fee so the command can build explicit change.
