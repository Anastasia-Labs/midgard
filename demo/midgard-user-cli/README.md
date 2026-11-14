# midgard-user-cli

`midgard-user` submits a user's own Midgard deposits and withdrawals. It signs
with the user's wallet and talks to L1 only through the user's own indexer
(Kupmios or Blockfrost), never through an operator node's ledger state. A
withdrawal also reads one committed L2 output from a Midgard node's public HTTP
API.

Each submission is named by `--submission-id`. The tool records what it signed
before broadcasting it, so if it is interrupted (crash, timeout, closed
terminal) you rerun the same command with the same id. The rerun follows the
recorded transaction to the end, and builds a new one only after the last one
has expired unseen. It never builds a second transaction while one may still
land.

The submission logic itself is `submitDepositProgram` and
`submitWithdrawalProgram` in `@al-ft/midgard-sdk`. This package adds the
command line, the file record and the provider setup.

## Commands

```sh
midgard-user deposit  --submission-id ID --l2-address ADDR --lovelace N [options]
midgard-user withdraw --submission-id ID --l2-out-ref TXHASH#INDEX \
                      --l1-address ADDR --node-endpoint URL [options]
midgard-user status   --submission-id ID --kind deposit|withdrawal [options]
```

`status` only reads. It prints the recorded attempts, each transaction's L1
status, the indexer's slot and one of four verdicts:

- `confirmed`: a recorded transaction is on chain;
- `in-flight`: the latest transaction may still land;
- `dead`: the latest transaction expired unseen, so a rerun builds a new one;
- `no-attempts`: nothing was signed yet.

### Options shared by every command

| Option                       | Environment fallback           | Meaning                                            |
| ---------------------------- | ------------------------------ | -------------------------------------------------- |
| `--submission-id <id>`       |                                | 1 to 128 characters; names the request             |
| `--manifest <path>`          | `MIDGARD_DEPLOYMENT_MANIFEST`  | the deployment manifest (its identity is checked)  |
| `--l1 kupmios\|blockfrost`   | `L1_ACCESS`                    | which indexer to use                               |
| `--kupo-url`, `--ogmios-url` | `L1_KUPO_URL`, `L1_OGMIOS_URL` | Kupmios endpoints                                  |
| `--blockfrost-url`           | `L1_BLOCKFROST_URL`            | Blockfrost endpoint (named networks only)          |
| `--blockfrost-project-id`    | `L1_BLOCKFROST_PROJECT_ID`     | Blockfrost project id                              |
| `--record-dir <path>`        | `MIDGARD_USER_RECORD_DIR`      | record root, default `~/.midgard/user-submissions` |
| `--json`                     |                                | print the result as one JSON object                |

The L1 network and slot mapping come from the manifest.

### Wallet and time options (`deposit`, `withdraw`)

Give exactly one seed source:

- `--wallet-seed-phrase-env NAME` reads the phrase from the environment
  variable `NAME`;
- `--wallet-seed-phrase-file PATH` reads it from an absolute path to a regular
  file (not a symbolic link) of mode `0600`. Group or world access is refused.

The phrase is never printed and never written to the record.

`--timeout-ms <ms>` bounds one run. The default covers two transaction
validity windows plus the deployment's event-history protection wait. When it
runs out, the transaction in flight stays recorded and the command exits 75.

### `deposit`

| Option                                | Meaning                                                    |
| ------------------------------------- | ---------------------------------------------------------- |
| `--l2-address <address>`              | the L2 recipient                                           |
| `--lovelace <n>`                      | lovelace to deposit, greater than zero                     |
| `--asset <policyId.assetName:amount>` | an additional asset; repeat for more (hex policy and name) |
| `--l2-datum <cbor>`                   | Plutus data CBOR attached to the L2 output                 |
| `--structural-lovelace <n>`           | ADA kept apart from the deposited value; quoted if omitted |

### `withdraw`

| Option                        | Meaning                                                          |
| ----------------------------- | ---------------------------------------------------------------- |
| `--l2-out-ref <txHash#index>` | the L2 output to withdraw                                        |
| `--l1-address <address>`      | the L1 payout address                                            |
| `--l1-datum <cbor>`           | Plutus data CBOR for the payout                                  |
| `--refund-address <address>`  | where a refused withdrawal's ADA returns; default `--l1-address` |
| `--refund-datum <cbor>`       | Plutus data CBOR for the refund                                  |
| `--lovelace <n>`              | order lovelace; quoted if omitted                                |
| `--node-endpoint <url>`       | a Midgard node's public HTTP API serving this deployment         |

On the first run the tool checks, through the node, that:

- the node serves the same deployment as the manifest;
- the output is unspent;
- the transaction that produced it is committed;
- the output belongs to the signing wallet's payment key.

It then records the output's value. A rerun uses the recorded value and does
not contact the node.

## Operator rehearsal commands

`forced-order` and `plutus-tx` produce the forced orders and Plutus L2
transactions the Preprod rehearsal journey needs
([public_testnet_readiness.md](../../docs/public_testnet_readiness.md),
"Journey"). They spend the wallet's own L2 outputs, which the node at
`--node-endpoint` must serve (deposit to the wallet's L2 address first). They
keep no record: a rerun builds and submits a new transaction.

```sh
midgard-user forced-order --shape SHAPE --node-endpoint URL \
                          --manifest PATH --l1 kupmios --kupo-url URL --ogmios-url URL \
                          --wallet-seed-phrase-file PATH [--json]
midgard-user plutus-tx    --node-endpoint URL --manifest PATH \
                          --wallet-seed-phrase-file PATH [--json]
```

Both refuse a node that serves a different deployment from the manifest.

`forced-order` builds one L2 transaction of the named shape, checks it with
`screenMidgardForcedTxAdmission` (the same checks as the tx-order mint),
mints a forced order carrying it on L1 and waits until the order is on chain.
It prints the L2 transaction id, the forced bytes, the order's L1 hash, its
event id and the time by which a block must include it. A refused shape stops
at the screen: the command exits 1 naming the check, and nothing is
submitted.

| `--shape`                 | The L2 transaction                      | The mint                                 |
| ------------------------- | --------------------------------------- | ---------------------------------------- |
| `plain`                   | one spend input, outputs without data   | admits                                   |
| `inline-datum`            | an output with an inline datum          | admits                                   |
| `native-mint`             | a mint under the wallet's native policy | admits                                   |
| `reference-input`         | a reference input (needs 2 L2 outputs)  | admits                                   |
| `required-signer`         | the wallet's key as a required signer   | admits                                   |
| `validity-upper-bound`    | an L2 validity upper bound              | refuses at `has_no_validity_upper_bound` |
| `plutus-mint`             | a PlutusV3 mint (it has a redeemer)     | refuses at `has_no_redeemers`            |
| `reference-script-output` | an output with a reference script       | refuses at `outputs_are_admissible`      |

The mint's other refusals (no spend input, auxiliary data, observers on an
untagged network, an oversized output value, the fixed item prefixes and the
mint order) need bytes lucid-midgard never builds, so no shape produces them.
Kupo must index the wallet, the tx-order address and the hub oracle address.

`plutus-tx` mints one token under an always-succeeds PlutusV3 policy, has the
node evaluate it, submits it and waits for the node's verdict. It exits 0 on
`accepted` or `committed` and 1 on `rejected`, printing the node's code.

## Indexer requirements

Kupo must index every address the submission reads or creates outputs at.
That means the wallet's address and the deployment's deposit or withdrawal
list address. Otherwise the wallet's coins or the transaction's status look
missing. Before signing anything, the tool asks Kupo (`GET /patterns/{address}`)
whether each address is covered, and refuses by name if one is not.

Run Kupo with `--match "*"`, or with patterns that cover both addresses, and
without `--prune-utxo`.

## Exit codes

| Code | Meaning                                                                      |
| ---- | ---------------------------------------------------------------------------- |
| 0    | confirmed (or `status` and `--help` succeeded)                               |
| 75   | pending: a transaction may still land; rerun with the same `--submission-id` |
| 1    | refused: bad input, a conflicting request, or an unusable manifest/indexer   |

## The record

```
<record-dir>/<manifestId>/<sha256("Deposit:" | "Withdrawal:" + id)>/   mode 0700
  intent.json         the request and its intent hash; written once
  attempt-0001.json   one signed transaction, its hash and TTL slot; written once
  attempt-0002.json   ...
```

Every file is created once, with mode `0600`, using a no-replace atomic write.
Nothing in the record is ever rewritten or deleted.

A rerun whose request differs from `intent.json` is refused: use a new
`--submission-id` for a new request. Each attempt file is checked when it is
read, and a file that does not check is refused by name.
