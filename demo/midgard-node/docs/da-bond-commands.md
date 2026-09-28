# DA bond pool commands

The `da-bond` command group operates the pooled DA committee bond: `status`,
`top-up`, `withdraw begin|cancel|complete`, `witness` and `assemble`. This page
covers funding, top-up, the owner-quorum withdrawal, the refusals, and the
alerts the watcher and the DA committee node raise about the pool.

## The pool

One DA bond pool backs every attestation of the DA committee as a whole. It is a
single UTxO at the pool address holding the pool NFT, with the datum `Bonded` or
`Withdrawing { unlock_at }`. No committee member has a bond of their own.

- **Backing** is the pool's lovelace above `da_bond_pool_floor_lovelace`. The
  floor keeps the UTxO above min-UTxO and is never counted.
- **An attestation applies** only while the pool is `Bonded` and its backing is
  at least `da_bond_lovelace` (one DA bond). A `Withdrawing` pool, or a pool
  with less than one bond of backing, backs no attestation. The committee node
  then backs off Apply (`da_bond_pool_apply_backoff`) until the pool can back
  one again.
- **A lost availability challenge slashes the pool.** The Timeout takes one DA
  bond, or the whole backing when less is left. Up to
  `da_slash_penalty_lovelace` of it is paid as the transaction fee and the rest
  goes to the challenger. A slash is
  valid in both states, so a `Withdrawing` pool stays slashable.
- **Anyone can top it up.** Contributors hold no share and no claim.
- **Only the governance owner quorum can take backing out**, in two steps with
  a delay that outlasts every challenge the pool could still owe.

A pool with exactly one bond of backing is short again after one slash, and
attestations pause until someone tops it up. Keeping more than one bond of
backing lets attestations continue after a slash.

The amounts come from the deployment profile
(`config/deployments/<profile>.yaml`, `da_bond` and `timing`):

| Profile field                      | `preprod-testing`, `local-devnet-testing` | `preprod-public`, `mainnet` |
| ---------------------------------- | ----------------------------------------- | --------------------------- |
| `da_bond_lovelace`                 | 500 ADA                                   | 100,000 ADA                 |
| `da_slash_penalty_lovelace`        | 100 ADA                                   | 25,000 ADA                  |
| `da_bond_min_top_up_lovelace`      | 5 ADA                                     | 1,000 ADA                   |
| `da_bond_pool_floor_lovelace`      | 5 ADA                                     | 5 ADA                       |
| `timing.da_bond_withdraw_delay_ms` | 2,340,000 (39 min)                        | 778,080,000 (9 d 8 min)     |
| `timing.max_validity_range_ms`     | 480,000 (8 min)                           | 480,000 (8 min)             |

## Funding at deployment

The pool is created by the atomic protocol initialization (`init`), in the same
transaction as the hub oracle, the state queue and the DA params governor. That
transaction mints the pool NFT and funds the pool with the floor plus one DA
bond (`da_bond_pool_floor_lovelace + da_bond_lovelace`), paid by the wallet
that runs `init`. The pool is therefore `Bonded` with one full bond of backing
before the first attestation, and no separate top-up is needed to start. Fund
the init wallet for this on top of its other init costs: 505 ADA on the testing
profiles, 100,005 ADA on the public ones. The devnet protocol bootstrap runs
the same `init`.

The pool NFT is never burned and the pool cannot be initialized again. `init`
refuses an on-chain deployment that lacks the pool as partially initialized,
naming `da-bond-pool` in `missing_components`. After initialization, keep the
pool funded with top-ups.

## Running the commands

Build from `demo/midgard-node` with `pnpm run build`, then run
`node dist/index.js da-bond --help` and `node dist/index.js da-bond <command> --help`.

Every command except `witness` reads the chain and takes the verified finalized
contract deployment manifest:

```sh
node dist/index.js da-bond status \
  --manifest /var/lib/midgard/contract-deployment-info.json \
  --kupo-url http://127.0.0.1:1442 \
  --ogmios-url http://127.0.0.1:1337
```

Kupo and Ogmios URLs may instead come from `L1_KUPO_KEY` and `L1_OGMIOS_KEY`.
The commands authenticate the pool's reference scripts through the manifest's
reference-script tokens, and the DA params UTxO by the governor NFT at the
governor address. The withdraw delay is the manifest's
`deploymentProfile.timing.da_bond_withdraw_delay_ms`.

The network is the manifest's. A `Custom` deployment (the local devnet) has no
built-in slot mapping, so the commands derive it the way the node does: from
the Shelley genesis and a live tip read through the local Ogmios, checked
against each other. If that fails, the command refuses with
`Refusing the Custom deployment: ... from the local Ogmios at <url> failed: ...`
before building anything. Mainnet, Preprod and Preview do not query it.

Results are JSON on stdout, with amounts as decimal lovelace strings and times
as POSIX milliseconds (with an ISO copy where shown). A refusal prints
`da-bond <command>: <message>` on stderr and exits 1.

A command that submits exits 0 only once its transaction is confirmed and the
pool status has been read back. A failure after the submit still exits 1, and
its message names the transaction: `Transaction <txHash> was submitted, but
waiting for its confirmation failed: ...` or `Transaction <txHash> is confirmed,
but reading the pool status failed: ...`. A non-zero exit whose message names a
txHash means that transaction may be on chain: check that hash before running
the command again, because a second `top-up` pays the pool twice.

## `status`

`da-bond status` prints the pool's state and backing:

```json
{
  "poolOutRef": "<txHash>#0",
  "state": "withdrawing",
  "lovelace": "505000000",
  "backing": "500000000",
  "requiredBacking": "500000000",
  "belowBond": false,
  "unlockAt": "1790000000000",
  "unlockAtIso": "2026-09-21T14:13:20.000Z",
  "unlockable": false
}
```

- `state` is `bonded` or `withdrawing`.
- `lovelace` is the whole pool value, floor included. `backing` is the lovelace
  above the floor, and 0 when the pool is at or below the floor.
- `requiredBacking` is `da_bond_lovelace`. `belowBond` is
  `backing < requiredBacking`. It is independent of `state`: a `Withdrawing`
  pool can also be short.
- `unlockAt`, `unlockAtIso` and `unlockable` appear only while `withdrawing`.
  `unlockable` compares this machine's clock with `unlock_at`.

Attestations can apply only when `state` is `bonded` and `belowBond` is false.

## `top-up`

Anyone can top up, in either state. The amount must be at least
`da_bond_min_top_up_lovelace`, and only lovelace can be added.

```sh
node dist/index.js da-bond top-up \
  --manifest /var/lib/midgard/contract-deployment-info.json \
  --amount 500000000 \
  --wallet-seed-env DA_BOND_FUNDER_KEY
```

`--wallet-seed-env` names the environment variable that holds the funding
wallet's secret; the secret is never printed. It is either a bech32
`ed25519_sk`/`ed25519e_sk` key, which funds from that key's enterprise address,
or a mnemonic, which funds from its base address, account 0, as the node selects
`L1_OPERATOR_SEED_PHRASE`. The wallet pays the amount and the fee. The command
signs, submits, waits for confirmation and prints
`{action: "top-up", txHash, amount, previousPoolOutRef, status}`, where `status`
is the new `status` readout.

A top-up into a `Withdrawing` pool is backing the owners can then withdraw.

## Withdrawal

Taking backing out needs the DA params owner quorum: at least `update_threshold`
distinct owners from the live DA params datum. `init` sets the owners from
`DA_OWNERS_HEX` (packed payment key hashes), or from the locally held DA keys
when it is unset, and `update_threshold` to two thirds of them, rounded up. It
runs in two steps:

1. **BeginWithdraw** turns `Bonded` into `Withdrawing { unlock_at }`, with
   `unlock_at` = the transaction's upper validity bound + `da_bond_withdraw_delay_ms`.
   The value does not change.
2. After `unlock_at`, **CompleteWithdraw** pays `--amount` (at most the backing)
   to `--to` and returns the pool to `Bonded` with the remainder. It needs a
   validity lower bound at or after `unlock_at`.

Or, at any time while `Withdrawing`, **CancelWithdraw** returns the pool to
`Bonded` with its value unchanged.

From the moment BeginWithdraw lands, the pool backs no attestation: Apply is
refused until a CancelWithdraw or CompleteWithdraw returns it to `Bonded` with
at least one bond of backing. An attestation that cannot apply within the DA
attestation timeout lapses. Begin a withdrawal only when attestations may pause
for the whole delay, or cancel it to resume them. A `Withdrawing` pool still
pays a slash, so a withdrawal cannot outrun a challenge the pool owes. A
CompleteWithdraw that leaves less than one bond of backing leaves the pool
`Bonded` but short, and attestations stay paused until a top-up.

### Multi-signer flow

Owner keys never meet on one machine. The transaction is built unsigned, each
owner witnesses it on their own machine, and one machine assembles the
witnesses and submits.

**1. Build unsigned.** Anyone with the manifest can build; no owner key is
needed.

```sh
node dist/index.js da-bond withdraw begin \
  --manifest /var/lib/midgard/contract-deployment-info.json \
  --fee-address addr1... \
  --signers <ownerKeyHashA>,<ownerKeyHashB> \
  --build-unsigned begin.unsigned.json
```

- `--signers` lists the 28-byte payment key hashes of **exactly** the owners
  who will sign, comma separated. Each becomes a required signer of the
  transaction, so every listed owner must witness it; an owner who is listed
  but does not sign blocks assembly. List at least `update_threshold` owners,
  each once.
- `--fee-address` is a key address whose UTxOs pay the fee and the collateral.
  Its key must also witness the transaction.
- `withdraw begin` takes `--valid-for-ms <ms>`, the validity range length,
  counted from now. It defaults to, and may not exceed, the maximum validity
  range (`MAX_VALIDITY_RANGE_LENGTH_MS`, the profile's
  `timing.max_validity_range_ms`). `unlock_at` counts from the end of this
  range.
- `withdraw complete` requires `--amount <lovelace>` and `--to <bech32>`. It is
  built with a lower bound of now, so build it at or after `unlock_at`.
- `withdraw cancel` takes neither.

The command writes the unsigned file (format `midgard-da-bond-unsigned-v1`:
network, manifest id, `action`, `txCbor`, `txBodyHash`, `requiredSigners`,
`feePayerKeyHash`, `poolOutRef`, `daParamsOutRef`, and `validFrom`/`validTo`
when the transaction has them), submits nothing, and prints
`{action, unsignedFile, txBodyHash, requiredSigners, feePayerKeyHash, updateThreshold, poolOutRef, daParamsOutRef, validFrom?, validTo?, validToIso?, unlockAt?, unlockAtIso?, amount?, to?, submitted: false}`.
`unlockAt` is printed for `begin`; `amount` and `to` for `complete`. The file is
created new and never overwrites an existing file.

**2. Witness, once per key, on the key holder's machine.** `witness` is
offline: it reads no manifest and no chain, and only the one named environment
variable.

```sh
node dist/index.js da-bond witness begin.unsigned.json \
  --key-env DA_OWNER_KEY \
  --out owner-a.witness.json
```

The key is a bech32 `ed25519_sk`/`ed25519e_sk` key, or a mnemonic (account 0's
payment key). `witness` refuses a key that is neither a required signer nor the
fee payer of the transaction. With `--out` it writes a new witness file (format
`midgard-da-bond-witness-v1`) and prints
`{witnessFile, action, amount?, outputs, txBodyHash, keyHash, roles}`, where
`roles` lists `required-signer` and/or `fee-payer`. `action` and `amount` (for
`CompleteWithdraw`) are decoded from the transaction's pool redeemer, and
`outputs` lists every output the transaction pays as `{address, lovelace}`.
Without `--out` it prints the witness document on stdout. Every owner in `--signers` and the fee payer each produce a
witness; one witness serves both roles when the fee payer is also a listed
owner.

`witness` and `assemble` both refuse a file whose `txBodyHash`,
`requiredSigners` or `action` differ from what its transaction carries. Check
what you are signing before handing the witness back, above all the `amount`
and the `outputs` of a CompleteWithdraw: `witness --out` prints them from the
transaction itself. Without `--out`, decode the file's `txCbor` with a
transaction viewer you trust.

**3. Assemble and submit.**

```sh
node dist/index.js da-bond assemble \
  --manifest /var/lib/midgard/contract-deployment-info.json \
  begin.unsigned.json owner-a.witness.json owner-b.witness.json fee-payer.witness.json
```

`assemble` checks, in order: the file's network and manifest id match the
deployment; each witness is for this transaction and carries a valid signature
from the key it claims; the pool is an input and the DA params UTxO a reference
input of the transaction; that DA params UTxO is still live and authentic; the
distinct owners that witnessed (and are required signers) reach
`update_threshold`; every required signer and the fee payer witnessed; the pool
input is still live; and the validity window has started and not ended. Only
then does it merge the witnesses into the transaction, submit it, wait for
confirmation, and print `{action, txHash, ownerWitnesses, updateThreshold, status}`.

Below the threshold it refuses with exactly this message and submits nothing:

```text
Refusing to submit: <n> distinct DA params owner witness(es), update_threshold is <t>; nothing was submitted
```

An owner's witness counts once however many times it is supplied, and a witness
from a key that is not an owner, or not a required signer of the transaction,
does not count.

### Timing and rebuilds

- The BeginWithdraw transaction is valid for at most
  `MAX_VALIDITY_RANGE_LENGTH_MS` (8 minutes on every current profile) from the
  moment it is built. Collect every witness and assemble within that window;
  after it, `assemble` refuses and the transaction must be rebuilt and
  witnessed again. CancelWithdraw has no validity bounds, and CompleteWithdraw
  only a lower bound.
- The transaction spends the pool UTxO and references the DA params UTxO. A
  top-up, a slash or another withdrawal step spends the pool UTxO, and a DA
  params rotation spends the DA params UTxO. Either one between build and
  assemble makes `assemble` refuse; rebuild and collect the witnesses again.
- The quorum is checked against the DA params the transaction references. After
  a rotation of owners, rebuild so the new owners' quorum applies.

## Refusals

Each refusal happens before anything is submitted.

| Command                       | Message (abridged)                                                                                                                                     | What to do                                                                                                                           |
| ----------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------ |
| `top-up`                      | `Refusing top-up: <a> lovelace is below da_bond_min_top_up_lovelace <m>; nothing was submitted`                                                        | Top up at least the minimum.                                                                                                         |
| `withdraw complete`           | `Refusing withdraw complete: the pool unlocks at unlock_at <u> (<iso>), after this transaction's lower bound <l> (<iso>); retry at or after unlock_at` | Wait until `unlock_at`. The lower bound is rounded down to a slot, so right at `unlock_at` retry one slot later.                     |
| `withdraw *`, `assemble`      | `Refusing to submit: <n> distinct DA params owner witness(es), update_threshold is <t>; nothing was submitted`                                         | At build time, `--signers` lists fewer than `update_threshold` owners; at assembly, fewer owners witnessed. Add owners or witnesses. |
| `withdraw *`                  | `signer <keyHash> is not a DA params owner (owners: ...)` / `signer <keyHash> is listed twice in --signers`                                            | Fix `--signers`: owners only, each once.                                                                                             |
| `withdraw begin`              | `the pool is already Withdrawing (unlock_at <u>); cancel or complete that withdrawal first`                                                            | Cancel or complete the pending withdrawal.                                                                                           |
| `withdraw cancel`, `complete` | `the pool is Bonded; begin a withdrawal first`                                                                                                         | Begin a withdrawal first.                                                                                                            |
| `withdraw complete`           | `amount <a> lovelace exceeds the pool backing <b> (lovelace above the <f> floor)`                                                                      | Withdraw at most the backing shown by `status`.                                                                                      |
| `witness`                     | `Refusing to witness: key <k> is neither a required signer (...) nor the fee payer <f> of this <action> transaction`                                   | Use a listed owner's key or the fee payer's key, or rebuild with this owner in `--signers`.                                          |
| `assemble`                    | `DA bond pool transaction lacks a witness for required signer(s) <keyHashes>`                                                                          | Collect the missing witness: a listed owner or the fee payer did not sign. Rebuild without an owner who will not sign.               |
| `assemble`                    | `witness <file> is for transaction <a>, not <b>` / `witness <file> claims key <k> but carries ...`                                                     | The witness was made for another build or is corrupt. Witness this file's transaction again.                                         |
| `assemble`                    | `the pool input <ref> is no longer live; rebuild the unsigned transaction`                                                                             | A top-up, slash or withdrawal step spent the pool. Rebuild and witness again.                                                        |
| `assemble`                    | `the DA params reference input <ref> is no longer live; rebuild the unsigned transaction`                                                              | The DA params rotated. Rebuild and witness again.                                                                                    |
| `assemble`                    | `the transaction's validity ended at <t> (<iso>); rebuild the unsigned transaction`                                                                    | The BeginWithdraw window passed. Rebuild and collect witnesses faster.                                                               |
| `assemble`                    | `the transaction is valid only from <t> (<iso>); retry then`                                                                                           | Retry at that time.                                                                                                                  |
| `assemble`                    | `the file is for network ...` / `the file is for deployment ..., not ...`                                                                              | Use the manifest of the deployment the file was built for.                                                                           |
| `witness`, `assemble`         | `action <a> is not the transaction's pool redeemer <r>`                                                                                                | The file was edited or corrupted. Rebuild it; do not witness it.                                                                     |

The first three rows are the pool validator's own refusals: below the minimum
top-up, before `unlock_at`, and a missing owner quorum. The CLI checks them
before building so that nothing reaches the chain.

## Alerts

The watcher and the DA committee node both read the pool and report when it
cannot back an attestation.

| Event on L1                                           | Watcher alert                      | Committee node                                                                    |
| ----------------------------------------------------- | ---------------------------------- | --------------------------------------------------------------------------------- |
| A slash or CompleteWithdraw leaves less than one bond | `da_bond_pool_under_backed` fires  | event `da_bond_pool_backing_short`; readiness reason `da_bond_pool_backing_short` |
| A top-up brings the backing back to at least one bond | `da_bond_pool_under_backed` clears | event `da_bond_pool_backing_restored`; the reason goes                            |
| BeginWithdraw lands                                   | `da_bond_pool_withdrawing` fires   | event `da_bond_pool_withdrawing`; readiness reason `da_bond_pool_withdrawing`     |
| CancelWithdraw or CompleteWithdraw lands              | `da_bond_pool_withdrawing` clears  | event `da_bond_pool_bonded`; the reason goes                                      |

**Watcher.** The watcher reads the pool on every availability reconcile, in a
read of its own bound to the same finalized point as its availability
snapshots, whether or not a header is pending. `GET /v1/status` serves the latest readout as `daBondPool`
(`state` `missing`, `bonded` or `withdrawing`, `lovelace`, `backing`,
`requiredBacking`, `belowBond`, `unlockAt` and `unlockable` while withdrawing,
`alerts: {underBacked, withdrawing}`, and `observedAtMs`), or `null` before the
first read. A missing pool counts as under-backed. The two alert codes appear in
`activeAlerts` with the deployment manifest id as subject, and count in
`/v1/metrics` `activeAlertCount`, but they are informational: they never add a
readiness reason and never make the watcher not ready, and the pool's state
never stops the watcher from challenging. A failed pool read (a transport error,
or a pool output that fails authentication) is reported only: it never changes
the availability phase or the watcher's readiness. `daBondPool` keeps the last
good readout, whose `observedAtMs` shows its age, and `GET /v1/status` serves
the latest failure as `daBondPoolReadFailure: {error, failedAtMs}` until the
next good read sets it back to `null`. A ready watcher can therefore hold a
stale pool readout; check `daBondPoolReadFailure` and `observedAtMs`.

**Committee node.** A committee node with `DA_L1_SUBMISSION_ENABLED=true` reads
the pool after every tick and before every Apply (`--once` does not read it).
Because it cannot attest while the pool is short or withdrawing, it reports
either as a readiness reason, and `/readyz` returns 503:

```text
da_bond_pool_backing_short: backing=<lovelace>, required=<da_bond_lovelace>, checkedAt=<iso>
da_bond_pool_withdrawing: unlockAt=<posix ms>, checkedAt=<iso>
```

Each transition is one JSON line on stderr, for example
`{"event":"da_bond_pool_backing_short","backing":"0","required":"500000000","checkedAt":"..."}`.
The fields are `backing`, `required`, `unlockAt` (while withdrawing) and
`checkedAt`, all strings. The first read reports only a short or withdrawing
pool, and identical reads report nothing. A failed read emits
`{"event":"da_bond_pool_read_failed","error":...,"failedAt":...}` once per run of
failures and keeps the last good read's reasons. Separately, each Apply the pool
cannot back is logged as `da_bond_pool_apply_backoff` with its reason
(`pool-under-backed`, `pool-withdrawing` or `pool-unavailable`) and retried on
the next reconcile.

To clear an under-backed alert, top up until `status` shows `belowBond: false`.
To clear a withdrawing alert, have the owners cancel the withdrawal, or complete
it and top up if the remainder is short.

See also the [availability challenge commands](availability-challenge-commands.md),
which open, answer and time out the challenges that slash the pool.
