# Availability challenge commands

The `availability-challenge` command group exposes `open`, `respond`, `settle`,
`close`, `timeout`, `status` and `recover`. All actions use the verified finalized
deployment manifest and local canonical Kupmios source. Mutation commands use
the same SDK builders and durable operation executor as the watcher and committee
responder.

Build from `demo/midgard-node` using `pnpm run build`. Inspect the group with
`node dist/index.js availability-challenge --help` and each action with
`node dist/index.js availability-challenge open --help`.

Every action requires these flags:

```sh
node dist/index.js availability-challenge status \
  --manifest /var/lib/midgard/contract-deployment-info.json \
  --journal /var/lib/midgard/availability/actor.sqlite \
  --header-hash "$HEADER_HASH" \
  --wallet-seed-env AVAILABILITY_ACTOR_SEED
```

`HEADER_HASH` is the 28-byte header hash in lowercase hexadecimal. Load the
dedicated actor mnemonic into the named environment variable through your secret
manager. Fund its enterprise address; the CLI selects an enterprise wallet to
match the challenge's payment-key funding and refund addresses. The actor
payment credential must differ from the configured operator, merge and
reference deployment wallets.
The commands are tools, not a role: they never read the node's follower store.
Choose the L1 access with `--l1` (or `L1_ACCESS`):

- `--l1 kupmios` (`L1_KUPO_URL`, `L1_OGMIOS_URL`) runs every action. Kupo and
  Ogmios supply the chain history the commands need: the inclusion of each
  operation, foreign spends and retained outputs.
- `--l1 node`, the default when `L1_NODE_SOCKET_PATH` is set, runs `status`
  only. The local ledger holds no transaction history, so every other action
  is refused before anything is built or submitted, naming `--l1 kupmios`.
- `--l1 blockfrost` is refused: no history reader is built for it.

The journal must have a canonical absolute path on durable storage. Processes
using the same actor wallet must share that journal. Keep it across restarts:
the executor records exact signed transactions before submission, reconciles
ambiguous submission, and retains input reservations until finality or proven
expiration. `recover` reconciles that actor's journal for the deployment before
new work. It can report operations for other headers handled by the same actor.
An intent whose inputs were all spent by another transaction, such as a rival
Timeout of the same header, expires only once that spend is verified from the
spending transaction's own bytes, read from chain history. After Apply the full
commitment survives only in the spent DA attestation output; opening reads it
from that output through chain history. Opening a
challenge starts that header's workflow, which lasts until its terminal
transaction reaches finality. The same actor wallet may open challenges on other
headers of the same deployment meanwhile; the journal refuses operations for a
different deployment while any workflow is live.

Add the following flags to the common arguments for each mutation:

| Action    | Additional arguments and behavior                                                                                                                                                                                                                                                                               |
| --------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `open`    | `--collateral-out-ref <txHash#index>` and `--funding-out-ref <txHash#index>`. The funding coin must hold exactly the configured challenger bond, challenge record lovelace and maximum opening fee. The command recovers the attested commitment from chain history and checks its hash against the queue node. |
| `respond` | `--collateral-out-ref <txHash#index>` and `--payload-file <path>`. Reads the exact retained envelope bytes, verifies their frozen commitment, and publishes one next chunk. Optional `--tranche-index <0..15>` selects an active tranche.                                                                       |
| `settle`  | `--collateral-out-ref <txHash#index>`. Settles the next ordered completed tranche, or an unfinished tranche after the strict response deadline.                                                                                                                                                                 |
| `close`   | `--collateral-out-ref <txHash#index>`. Closes a fully answered challenge and preserves the frozen refund beneficiaries.                                                                                                                                                                                         |
| `timeout` | `--collateral-out-ref <txHash#index>`, plus `--funding-out-ref <txHash#index>` when descendant pruning or head removal needs actor fee funding. Advances one required step: expired tranche settlement, unavailable timeout, descendant pruning or final head removal.                                          |

Collateral is explicit plain ADA owned by the dedicated actor; it must not
overlap spending inputs. The unavailable timeout slashes the DA bond pool: it
takes one DA bond, or the whole backing when less is left, pays the penalty
share as fee and the rest to the challenger. Its exact fee is that penalty
share plus the challenger's own fee contribution, which is at most
`maxTimeoutFeeLovelace`. The pool and the challenger reserve pay it; the actor
wallet adds no input and receives no change. Its collateral must hold at least
the ledger collateral percentage (150% on Cardano) of `daSlashPenaltyLovelace +
maxTimeoutFeeLovelace`. The queue node's rent goes to the actor's base address,
because the builder refuses to merge it into the protected challenger refund.
Output references use a lowercase 32-byte transaction hash and canonical decimal
output index. Live reference role NFTs, script hashes,
withdrawal registrations, transaction budgets and protocol deadlines are checked
before submission.
Opening and removal also require enough unreserved plain ADA in the actor wallet
to pay the current descendant removal path and leave minimum change. The
challenger bond, challenge record lovelace and opening fee are additional to that
reserve. The check excludes collateral and reservations from every deployment in
the shared journal; a longer queue can require additional funds before timeout
proceeds.

Each invocation advances at most one transaction. Repeated `respond`, `settle`
or `timeout` invocations first reconcile prior intent and then continue from
authenticated on-chain progress. An included transaction can permit the next
step while its reservations remain until finality. Pending or uncertain work is
reported without constructing a replacement transaction.

`status` reports the canonical point, the challenge record, response deadline,
DA bond pool state and backing, ordered tranche progress, queue availability and
unfinalized operation metadata. To read, top up or withdraw from the pool
itself, use the [DA bond pool commands](da-bond-commands.md).
It does not submit transactions or disclose stored signed CBOR. Canonical-source
disagreement or rollback interrupts mutation and requires recovery against the
current authenticated chain.

For an offline inventory across every actor, deployment and stored state, use
[availability journal holds](availability-journal-commands.md). Its stored-row
inventory carries no canonical clearing authority.
