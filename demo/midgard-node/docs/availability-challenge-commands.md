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
  --wallet-seed-env AVAILABILITY_ACTOR_SEED \
  --kupo-url http://127.0.0.1:1442 \
  --ogmios-url http://127.0.0.1:1337
```

`HEADER_HASH` is the 28-byte header hash in lowercase hexadecimal. Load the
dedicated actor mnemonic into the named environment variable through your secret
manager. Fund its enterprise address; the CLI selects an enterprise wallet to
match the challenge's payment-key funding and refund addresses. The actor payment credential must differ from the configured operator,
merge and reference deployment wallets, and from the manifest's bond owner.
Kupo and Ogmios URLs may instead come from `L1_KUPO_KEY` and `L1_OGMIOS_KEY`.

The journal must have a canonical absolute path on durable storage. Processes
using the same actor wallet must share that journal. Keep it across restarts:
the executor records exact signed transactions before submission, reconciles
ambiguous submission, and retains input reservations until finality or proven
expiration. `recover` reconciles that actor's journal for the deployment before
new work. It can report operations for other headers handled by the same actor.
Opening a challenge reserves the actor wallet for that header and deployment
until its terminal transaction reaches finality. Use a separate actor wallet
for an independent concurrent challenge.

Add the following flags to the common arguments for each mutation:

| Action    | Additional arguments and behavior                                                                                                                                                                                                           |
| --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `open`    | `--collateral-out-ref <txHash#index>` and `--funding-out-ref <txHash#index>`. Funding must cover exactly the configured challenge bond and opening fee.                                                                                     |
| `respond` | `--collateral-out-ref <txHash#index>` and `--payload-file <path>`. Reads the exact retained envelope bytes, verifies their frozen commitment, and publishes one next chunk. Optional `--tranche-index <0..15>` selects an active tranche.   |
| `settle`  | `--collateral-out-ref <txHash#index>`. Settles the next ordered completed tranche, or an unfinished tranche after the strict response deadline.                                                                                             |
| `close`   | `--collateral-out-ref <txHash#index>`. Closes a fully answered challenge and preserves the frozen refund beneficiaries.                                                                                                                     |
| `timeout` | `--collateral-out-ref <txHash#index>`, plus `--funding-out-ref <txHash#index>` when removal needs actor fee funding. Advances one required step: expired tranche settlement, unavailable timeout, descendant pruning or final head removal. |

Collateral is explicit plain ADA owned by the dedicated actor; it must not
overlap spending inputs. Output references use a lowercase 32-byte transaction
hash and canonical decimal output index. Live reference role NFTs, script hashes,
withdrawal registrations, transaction budgets and protocol deadlines are checked
before submission.
Opening and removal also require enough unreserved plain ADA in the actor wallet
to pay the current descendant removal path and leave minimum change. The opening
bond and fee are additional to that reserve. The check excludes collateral and
reservations from every deployment in the shared journal; a longer queue can
require additional funds before timeout proceeds.

Each invocation advances at most one transaction. Repeated `respond`, `settle`
or `timeout` invocations first reconcile prior intent and then continue from
authenticated on-chain progress. An included transaction can permit the next
step while its reservations remain until finality. Pending or uncertain work is
reported without constructing a replacement transaction.

`status` reports the canonical point, retained bond state, response deadline,
ordered tranche progress, queue availability and unfinalized operation metadata.
It does not submit transactions or disclose stored signed CBOR. Canonical-source
disagreement or rollback interrupts mutation and requires recovery against the
current authenticated chain.
