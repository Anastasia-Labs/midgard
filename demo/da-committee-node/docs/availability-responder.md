# Committee availability responder

When `DA_L1_SUBMISSION_ENABLED=true`, the committee process also runs the
availability responder after each authenticated watcher scan. It discovers live
availability challenge records by their `DACH` tokens, authenticates each
against its `Challenged` state-queue node, verifies retained envelope bytes
against the frozen signed commitment, publishes the next ordered chunk, settles
published tranches and closes fully answered challenges through the shared SDK
builders and durable operation executor.

Each record is judged on its own. A record whose state-queue node is gone or is
not `Challenged` by it is stranded: a Timeout or fraud removal pruned its block
while it was challenged, and nothing can spend it again. The responder skips it
and writes one `availability_responder_skipped_record` line to stderr. A record
that fails authentication is skipped and reported the same way. Neither stops
discovery of the other challenges.

Set both responder variables explicitly:

```sh
DA_AVAILABILITY_JOURNAL_PATH=/var/lib/midgard/committee/availability.sqlite
DA_AVAILABILITY_SUBMITTER_KEY_SOURCE=file:/run/secrets/availability-responder.key
```

The journal path must be an absolute path on durable storage. Every process
using the same responder wallet must share this journal. The responder payment
credential must differ from `L1_SUBMITTER_KEY_SOURCE`, including when two key
source strings resolve to the same key. The attestation submitter does not share
the responder's resource reservations.

Actuation requires the configured `local_node` chain-sync authority and an
aligned Kupmios query provider. Startup verifies deployed reference role NFTs,
script hashes, all four availability withdrawal registrations and sufficient
plain ADA collateral in the responder wallet. Publication, settlement and close
fees come from the challenger's protected on-chain fee shares. The responder
does not automatically fund itself from the operator wallet.

The responder's withdrawal registrations are read from the local node ledger rather than from Ogmios, which omits
registered reward accounts that have no stake-pool delegation. With a `kupmios:`
provider, set all three of these absolute paths, or none:

```sh
CARDANO_LOCAL_NODE_SOCKET_PATH=/run/cardano/node.socket
CARDANO_LOCAL_NODE_CONFIG_PATH=/etc/cardano/config.json
CARDANO_NATIVE_CHAIN_SYNC_BINARY_PATH=/opt/midgard/midgard-chain-sync
```

Build the helper with `pnpm --dir demo/midgard-watcher run native:build`. The
node configuration's Shelley genesis must match the configured network. The
query uses `CARDANO_LOCAL_NODE_AUTHORITY_ID` when set, otherwise
`local-cardano-node`. Without these settings, registration checks fail closed.

`--once` executes one responder cycle. The normal service continues on each
poll. The `availability_responder` event reports pending, included, confirmed,
unavailable or failed work. Canonically included transactions allow the next
chunk to proceed while the journal keeps reservations until the transaction can
no longer land (past validity or a provable conflicting spend). Restart
reconciles the exact persisted signed transaction before constructing new work;
uncertain inclusion or a changed canonical boundary pauses actuation. Evidence
about a confirmed transaction that neither confirms nor contradicts it, or a
conflicting transaction, holds that intent: the event reports `held` with the
transaction and reason on stderr, nothing new is signed, and readiness fails
with `availability_operation_held:<tx>: <reason>` until a reconciliation finds
no hold.

Publication, settlement and close spend only protocol outputs, so another member,
a watcher or anyone copying the transaction can land the same step first. Once
past its validity, the responder's own intent expires when a valid transaction
that lists one of its inputs has spent it at finality depth, verified from that
transaction's raw bytes; the responder then selects its next step from live
state. Ogmios must therefore run with `--include-transaction-cbor`. Without it
the responder stops with an error naming that flag and releases nothing.

Publication is permissionless. The live challenge record retains the attested
commitment the committee signed. A later committee configuration does not
replace it or redirect refunds; a challenge that times out slashes the DA bond
pool, not any individual member. The responder uses retained bytes
even when public retrieval is unavailable. Missing or corrupt retained data is
reported without submitting a response; timeout remains available through the
independent challenger workflow.

Answering every challenge before its response deadline is what keeps the pool
whole: an unanswered challenge on the queue head times out and takes one DA
bond from the pool. The Timeout also prunes the head's descendants without a
further slash, so liability is one bond per withholding episode. A challenge
also stalls the whole chain while it lasts: the state queue refuses every
Append while its head is `Challenged`, until the challenge is closed, which
sets the head to `Published`, or times out.

The native queue scanner collects retention evidence from finalized ordered
transitions and their exact consumed queue outputs. Published outputs prove a
closed challenge; final unavailable removal binds the original challenge through
the correction lock. Evidence is persisted with each terminal header and
revalidated after restart, including the authenticated block end time.

Retention synchronizes the local chain authority before deletion and holds its
cursor lock while the store atomically checks the terminal evidence, healthy
source, finality and deadline. An unconsumed rollback blocks deletion; source
quarantine revokes stored evidence. Missing evidence, generic terminal status,
and absent challenge UTxOs retain the payload.
