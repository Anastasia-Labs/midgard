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

Actuation requires the committee's L1 chain follower on the operator's own
cardano-node. The responder reads its canonical boundary (the follower's view),
transaction status, inputs and every rival spend from the follower's facts, and
submits through the same node; no chain index (Kupo, Ogmios) is configured. The
responder is built on the first drain the follower is ready for: until then
each drain reports `awaiting_scan` with the follower's reasons, and a failed
construction reports `failed` and is retried on the next drain, with the
process up. Construction verifies deployed reference role NFTs, script hashes,
all four availability withdrawal registrations and sufficient plain ADA
collateral in the responder wallet. Publication, settlement and close fees come
from the challenger's protected on-chain fee shares. The responder does not
automatically fund itself from the operator wallet.

The follower and the withdrawal-registration query use the local node through
these three absolute paths, set all together, and `L1_ORIGIN`, the
deployment's origin point (`<slot>.<block hash>`, as
`midgard-l1-follower find-origin` prints it):

```sh
CARDANO_LOCAL_NODE_SOCKET_PATH=/run/cardano/node.socket
CARDANO_LOCAL_NODE_CONFIG_PATH=/etc/cardano/config.json
CARDANO_L1_NODE_TRANSPORT_BINARY_PATH=/opt/midgard/midgard-l1-node-transport
```

The binary is the node transport sidecar; build it with
`pnpm --dir demo/l1-node-transport run native:build`. The
node configuration's Shelley genesis must match the configured network. The
query uses `CARDANO_LOCAL_NODE_AUTHORITY_ID` when set, otherwise
`local-cardano-node`. Without these settings the committee stays unready with
`l1_follower_unconfigured`, and registration checks fail closed.

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
transaction's raw bytes, which the follower stores with each block; the
responder then selects its next step from live state. A rollback of the view a
pass reconciled at, or a view that moves during a spend read, reports
`awaiting_scan` and releases nothing; the next pass reads again at one view.

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

The committee's L1 follower supplies the retention evidence: a header's exit
from the state queue (merged or removed) becomes its terminal record only once
that exit is final, deeper than k blocks on the follower's chain. Published
outputs prove a closed challenge; final unavailable removal binds the original
challenge through the correction lock. The terminal record is persisted with the
header and carries the exit's block, including the authenticated block end time.

Retention runs only against the follower's view read in the same tick, and the
store atomically checks the terminal evidence, finality and deadline. While the
follower holds the committee unready (catching up, a rollback it is still
absorbing, or `rollback_beyond_k`), no view is read and nothing is deleted; the
process stays up and `/readyz` names the reason. Missing evidence, generic
terminal status, and absent challenge UTxOs retain the payload.
