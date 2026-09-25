# Committee availability responder

When `DA_L1_SUBMISSION_ENABLED=true`, the committee process also runs the
availability responder after each authenticated watcher scan. It discovers live
challenged bonds, verifies retained envelope bytes against the frozen signed
commitment, publishes the next ordered chunk, settles published tranches and
closes fully answered challenges through the shared SDK builders and durable
operation executor.

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
script hashes, all five availability withdrawal registrations and sufficient
plain ADA collateral in the responder wallet. Publication, settlement and close
fees come from the challenger's protected on-chain fee shares. The responder
does not automatically fund itself from the operator wallet.

Withdrawal registrations, for both the responder and the attestation submitter,
are read from the local node ledger rather than from Ogmios, which omits
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
chunk to proceed while the journal keeps reservations until finality. Restart
reconciles the exact persisted signed transaction before constructing new work;
uncertain inclusion or a changed canonical boundary pauses actuation.

Publication is permissionless. The live challenge retains the original signer
bitmap, committee hash and bond owner. A later committee configuration does not
replace those values or redirect refunds. The responder uses retained bytes
even when public retrieval is unavailable. Missing or corrupt retained data is
reported without submitting a response; timeout remains available through the
independent challenger workflow.

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
