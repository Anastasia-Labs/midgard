# Midgard Watcher

`midgard-watcher` is the independent Midgard verifier and challenger package.
It is distinct from the DA committee service in `../da-committee-node`.

Thread model: watchers are independent and self-selecting. A watcher that
identifies a fault opens its own fault-proof thread and drives it to
completion; it has no obligation to threads opened by other parties, and
soundness per fault requires only one honest active challenger. See
`docs/fault-proofs/architecture.md` §5.

W00 provides the workspace, executable, and verification scaffold. W01 adds a
strict configuration language. This checkpoint also implements the strict
W10-W17 and W23 observation, source-consistency, finality, rollback, protocol
indexing, and authority-digest state machines as library surfaces. It does not
make the service production-ready or enable operational `start`/`replay`.
Production readiness remains false until deployment identity, durable state,
public indexing, deterministic verification, proof actuation, and local/live
acceptance work in W02-W46, WG1, and WG2 is complete.

## Configuration

`parseWatcherConfig` and `parseWatcherConfigJson` accept only
`midgard-watcher-config-v1`. The schema is exact: unknown and duplicate fields
are rejected, numeric controls are positive and bounded, and diagnostics never
include rejected values.

```json
{
  "schemaVersion": "midgard-watcher-config-v1",
  "mode": "acceptance",
  "targetNetwork": "Preprod",
  "l1": {
    "source": {
      "sourceMode": "external_providers",
      "providers": [
        {
          "identity": "provider-a",
          "operatorIdentitySha256": "1111111111111111111111111111111111111111111111111111111111111111",
          "endpoint": "https://cardano-a.example"
        },
        {
          "identity": "provider-b",
          "operatorIdentitySha256": "2222222222222222222222222222222222222222222222222222222222222222",
          "endpoint": "https://cardano-b.example"
        }
      ]
    },
    "requestTimeoutMs": 10000,
    "maxConcurrency": 8,
    "finality": {
      "depth": 15,
      "rollback": {
        "beforeFinality": "rewind",
        "afterFinality": "quarantine",
        "maxDepth": 15
      }
    }
  },
  "da": {
    "peers": [
      {
        "identity": "da-peer-a",
        "multiaddr": "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345"
      }
    ],
    "requestTimeoutMs": 10000,
    "maxConcurrency": 8
  },
  "storage": {
    "driver": "sqlite",
    "path": "/var/lib/midgard-watcher/watcher.sqlite",
    "rollbackAuthorityKeySource": {
      "kind": "environment",
      "variable": "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY"
    }
  },
  "proverWallet": {
    "keySource": {
      "kind": "environment",
      "variable": "MIDGARD_WATCHER_PROVER_KEY"
    }
  },
  "deadlines": {
    "daFetchMs": 60000,
    "daPublishMs": 60000,
    "proofConstructMs": 300000,
    "proofSubmitMs": 120000
  }
}
```

The L1 source vocabulary is an exact disjoint union, but only the external
provider branch is currently selectable on the wire:

- `local_node` is retained as pure state vocabulary for the deferred,
  peer-authenticated native adapter. The current parser rejects it before
  reading any socket-path or query fields; no pathname-based authority can be
  instantiated. A future adapter must bind peer identity to the connected
  socket, not to a pathname.
- `external_providers` requires at least two independently operated providers
  with distinct identities, operator identity hashes, and canonical public
  HTTPS endpoints. Their same-network, compatible-chain-point evidence must
  agree before a protocol decision is authorized.

This is a prelaunch API retirement: the former local pathname-authority
constructor, type, and exports have no compatibility alias. `start` and
`replay` remain transport-free scaffolds and exit with code `78`; neither can
turn a rejected local configuration into a connection attempt.

DA peers must be public direct-TCP libp2p multiaddresses of the form
`/dns4/<host>/tcp/<port>/p2p/<PeerID>` (or `dns6`). HTTP(S), WebSocket,
relay, and TLS multiaddr layers are rejected. Before each request the watcher
checks that the embedded `PeerID` is the configured peer; after dialing, it
checks that the Noise-authenticated remote identity is the same peer. It emits
accepted public-DA evidence as `public_or_permissionless_da`, never as
committee-attested evidence.

The SQLite path must be absolute and durable. The rollback-authority key and
prover key are separate required secret references, each through a named
environment variable or an absolute file. The rollback-authority source must
resolve to the same 32-byte key across restarts and restores; missing, changed,
reused, random, or ephemeral key material fails closed because W13 recovery
snapshots are authenticated with it. Inline keys, seeds, tokens, and password
fields are always rejected.

Finality is explicit: pre-finality rollback rewinds pending work. A mode-valid,
agreed canonical contradiction after finality opens a durable incident, and
W13 automatically rewinds and resumes replay when exact node-derived W10 bytes
and W11 agreement prove both branches to a common ancestor within Cardano's
fixed `k = 2160` recovery bound. Transient pending, unavailable, quarantined,
or same-point content-mismatched source evidence holds only the current
protocol decision and preserves the finalized binding without opening an
incident. Request timeouts,
concurrency, pre-finality rollback depth, and DA/proof deadlines use the
exported `WATCHER_CONFIG_BOUNDS`; deadline values must also cover their
corresponding request timeout.

No operational command consumes the new library surfaces in this checkpoint.
`start` and `replay` therefore continue to return the W00
`foundation_incomplete` result with exit code 78.
Before either command can be enabled, the SQLite implementation must persist
the authenticated W03/W13 recovery bundle and its compare-and-swap revision
in one transaction. It must then publish the emitted HMAC-bound trusted head
with an expected-prior compare-and-swap to an independently protected,
monotonic, non-rollbackable authority before the associated recovery result is
acted upon. A row in the same rollbackable database is not a freshness
authority. If the process crashes after the database commit but before that
publication, the reconciliation API permits only the authenticated epoch-zero
head or one exact direct successor, returns no protocol decision, and requires
external CAS plus read-back before load. Startup rejects any older, skipped,
divergent, tampered, or deployment-mismatched head/snapshot pair.

The live W10 transport capability proves the configured provider endpoint and
TLS peer identity. Its current normalization call is an in-process boundary:
the future watcher-owned Cardano/provider wire adapter
must pass only bytes it decoded from that exact live connection. This
checkpoint does not include that wire adapter and therefore does not claim
that an arbitrary caller-supplied observation was read from the socket. W10
and W14 acceptance requires the operational adapter to close that boundary;
the disabled `start` and `replay` commands prevent this library checkpoint
from being used as a production provenance path.

## Commands

- `pnpm run build` builds the library and CLI.
- `pnpm run typecheck` checks the TypeScript sources.
- `pnpm run lint` checks the package with the workspace lint policy.
- `pnpm test` runs the focused scaffold and strict-configuration tests.
- `pnpm run start` invokes the service entry point.
- `pnpm run replay` invokes the offline replay entry point.

At this checkpoint, `start` and `replay` intentionally exit nonzero with a
structured `foundation_incomplete` result. They must not become ready through
a default, demo, or compatibility path. Later work packages replace these
refusal paths only after their required trust boundaries and acceptance
evidence exist.
