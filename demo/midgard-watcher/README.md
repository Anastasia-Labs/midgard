# Midgard Watcher

`midgard-watcher` is the independent Midgard verifier and challenger package.
It is distinct from the DA committee service in `../da-committee-node`.

W00 provides the workspace, executable, and verification scaffold. W01 adds a
strict configuration language, but does not make the service production-ready.
Production readiness remains false until deployment identity, durable state,
public indexing, deterministic verification, proof actuation, and local/live
acceptance work in W02–W46, WG1, and WG2 is complete.

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
        "multiaddr": "/dns4/da-a.example/tcp/443/tls/ws/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345"
      }
    ],
    "requestTimeoutMs": 10000,
    "maxConcurrency": 8
  },
  "storage": {
    "driver": "sqlite",
    "path": "/var/lib/midgard-watcher/watcher.sqlite"
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

The L1 source is an exact disjoint union:

- `local_node` uses one watcher-operated Cardano full node and its chain-sync
  stream as the consensus authority. Its `chainSync` record binds an absolute
  node socket and genesis identity. Zero to eight Ogmios, Kupo, Kupmios, or
  db-sync query surfaces may share that node; they do not count as independent
  providers and must remain aligned with its network and canonical chain point.
- `external_providers` requires at least two independently operated providers
  with distinct identities, operator identity hashes, and canonical public
  HTTPS endpoints. Their same-network, compatible-chain-point evidence must
  agree before a protocol decision is authorized.

DA peers must be public DNS libp2p multiaddresses. The SQLite path must be
absolute and durable. Prover keys are referenced only through a named
environment variable or an absolute file; an inline key, seed, token, or
password field is always rejected.

Finality is explicit: pre-finality rollback rewinds pending work, while a
post-finality rollback quarantines the watcher. Request timeouts, concurrency,
finality/rollback depth, and DA/proof deadlines use the exported
`WATCHER_CONFIG_BOUNDS`; deadline values must also cover their corresponding
request timeout.

No operational command consumes the configuration at W01. `start` and `replay`
therefore continue to return the W00 `foundation_incomplete` result with exit
code 78.

## Commands

- `pnpm run build` builds the library and CLI.
- `pnpm run typecheck` checks the TypeScript sources.
- `pnpm run lint` checks the package with the workspace lint policy.
- `pnpm test` runs the focused scaffold and strict-configuration tests.
- `pnpm run start` invokes the service entry point.
- `pnpm run replay` invokes the offline replay entry point.

At W01, `start` and `replay` intentionally exit nonzero with a structured
`foundation_incomplete` result. They must not become ready through a default,
demo, or compatibility path. Later work packages replace these refusal paths
only after their required trust boundaries and acceptance evidence exist.
