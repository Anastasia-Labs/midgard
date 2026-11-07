# Public HTTP operator configuration

The compiled `preprod-public`, `preprod-rehearsal` and `mainnet` profiles require
authenticated release inputs before opening the API socket, and serve plain HTTP
only on a Unix socket behind a TLS ingress; the node itself does no TLS and has
no TCP listener. Explicit development/testing profiles retain their local HTTP
behavior. Public mode cannot disable configuration
loading or read secrets from a dotenv file.

`MIDGARD_CONFIG_FILE` must name an absolute, private regular YAML or JSON file. The operator,
merge, settlement and reference publisher seeds, admin key and database password
are mounted values in that configuration. Public wallet commands use
`--wallet-seed-phrase-file` (or `--seed-phrase-file` for address derivation), never a
literal seed argument. The node refuses a missing, symlinked, oversized or
nonprivate secret file. Do not put keys, seeds or passwords on the command line.

The public listener also requires these inputs:

- `HTTP_SOCKET_PATH`, an absolute socket path of at most 100 bytes whose
  directory grants no access to other users (mode 0700 or 0750). The node
  replaces a socket a killed node left there, refuses any other file, and
  makes its socket mode 0660.
- `MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE` containing the established signed
  release identity; `MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE` is a separately
  configured external Ed25519 trust-root list.
- `MIDGARD_PUBLIC_RELEASE_PROVENANCE_FILE` and
  `MIDGARD_PUBLIC_FINALIZED_MANIFEST_FILE` authenticated by that signature.
- `MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH` for the actual deployment contracts.
- `MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE` for retained deployment parameters
  bound by the signed provenance, and `MIDGARD_BLUEPRINT_CLASSIFICATION_FILE`
  matching its exact explicit unused-validator classifications.
- `MIDGARD_PUBLIC_MANIFEST_ID`, full forty-character
  `MIDGARD_PUBLIC_RELEASE_REVISION` and digest-pinned
  `MIDGARD_PUBLIC_NODE_IMAGE`, `MIDGARD_PUBLIC_POSTGRES_IMAGE`,
  `MIDGARD_PUBLIC_INGRESS_IMAGE` agreeing with the signed release.

Supplied revision/image strings identify expected release inputs. The release
process separately proves that the running images contain that frozen source;
configuration strings alone do not establish binary correspondence. Mounted inputs
have separate byte bounds: finalized manifest and contract deployment info 16 MiB,
parameter registry and signed authority 32 MiB, classification 128 KiB, external
trust roots 16 KiB, provenance 64 KiB. Private runtime configuration, secrets and
certificates retain 64 KiB bounds. The actual current public 120-day constructor's 525
contracts serialize to 10,057,128 B of manifest and 20,226,934 B of registry;
its 620 explicit unused-validator fixture classifications occupy 72,078 B.
These measurements calibrate framing; fixture publication outpoints and unused
reasons establish no live release approval. Acceptance separately checks genuine
signed authority/provenance, final source/images and supported adjacent refusals.

On its socket the node keys every request by `X-Midgard-Client-IP`, which the
TLS ingress replaces with the connecting address; a request without it is
refused (`E_HTTP_CLIENT_IDENTITY_UNAVAILABLE`). The socket has no network
address, so only processes that can enter its directory reach it.

Over TCP, `HTTP_TRUSTED_PROXY_PEERS` (empty by default) lists up to 64 IPv4 or
IPv6 addresses or CIDR ranges, at least /8 for IPv4 and /24 for IPv6 and with
no host bits set, such as a cloud load balancer's published ranges. Only a
peer in one of them may provide `X-Midgard-Client-IP`, and it must replace the
header with its own client address. The setting is refused together with
`HTTP_SOCKET_PATH`. Other forwarded headers have no role in quotas. Direct
requests use their actual TCP peer.

## Installed actions

Public reads are `GET /healthz`, `/readyz`, `/pipeline-status`, `/protocol-info`,
`/tx`, `/tx-status`, `/deposit-status`, `/txs`, `/utxo`, `/utxos`, `/block`.
Readonly batches are `POST /tx-status` and `POST /utxos`.
`POST /submit` and `/tx/evaluate` have their declared bounded request
envelopes. Other CLI operations have no HTTP dispatcher.

Peer routes are **POST** `/peer/txs` and `/peer/inventory`, for operators
sharing accepted L2 transactions (`L2_TX_PEERS`). Each takes
`Content-Type: application/cbor`, a canonical CBOR array of byte strings
(transactions, or 32-byte transaction ids), and two headers:
`X-Midgard-Operator-Key`, the 32-byte Ed25519 operator verification key as
lowercase hex, and `X-Midgard-Operator-Signature`, its signature over
SHA-256 of `"midgard/l2-peer/v1\0" || path || "\0" || hub-oracle policy id ||
body`. A missing or invalid signature returns 401 and a key outside the
node's on-chain active operator set returns 403. Transactions pass the
`/submit` checks and are admitted with source `peer`; `/peer/inventory`
answers the ids the node has no admission row for.

`/tx-status` answers `committed` once a block includes the transaction, with
that block's `headerHash`, and `committedMeaning` says how the node knows:
`immutable_db_inclusion_not_confirmed_ledger_merge` when the block is one of
this node's own, `landed_block_inclusion_not_confirmed_ledger_merge` when it
is a block another operator committed and this node saw land on L1. Neither
means the merge is final: `confirmedLedgerFinalized` turns `true` once the
block is merged into the confirmed ledger and that merge is confirmation-depth
deep. A rollback that takes the landed block off L1 clears that inclusion,
and the transaction answers from the row this node still holds for it:
`pending_commit` from `processed_mempool` (`awaiting_local_recovery` while a
local block finalization is pending), `accepted` from `mempool`. Once no
rollback can reach the block's merge, the node keeps a landed-block inclusion
for as long as it keeps its own committed transactions, so the answer stays
`committed`. `/pipeline-status` lists both meanings, as an
array, under `finalityVocabulary.txStatusCommittedMeaning`.

Protected reads are `GET /stateQueue`, `/operator/status` and `/admin/session`.
They require `X-Midgard-Admin-Key`; the session response is not cacheable.
Protected mutations are **POST** `/init`, `/commit`, `/merge`, `/logBlocksDB` and
`/logGlobals`. GET mutations are refused before execution. Each mutation has an
empty body and no query, and carries these headers:

- `X-Midgard-Admin-Session`: the current authenticated `/admin/session` value.
- `X-Midgard-Admin-Timestamp`: current Unix milliseconds as thirteen decimal digits.
- `X-Midgard-Admin-Nonce`: thirty-two random bytes encoded as lowercase hex.
- `X-Midgard-Admin-Signature`: lowercase HMAC-SHA256 under the mounted admin key
  over `JSON.stringify(["POST", exactTarget, session, timestamp, nonce])`.

The trimmed key signs exactly those bytes. A signature is admissible within thirty
seconds of its timestamp. A nonce is claimed before execution, including failed
execution; replay returns 409, invalid signatures return 401, and a full bounded
replay table returns 503. A process restart changes the session. Do not retry a
mutation automatically: first resolve its existing durable intent/state, then
create a fresh signed request if the operation is still required.

## Bounds and operator holds

JSON bodies are at most 64 KiB. A proof submission or evaluation body is one V1
envelope around at most one largest transaction: 295,048 B (the transaction bound
plus seven envelope bytes); a larger declared length gets 413 before the body is
read, and a `/submit` without `Content-Length` gets 411. A `/peer/txs` body holds at most one largest transaction
(about 288 KiB) and a `/peer/inventory` body at most 20,000 ids (about 664 KiB); peer
bodies are read under an ingress pool of their own, apart from `/submit`'s, and a
peer body of unknown length holds only the larger of those two bounds against the
shared ingress byte budget. Peers and users never hold each other's permits or
rate tokens, but their admissions share the durable admission backlog: a peer
burst that fills it makes `/submit` answer 503 until it drains, and the reverse.
The `peer` rate class has no global bucket, because it is charged before the
signature is checked. Targets
are at most 2 KiB and responses at most 1 MiB.
Pages use `limit` (default 25, maximum 100) and an exclusive lowercase hex `cursor`;
`nextCursor` is null at the end. Address-history and block cursors are transaction
IDs; UTXO cursors are the canonical thirty-eight-byte ledger references. State-queue
cursors bind both queue order and authenticated follower view; a changed view
returns 409, requiring a new first page.

SQL reads reserve 64 KiB of each response for JSON framing and out-reference keys,
leaving 480 KiB of stored payload before hex encoding. A page stops before a byte
barrier and resumes from the last returned key. An oversized first row or an
out-reference batch exceeding that payload budget returns
`E_HTTP_READ_DATA_LIMIT` with status 503; rows are never silently skipped. The
CLI splits only that explicit batch refusal, sequentially, under its existing
10,000-reference and sixty-second query limits. A single oversized row remains
an operator hold. Transaction status reads fetch presence/status metadata rather
than transaction or proof-material blobs. Public status and submission errors
preserve authoritative rejection codes and identifiers while suppressing arbitrary
upstream diagnostic text; explicit testing/private profiles retain that text.

Per-client, per-prefix and global rates, concurrent requests and weighted
bodies are bounded before parsing/evaluation. A client is an IPv4 address or an
IPv6 /56, and a submission is charged per transaction: 20 a second per client
with a burst of 100, and 200 a second per IPv4 /24 or IPv6 /48. Submissions
have no global rate; load shedding bounds the total. The public classes keep
their buckets in fixed hashed arrays, so memory does not grow with the number
of clients and no client is evicted or refused for lack of room. A rate refusal returns 429 with `E_HTTP_RATE_LIMITED`
and `Retry-After` in seconds until every refusing bucket has a token again; full
capacity returns 503 with `Retry-After: 5`. A 429, 503 or 409 keeps the
connection, so the client can retry on it; other refused POSTs (malformed,
oversized, unauthorized) close it. Health, readiness and admin routes
have their own small client table, concurrency budget and database connections, so
public traffic cannot refuse them. Classes, defaults and the
`HTTP_RATE_LIMIT_*` variables are in the docs-site server HTTP API page; public
profiles refuse `HTTP_RATE_LIMIT_ENABLED=false`.
Advisory evaluation never enters the production worker queue, and pools with fewer
than two workers refuse it. HTTP reads use a separate four-connection database
pool with fail-fast request quotas, three permits for public reads and one for
readiness and admin reads, preserving production SQL pools. Pending
withdrawals whose derived canonical references are unknown hold UTXO reads with
`E_PUBLIC_READ_MIGRATING`; a bounded follower-owned backfill preserves the original
rows. An oversized stored reference holds ingestion for operator inspection;
archived corruption has no eventual-completion guarantee.

## Full-stack composition

The `e2e-stack` command deploys and runs a preprod-public node when its `.env`
names `MIDGARD_DEPLOYMENT_PROFILE=preprod-public` and its stack configuration has
a `public` section. The operator steps are in the docs-site page
`operators/node/preprod-public-deploy`. The stack:

- runs the release's digest-pinned node, migration and Postgres images, as the
  host user that owns the private files;
- writes the node's secrets and DA identity to a private generated
  `MIDGARD_CONFIG_FILE` in its run directory, keeps them out of the container
  environment, and mounts the trust-root and release files;
- gives the node the Cardano socket at `/ipc/node.socket`, its configuration
  at `/cardano-config/config.json` and the image's own L1 transport binary;
- authenticates the release before it starts the node, and stops with the
  files to sign when it does not verify yet.

The stack always runs the TLS ingress, configured by
`scripts/public-ingress.mjs`: a digest-pinned nginx that holds the TLS
certificate and key and forwards plain HTTP, over keepalive connections, to the
node's socket. The socket lives in a run-directory folder of mode 0700, owned
by the host user both containers run as, and mounted only into those two. The
ingress has a Docker network of its own and no route to the node over TCP; the
node publishes no API port. The ingress caps `/submit` and `/tx/evaluate`
bodies at the node's submission limit (295,048 bytes), caps each address at 4
requests in flight, and rate limits only the health, readiness and admin
routes; the node's limiter decides submit and read routes. Its own refusals are
JSON with `Retry-After`, using the node's error codes. `public.ingressHostPort`
adds an extra host port for it. No Docker socket is mounted.

A started stack does not prove load headroom, block production under load or
live Preprod acceptance; those gates run on the final composed release.

The separate explicit framing gate uses retained genuine constructor measurements:

```sh
MIDGARD_PUBLIC_FRAMING_MEASUREMENT_FILE=/absolute/measurement.json \
  node ../../scripts/contrib.mjs test --package midgard-node \
  --config vitest.public-http-framing.config.mjs \
  --file tests/public-http-release-framing.acceptance.test.ts
```

It refuses absent inputs and checks their declared byte hashes, actual large files,
exact per-artifact envelope and adjacent refusal. Ordinary CI does not collect
this manual gate. It proves mounted framing of the real public constructor roster;
signature, SDK reconstruction, running public image and live gates remain separate.
