# Public HTTP operator configuration

The compiled `preprod-public`, `preprod-rehearsal` and `mainnet` profiles require
authenticated release inputs and TLS before opening the API socket. Explicit development/testing
profiles retain their local HTTP behavior. Public mode cannot disable configuration
loading or read secrets from a dotenv file.

`MIDGARD_CONFIG_FILE` must name an absolute, private regular JSON file. The operator,
merge, settlement and reference publisher seeds, admin key and database password
are mounted values in that configuration. Public wallet commands use
`--wallet-seed-phrase-file` (or `--seed-phrase-file` for address derivation), never a
literal seed argument. The node refuses a missing, symlinked, oversized or
nonprivate secret file. Do not put keys, seeds or passwords on the command line.

The public listener also requires these mounted inputs:

- `MIDGARD_PUBLIC_TLS_CERT_FILE`, `MIDGARD_PUBLIC_TLS_KEY_FILE` and
  `MIDGARD_PUBLIC_API_ORIGIN` (HTTPS, matching certificate hostname).
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

`HTTP_TRUSTED_PROXY_PEERS` is empty by default. Only explicitly configured private
IPv4 proxy socket peers may provide `X-Midgard-Client-IP`, and the deployed proxy
must replace it with its TCP client address. Other forwarded headers have no role
in quotas. Direct requests use their actual TCP peer.

## Installed actions

Public reads are `GET /healthz`, `/readyz`, `/pipeline-status`, `/protocol-info`,
`/tx`, `/tx-status`, `/deposit-status`, `/txs`, `/utxo`, `/utxos`, `/block`.
Readonly batches are `POST /tx-status` and `POST /utxos`.
`POST /deposit/build`, `/submit` and `/tx/evaluate` have their declared bounded
request envelopes. Other CLI operations have no HTTP dispatcher.

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

JSON bodies are at most 64 KiB; proof submission/evaluation bodies use the compiled
64 MiB protocol envelope. Targets are at most 2 KiB and responses at most 1 MiB.
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

Per-peer and global rates, concurrent requests and weighted bodies are bounded
before parsing/evaluation. A rate refusal returns 429 with `E_HTTP_RATE_LIMITED`
and `Retry-After` in seconds until every refusing bucket has a token again; full
capacity returns 503 with `Retry-After: 5`. Health, readiness and admin routes
have their own client table, concurrency budget and database connections, so
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

## Existing full-stack composition

`scripts/public-operator-compose.mjs compose ABSOLUTE_OUTPUT_PATH` renders a
**public HTTP overlay**, and `ingress ABSOLUTE_OUTPUT_PATH` renders its nginx
configuration. Both create a new private output file and refuse mutable role
images/configuration mismatches. The overlay has no complete chain or DA stack.
The existing full-stack harness must supply its authenticated
`producer-container-committee-host` producer manifest at `/app/stack/producer.json`,
private authenticated DA transport and reachable existing host-network committee
peers. It must also supply the actual Cardano socket at `/ipc/node.socket` with
verified UID/GID access, read-only Cardano configuration at
`/cardano-config/config.json`, and pinned native L1 transport from the public image
or the existing explicit mount. No Docker socket belongs in this overlay.

Only ingress TLS is published. Rendering ports, resource limits or release strings
does not prove full-stack readiness, load headroom, block production under load,
or live Preprod acceptance; those gates run on the final composed release.

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
