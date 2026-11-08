# Public submit ingress: redesign for high throughput

**Status:** Ready for implementation. Owner decisions ruled 2026-11-06 (see the end). Nothing here is implemented yet.

**Last reviewed:** 2026-11-06, against tree `dcbecd108`.

**Related:**
- [`docs/research/l2-tx-ingress-throughput.md`](../research/l2-tx-ingress-throughput.md):
  how Base, OP Mainnet and Arbitrum take transactions, with primary-source
  citations.
- [`docs/exec-plans/l2-throughput-beyond-ingress.md`](l2-throughput-beyond-ingress.md):
  the limits behind durable admission (block production) and the read
  surfaces. Block production caps the node at about 140 TPS today, whatever
  ingress does.

## Scope and goal

This plan covers the path from a client's `POST /submit` to the 202 and the
tx's durable admission row behind it. Validation, block production and reads
are out of scope.

**Goal:** ingress must not be the bottleneck at the network's intended
1,000 TPS of real-world traffic. That traffic comes from many clients, some
behind shared addresses, with mixed tx sizes and some hostile load.

**Design target:** at least 5,000 TPS of admissions per node, so that 1,000
TPS uses at most 20 % of capacity. The 5× headroom allows for:

- bursts (L1 block gaps make inclusion bursty, and clients retry);
- slower cloud cores;
- peer-forwarded admissions sharing the same durable writer;
- hostile traffic that must be refused at line rate.

## How production L2s do it (short form)

The sources are in the research doc.

- **None of Base, OP Mainnet or Arbitrum One caps total submits at the edge.**
  Total throughput is bounded by the fee market and by bounded pools that
  evict by price.
- **Per-IP limits apply only to the free public endpoints.** Those endpoints
  publish no numbers and say they are not for production. Heavy users get API
  keys, use providers, or run their own forwarding nodes.
- **proxyd's limiter is fixed-window counters.** Its map is dropped whole each
  window. There is no table cap, and the rate is never coupled to the table
  size.
- **Each element of a batch is charged against the limit.** proxyd allows 100
  per batch, geth 1,000.
- **All checks are synchronous and in memory before the hash comes back:**
  size, signature, intrinsic gas, fee, nonce and balance. Pool-full eviction
  is by price. reth validates on worker tasks behind a bounded channel.
- **No chain fsyncs each tx before acknowledging it.**

## Current limits

Paths are under `demo/midgard-node/src/`.

**Measurement conditions.** The measurements were taken on 2026-11-06 on an
AMD Ryzen 9 9950X3D with Node 24, against the built `dist/` (flagged slightly
stale by doctor). The scripts are in the session scratchpad. Treat the numbers
as order-of-magnitude. A cloud vCPU is likely 1.5–2× slower.

**Relocation.** Since `dcbecd108`, the submit checks live in
`checkSubmittedTransaction` (`commands/http/transaction-admission.ts:46-99`),
which `/submit` and `/peer/txs` share. The logic is unchanged.

| # | Limit | Where | Effect |
| --- | --- | --- | --- |
| F1 | `write` is 1/s per client and 4/s globally. The client table holds 8,192 keys with 60 s idle eviction. Config refuses any global rate that could add more keys per minute than the table holds. | `services/http-rate-limits.ts:43-45,79`, `services/config.http-proxy-peers.ts:111`, `commands/listen-http-rate-limit.ts:179` | Hard cap of 4 TPS. `write` can rise to at most about 27/s without cutting `read`. |
| F2 | 64 public requests in flight and 4 per client, shared by submit, reads, evaluate and `/tx-status`. The slot is held until the response, which includes the durable commit. | `commands/listen-http-ingress.ts:78,111,303` | The writer waits up to 100 ms or 128 rows per batch (`services/admission-writer.validate-options.ts:14-22`). By Little's law that allows about 640–1,070 TPS for all public clients together, and about 40–66 TPS per IP. |
| F3 | Decode and checks run on the main thread, and the tx is decoded 3 times. | `commands/http/submit.ts:144`, `commands/http/transaction-admission.ts:46-99`. The consensus check re-decodes the tx: midgard-core `consensus-validation.validate-midgard-consensus-tx.ts:219-225`. | Cost per tx: 1-in/2-out 244 µs, 4/4 632 µs, 16/16 3,287 µs. About 2/3 of it is the consensus check's re-decode. On a cloud core, one thread saturates below 1,000 TPS. |
| F4 | The body may be 64 MiB, the DA payload bound. With no `Content-Length`, a request reserves all 64 MiB, half of the 128 MiB public byte budget (`listen-http-ingress.ts:115-116`). | `commands/http/submission-capacity.ts:8,60`, `commands/listen-http-ingress.ts:234,242`, `services/config.make-config.ts:298-304` | The envelope is `[version, txCbor]` and the tx is at most 295,041 B (`transaction-admission.ts:54`). Requests with no length run one at a time. |
| F5 | TLS 1.3 runs in the node process with at most 256 connections. Every POST answered with status 400 or above, including 429 and 503, gets `connection: close`. | `commands/listen-public-http.ts:477-488`, `commands/listen-http-ingress.ts:299` | Handshakes cost main-thread CPU. Under overload every refusal forces a reconnect and a new handshake. |
| F6 | Nothing checks the signature, fee or inputs before the durable write. | Fees are enforced only in the validation pool (`services/validation-pool.make-validation-pool.ts:260`). | A self-signed tx with made-up inputs and no fee is free to send, and it takes a backlog slot. |
| F7 | The Effect HTTP stack handles 10.7k req/s on one core, against 42k for raw Node. The client ran in the same process, so this is an upper bound on the cost. | `commands/listen.startup-http.ts` | About 90 µs per request. One tx per request caps a process near 10k TPS. |
| F9 | The durable backlog holds 10,000 rows or 64 MiB. | `services/config.make-config.ts:282-287` | About 10 s of buffer at 1,000 TPS. |
| F10 | Every admission batch runs in one transaction under a single global advisory lock (`ADMISSION_BYTE_QUOTA_LOCK_KEY`). Inside it, the byte quota is recomputed as `SUM(octet_length(payload))` over every queued or validating row. | `database/txAdmissions.admit-reserved-batch.ts:49-70`, `database/txAdmissions.admit-without-byte-quota.ts:215-228` | The writer's 2 shards are serialized, and each batch costs O(backlog). Measured (test Postgres, 500 B txs, `fsync=off`): about 10k rows/s at a 10k backlog, 3.8k at 100k, 0.9k at 300k. Raising the backlog (D5) without fixing this collapses admission below 1,000 TPS. |

| F11 | When the in-memory backlog reservation fails, each tx falls back to `TxAdmissionsDB.admit`, which opens a transaction, takes the same lock and runs the same scan only to touch a duplicate or refuse. | `commands/http/transaction-admission.ts:135-142`, `database/txAdmissions.admit-without-byte-quota.ts:230-280` | Overload multiplies database work: the full-backlog path is the most expensive one. |
| F12 | `/peer/txs` admits a body's txs one at a time and awaits each durable commit. | `commands/http/peer-routes.ts:142-166`; forwarder timeout 30 s, `fibers/l2-tx-peer-forwarder.ts:47` | At low load each commit waits for the collector deadline (100 ms), so a peer lane ingests about 10 TPS and a large body outlives the 30 s timeout. At 1,000 TPS every active operator must ingest the others' forwarded traffic too. |
| F13 | The public edge nginx (`scripts/public-ingress.mjs`) rate-limits every route at 10 r/s per IP (burst 30) and 100 r/s for the whole server (burst 200). It forwards a fixed header list (`proxy_pass_request_headers off`) and has no upstream `keepalive`. | `scripts/public-ingress.mjs:63-82,107` | The edge caps the node at 100 requests/s before any node limit applies. Every proxied request opens a new TLS connection to the node. |
| F14 | Readiness fails at `READINESS_MAX_DURABLE_ADMISSION_BACKLOG` (default 10,000). The writer's `capacity.take(1)` waits for a slot instead of refusing. | `commands/http/readiness.ts:219`, `services/admission-writer.make-admission-writer-with-options.ts:416` | A larger backlog makes a load balancer drop the node long before the backlog is full. Up to 20,000 waiters can each hold a body, with no byte bound. |

The submission-capacity permits (`SUBMIT_INGRESS_MAX_CONCURRENCY` and bytes)
are already released before the durable write (`commands/http/submit.ts:177-210`,
the suspended admission effect). Only the HTTP ingress slot of F2 is held
across the commit.

## Principles

1. **Rate limits give fairness, not capacity.** Capacity is protected by
   shedding load on real queue state.
2. **Reject cheaply before storing expensively.**
3. **The main thread only does I/O.** CPU work runs on worker threads.
4. **Charge per tx and byte, not per request.**

## Design

### D1 — Fixed-memory limiter

The per-client `Map` becomes a **hashed bucket array** per rate class. It has
2 rows of 2^18 cells, and each cell holds a `Float32` token count and a
`Uint32` refill time in typed arrays. That is about 4 MiB per class, fixed.

- A key maps to one cell per row through a keyed hash, with a secret drawn at
  random at process start.
- A request of cost `c` is admitted only if every one of its cells, once
  refilled, holds at least `c` tokens. Admission debits all of them.
- There is no allocation, no eviction and no "table full" refusal.
- A collision only tightens a limit. An honest key is wrongly limited only if
  saturated keys share its cell in both rows, with probability about `(K/2^18)^2`.
- `HTTP_PEER_MAX_ENTRIES`, `httpPublicKeysPerIdleWindow` and the config-time
  check are deleted.
- **Refill time** is stored in `Uint32` seconds since process start (no wrap
  for 136 years). A millisecond counter would wrap after 49.7 days.
- **Per-client in-flight count** (today `peer.active`) moves to a third
  `Uint16` array on the same cells, incremented on admit and decremented on
  release. Collisions only tighten it.
- The liveness, admin and peer classes keep the existing small `Map`; they
  have few keys.

A second, looser tier is keyed on the IPv4 /24 or the IPv6 /48. A client is
an IPv4 address or an IPv6 /56. Keying IPv6 on /64 would let one /48 claim
65,536 keys, and the `(K/m)^2` bound only holds when K is far below 2^18.

The alternatives considered are proxyd-style fixed windows with a size-capped
map, or a 1M-key LRU with free eviction. The hashed array wins because its
memory and per-request cost are constant.

### D2 — Client tiers

| Tier | Identity | Write quota (proposed) | State |
| --- | --- | --- | --- |
| Anonymous | Socket IP, or `x-midgard-client-ip` from a trusted proxy (unchanged) | 20 tx/s per IP, burst 100; 200 tx/s per /24 or /48 | D1 arrays |
| Peer operator | Existing `peer` class (`dcbecd108`): operator-key signature against the on-chain active set | Per-peer request rate as shipped; bounded by D5 | Existing |

There are no API keys (owner, 2026-11-06). There is no fixed global tx/s cap.
D5's load shedding is the global bound. It sheds anonymous traffic before
peers.

### D3 — Cheap single-tx requests (no batch route)

Wallets send one tx per call, and none of Base, OP Mainnet or Arbitrum offers
a batch-submit route (only generic JSON-RPC batch arrays, used by bots and
infrastructure). So there is no `/submit/batch` (owner, 2026-11-06). The cost
per single-tx request is cut instead:

- **TLS off the node** (D7). A new TLS 1.3 connection per request measured
  390 µs of node CPU (ECDSA P-256) or 570 µs (RSA-2048), against 20–25 µs on
  a reused connection. Wallet traffic is mostly one tx per connection, so in
  the node this alone caps a core near 2.7k req/s.
- **A lean `/submit` handler.** The Effect HTTP stack costs about 90 µs per
  request, against about 24 µs for raw Node (F7). Today's whole `/submit`
  handler measures about 475–600 µs of main thread per request (M2
  baseline, #871), so one thread saturates near 2,000 req/s and this
  handler is required for 5,000 TPS. `/submit` gets a direct
  handler that keeps the same ingress checks (identity, size, rate) and
  hands off to the worker pool, without the generic router and per-request
  metric timers. The other routes keep the Effect stack.
- **Cheap refusals.** Before #881 every refusal wrote info-level log lines
  and every request ran four or five Effect metric timers, even with
  monitoring off. The lean handler counts refusals by reason in plain
  counters (`submit_refusal_count{reason}`) and logs only the first refusal
  per reason per second. It records one latency histogram, not one
  timer per stage. At 5,000 TPS with 20 % junk, this avoids at least 1,000 log
  lines per second.

**Peer route (fixes F12).** `/peer/txs` keeps its signed multi-tx body, but
the writer gains a batch-admit call: the route checks every tx, then admits
them all in one writer submission and waits once.

Measured (M3, #882, 2026-11-07): the ack from memory (#874) had already
removed F12's commit wait, so the per-tx route took about 5,200 tx/s per lane
on 500-tx bodies (8,200 on full bodies of 888 txs of 327 B); one writer
submission per body takes 6,700–8,200 and 9,300–9,800 (two runs, on a
machine shared with other test runs). A full body is answered in 78–87 ms at
the median and 160–180 ms at worst. So the receiver is not what bounds a lane:
the `peer` rate class is (4 requests a second, burst 8), which caps a lane at
about 3,500 tx/s for 327 B txs and about 1,100 tx/s for 1 KiB txs. That
clears the 1,000 TPS public target, so the body bound stays one largest tx
(about 288 KiB, so any tx fits in a body of its own). Forwarding 5,000 TPS
on one lane would need a larger body or a faster peer rate. The forwarder's
request timeout drops from 30 s to 10 s: 50× the slowest full body, and a
peer slower than that is treated as stuck (the lane backs off and catches up
by inventory instead of holding its one request for 30 s).

### D4 — Checks off the main thread, decoding once

- **Decode once.** The consensus check runs on the decoded value:
  `validateMidgardConsensusTx(decoded, length)` already exists, and the
  `…Cbor` wrapper is what re-decodes. The submitted-tx decode
  (midgard-validation `decodeMidgardSubmittedTxFromCanonicalCbor`,
  `ledger-tx/codec.encode-mint.ts:327`) already runs the core full decode
  first, so its native tx can be handed to the consensus check. Measured
  after decode-once landed (#876, 2026-11-07): the whole check costs about
  0.075 ms (1 in, 2 out), 0.12 ms (4/4) and 0.33 ms (16/16). Decode-once
  saves 10–30 % against the old path on the same build. The earlier figures
  (0.29, 0.83 and 5.6 ms) came from a stale build.
- **Ingress worker pool.** `checkSubmittedTransaction` and the envelope decode
  run on a dedicated `worker_threads` pool, separate from the
  block-validation pool. The size is configurable.
- **Main thread.** It reads the body, posts the bytes to a worker (batched per
  message for D3), and queues the result for admission.
- **Worker result:** `{txId, canonical bytes, full hash, spent outrefs,
  produced outrefs, fee, size}`. The full hash moves off the main thread
  (today `database/txAdmissions.group-reserved-admission-variants.ts`).
- **Hand-off.** The main thread groups txs from concurrent requests into one
  message per worker per tick: 45 µs per tx sent alone, 4–6 µs batched
  (measured).
- A new `SUBMIT_INGRESS_WORKER_QUEUE` bounds the worker queue.
  `SUBMIT_INGRESS_MAX_CONCURRENCY` keeps sizing the peer pool
  (`commands/http/submission-capacity.ts`).
- **Landed (#885).** `services/submit-check-pool.ts` runs
  `checkSubmissionBody` (`commands/http/submission-check.ts`, the module the
  handler used) in `workers/submit-check.ts`, `SUBMIT_INGRESS_WORKER_POOL_SIZE`
  workers (default 2). A `/submit` request reserves a queue slot (default 256
  slots, and `SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES` bytes) before its body is
  read and holds it to the verdict; a full queue answers 503 at once. The
  bodies of one tick go to each worker as one message, transferred, and an
  accepted tx comes back as one transferred buffer of tx id, full hash and
  canonical bytes. A worker takes bodies once it has loaded the check; one
  that exits, throws, does not load within 30 s or leaves a batch unanswered
  for 10 s is terminated, its requests get 503, and it is replaced after a
  backoff of 50 ms doubling to 5 s. `/submit` no longer takes the
  `SUBMIT_INGRESS_MAX_CONCURRENCY` permits.

### D5 — In-flight limits, group commit, load shedding

- **Split budget.** Requests reading a body or waiting on a worker hold a
  CPU/byte slot (about 256). With the ack from memory (D9) no request waits
  for the database. The per-client in-flight limit is 8.
- **Fail-fast writer (fixes F14).** The writer refuses instead of waiting:
  `capacity.take(1)` becomes a try-take, and the writer queue gets a byte
  bound (e.g. 64 MiB of queued payload) beside its row bound.
- **Group commit.** Keep the pipelined collector and persister, but drop the
  fixed deadline: the collector hands over whatever is queued (up to 256
  rows) as soon as the persister is free. One transaction per batch. The 202
  no longer waits for it (D9). Shards stay keyed by tx id, so one tx id is
  always written in order.
- **Asynchronous commit (owner, 2026-11-06).** The admission transaction runs
  `SET LOCAL synchronous_commit = off`, as lease claims already do
  (`database/txAdmissions.admit-reserved-batch.ts:186`). Since the 202 comes
  from memory (D9), waiting for the WAL flush buys nothing. Removing it takes
  the disk flush off the writer and leaves disk bandwidth to block and
  validation writes, which stay synchronous. A Postgres crash can lose the
  last few hundred milliseconds of admissions (about 3 × `wal_writer_delay`),
  inside D9's loss window. Terminal marks stay synchronous, and their commit
  flushes the earlier admission records in WAL order.
- **Batch admit.** The writer takes a list of checked txs and resolves them
  together (the peer route uses it).
- **O(1) byte quota (fixes F10).** Track backlog payload bytes the way the
  row backlog is already tracked: an in-memory gauge
  (`fibers/admission-backlog-gauge.ts`) reserved before the write and
  reconciled by the periodic live-count refresh. A new `payload_bytes`
  integer column on the admission row, set at insert, makes that correction
  a sum over a narrow column on the existing partial indexes. It never reads
  payloads. Drop the per-batch `SUM` and the global advisory lock. Then the
  shards write in parallel. With this
  shape the insert alone measured about 36k rows/s at 64–256 rows per
  statement on 2 connections. This must land before the backlog is raised.
- **No database fallback when full (fixes F11).** A failed backlog
  reservation answers 503 from memory. Duplicates are detected in memory
  (D9), not with the quota transaction.
- **Load shedding (done, #883).** The signal is the in-memory row and byte
  gauges. Shed with 503 and `Retry-After` at 80 % for anonymous traffic and
  100 % for peers. The admission layer (`admitCheckedTransactions`) picks the
  limit by submit source, so any HTTP handler gets it, and the refusal is
  decided before any database work. `MAX_DURABLE_ADMISSION_BACKLOG` covers at
  least 60 s at the design rate: 300,000 rows, with a 1 KiB-per-row byte
  bound (307,200,000 bytes). Both are provisional until M1 (#877). Raising
  them costs no node memory: the gauges are counters, the pending set keeps
  its own 20,000-row and 64 MiB bounds, and the recent-id ring is fixed at
  2^20 ids. The backlog itself sits in Postgres. The gauge refresh's
  `SUM`/`COUNT` grows with the backlog (M1, M7).
- **Readiness.** `READINESS_MAX_DURABLE_ADMISSION_BACKLOG` follows the backlog
  limit at 95 % (285,000 rows by default), above the 80 % anonymous share,
  so shedding starts before the node turns not-ready.
- **Payload deleted at the terminal mark (owner, 2026-11-06).** Today a tx's
  bytes sit in `tx_admission_payloads` forever, beside the copy that
  mark-accepted writes into the mempool table. Nothing reads the payload after
  the terminal mark: peer sharing reads the mempool table
  (`database/l2PeerShare.ts:32-39`), and correction rejects read only the
  status. So mark-accepted and mark-rejected delete the payload row in the
  same transaction. The one reader that breaks is the duplicate check, which
  compared the stored bytes (`touchDuplicateCount` in
  `txAdmissions.admit-without-byte-quota.ts`, deleted by #872). It compares
  instead the 32-byte full hash, which moves onto the admission row. The full
  hash covers the witnesses, so equal hashes mean equal canonical bytes.
- **Retention.** Terminal admission rows, now small without their payloads,
  are deleted after a retention window. Today they are never deleted, so the
  primary key grows with every tx (3.6M rows per hour at 1,000 TPS).
- **Size bounds.** The `/submit` body is capped at the largest tx plus the
  envelope, about 296 KB. A request without `Content-Length` gets 411. The
  config floor that holds `SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES` at
  `maxDaPayloadBytes` or more (`services/config.make-config.ts:303-316`)
  drops to that cap.

### D6 — Fail fast before the durable write

**Stateless, in the worker. Definitive (done, #886; in the submit check
workers since #885).** These reuse Phase A
(`validatePhaseASingleDecoded`, midgard-validation
`phase-a.validate-phase-asingle.ts`, which returns what the validation pool's
`validatePhaseASingle` returns) and its reject codes, so ingress and
validation cannot disagree:
- Vkey witnesses verify over the body hash (Ed25519 costs about 52 µs each,
  measured).
- The fee is at least `MIN_FEE_A × size + MIN_FEE_B`. Both default to 0
  (`services/config.make-config.ts:267-274`), so the public profile must set
  them or this check refuses nothing.
- The validity interval has not passed, judged with a margin for the node's
  clock.
- The network id is correct.

As built (#886), `checkSubmittedTransaction` runs the whole of Phase A with
the validation pool's settings on its own decode and refuses only
on `E_INVALID_SIGNATURE`, `E_MIN_FEE` and `E_NETWORK_ID_MISMATCH`, so every
refusal is one Phase A itself makes; Phase A's other verdicts go on to the
validation pool. Expiry is Phase B's check (`E_VALIDITY_INTERVAL_MISMATCH`)
judged against the wall clock 60 s back (`SUBMIT_EXPIRY_CLOCK_MARGIN_MS`),
not the follower's slot estimate, so a node clock running ahead does not
refuse what validation would accept. The check stays a pure function of the
bytes, the rules (network, fee rule, L1 slot mapping) and the clock. A
signature verifier that is not initialised answers 503, not a verdict. In
the submit check workers the rules are read once on the main thread (the
slot mapping from Lucid) and handed to every worker as plain data; each
worker reads its own clock. A worker says it is ready only once libsodium
has initialised (bounded at 10 s, under the pool's 30 s start bound); one
that cannot initialise stops, so its waiting bodies answer 503 and a fresh
worker is started. The node serves `/submit` only once a worker is ready
(waiting at most that 30 s start bound), so requests arriving at startup do
not hold ingress slots while workers load.
`/peer/txs` runs the same checks: only the stateful check is skipped for
peers (below). Phase A's decoded entry point (`validatePhaseASingleDecoded`)
reads the check's decode, so the tx is decoded once. It returns exactly what
`validatePhaseASingle` returns for the same bytes, and the two share every
check after the decode; see the capacity table.

**Stateful, on the main thread. Advisory.**
- Each input must be in the mempool ledger state, or be an output of a pending
  admitted tx. That needs an index of the outputs pending txs produce, rebuilt
  from the backlog on restart.
- One pending tx claims each input. A second tx spending a claimed input is
  refused with 409.
- Claims are released by hooks in mark-accepted, mark-rejected, the peer
  forget path and lease requeue.
- A full backlog refuses. It does not evict or replace (RBF) a tx already
  acknowledged with 202: that would break the ack and the peers' copies.
- Peer-sourced txs skip the stateful check. A peer's tx may spend an output
  this node has not seen yet, and the peer resends it on the next L1 advance
  (`fibers/tx-queue-processor.tx-queue-processor-action.ts:54-57`).
- The claim table is per process, so D8 ingress processes skip it; their
  node runs it at the internal route.

The spendable set already lives in memory: `MempoolLedgerCache`
(`services/mempool-ledger-cache.mempool-ledger-cache-service.ts`, a `Map` of
outref to output). Its `currentState` must be read under the phase-B lock
that validation holds, so the ingress check needs a lock-free read view (a
snapshot reference swapped after each patch), not a call per tx under the
lock.

Full validation stays authoritative.

### D7 — Transport

The edge already exists: `scripts/public-ingress.mjs` generates the public
nginx config (F13). It changes as follows.
- **nginx talks to the node over plain HTTP on a Unix socket (owner,
  2026-11-06).** TLS ends at the nginx edge. Today nginx re-encrypts to
  `https://midgard-node` with certificate checks (`scripts/public-ingress.mjs:41,102-106`),
  and the public profile forces an HTTPS listener in the node
  (`commands/listen-public-http.ts:426-488`). Even on reused connections that
  costs the node 20–25 µs per request, roughly 30–40 % of the main-thread
  budget below. The public profile gains a plain HTTP listener on a Unix
  socket shared with the nginx container through a volume, readable only by
  the two containers' users. The node's public TCP listener goes away.
- Add an upstream block with `keepalive` to that socket, and raise the node's
  `maxRequestsPerSocket` (100) for it.
- Remove `limit_req` from the submit, batch and read routes; the node's D1
  limiter does that job per tx. Keep nginx limits only on connection count
  and on liveness and admin routes.
- Cap the `/submit` body at about 296 KB instead of 64 MiB.
- `HTTP_TRUSTED_PROXY_PEERS` today accepts 1–8 private IPv4 addresses
  (`services/config.http-proxy-peers.ts:15-41`). Allow IPv6 and the address
  ranges of cloud load balancers.
- Drop `connection: close` on 429, 503 and 409. Keep it only for malformed
  requests. (Duplicates already return 200 and keep the connection.)

### D8 — Scale out (optional, beyond one node)

An operator can run extra ingress-only processes behind its load balancer.
They run D1–D6 and forward pre-checked txs to their node, which
admits them through one internal batched route. Every operator is already an
ingress point through the `dcbecd108` tx sharing, so users can submit to any
operator.

### D9 — Acknowledge from memory (owner, 2026-11-06)

`/submit` answers 202 once the tx has passed the D6 checks and entered the
node's in-memory pending set. The Postgres write follows by group commit, as
on OP Mainnet and Base, whose pools answer from memory. (Arbitrum answers
later, after the tx is executed into an L2 block.)

- **Pending set.** Txs keyed by id, with their claimed inputs and produced
  outputs (shared with D6). An entry leaves the set when its row commits;
  the claims stay until the tx is accepted or rejected.
- **Bounded.** The unpersisted part has a row and byte cap. If Postgres stalls
  and the cap fills, `/submit` answers 503, so memory never grows without
  bound and the loss window stays bounded.
- **Reserve before the ack.** The row and byte gauges (D5) are reserved
  before the 202, so the write can never refuse a tx for quota after it was
  acknowledged.
- **Duplicates, without a database read on the request path.** The request
  path checks the pending set and a bounded in-memory cache of recently
  committed tx ids (sized by count, for example the last 1M ids), in that
  order, before the D6 input check. A hit answers 200 as today. A miss goes
  on as a new tx. The write's `ON CONFLICT` then resolves it after the ack:
  - same full hash as a stored row: the stored row is touched, as
    `touchDuplicate` does today;
  - same tx id, different bytes (other witnesses): the stored version wins
    and the new bytes are dropped. Both versions have the same body, so the
    ledger effect is the same. `/tx-status` reports the stored row.

  An old tx that misses the cache and whose inputs are already spent is
  refused by the D6 input check. That refusal's message tells the client to
  read `/tx-status` before treating the tx as failed.
- **`/tx-status`** reports txs still in the pending set as `queued`; today it
  reads only Postgres (`database/http-tx-status-facts.ts`).
- **Peer route.** `/peer/txs` uses the same path and answers once the body's
  txs are in the pending set. The sending operator keeps its own durable
  copy and resends on the next L1 advance, so a crash here loses nothing for
  good.
- **Validation and peer forwarding** keep reading committed rows. The
  tx-queue processor is woken after each commit, as now.
- **Write failure.**
  - A transient error (connection loss, serialization failure, timeout)
    retries the batch with backoff. The txs stay in the pending set and keep
    their claims.
  - Any other error splits the batch and retries row by row. A row that
    still fails leaves the pending set, its claims are released, its status
    becomes `unknown`, and a counter and an error log record it. One bad row
    must not block its shard.
  - If the pending set cannot flush for longer than a set time (for example
    30 s), the node turns not-ready and `/submit` answers 503. It recovers
    by itself when writes succeed again, with no manual step (liveness
    first).
- **Shutdown.** A graceful stop stops taking new txs, then flushes the
  pending set within a bounded time (for example 10 s). Anything still
  unwritten after that is logged by tx id and counted as lost.
- **Crash.** Txs acknowledged since the last commit (normally one batch
  interval, at most the cap) are lost. Their `/tx-status` turns `unknown`, and
  the wallet resubmits. `PUBLIC_HTTP.md` and the docs-site API page say so:
  202 means accepted, not stored.

The F2 limit (an HTTP slot held through the commit) goes away, because no
request waits for the database.

## Capacity budget (design, one node)

All figures were measured on the machine named under Current limits. Halve
them for a cloud vCPU.

| Stage | Cost per tx | Capacity |
| --- | --- | --- |
| TLS in the node, new connection per request | 390 µs (ECDSA P-256), 570 µs (RSA-2048) of node CPU | ~2.7k / ~1.8k req/s per core, trivial handler |
| TLS in the node, reused connection | 20–25 µs | ~40–50k req/s per core |
| Plain HTTP over a Unix socket from nginx (D7) | none in the node | — |
| HTTP framework (main thread), Effect stack | ~90 µs/request | ~10k req/s |
| HTTP framework, raw Node | ~24 µs/request | ~42k req/s |
| Hand-off to workers (main thread) | 45 µs one tx per message; 4–6 µs at 16–64 per message | 22k tx/s single, 170–250k batched |
| Worker checks, decode-once (measured on #876) | 0.075 ms (1/2), 0.12 ms (4/4), 0.33 ms (16/16) | ~100k tx/s (1/2) to ~24k tx/s (16/16) on 8 workers |
| Worker checks with D6, Phase A on the check's decode (measured on #886) | 0.19 ms (1/2), 0.44 ms (4/4), 1.52 ms (16/16); 0.46, 0.78 and 2.08 ms while Phase A decoded the bytes again. Signatures dominate at 16/16: 16 vkeys × ~55 µs ≈ 0.88 ms is the floor, the rest ~0.64 ms | ~42k tx/s (1/2), ~18k (4/4), ~5.3k (16/16) on 8 workers |
| Durable write, today (F10) | O(backlog) | ~10k rows/s at 10k backlog, ~0.9k at 300k |
| Durable write, O(1) quota | 3.5 ms per 64-row statement | ~36k rows/s on 2 connections, `fsync=off` |
| Main thread residual: ingress check, writer enqueue, `Deferred`, response | estimate 20–50 µs; measured far higher (next row) | — |
| Lean raw-Node `/submit` handler, main thread, check on two workers (M2, #885) | 335 µs/request at 10 % keep-alive; 288 µs with every connection kept alive | ~3.0k req/s |
| Lean raw-Node `/submit` handler, main thread, submit check still on it (M2, #881) | 506 µs/request (429 µs less the 77 µs check) at 10 % keep-alive; 467 µs with every connection kept alive | ~2.0k req/s |
| Effect-router `/submit` handler before #881, main thread (M2 baseline, #871) | ~475–600 µs/request; 688 µs in the same-day rerun beside the row above | ~1.5–2.1k req/s |
| Peer ingest, before the ack from memory (F12) | one commit wait per tx | ~10 TPS per lane at low load |
| Peer ingest, batch admit (M3, #882) | one writer submission per body; full body ~80 ms p50, under 0.2 s max | ~6.7–8.2k tx/s per lane (500-tx bodies), ~9.3–9.8k (full bodies); the `peer` rate class caps a lane at ~3.5k tx/s for 327 B txs |

**Binding stage.** Every request carries one tx. With TLS at the edge, a lean
`/submit` handler and grouped worker hand-off, the main thread spends about
24 + 5 + 20–50 µs per tx: roughly 50–80 µs, so 12–20k TPS on this machine
and 6–10k on a cloud core (an estimate M2 does not bear out yet: with the
check on workers the lean handler measures 335 µs per request, and 288 µs
when every connection is kept alive, so connection setup does not explain
the gap; where the rest goes needs a profile). With TLS in the node and one
connection per wallet request, the handshake alone is 390–570 µs, which
caps the node near 1,000 TPS on a cloud core. The workers need about 1.3
cores at 5,000 TPS. Once F10 is fixed, the database has about 7× headroom.

That estimate counts no TLS in the node, which needs the Unix socket of D7.
Kept on a reused TLS connection, it would add 20–25 µs per request.

Not measured: where the lean handler's main-thread residual goes (M2
measures its total), and the other Postgres writers
that share the database and its WAL at 1,000 TPS (validation, mempool and
block persistence). With asynchronous commit (D5), admission batches no
longer wait for a WAL flush.

## Rollout

| Phase | Content | Gate |
| --- | --- | --- |
| P0 | Harness and the measurements below. The harness sends one tx per request from many source IPs (or through the trusted-proxy header), mostly on new connections as wallets do, from a pre-built corpus of valid txs spending a 300k-UTxO genesis, plus a junk mix. A **drain stub** marks admitted rows terminal at a set rate, so ingress is measured apart from block production (about 140 TPS today). | Baseline report through nginx on the public profile |
| P1 | F10 O(1) byte quota and no global lock; F11 no fallback; F14 fail-fast writer and readiness; F4 size cap and 411; F5 keep-alive on refusals; group commit without deadline; asynchronous commit for admissions; `payload_bytes` column; payload deleted at the terminal mark, duplicate check by full hash; D9 ack from memory with the bounded pending set; D4 decode-once (still on the main thread); retention. | Admission rows/s flat from 10k to 300k backlog; 503 under overload costs no database work |
| P2 | D7 TLS at the edge, Unix socket to the node and nginx changes; D1; D3 lean `/submit` handler with sampled refusal logs; batch admit for the peer route (F12); D5 shedding and larger backlog. | With the drain stub: 5,000 TPS admitted from 1,000+ source IPs on new connections; a peer body of 500 txs admitted in under 1 s |
| P3 | D4 worker pool, D6. | With the drain stub: 5,000 TPS admitted for 10 min with 20 % junk added; no junk that the checks catch reaches Postgres; p99 admission latency under 250 ms |
| P4 | D8 (optional). | Second ingress process adds capacity |

## Readiness (review of 2026-11-06)

**Ready for implementation:** yes. Every owner decision is ruled. The
remaining open values (backlog size, shard count, shed thresholds, the
pending-set cap, whether D8 is needed) are set by the P0 measurements below,
not by design choices. D9 joins P1, because it changes the ack and the
pending set that D6 builds on. A second review on 2026-11-06 found four gaps
in D9 and closed them: the duplicate check read Postgres on the request path,
the gauges had to be reserved before the ack, one bad row could block a shard
forever, and shutdown had no time bound.

**Tickets:** tracking issue Anastasia-Labs/midgard#870, with #871–#889
(filed 2026-11-06).

**Confidence that submit stops being the bottleneck:** high for 1,000 TPS,
moderate for the 5,000 TPS target. All the stages measured so far clear
5,000 TPS once F10 is fixed, TLS leaves the node and the lean handler
replaces today's (~500 µs per request). What could still bind: the lean
handler's own cost and the stages not yet measured. Without the F10, F11, F13 fixes and TLS at the
edge, submit cannot reach 1,000 TPS whatever else lands.

**Measurements still needed (P0):**

| # | What | Decides |
| --- | --- | --- |
| M1 | Admission batch latency at 10k/100k/300k backlog and 1M+ terminal rows, after the F10 fix, with asynchronous commit and the validation and block writers running against the same WAL (`fsync=on`) | Backlog size, shard count, shed threshold |
| M2 | Main-thread µs per single-tx `/submit` through the lean handler, writer and worker stubbed (baseline on the Effect handler done: 475–600 µs, 688 µs on 2026-11-07; lean handler with the check on the main thread, #881: 506 µs, 467 µs all keep-alive; check on two workers, #885: 335 µs, 288 µs all keep-alive) | Whether D8 is needed for 5,000 TPS |
| M3 | Peer ingest TPS per lane, before and after batch admit | Peer body size and forward timeout |
| M7 | Gauge drift at 5,000 TPS with the 500 ms refresh | Whether the shedding signal is exact enough |

Done: decode-once cost (D4), worker hand-off (D4), the insert statement
without the quota scan (D5), the quota scan's growth (F10), TLS handshake
cost (D7) and peer ingest (M3, under D3's peer route).

**Docs and tests to migrate:** `demo/midgard-node/PUBLIC_HTTP.md` (bounds),
`docs/public-testnet-parameters.md`, the public compose file and
`scripts/public-ingress.mjs` with its tests (the Unix socket), the docs-site HTTP API and submission
observability pages, `demo/midgard-node/README.md`, the rate-limit, ingress,
liveness and peer-route tests, and the admission database tests. lucid-midgard
treats 429 as not retryable and reads `/protocol-info` on every submit
(`demo/lucid-midgard/src/provider.ts:280-320`). With D9 it must also resubmit
a tx whose `/tx-status` turns `unknown` after a 202, and it must read
`/tx-status` before treating a spent-input refusal as a failure.

## Owner decisions

All ruled 2026-11-06:

1. No API keys on submit, and no batch-submit route.
2. **Ack from memory** (D9).
3. **Stateful input check** (D6): build it; first-seen wins; no replacement
   of an acknowledged tx.
4. **TLS at the nginx edge** with reused connections to the node (D7). The
   hop from nginx to the node is plain HTTP over a Unix socket (efficiency
   review).
5. **Minimum fee:** `MIN_FEE_A` = 1 lovelace per byte, `MIN_FEE_B` = 2,000
   lovelace, set in `demo/midgard-node/config.preprod-public.example.yaml`.
   The schedule is committed in every block header, checked by the min-fee
   fraud proof, and must match across operators
   (`mpf/verified-block-import.ts:275-285`).
6. **Efficiency review:**
   - asynchronous commit for admission writes (D5);
   - payload deleted at the terminal mark, with a `payload_bytes` column (D5);
   - sampled refusal logs (D3).

   **Out of scope:** the client's request mix. lucid-midgard fetches
   `/protocol-info` before each submit and polls `/tx-status` every second,
   so one wallet submit costs about 4–17 requests. The streaming outcome
   feed (R2 in the block-production doc) is the fix, not this plan.
