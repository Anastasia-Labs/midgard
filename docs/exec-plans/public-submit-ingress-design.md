# Public submit ingress: redesign for high throughput

**Status:** Active. The design below is implemented except the optional D8;
the P1–P3 release gates are outstanding.

**Last reviewed:** 2026-11-23 (outstanding gates and measurements, against
issues #870, #877, #884 and #889 and `demo/midgard-node`).

**Related:**

- [`demo/midgard-node/PUBLIC_HTTP.md`](../../demo/midgard-node/PUBLIC_HTTP.md):
  the current `/submit` behaviour, bounds and refusals.
- [`demo/midgard-node/README.md`](../../demo/midgard-node/README.md), "Submit
  Ingress Load Harness": `pnpm load:submit-ingress`, the drain stub and the benches.
- [`docs/research/l2-tx-ingress-throughput.md`](../research/l2-tx-ingress-throughput.md):
  how Base, OP Mainnet and Arbitrum take transactions, with primary-source
  citations.

## Scope and goal

The path from a client's `POST /submit` to the 202 and the durable admission
row behind it. Validation, block production and reads are out of scope, and
so is the client's request mix (lucid-midgard's `/protocol-info` and
`/tx-status` polling; the streaming outcome feed is the fix for that).

**Goal:** ingress must not be the bottleneck at the network's intended
1,000 TPS of real-world traffic from many clients, some behind shared
addresses, with mixed tx sizes and some hostile load.

**Design target:** at least 5,000 TPS of admissions per node, so that 1,000
TPS uses at most 20 % of capacity. The headroom covers bursts from L1 block
gaps and retries, slower cloud cores, peer-forwarded admissions sharing the
writer, and hostile traffic refused at line rate.

## Principles

1. Rate limits give fairness, not capacity. Capacity is protected by
   shedding load on real queue state.
2. Reject cheaply before storing expensively.
3. The main thread only does I/O. CPU work runs on worker threads.
4. Charge per tx and byte, not per request.

## Design decisions

Behaviour and limits are in `PUBLIC_HTTP.md`; this section records the
choice and its reason.

- **Fixed-memory limiter.** Public rate classes keep their buckets in
  fixed hashed arrays keyed with a per-process secret, so memory and
  per-request cost are constant and there is no eviction or "table full"
  refusal; a collision only tightens a limit. A looser tier is keyed on the
  IPv4 /24 or IPv6 /48; a client is an IPv4 address or an IPv6 /56.
- **Client tiers.** Anonymous clients and signed peer operators only.
  There are no API keys and no fixed global tx/s cap; load shedding is the
  global bound and sheds anonymous traffic first.
- **Cheap single-tx requests.** No batch-submit route: wallets send one tx
  per call and none of the surveyed L2s
  offers one. `/submit` has a lean handler outside the Effect router and
  samples its refusal logs. `/peer/txs` keeps its signed multi-tx body and
  admits it in one writer submission.
- **Checks off the main thread, decoding once.** The submit check runs on
  a dedicated worker pool (`services/submit-check-pool.ts`), separate from
  block validation, and the consensus check reads the submit decode.
- **In-flight limits, group commit, load shedding.** Requests never wait
  for the database. The writer refuses instead of waiting, group-commits with
  a bounded linger, and runs admissions with asynchronous commit. Backlog rows and bytes are in-memory gauges; `/submit` sheds
  with 503 at 80 % of the backlog and peers at 100 %. The payload is deleted
  at the terminal mark and duplicates compare the full hash.
- **Fail fast before the durable write.** Stateless checks reuse Phase A
  and refuse only on verdicts Phase A itself makes, so ingress and validation
  cannot disagree. The stateful input check is first-seen wins: a pending tx
  claims its inputs, and an acknowledged tx is never replaced. Peer-sourced txs skip the stateful check. Full validation
  stays authoritative.
- **Transport.** TLS ends at the nginx edge, which talks plain HTTP to the
  node over a Unix socket with reused connections. nginx rate-limits only
  liveness routes and connection count; the node's fixed-memory limiter does
  the rest.
- **D8, scale out (optional).** Extra ingress-only processes behind an
  operator's load balancer run the limits and checks above and forward pre-checked txs to their
  node through one internal batched route. Not built; P2 decides whether it
  is needed.
- **Acknowledge from memory.** `/submit` answers 202 once the tx has passed
  the fail-fast checks and entered the bounded in-memory pending set;
  the Postgres write follows by group commit. A crash can lose txs
  acknowledged since the last commit, so a 202 means accepted, not stored,
  and a wallet resubmits a tx whose `/tx-status` turns `not_found`.

The minimum fee schedule of this design (`MIN_FEE_A` = 1 lovelace
per byte, `MIN_FEE_B` = 2,000 lovelace) is set in
`demo/midgard-node/config.preprod-public.example.yaml`; it is committed in
every block header and must match across operators.

## Outstanding gates

Each gate runs the load harness with the drain stub, through nginx on the
public profile, and records its results on the issue.

| Phase | Issue | Gate                                                                                                                                                     |
| ----- | ----- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| P1    | #877  | Admission rows/s flat from a 10k to a 300k backlog; a 503 under overload costs no database work; M1 recorded.                                            |
| P2    | #884  | 5,000 TPS admitted from 1,000+ source IPs on new connections; a peer body of 500 txs admitted in under 1 s; M2 recorded and D8 decided on #870.          |
| P3    | #889  | 5,000 TPS admitted for 10 minutes with 20 % junk added; no junk that the checks catch reaches Postgres; p99 admission latency under 250 ms; M7 recorded. |
| P4    | —     | Only if P2 finds D8 needed: a second ingress process adds capacity.                                                                                      |

## Measurements still needed

| #   | What                                                                                                                                                                                                                                                              | Decides                                                                                                                     |
| --- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| M1  | Admission batch latency at 10k/100k/300k backlog and 1M+ terminal rows, with asynchronous commit and the validation and block writers sharing the WAL (`fsync=on`)                                                                                                | `MAX_DURABLE_ADMISSION_BACKLOG`, its byte bound, the writer shard count and the pending-set cap, all provisional until then |
| M2  | Main-thread µs per single-tx `/submit` through the lean handler with the writer stubbed (`pnpm bench:submit-main-thread`). The last runs measured 82–119 µs, well above the 20–50 µs estimate for the handler's own work; where the residual goes is not profiled | Whether D8 is needed for 5,000 TPS                                                                                          |
| M7  | Gauge drift at 5,000 TPS with the 500 ms refresh                                                                                                                                                                                                                  | Whether the shedding signal is exact enough                                                                                 |

Not measured: the other Postgres writers (validation, mempool and block
persistence) sharing the database and its WAL at 1,000 TPS. M1 covers them.
