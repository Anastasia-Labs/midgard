# Production L2 transaction ingress and throughput

Reviewed: 2026-11-06.

Document class: research evidence for sizing and redesigning the Midgard node's
public transaction-submit path (today: per-IP token buckets plus a global
`write` bucket of 4/s burst 16, an 8,192-key peer table with 60 s idle
eviction, ingress concurrency 4, durable admission backlog 10,000 rows /
64 MiB; see
[`http-rate-limits.ts`](../../demo/midgard-node/src/services/http-rate-limits.ts)).
The sources describe Base, OP Mainnet and Arbitrum One. The recommendations at
the end are engineering inferences, not adopted decisions.

Source pins used throughout (code links are permalinks at these commits):

| Repo | Commit / tag | Notes |
| --- | --- | --- |
| `ethereum-optimism/infra` (proxyd) | `6634d7a86a2203eea9324fac17226059b2ed3d0b` | `main`, 2026-11-06 |
| `ethereum-optimism/op-geth` | `79e9d244748a1201fe686ca8a882b5fe7e173348` | `optimism` branch |
| `OffchainLabs/nitro` | `ccc5c828d9ea22f161efc784bac4d4a6559b7e59` | `master` |
| `base/base` (base-reth-node, builder) | `v1.5.4` = `8bb377afad0d3521381487fc99edd10640418a63` | latest release, 2026-11-06 |
| `paradigmxyz/reth` | `76c88b4f3eee1632d50824ebe0ccaeae0a65ec49` | `main` |

## Summary of the patterns

1. **No production L2 limits aggregate submit throughput at the edge.** Per-IP
   limits apply only to the free public endpoints, and they are documented as
   unsuitable for production. Real volume goes through providers, API keys or
   self-run nodes, which forward to the sequencer without per-IP quotas. What
   bounds total throughput is the fee market (a base-fee floor plus EIP-1559
   escalation), block gas limits, and bounded queues that evict by price.
2. **Rate-limit state is not a capacity-coupled table.** proxyd uses
   fixed-window counters. The in-memory map is dropped wholesale at each window
   boundary, and the Redis keys carry a TTL. There is no per-key eviction
   policy, no table cap, and no "global rate must stay below table size"
   invariant.
3. **Ack after in-memory admission, never after a per-tx fsync.** geth/op-geth,
   reth and Base return the hash once the tx is in the in-memory pool (or once
   the sequencer accepts it). Arbitrum's sequencer acks only after the tx is
   executed into an L2 block. None of them makes a durable per-tx disk write a
   precondition of the ack.
4. **Validate once, at the edge, and pass the result inward.** Replica or
   mempool nodes do decode, signature recovery and stateful pre-checks, then
   forward to one sequencer or builder. Base forwards pre-recovered senders in
   batches of up to 1,024 so that builders skip signature verification. The
   Nitro sequencer does only cheap checks before queueing.
5. **Batching is supported but does not buy quota.** JSON-RPC batch arrays are
   accepted (geth 1,000 elements; proxyd 100 by default, 1,000 hard cap), but
   proxyd charges every element against the rate limit. Batching pays off
   internally: pool inserts, forwarder-to-builder RPC and validation queues.

## 1. Ingress topology

| Chain | User-facing write path | Forwarding hop | Sequencer exposure |
| --- | --- | --- | --- |
| OP Mainnet | `https://mainnet.optimism.io` (rate-limited, no WS) or a provider or own node ([OP docs](https://docs.optimism.io/op-mainnet/network-information/connecting-to-op)) | Replica op-geth/op-reth sets `--rollup.sequencerhttp` and forwards `eth_sendRawTransaction`. "The OP Stack does not currently have a public mempool, so configuring this is required if you want your node to support transaction submission" ([OP operations guide](https://docs.optimism.io/chain-operators/guides/management/operations)). Example replica flags: `--rollup.sequencerhttp=https://mainnet-sequencer.optimism.io/ --rollup.disabletxpoolgossip` ([OP execution clients](https://docs.optimism.io/node-operators/guides/configuration/execution-clients)) | Dedicated write-only URL `mainnet-sequencer.optimism.io`: "The sequencer URL is write only" ([OP networks](https://docs.optimism.io/superchain/networks)). What sits behind it is not documented. |
| Base | `https://mainnet.base.org` for reads; `https://mainnet-sequencer.base.org` for "Transaction submission … only to send transactions" ([Base network info](https://docs.base.org/network-information)) | Nodes forward to the sequencer (`RETH_SEQUENCER_HTTP`, [Base troubleshooting](https://docs.base.org/specifications/node-operators/troubleshooting)). Inside Base, *mempool nodes* forward each pending tx to **every** builder as pre-validated batches ([tx-forwarding README](https://github.com/base/base/blob/8bb377afad0d3521381487fc99edd10640418a63/crates/execution/tx-forwarding/README.md); [transaction-events README](https://github.com/base/base/blob/8bb377afad0d3521381487fc99edd10640418a63/docs/transaction-events/README.md)) | Base's event journal names an edge producer `base-routing/proxyd` with `PROXY_RECEIVED → PROXY_VALIDATION_* → PROXY_ROUTED_TO_BACKEND` events ([same README](https://github.com/base/base/blob/8bb377afad0d3521381487fc99edd10640418a63/docs/transaction-events/README.md)). That implies proxyd fronts submissions → mempool nodes → builders. This is an inference from the event vocabulary, not a published topology. |
| Arbitrum One | `https://arb1.arbitrum.io/rpc` ("No uptime, latency, or rate-limit guarantees") or `https://arb1-sequencer.arbitrum.io/rpc`, which "Accepts only `eth_sendRawTransaction` and `eth_sendRawTransactionConditional`" and is "best-effort … no formal SLA" ([Arbitrum chain info](https://docs.arbitrum.io/build-decentralized-apps/reference/node-providers)) | Full nodes set `--execution.forwarding-target` ("Where the node forwards `eth_sendRawTransaction`; auto-filled from chain info for known chains") ([Nitro node roles](https://docs.arbitrum.io/run-a-node/assign-node-roles)). The forwarder tries targets in order, or picks one from Redis ([forwarder.go L126-L187, L345](https://github.com/OffchainLabs/nitro/blob/ccc5c828d9ea22f161efc784bac4d4a6559b7e59/execution/gethexec/forwarder.go#L126-L187)). Forwarding nodes pre-check txs at `--execution.tx-pre-checker.strictness` (default 20) | The sequencer page says: "if this node is not the active Sequencer, the transaction is forwarded to the active one" and "The mempool stays private" ([How the Sequencer works](https://docs.arbitrum.io/how-arbitrum-works/sequencer)). The edge in front of `arb1-sequencer` is not documented. |

Mechanics of the forwarding hop:

- **op-geth replica** (`EthAPIBackend.SendTx`): synchronously calls the
  sequencer's `eth_sendRawTransaction` and returns the sequencer's error. It
  then optionally adds the tx to the local pool, and a local-pool failure after
  a successful forward is only logged
  ([api_backend.go L332-L358](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/eth/api_backend.go#L332-L358)).
  Local pool admission is off by default when `--rollup.sequencerhttp` is set
  ([flags.go L2148](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/cmd/utils/flags.go#L2148)).
- **reth replica**: forwards first, then inserts into the local pool
  ([eth/helpers/transaction.rs L88-L104](https://github.com/paradigmxyz/reth/blob/76c88b4f3eee1632d50824ebe0ccaeae0a65ec49/crates/rpc/rpc/src/eth/helpers/transaction.rs#L88-L104)).
- **Base mempool node → builder**: one reader and queue per builder. Batches go
  out as `base_insertValidatedTransactions`, default batch size 100, 200 RPC/s
  per forwarder, resend after 4 s if not included, with 20% FIFO picks and 80%
  highest-tip picks
  ([tx-forwarding README](https://github.com/base/base/blob/8bb377afad0d3521381487fc99edd10640418a63/crates/execution/tx-forwarding/README.md)).
  The builder "inserts them directly into the pool without re-validating
  signatures, since the sender addresses are trusted from the forwarding
  mempool node". The batch cap is `MAX_VALIDATED_INSERT_BATCH = 1024`
  ([builder/rpc.rs L41-L85](https://github.com/base/base/blob/8bb377afad0d3521381487fc99edd10640418a63/crates/execution/txpool/src/builder/rpc.rs#L41-L85)).
- **Base Flashblocks are gone.** Denim (mainnet 2026-11-05) replaced
  Flashblocks with canonical 200 ms blocks
  ([Denim 200ms blocks](https://docs.base.org/upgrades/denim/200ms-blocks)).
- **Arbitrum Timeboost → PGA.** Arbitrum docs describe a Priority Gas Auction
  replacing Timeboost's 60-second express lane and its 200 ms delay on
  everyone else ("PGA removes that delay"), with 125 ms ordering rounds
  ([Sequencer](https://docs.arbitrum.io/how-arbitrum-works/sequencer)). The
  PGA page still says it "activates on Arbitrum One some time after the onchain
  vote passes"
  ([PGA intro](https://docs.arbitrum.io/how-arbitrum-works/timeboost/how-to-use-timeboost)).
  Activation status: see Unverified. Nitro still carries the express-lane code
  paths (`PublishExpressLaneTransaction`)
  ([sequencer.go L614-L720](https://github.com/OffchainLabs/nitro/blob/ccc5c828d9ea22f161efc784bac4d4a6559b7e59/execution/gethexec/sequencer.go#L614-L720)).

## 2. Rate limiting

### proxyd (OP Labs' RPC edge)

| Aspect | Behaviour | Source |
| --- | --- | --- |
| Algorithm | **Fixed window** counter, not a token bucket. `truncateNow(dur)` buckets time; a key may take `max` per window | [frontend_rate_limiter.go L73-L140](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/frontend_rate_limiter.go#L73-L140) |
| In-memory state | A single `limitedKeys` map per window. When the truncated timestamp changes, the **whole map is replaced** ("reset limits"). There is no per-key eviction, no LRU, no cap; memory is O(distinct keys in one window) | same, L23-L91 |
| Redis state | `INCR rate_limit:<prefix>:<key>:<window>` + `PEXPIRE(dur-1ms)`, pipelined. State expires on its own. `FallbackRateLimiter` falls back to a secondary limiter if Redis errors | same, L93-L164 |
| Keys | Base limit plus optional per-method overrides, all keyed by the client IP. `Global: true` on an override does **not** create a shared bucket: it means the method stays limited even for exempt origins/UAs | [server.go L1163-L1219](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/server.go#L1163-L1219), [config.go L68-L83](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/config.go#L68-L83) |
| Client IP | Header `X-Forwarded-For` by default (`ip_header_override` to change), falling back to `RemoteAddr`. Takes the **leftmost** XFF entry, which is spoofable unless the load balancer in front overwrites the header | [server.go L1074-L1079](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/server.go#L1074-L1079), [backend.go L1942-L1945](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/backend.go#L1942-L1945) |
| Heavy users | `X-Api-Key` from a configured key list bypasses all frontend limits. `exempt_origins` and `exempt_user_agents` regexes exempt everything except "global" methods | [server.go L411-L417](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/server.go#L411-L417) |
| Sender limit | Optional limiter keyed `"<from>:<nonce>"`, so it caps resubmissions of the *same nonce* rather than a sender's throughput. It needs ECDSA recovery at the edge. Skipped for API-key traffic. Interval ≥ 1 s | [tx_filter_modules.go L16-L71](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/tx_filter_modules.go#L16-L71), [proxyd.go L110](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/proxyd.go#L110) |
| Bounded side state | Rejection metrics use a separate tracker capped at `maxRateLimitTrackerKeys = 50_000` hashed keys, flushed per window. On overflow it **only stops counting**; admission is unaffected | [rate_limit_tracker.go](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/rate_limit_tracker.go), [server.go L55](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/server.go#L55) |
| Other caps | Body 256 KiB default; method whitelist (unknown methods collapse to one metric label to prevent memory DoS); outbound concurrency semaphore `max_concurrent_rpcs` (example config 1,000) | [server.go L46, L626-L640](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/server.go#L626-L640), [proxyd.go L115-L125](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/proxyd.go#L115-L125), [example.config.toml L22-L25](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/example.config.toml#L22-L25) |

op-txproxy, which sits behind proxyd for `eth_sendRawTransactionConditional`
only, rate-limits that method at the proxy "to terminate these requests early"
(`--sendRawTxConditional.ratelimit`, default 5000)
([op-txproxy](https://docs.optimism.io/chain-operators/tools/op-txproxy)).

### Published public-endpoint limits

| Endpoint | Published statement | Numbers |
| --- | --- | --- |
| `mainnet.base.org` | "Our RPCs are rate-limited, they are not suitable for production apps" ([Base run a node](https://docs.base.org/specifications/node-operators/run-a-node)); "Public endpoints are rate-limited and are not suitable for production traffic" ([Base throughput and limits](https://docs.base.org/specifications/transactions/throughput-and-limits)) | none published |
| `mainnet.optimism.io` | "rate limited and do not support websocket connections" ([OP connecting](https://docs.optimism.io/op-mainnet/network-information/connecting-to-op)) | none published |
| `arb1.arbitrum.io/rpc` | "limits requests to prevent denial of service (DoS) attacks. Hitting the rate limit can result from either the frequency of your requests or the resources required to process them" ([Arbitrum troubleshooting](https://docs.arbitrum.io/build-decentralized-apps/troubleshooting-building)) | none published |
| `arb1-sequencer.arbitrum.io/rpc` | bounded in-memory queue of 1,024 txs, 12 s queue timeout, "context deadline exceeded … safe to retry" ([Arbitrum chain info](https://docs.arbitrum.io/build-decentralized-apps/reference/node-providers)) | queue 1,024 / 12 s |

How they avoid tying aggregate throughput to per-client state:

- Window-reset counters (above) bound memory by distinct keys per window, with
  no cap that admission must protect.
- There is no global admission cap at the edge. Global capacity is the
  sequencer's: a bounded queue or pool with fee-ordered eviction, plus the
  block gas limit.
- Base states that "Endpoint limits do not change Base protocol capacity, but
  they can become the practical bottleneck", and points production users to
  providers or their own node
  ([Base throughput and limits](https://docs.base.org/specifications/transactions/throughput-and-limits)).

## 3. Admission and validation on the hot path

### geth / op-geth (`eth_sendRawTransaction` → `txpool.Add`)

Synchronous, before the hash is returned:

| Step | Detail | Source |
| --- | --- | --- |
| Decode | `tx.UnmarshalBinary` | [api.go L1954](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/internal/ethapi/api.go#L1954) |
| RPC policy | fee cap sanity (`--rpc.txfeecap`, default 1 ether); EIP-155 required | [api.go L1856-L1866](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/internal/ethapi/api.go#L1856-L1866), [ethconfig L76](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/eth/ethconfig/config.go#L76) |
| Dedup | `pool.all.Get(hash)` before any signature work | [legacypool.go L1001-L1015](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/core/txpool/legacypool/legacypool.go#L1001-L1015) |
| Stateless checks, **outside the pool lock** | size ≤ `txMaxSize` = 4×32 KiB = 128 KiB; gas caps; **ECDSA sender recovery** (cached in the tx); intrinsic gas; min tip | [validation.go L84-L183](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/core/txpool/validation.go#L84-L183), [legacypool.go L52-L58](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/core/txpool/legacypool/legacypool.go#L52-L58) |
| Stateful checks, **under the pool lock** | nonce ≥ state nonce; balance ≥ cost (OP adds the L1 rollup cost via `RollupCostFn`); cumulative pending cost | [validation.go L292-L335](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/core/txpool/validation.go#L292-L335) |
| Pool full | if slots > `GlobalSlots+GlobalQueue`: reject `ErrUnderpriced` if the tx is cheaper than the cheapest pooled one, otherwise **evict the cheapest**. Throttle with `ErrTxPoolOverflow` if more than `GlobalSlots/4` changes since the last reorg | [legacypool.go L795-L820](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/core/txpool/legacypool/legacypool.go#L795-L820) |

Asynchronous: promotion and reorg (`requestPromoteExecutables`; the
`NewTxsEvent` broadcast happens in `runReorg`)
([legacypool.go L1221-L1399](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/core/txpool/legacypool/legacypool.go#L1221-L1399)).
`Add` takes a slice, so batches share one lock acquisition.

Defaults: `PriceLimit 1`, `PriceBump 10` (%), `AccountSlots 16`,
`GlobalSlots 5,120`, `AccountQueue 64`, `GlobalQueue 1,024`, `Lifetime 3h`
([legacypool.go L171-L185](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/core/txpool/legacypool/legacypool.go#L171-L185)).

Durability (this corrects the brief's premise). In current geth/op-geth,
RPC-submitted txs **are** tracked as locals, unless `--txpool.nolocals` is set.
`TxTracker.TrackAll` appends them to `transactions.rlp`, but the write is
`rlp.Encode` to an `O_APPEND` file, **with no fsync and with the error
discarded** (`_ = tracker.journal.insert(tx)`). Temporary rejects such as pool
full or underpriced return success, and the tracker resubmits them every minute
([api_backend.go L360-L384](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/eth/api_backend.go#L360-L384);
[tx_tracker.go L77-L115](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/core/txpool/locals/tx_tracker.go#L77-L115);
[journal.go L130-L148](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/core/txpool/locals/journal.go#L130-L148);
[locals/errors.go L29-L41](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/core/txpool/locals/errors.go#L29-L41)).
op-geth also adds `JournalRemote`, which journals the whole pool periodically
([legacypool.go L147-L150](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/core/txpool/legacypool/legacypool.go#L147-L150)).
In short, the ack is best-effort durable at most; it is never fsynced per tx.

### reth / op-reth

Validation runs on 1 critical plus N additional **blocking worker tasks**
(`--txpool.additional-validation-tasks`, default 1) fed by a bounded channel of
capacity 2×workers. That channel is the backpressure point
([validate/task.rs L95-L220](https://github.com/paradigmxyz/reth/blob/76c88b4f3eee1632d50824ebe0ccaeae0a65ec49/crates/transaction-pool/src/validate/task.rs#L95-L220);
[config.rs L12-L35](https://github.com/paradigmxyz/reth/blob/76c88b4f3eee1632d50824ebe0ccaeae0a65ec49/crates/transaction-pool/src/config.rs#L12-L35)).
Other limits:

- `--txpool.max-batch-size` batches pool insertions "to reduce lock contention
  when processing many concurrent transaction pool insertions"
  ([batcher.rs](https://github.com/paradigmxyz/reth/blob/76c88b4f3eee1632d50824ebe0ccaeae0a65ec49/crates/transaction-pool/src/batcher.rs);
  [args/txpool.rs L415-L417](https://github.com/paradigmxyz/reth/blob/76c88b4f3eee1632d50824ebe0ccaeae0a65ec49/crates/node/core/src/args/txpool.rs#L415-L417)).
- Default subpool 10,000 txs / 20 MB, 16 slots per sender, 10% price bump,
  128 KiB max input
  ([config.rs](https://github.com/paradigmxyz/reth/blob/76c88b4f3eee1632d50824ebe0ccaeae0a65ec49/crates/transaction-pool/src/config.rs#L12-L35);
  [validate/constants.rs](https://github.com/paradigmxyz/reth/blob/76c88b4f3eee1632d50824ebe0ccaeae0a65ec49/crates/transaction-pool/src/validate/constants.rs)).

Base's builder pool additionally gates admission per account:

- a sender inflight-count cap;
- a payer cap where "the sum of the max cost of all sponsored transactions must
  not exceed the payer's balance", evicting the lowest priority first
  ([limits.rs L1-L17](https://github.com/base/base/blob/8bb377afad0d3521381487fc99edd10640418a63/crates/execution/txpool/src/limits.rs#L1-L17)).

### Arbitrum Nitro sequencer

Before queueing, `publishTransactionToQueue` does only cheap work
([sequencer.go L730-L790](https://github.com/OffchainLabs/nitro/blob/ccc5c828d9ea22f161efc784bac4d4a6559b7e59/execution/gethexec/sequencer.go#L730-L790)):

- an optional parent-chain "expected surplus" hard threshold;
- an optional sender whitelist (the only place it recovers the sender);
- a tx-type check;
- a non-blocking-with-timeout push onto `txQueue`, a Go channel of
  `queue-size` (default 1,024).

At block creation, every `max-block-speed` (250 ms default), the sequencer:

- drains up to `max-block-tx-candidates` (3,000);
- drops cancelled, oversized (> `max-tx-data-size` 95,000 B) and below-base-fee
  items;
- prechecks nonces through a nonce cache. Too-high nonces are parked in a
  `nonce-failure-cache` (1,024 entries, 1 s expiry) and revived when the
  predecessor lands;
- executes the transactions.

Sources:
[sequencer.go L209-L234 defaults](https://github.com/OffchainLabs/nitro/blob/ccc5c828d9ea22f161efc784bac4d4a6559b7e59/execution/gethexec/sequencer.go#L209-L234),
[L1039-L1215](https://github.com/OffchainLabs/nitro/blob/ccc5c828d9ea22f161efc784bac4d4a6559b7e59/execution/gethexec/sequencer.go#L1039-L1215).

**The RPC call blocks until the tx has been sequenced.** `PublishTransaction`
waits on `resultChan`, which is filled after block execution
([L614-L648](https://github.com/OffchainLabs/nitro/blob/ccc5c828d9ea22f161efc784bac4d4a6559b7e59/execution/gethexec/sequencer.go#L614-L648),
[L1440-L1491](https://github.com/OffchainLabs/nitro/blob/ccc5c828d9ea22f161efc784bac4d4a6559b7e59/execution/gethexec/sequencer.go#L1440-L1491)).
The docs confirm: "A successful `eth_sendRawTransaction` response means that the
sequencer has already put your transaction in an L2 block"
([Arbitrum chain info](https://docs.arbitrum.io/build-decentralized-apps/reference/node-providers)).

Forwarding nodes run `PreCheckTx` at strictness 20 by default
([tx_pre_checker.go L43-L66, L137-L240](https://github.com/OffchainLabs/nitro/blob/ccc5c828d9ea22f161efc784bac4d4a6559b7e59/execution/gethexec/tx_pre_checker.go#L137-L240)).
It checks:

- gas ≥ 21,000 and the tx type;
- sender recovery;
- fee cap ≥ base fee;
- nonce not too low;
- intrinsic gas;
- balance ≥ cost;
- intrinsic gas plus L1 data gas.

At strictness 30 it also rejects nonce gaps.

## 4. Batching

| Layer | Batch support | Limit | Rate accounting |
| --- | --- | --- | --- |
| geth/op-geth JSON-RPC | yes | `--rpc.batch-request-limit` 1,000; response 25 MB ([node/defaults.go L68-L69](https://github.com/ethereum-optimism/op-geth/blob/79e9d244748a1201fe686ca8a882b5fe7e173348/node/defaults.go#L68-L69)) | none at the node |
| proxyd frontend | yes | `[batch] max_size` default 100, hard cap 1,000; upstream batches split to 10 ([server.go L42-L53, L468](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/server.go#L42-L53)) | **each element** takes the base and per-method limit ("rate limited individual RPC in a batch request") ([server.go L644-L668, L774](https://github.com/ethereum-optimism/infra/blob/6634d7a86a2203eea9324fac17226059b2ed3d0b/proxyd/server.go#L644-L668)) |
| Base mempool → builder | `base_insertValidatedTransactions` | ≤ 1,024 per call; forwarder batch 100 by default | 200 RPC/s per forwarder ([tx-forwarding README](https://github.com/base/base/blob/8bb377afad0d3521381487fc99edd10640418a63/crates/execution/tx-forwarding/README.md)) |
| reth pool insert | internal batcher | `--txpool.max-batch-size` (default 1) | n/a |
| Nitro sequencer | internal: drains up to 3,000 queued items per block | n/a | n/a |

None of the three chains offers a *protocol-level* "submit many txs in one tx"
path on the public endpoint. Batch arrays amortize HTTP/TLS cost only.

## 5. Horizontal scaling and throughput

- **Fan-in.** Reads are served by replicas behind proxyd or providers. Writes
  fan in to one active sequencer or builder set: op-conductor for OP; a
  Redis-coordinated forwarder for Nitro; the Base mempool tier forwarding to
  all builders. The sequencer absorbs only `eth_sendRawTransaction`.
  Dedicated write-only URLs (`*-sequencer.*`) exist so reads never reach it.
- **How the sequencer is protected.** Replicas reject invalid txs before
  forwarding (Nitro pre-checker, reth/geth pool validation on Base mempool
  nodes). The builder skips signature work (Base). Queues are bounded with
  timeouts (Nitro 1,024 / 12 s). Pools are bounded with price eviction
  (geth/reth). The Base forwarder resends with a per-builder bounded queue, so
  "a slow builder backpressures only its own reader".

| Chain | Published capacity | ≈ 21k-gas transfers/s | Source |
| --- | --- | --- | --- |
| Base | 40M gas/block, 200 ms blocks → **200 Mgas/s limit**; elasticity 5 → **40 Mgas/s target** | 9,524 at limit; 1,905 at target | [Base throughput and limits](https://docs.base.org/specifications/transactions/throughput-and-limits), [Base network fees](https://docs.base.org/specifications/transactions/network-fees) |
| Base (observed) | "sustained multiple bursts of over 5,000 TPS"; goal "1 gigagas/s" (Apr 2026) | — | [Introducing Base Azul](https://base.org/blog/introducing-base-azul) |
| Base (history) | Jun 2025: target 35 → 25 Mgas/s, limit 70 → 75 Mgas/s, because the target was above organic demand and attracted spam | — | [Scaling Base: sustain 10x growth](https://base.org/blog/scaling-base-sustain-10x-growth) |
| OP Mainnet | block gas limit 40M; 2 s blocks (see Unverified) → 20 Mgas/s | ≈ 952 at limit | [OP subblocks & gas](https://docs.optimism.io/app-developers/guides/transactions/subblocks-and-gas-usage) |
| Arbitrum One | block gas limit 32M; multi-gas-target pricing 60 Mgas/s (9 s window) … 10 Mgas/s (86,400 s window); sequencer closes blocks at 32 Mgas target / 64 Mgas hard per block, up to 8 blocks/s under load | 2,857 at the 60 Mgas/s short target; 476 at the 10 Mgas/s day target | [Arbitrum chain params](https://docs.arbitrum.io/for-devs/chain-params), [Sequencer](https://docs.arbitrum.io/how-arbitrum-works/sequencer) |

Arbitrum `gasUsed` also includes the L1-data component, so a simple transfer
costs more than 21k gas there
([Arbitrum gas estimation](https://docs.arbitrum.io/arbitrum-essentials/how-to-estimate-gas)).
The conversions above are upper bounds for the cheapest transaction type.
Base's own docs warn that "There is no single transactions-per-second value"
([Base throughput and limits](https://docs.base.org/specifications/transactions/throughput-and-limits)).

## 6. Spam and DoS economics

The fee floor is the primary spam control; rate limits are coarse
endpoint-abuse guards.

| Mechanism | Base | OP Mainnet | Arbitrum One |
| --- | --- | --- | --- |
| Base-fee floor | min base fee 0.005 gwei (Jovian); docs list "Spam Prevention: Extremely low fees can incentivize spam" ([Base fees](https://docs.base.org/specifications/transactions/network-fees)) | configurable since Jovian; OP Mainnet value not checked | gas price floor 0.02 gwei ([chain params](https://docs.arbitrum.io/for-devs/chain-params)) |
| Congestion pricing | EIP-1559, denominator 1000, max +0.4%/block | EIP-1559 | multi-window gas targets raise the base fee above target |
| Ordering | priority fee | "priority fee order (highest fee first)" ([OP differences](https://docs.optimism.io/op-stack/protocol/differences)) | FCFS → PGA (priority fee in 125 ms rounds, anti-starvation boost) |
| Replacement | ≥10% bump ([Base `eth_sendRawTransaction`](https://docs.base.org/base-chain/api-reference/ethereum-json-rpc-api/eth_sendRawTransaction)) | geth 10% `PriceBump` | no replaceable mempool; nonce-failure cache only |
| Pool-full policy | price eviction (reth/geth) | price eviction (geth) | queue timeout returns "context deadline exceeded" |
| Other | DA throttling: "even transactions with high priority fees may be delayed" | — | `max-revert-gas-reject`; expected-surplus hard threshold rejects all new txs when the batch poster is losing money ([sequencer.go L248, L736](https://github.com/OffchainLabs/nitro/blob/ccc5c828d9ea22f161efc784bac4d4a6559b7e59/execution/gethexec/sequencer.go#L736)) |

Base's June 2025 post attributes spam to an oversized gas target and proposes a
minimum base fee as the fix
([Scaling Base](https://base.org/blog/scaling-base-sustain-10x-growth)). Base
later shipped that fee in Jovian.

Without a public mempool, Arbitrum rejects at the forwarder (pre-checker: fee
cap ≥ base fee, balance ≥ cost) and at the sequencer (base fee, size, nonce).
Every accepted tx must pay at least the floor times the full gas including L1
data, so the cost of a flood scales with the bytes it carries.

## 7. Transferable to Midgard

Midgard's context: one Node process per operator; per-tx CBOR decode, canonical
checks, ed25519 witness checks and UTxO-state checks; a durable Postgres
admission queue; UTxO model.

### Local measurement (not a primary source)

Node v24.13.1 on a Ryzen 9 9950X3D: `crypto.verify` ed25519 takes ≈ 52 µs; a
1 KiB blake2b-512 takes ≈ 2.2 µs. At 1,000 TPS with two witnesses each, that is
≈ 0.1 core-seconds per second. Signature checks are therefore unlikely to be the
1,000 TPS bottleneck. The current 4/s cap is a policy number, not a CPU limit.

### Patterns and how they transfer

| Pattern | Transfers? | Midgard form |
| --- | --- | --- |
| No global edge cap; capacity set by fee floor + bounded queue | **Yes** | Remove the global `write` bucket as the throughput governor. Bound the admission queue (rows/bytes, as today) and when full evict or reject by **fee per byte** instead of first-come. A minimum fee per byte is the UTxO analogue of the minimum base fee: Cardano-style fees already scale with size. |
| Window-reset counters instead of an evicting peer table | **Yes** | Per-IP fixed-window counters in a map swapped every window (proxyd `MemoryFrontendRateLimiter`). Memory is bounded by keys per window, with no `HTTP_PEER_MAX_ENTRIES` coupling and no idle-eviction proof obligation. Keep a hard map-size safety valve that rejects new keys only, without touching the global rate. |
| API keys / exempt origins for heavy users | **Yes** | An `X-Api-Key` bypass for wallets, indexers and the CLI, as in proxyd. Trust `X-Forwarded-For` only from a configured proxy (proxyd takes the leftmost entry, which is spoofable without an LB that overwrites it). |
| Per-account nonce limits (`AccountSlots`, sender:nonce limiter) | **No direct analogue** | Use per-**input** exclusivity: one pending tx per spent outref (a mempool double-spend check). That bounds a spammer by the UTxOs they own, as Base's payer book bounds by balance. Chained spends of pending outputs map to Nitro's nonce-failure cache: park briefly (1 s / 1,024 entries in Nitro) or reject. |
| Replacement with ≥10% bump | **Partly** | A conflicting tx spending the same input may replace the pending one only with a higher fee (RBF). Otherwise first-seen wins. |
| Stateless checks outside the lock, stateful inside; dedup first | **Yes** | Order: hash dedup → size cap → CBOR decode + canonical checks + ed25519 (pure, parallelizable) → UTxO lookup and input reservation (serialized). Only the last step needs the single writer. |
| Validation worker pool (reth: blocking tasks behind a bounded channel) | **Yes** | Move decode and ed25519 into `worker_threads`, with a bounded channel as the backpressure point instead of an ingress concurrency of 4. |
| Validate at the edge, forward pre-validated batches (Base, Nitro pre-checker) | **Yes, later** | Stateless ingress replicas (processes or hosts) that decode and verify, then forward batches to the operator's admitter. Base trusts the pre-recovered sender. Midgard can forward the verified tx bytes plus the verification verdict if replicas are in the operator's trust domain. |
| Ack after in-memory admission, no per-tx fsync | **Adapt** | Midgard needs durability for its liveness guarantees, so keep durable admission but **group-commit**: accumulate validated txs for ≤ N ms (or M rows) and insert them in one Postgres transaction or `COPY`, acking all of them after commit. This amortizes the WAL flush (`synchronous_commit=on`) across many txs, a durability geth does not offer at all. |
| Ack after sequencing (Arbitrum) | **Optional** | A `submit-and-wait` variant could return after block inclusion. That is a client convenience; the default stays ack-on-admit. |
| JSON-RPC batch arrays, each element charged | **Yes** | Add a batch submit (≤ 100–1,000 txs). Charge each element against per-key limits, and amortize decode, the DB transaction and HTTP overhead. |
| Dedicated write-only endpoint | **Yes** | Separate the submit listener (or path class) from reads so read floods cannot starve submits. Midgard already has class-separated tables; also give submits their own concurrency pool. |

What does not transfer:

- account-nonce gap handling (`GlobalQueue`, `AccountQueue`);
- the L1-data-gas surcharge model;
- Timeboost/PGA ordering auctions. Midgard ordering is per-operator, and an
  auction is out of scope.

## Unverified

- Actual numeric rate limits on `mainnet.base.org`, `mainnet.optimism.io`,
  `arb1.arbitrum.io/rpc` and the `*-sequencer.*` URLs. None is published.
- Whether Base's and OP's public endpoints run proxyd, and with what config.
  Base's event-journal docs name a `base-routing/proxyd` edge producer; OP's
  production proxyd config is not public.
- What sits in front of `mainnet-sequencer.optimism.io`,
  `mainnet-sequencer.base.org` and `arb1-sequencer.arbitrum.io` (load balancer,
  CDN, proxyd, mempool tier), and whether the sequencer process is ever
  directly reachable.
- Whether Arbitrum One's PGA has activated (the docs describe it as replacing
  Timeboost, while the PGA page says activation is pending after the DAO vote),
  and its activation date.
- Arbitrum sustained or peak TPS: no Offchain Labs first-party figure found.
  Third-party trackers disagree.
- Base's "5,000 TPS bursts" is Base's own claim; it was not independently
  measured.
- OP Mainnet block time (2 s) as a current published parameter (seen only as
  docs defaults and examples), plus OP Mainnet's elasticity and minimum base
  fee.
- Whether the Base builder re-checks nonce and balance on
  `base_insertValidatedTransaction(s)`. The source comment only says
  signatures are skipped.
- Whether Nitro persists the block to disk before `eth_sendRawTransaction`
  returns. The docs and code show it returns after block creation and
  execution.
- Production values of `--txpool.additional-validation-tasks` on Base
  mempool nodes.
- Whether proxyd's per-backend `max_rps` is enforced. At this commit it is
  parsed into the backend struct, but no enforcement use was found.
- geth ECDSA recovery cost per tx. It was not benchmarked here; only Node
  ed25519 was measured locally.
