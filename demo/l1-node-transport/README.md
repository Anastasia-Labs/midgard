# @al-ft/l1-node-transport

The one node-to-client (N2C) transport of a Midgard role. A long-lived Go
sidecar (`native/`, built on gouroboros) holds the role's connection to its
local `cardano-node`. The TypeScript client (`src/`) supervises the sidecar
and offers four things:

- chain-sync streams with a credit window;
- local state queries;
- transaction submission;
- mempool membership.

The transport only moves bytes. It makes no qualification, depth, finality
or validity decision. Raw blocks, transactions, ledger answers and rejection
reasons cross the boundary as the node produced them. They are never
re-encoded and never hex-encoded.

```sh
pnpm --dir demo --filter @al-ft/l1-node-transport run native:build  # dist/native/midgard-l1-node-transport
pnpm --dir demo --filter @al-ft/l1-node-transport run native:test   # go vet + go test -race (mock node)
pnpm --dir demo --filter @al-ft/l1-node-transport run test          # TS client against the compiled binary + mock node
```

The opt-in real-node conformance needs a phase4 devnet run directory whose
`cardano-node` is up:

```sh
MIDGARD_L1_TRANSPORT_DEVNET_RUN_DIR=<run dir> pnpm --dir demo --filter @al-ft/l1-node-transport exec vitest run tests/devnet.test.ts
```

## Client

```ts
const transport = sharedL1NodeTransport({
  binaryPath,
  socketPath,
  networkMagic,
});
await transport.whenReady(30_000);
const stream = transport.openChainSync({ points, credit: 50 });
for await (const event of stream) {
  // persist event, then:
  stream.ack(event.seq);
}
```

The client has these behaviours:

- **Supervision.** The sidecar is restarted with doubling backoff, from
  250 ms up to 30 s. The backoff resets after 30 s of stable running. The
  supervisor never exits the host process. A sidecar that ends with a
  `fatal` code no restart repairs (`TRANSPORT_FAILED_REASONS`:
  `node_handshake_failed`, `version_unsupported`, `malformed_frame`,
  `client_protocol_violation`) is not restarted: the transport is failed
  until the host restarts it.
- **Readiness.** `readiness` and `onReadiness` report
  `{ready: true, nodeToClientVersion}`, `{ready: false, reason, detail}`
  while the transport is unready and retrying, or
  `{ready: false, failed: true, reason, detail}` once it has failed. The
  unready reason is one of:

  - `sidecar_starting`
  - `sidecar_restarting`
  - `sidecar_unavailable`
  - `node_unreachable`
  - `node_connection_lost` (also a connection the node dropped during the
    handshake)
  - `stopped`

  The failed reason is one of `TRANSPORT_FAILED_REASONS`;
  `node_handshake_failed` means the node refused the N2C handshake (wrong
  network magic, no common version, or an undecodable handshake answer).

- **Calls while unready.** A call made while the transport is unready
  rejects with `TransportUnavailableError(reason)`. Once the transport has
  failed, every call, `whenReady` and every open stream rejects with
  `TransportFailedError(reason)`.
- **Timeouts.** The sidecar refuses a ledger-state request or a submission
  that the node has not answered within half of `requestTimeoutMs` with
  `TransportRequestError(node_timeout)`. The sidecar and its streams stay
  up (see "Request deadline"). A call that still exceeds
  `requestTimeoutMs` rejects with `TransportTimeoutError` and kills the
  sidecar, which is then restarted.
- **Chain-sync streams.** A stream survives sidecar restarts. It reopens
  from its last delivered point (see "Resume") unless it was opened with
  `resume: false`. It reopens after a stream failure only when the failure
  is transient (`STREAM_REOPEN_CODES`: `node_connection_lost`,
  `node_unavailable`, `busy`). Any other `cs_failed` code or refused
  reopen ends the stream with `TransportRequestError(code)`, since a
  reopen would meet the same block or request.
- **Credit.** Credit is either a fixed window or an adaptive
  `CreditPolicy`. The adaptive policy uses `catchUpWindow` while the
  stream is more than `catchUpDistance` blocks behind the tip, and
  `tipWindow` otherwise.
- **Ledger-state queries.** `withLedgerState(point | "tip", use)` runs
  every query in `use` against one acquired ledger state, then releases
  it. Sessions are serialized. `query(q)` is a one-query session at the
  tip.
- **Shared instances.** `sharedL1NodeTransport` returns one instance per
  (binary, socket, magic, request timeout). Every caller in a process shares that instance,
  so a role holds one node connection.

## Frame protocol (version 1)

The sidecar speaks frames on stdin (client to sidecar) and stdout (sidecar
to client). Stderr carries free-text diagnostics only.

```
u32 big-endian headerLength | u32 big-endian payloadLength | header | payload
```

- **header.** One CBOR map with text keys. It is 1..65,536 bytes; a zero
  header length is malformed.
- **header encoding.** Tags, indefinite lengths, duplicate keys and unknown
  keys are refused. Nesting is at most 16 levels, with at most 4,096
  array elements and 64 map pairs.
- **payload.** Raw bytes, 0..64 MiB. Only `submit` (client),
  `cs_roll_forward`, `lsq_result` and `submit_rejected` (sidecar) carry
  one. Any other frame with a payload is malformed.
- **frame order.** Frames on one side are written whole and in order.
- **request ids.** Requests carry a client-chosen `id` (an unsigned
  integer). The answer echoes it. Answers to different request kinds may
  interleave.

### Encodings

| Name       | CBOR                              |
| ---------- | --------------------------------- |
| point      | `[]` (origin) or `[slot, hash32]` |
| tip        | `[point, blockNo]`                |
| tx input   | `[txId32, index]`                 |
| credential | `[0 (key) or 1 (script), hash28]` |

### Handshake

The first client frame must be `hello`:

```
{type: "hello", version: 1, socketPath: text, networkMagic: uint, requestDeadlineMs: uint}
```

`requestDeadlineMs` (1..86,400,000) bounds each ledger-state request and
submission (see "Request deadline").

The sidecar dials the node and runs the N2C handshake. On success it
answers `{type: "hello_ok", version: 1, nodeToClientVersion: uint}`. A
handshake the node refuses ends the session with `fatal
node_handshake_failed`; a connection the node drops during the handshake
ends it with `fatal node_connection_lost`. Both exit with status 69.

`midgard-l1-node-transport --protocol-version` prints the frame protocol
version (`1`) and exits.

A version mismatch ends the session with `fatal version_unsupported` and
exit status 64.

### Client to sidecar

| type             | fields                                                              | answer                                                           |
| ---------------- | ------------------------------------------------------------------- | ---------------------------------------------------------------- |
| `cs_open`        | `id, stream, points (1..256), startSeq, ackedSeq?, window (1..100)` | `cs_opened` or `cs_intersect_not_found` or `error`               |
| `cs_window`      | `stream, window (1..100)`                                           | none                                                             |
| `cs_ack`         | `stream, seq`                                                       | none                                                             |
| `cs_close`       | `id, stream`                                                        | `ok` (no frame of that stream follows it)                        |
| `lsq_acquire`    | `id, point?` (absent: the volatile tip)                             | `ok` or `error`                                                  |
| `lsq_release`    | `id`                                                                | `ok`                                                             |
| `lsq_query`      | `id, query, addresses? / txIns? / credentials?`                     | `lsq_result` (+ raw answer) or `error`                           |
| `submit`         | `id, era?` + payload: the raw transaction                           | `submit_accepted` or `submit_rejected` (+ raw reason) or `error` |
| `monitor_has_tx` | `id, txId (32 bytes)`                                               | `monitor_has_tx_result {has}`                                    |
| `monitor_sizes`  | `id`                                                                | `monitor_sizes_result {capacity, size, txCount}`                 |

### Sidecar to client

| type                     | fields                                                                            |
| ------------------------ | --------------------------------------------------------------------------------- |
| `cs_opened`              | `id, stream, point (the intersection), tip`                                       |
| `cs_intersect_not_found` | `id, stream, tip`                                                                 |
| `cs_roll_forward`        | `stream, seq, point, blockNo, blockType, prevHash?, tip` + payload: the raw block |
| `cs_roll_backward`       | `stream, seq, point, tip`                                                         |
| `cs_failed`              | `stream, code, message`: the stream ended; the session lives on                   |
| `lsq_result`             | `id` + payload: the node's raw answer                                             |
| `submit_accepted`        | `id`                                                                              |
| `submit_rejected`        | `id` + payload: the node's raw rejection reason                                   |
| `error`                  | `id, code, message`: one request refused; the session lives on                    |
| `fatal`                  | `code, message`: the session ends; it is the last frame                           |

### Chain-sync streams

- **Stream ids.** Each stream id must be above every stream id opened
  before it.
- **Intersection.** `cs_open` runs one `FindIntersect` over all of
  `points`. The node picks the first point of the list it knows.
- **Sequence numbers.** Every event carries the next `seq`. The first
  event after `cs_opened` is `startSeq + 1`.
- **Credit.** The sidecar sends a `RequestNext` only while
  `inFlight + (lastSeq - ackedSeq) < window`. `inFlight` counts the
  `RequestNext` messages outstanding. Requests already pipelined when the
  window shrinks are still answered, so the delivered-but-unacked events
  stay below the largest window in effect while those requests were in
  flight, not below the new one. The requests are pipelined on the wire
  (gouroboros v0.207 or later; earlier releases race their pipelined send
  path against the state machine and fail the connection). After the node
  answers `AwaitReply` at the tip, gouroboros holds further requests until
  agency returns, so the stream queues requests from a separate goroutine
  and keeps reading replies, acknowledgements and its close meanwhile.
  `ackedSeq` defaults to `startSeq`.
  - `cs_ack` must lie within `[ackedSeq, lastSeq]`.
  - `cs_window` changes the window.
- **Rollback to the intersection.** The first of `points` is the
  consumer's current position. The node's first reply after
  `FindIntersect` is a rollback to the intersection. The sidecar suppresses
  it when the intersection is the first point, since the consumer is
  already there. Otherwise it delivers the rollback as the stream's first
  event, `startSeq + 1`, so the consumer unwinds to the intersection before
  any block follows. A first reply that is not a rollback to the
  intersection is `protocol_violation`.
- **Close.** `cs_close` is answered at once, and no frame of the stream
  follows the answer. An auxiliary connection is then closed. On the
  primary connection, a `RequestNext` still in flight must be answered
  before its chain-sync instance can be lent again. At the tip that takes
  until the next block, about 20 s on mainnet. A stream opened meanwhile
  gets an auxiliary connection, so the node briefly sees one more
  connection. Nothing else changes.
- **Block identity.** `point`, `blockNo` and `prevHash` come from decoding
  only the block header; `prevHash` is absent at the chain's first block.
  The body is hashed and checked against the header's body hash, so a
  delivered block's transactions are the ones its hash names; a mismatch
  ends the stream with `block_body_mismatch`. Shelley-to-Mary blocks hash
  three body segments, Alonzo-to-Conway four, and a Dijkstra block its one
  body element; Byron blocks are not checked. The body is not otherwise
  decoded or validated.
- **Connections.** The first open stream uses the primary connection. A
  concurrently open stream gets an auxiliary N2C connection carrying
  chain-sync only. Up to 4 idle auxiliary connections are kept for reuse.
- **Primary-connection fault.** A fault on the primary connection is
  `fatal`.
- **Auxiliary-connection fault.** A fault on an auxiliary connection ends
  only its stream, with `cs_failed`.

### Resume

The client remembers, per stream:

- the last delivered `seq` and point;
- the acked `seq`;
- the 64 most recent forward points;
- the intersection;
- the original points.

Each `cs_failed` a resuming stream reopens from, and each refused reopen,
is recorded in `stream.interruptions` (`consecutive` since the last
delivered event, `total`, `last`) and passed to the `onInterrupted` option,
so a reopen loop is visible to the consumer.

After a sidecar restart (or `cs_failed`) it reopens the stream as follows:

- **points.** `[last point, recent forward points newest first,
intersection, original points]`, deduplicated, at most 256.
- **sequence and position.** `startSeq = lastSeq`, `ackedSeq = acked`.
  The last point comes first, as the consumer's position.
- **No rollback needed.** If the node still has the last point, delivery
  continues at `lastSeq + 1` with the next block: no gap, no duplicate.
- **Rollback needed.** Otherwise the node intersects at an older point, and
  that rollback arrives as `lastSeq + 1` before any block.
- **No intersection.** If no point intersects, the stream fails with
  `IntersectNotFoundError(resuming: true)`.

### Ledger state queries

- **Acquisition.** `lsq_acquire` acquires the given point, or the volatile
  tip when no point is given. It replaces any previous acquisition.
- **Query without acquisition.** Every `lsq_query` needs an acquisition and
  otherwise answers `error not_acquired`.
- **Era wrapper.** Shelley-based queries run in the node's current era.
  The sidecar removes the hard-fork era-match wrapper. A mismatch answers
  `error era_mismatch`.

### Request deadline

Each `lsq_acquire`, `lsq_release`, `lsq_query` and `submit` must be
answered within the hello's `requestDeadlineMs`, counted from the arrival
of its frame, so time spent queued counts.

- **Missed deadline.** A request the node has not answered in time is
  answered `error node_timeout`. The session and every stream carry on.
- **Late replies.** The node's late reply is still owed. The sidecar takes
  it before it sends the next request of that kind, within that request's
  deadline, and never hands it to another request. A late acquisition, or
  a late acquire failure, changes what is acquired.
- **Waiting requests.** A request whose deadline passes while the late
  reply has not arrived, or while it waits in the queue, is answered
  `error node_timeout` without being sent.
- **Submissions.** A submission answered `node_timeout` has an unknown
  outcome: the node may still accept it.

LocalTxMonitor has its own fixed 30 s bound, and missing it is
`fatal node_unresponsive`.

| query                              | parameters    | raw answer                                          |
| ---------------------------------- | ------------- | --------------------------------------------------- |
| `system_start`                     |               | `[year, dayOfYear, picosecondsOfDay]`               |
| `chain_block_no`                   |               | `[1, blockNo]` or `[0]` (origin)                    |
| `chain_point`                      |               | point                                               |
| `current_era`                      |               | era index                                           |
| `era_history`                      |               | the hard-fork interpreter summary                   |
| `protocol_params`                  |               | the current era's protocol parameters               |
| `utxo_by_address`                  | `addresses`   | map of tx input to output                           |
| `utxo_by_txin`                     | `txIns`       | map of tx input to output                           |
| `stake_deleg_deposits`             | `credentials` | map of credential to deposit                        |
| `filtered_delegations_and_rewards` | `credentials` | `[{credential: poolHash28}, {credential: rewards}]` |

Parameter lists hold 1..4,096 items.

### Submission and mempool

- **Submit.** `submit` sends the payload with `era` when it is given. Without
  `era`, the sidecar reads the era from the transaction's own encoding; an
  undeterminable era answers `error tx_undecodable`.
- **Rejection.** A rejection's payload is the node's raw `ApplyTxErr` bytes.
- **Undecodable transactions.** A transaction the node cannot decode makes
  the node close the connection. The sidecar then ends with `fatal
node_connection_lost` and the supervisor restarts it.
- **Mempool snapshots.** `monitor_has_tx` and `monitor_sizes` each acquire
  a fresh mempool snapshot and release it.
- **Transaction ids.** `MsgHasTx` carries the hard-fork transaction id
  `[era, txId]`, using the Conway index. The node matches ids by hash, so
  this finds a transaction of any Shelley-based era. The stock gouroboros
  `HasTx` sends the bare hash, which the node cannot decode, so the sidecar
  drives LocalTxMonitor itself. The mock node refuses the bare form as the
  node does.

### Codes

`error` codes are:

- `not_acquired`
- `era_mismatch`
- `acquire_point_too_old`
- `acquire_point_not_on_chain`
- `acquire_failed`
- `unknown_query`
- `invalid_request`
- `invalid_points`
- `busy` (more than 256 queued requests of one kind)
- `result_too_large`
- `tx_undecodable`
- `monitor_unavailable`
- `node_unavailable` (an auxiliary connection could not be opened)
- `node_timeout` (see "Request deadline")
- `protocol_violation` (an auxiliary connection's node answered
  `FindIntersect` out of protocol)
- `invalid_window`
- `invalid_ack`

`cs_failed` codes are:

- `node_connection_lost`
- `protocol_violation`
- `block_header_undecodable`
- `block_body_mismatch` (the block's body does not hash to its header's body
  hash)
- `block_bounds`

`fatal` codes are:

- `version_unsupported`
- `malformed_frame`
- `client_protocol_violation`
- `node_unreachable`
- `node_handshake_failed`
- `node_connection_lost`
- `node_unresponsive`
- `output_closed`
- `protocol_violation`
- `block_header_undecodable`
- `block_body_mismatch`
- `block_bounds`

A refusal of a client frame that breaks the protocol is `fatal
client_protocol_violation`. Examples include a bad window in `cs_open`, an
ack outside the delivered range, or a payload on a frame that takes none.

### Exit status

| status | meaning                                                                        |
| ------ | ------------------------------------------------------------------------------ |
| 0      | orderly: stdin closed, or SIGINT/SIGTERM                                       |
| 1      | fatal after the handshake                                                      |
| 64     | client misuse: malformed frame, bad hello, unsupported version, bad arguments  |
| 69     | the node was unreachable, dropped the connection, or refused the N2C handshake |
