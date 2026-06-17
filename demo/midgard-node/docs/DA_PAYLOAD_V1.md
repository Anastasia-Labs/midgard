# DaPayloadV1 Producer Contract

`midgard-node` is the producer for canonical `DaPayloadV1` objects. The DA
committee consumes these objects, but committee storage, validation, serving, and
signing are outside this node contract.

## Shape

`DaPayloadV1` is headerless. The authoritative header lives on Cardano L1 in the
state-queue UTxO identified by `header_hash`.

```text
DaPayloadV1 {
  version: 1
  header_hash: HeaderHash
  block_body: {
    utxos
    transactions
    deposits
    withdrawals
  }
}
```

The payload is encoded as canonical Midgard SDK data CBOR. Each body set is a
sorted sequence of `(key_cbor, value_cbor)` byte pairs.

## Production

The node builds the payload from its durable pending-finalization journal and
the finalized `mempool_ledger` UTxO state during local block finalization. Before
persisting a payload, it recomputes all body roots from the payload body entries:

- `utxos_root`
- `transactions_root`
- `deposits_root`
- `withdrawals_root`

UTXO, deposit, and withdrawal roots use the stored body entry values directly.
Transaction entries store full canonical transaction CBOR for availability, but
`transactions_root` is recomputed from each transaction's compact canonical root
projection. This matches the block commitment root and lets consumers obtain full
transaction data without changing the committed transaction-root semantics.

The recomputed roots must match the pending block commitment roots that were
checked against the on-chain state-queue header.

## API

```text
GET /da/payload?header_hash=<28-byte hex>
GET /da/payload/metadata?header_hash=<28-byte hex>
```

`GET /da/payload` returns canonical CBOR with content type `application/cbor`.

`GET /da/payload/metadata` returns convenience metadata: payload hash, payload
size, roots, block time bounds, and creation/update times. Consumers must still
fetch the authoritative header from L1 by `header_hash`.

## Retention

Payload retention uses the node-wide `RETENTION_DAYS` setting. Production
deployments must set this above the challenge window. `RETENTION_DAYS=0`
disables pruning. Any positive value must be at least 8 days, which exceeds the
currently expected 3-7 day production challenge window with a small operational
margin.
