# Midgard Node Go

This guide explains how to interact with the node's API to add transactions to the mempool and retrieve all transactions from the mempool.

## 1. Add a Transaction to the Mempool

Use the following `curl` command to add a transaction to the node's mempool:

```bash
curl -X POST localhost:8080/v1/tx \
  -H "Content-Type: application/json" \
  -d '{"id":"tx2", "cbor":"demo-data-2"}'
```

### Endpoint

- `POST /tx`

### Request Body

```json
[
  {
    "id": "tx1",
    "cbor": "demo-data-1"
  },
  {
    "id": "tx2",
    "cbor": "demo-data-2"
  }
]
```

- **id**: Unique identifier for the transaction.
- **payload**: The data associated with the transaction.

## 2. Retrieve All Transactions from the Mempool

Use the following `curl` command to fetch all transactions currently in the mempool:

```bash
curl localhost:8080/v1/mempool
```

### Endpoint

- `GET /mempool`

### Response

A JSON array of transactions in the mempool. Example:

```json
[
  {
    "id": "tx1",
    "cbor": "demo-data-1"
  },
  {
    "id": "tx2",
    "cbor": "demo-data-2"
  }
]
```
