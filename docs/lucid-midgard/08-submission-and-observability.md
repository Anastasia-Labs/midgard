# Submission and Observability

Submission in Midgard is durable admission first, not final transaction
validity.

## Submit Flow

`CompleteTx.submit()` on a witness-complete signed `CompleteTx` should:

1. Verify signatures locally.
2. Submit native tx hex to `/submit`.
3. Parse durable admission response.
4. Return a `SubmittedTx` object.

The durable admission response must be validated before exposure: tx id must
match the submitted native tx id, `httpStatus` must be `200` or `202`, `status`
must be one of the durable node admission states
(`queued | validating | accepted | rejected`), timestamp fields must be strings
when present, and `duplicate` must be a required boolean. HTTP `202` is a new
admission and must be `queued` with `duplicate=false`; HTTP `200` is a
same-byte duplicate admission and must use `duplicate=true`.

The response may be:

- New admission, HTTP `202`.
- Duplicate same-byte admission, HTTP `200`.
- Tx id bytes conflict, HTTP `409`.
- Backlog full, HTTP `503`.
- Payload or normalization failure, HTTP `400` or `413`.

## SubmittedTx

`SubmittedTx` should expose:

- `txId`
- `admissionStatus`
- `duplicate`
- `firstSeenAt`
- `lastSeenAt`
- `awaitStatus(options?)`
- `status()`

If the node status endpoint is unavailable, `awaitStatus` should fail with a
capability error rather than spin on unavailable functionality.

Submit and status helpers also expose safe-result and lazy `Effect` variants.
They preserve the same durable admission and status semantics as the Promise
methods.

## Structured Logs

The library should emit optional structured diagnostics:

- Builder operation summary.
- Native byte length.
- Fee iterations.
- Selected inputs.
- Change outputs.
- Tx id.
- Local validation result.
- Submit response metadata.

Diagnostics must not include seed phrases, private keys, or full signatures by
default.

## Retry Policy

Automatic submit retries are safe only for transport errors and backlog-full
responses when the same signed bytes are retried. Do not rebuild silently during
retry, because rebuilding may change tx id, fee, inputs, or validity interval.

## Conflicts

`E_TX_ID_BYTES_CONFLICT` means the same tx id already exists with different
normalized bytes. This is a hard integrity error. The library must not
auto-resubmit with modified bytes.

## Status Semantics

Status APIs should distinguish:

- Submitted/admitted.
- Queued.
- Validating.
- Accepted into mempool.
- Rejected with code/detail.
- Included/finalized if future APIs expose that state.

Never label durable admission as confirmed validity.
