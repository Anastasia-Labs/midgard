# Phase 5 operational DA envelope binding

The Phase 5 multi-process gate uses a checked-in, generated 50,000-transaction
operational envelope. It does not synthesize a structural fallback at test time.

## Bound 50k gate

The source-of-truth artifacts are:

- `tests/fixtures/da-operational-50k/envelope-50000.cbor`
- `tests/fixtures/da-operational-50k/measurement.json`

Both files are intentional PR artifacts and must be present in a clean checkout.
The gate fails closed rather than generating or accepting a smaller structural
fixture.

The measurement was generated from the first 50,000 rows of the verified Phase
1 Preprod corpus whose full-corpus SHA-256 is
`61c53f60e2993bbd09df61510437d2f944a87c00aef135025404e5a4c7ef0e59`.
The report binds the exact consumed prefix as
`4c08d4c17df63a8e004f4ee3ba24ca92eacbabff8ce273ac98c4be23d396b26e`.

The bound result is:

- uncompressed payload: 41,949,577 bytes;
- zstd V3 envelope: 13,681,302 bytes;
- envelope SHA-256:
  `d3601c2595f1ab6af5c99f297c1608d0447fd0147a07bcc277595b357e8b79d6`;
- measurement report: 1,845 bytes, SHA-256
  `dbb8aaee5078f94312f33edfdf2320c2ffac13c738953045098ae5f853117930`;
- V1 maximum payload bytes: 67,108,864; and
- both the declared inner payload and stored envelope fit the V1 limit.

The multi-process test verifies the report scenario, row count, corpus-prefix
hash, transport limit, envelope size, envelope hash, and both limit verdicts
before publishing the bytes. Missing or mismatched artifacts fail the gate.
The report-file SHA-256 above is a review-time provenance check; after an
intentional regeneration, reviewers must verify the new report and update this
document together with the bound constants in the test.

Regenerate from a verified Phase 1 corpus with:

```bash
cd demo/midgard-node
MIDGARD_DA_MEASUREMENT_SCALES=50000 \
MIDGARD_DA_MEASUREMENT_TRACE_STEPS_PER_TX=1 \
MIDGARD_DA_MEASUREMENT_WRITE_INNER=false \
MIDGARD_DA_MEASUREMENT_ARTIFACT_DIR=tests/fixtures/da-operational-50k \
node scripts/measure-da-envelope.mjs \
  <phase1-corpus.ndjson> \
  tests/fixtures/da-operational-50k/measurement.json
```

## 100k V1 result is not an implementation fallback

The earlier operational 100,000-transaction measurement remains useful policy
evidence:

- uncompressed payload: 84,506,373 bytes;
- zstd V3 envelope: 28,370,604 bytes; and
- the stored envelope fits 64 MiB, but the declared inner payload exceeds the
  V1 67,108,864-byte maximum.

V1 validates the declared inner size as well as the stored envelope size.
Therefore the 100k result is not accepted by the current protocol even though
compression makes the wire bytes smaller. Supporting it requires an externally
approved protocol-policy revision (or a negotiated successor transport
version) that raises the decompressed/inner limit with resource and denial-of-
service analysis. Until that prerequisite is approved and implemented, 50k is
the bound operational gate and no test may substitute the 100k envelope or a
structural fixture.
