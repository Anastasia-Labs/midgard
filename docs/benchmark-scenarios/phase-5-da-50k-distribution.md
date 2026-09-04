# Phase 5 exact-50k DA publication distribution gate

> **Historical, not runnable under canonical V1:** The checked envelope,
> measurement, runner, and package commands described below were invalidated
> during canonical V1 consolidation. Regenerating the newest V1 payload shape
> from the same 50,000 canonical transactions produced a 71,049,618-byte inner
> payload, above the retained 67,108,864-byte DA bound. The obsolete V3/V2
> artifact was removed rather than relabeled, incompletely regenerated, or
> accepted through a compatibility path. A replacement distribution gate is
> deferred to the mandatory capability-floor/proof-completion follow-up, which
> may use bounded chunking or proof continuations without weakening admission.

**Last reviewed:** 2026-07-22

This is the formal closeout command for the Phase 5 `<=2 s` threshold-ACK p99
criterion. It measures 100 semantically independent 50,000-transaction V1
envelopes over one long-lived producer transport and three long-lived,
separate committee processes. The report verifier recomputes nearest-rank
p50/p95/p99/max from the raw samples. A p99 above 2,000 ms is retained as a
truthful failed verdict; it is never rounded, waived, or converted to a pass.

## Current evidence and remaining blocker

The checked contract contains exactly one operational envelope:

- `demo/midgard-node/tests/fixtures/da-operational-50k/envelope-50000.cbor`
  is 13,681,302 bytes with SHA-256
  `d3601c2595f1ab6af5c99f297c1608d0447fd0147a07bcc277595b357e8b79d6`;
- its inner payload is 41,949,577 bytes with SHA-256
  `0cad493355048c36b85c9d9998863c47b5fe8c012b4de1ae88dd91f7587603d0`;
- its first-50k corpus-window SHA-256 is
  `4c08d4c17df63a8e004f4ee3ba24ca92eacbabff8ce273ac98c4be23d396b26e`;
- its header hash is
  `8ffd0001ced7f02bc858def1b3bd6f254a90e1ae908529985e7d7d99`.

Replaying those bytes 100 times would produce one `accepted` result followed
by 99 `duplicate` results and would measure store deduplication, not
publication. Changing only the header would likewise not create independent
transaction sets. Both remain rejected by the gate and report verifier.

The offline provenance blocker is now closed by
`demo/midgard-node/logs/phase5-historical-corpus-20260714`. The retained suite
contains 5,000,000 verified rows across 4,096 chains, 8,192 index runs, and 100
disjoint 50,000-row windows. All 100 entries have unique header, envelope,
inner, transaction-set, and canonical-content hashes. The exact retained
identities are:

| Artifact                 | SHA-256                                                            |
| ------------------------ | ------------------------------------------------------------------ |
| source corpus            | `d8282fed16e1fbf2f9d6a7a1ca4e302d1e8d6537b4f7a7bd0b05bdbfe1292f25` |
| source index             | `1808ac8a8e004bfee0969ff3d5c119cf6cb56e2b82d1c3c7b14ab40921f8884f` |
| source manifest          | `88548d399232e1f314e359142a4981f7a88504248d4996d1c8497e5dc1a50c13` |
| source verification      | `df874b0eb750d8561635abfc108040ae4e8f41a93b3866941925898a9d89a875` |
| source binding           | `721eb3a5dc1243f1a75db5bcd04fb033672664a3f4496c112cab6cafa45706a1` |
| source generation result | `fa8dbf0410e97b6ee9b2776fbb55827df80d267d053984d99fc9770242e99950` |
| fixture-suite manifest   | `4c073f9fa64042b670fe6ace483d1254e13d999f77eb311129b3f84035081e96` |

This suite is intentionally scoped as
`historical-offline-corpus-extension`, with `freshLiveClaim: false`,
`phase1FormalBindingCompatible: false`, and
`phase2ValidationCorpusCompatible: false`. It closes only the Phase 5 offline
cardinality/provenance prerequisite. It is not a fresh-live benchmark.

The final Node `v22.22.2` consumer loader passed against this exact published
suite. It repeated the complete historical provenance check, rescanned all
5,000,000 source rows and all 100 disjoint windows, decoded all 100 envelopes,
and reproduced every identity in the table plus the checked first-window
anchor.

The remaining blocker is execution evidence: the Docker three-process formal
distribution has not run, so there are no 100 raw threshold/all-peer timing
samples and no ≤2 s p99 verdict. The honest status remains **NO-GO** until that
report is produced and verified. Separately, the operational 100k payload is
still 84,506,373 decompressed bytes, over the pinned 64 MiB limit, so the 100k
one-hour soak remains blocked.

## Fixture-suite contract

`MIDGARD_DA_PHASE5_FIXTURE_SUITE` names a JSON file with this shape:

```json
{
  "schemaVersion": "midgard-phase-5-da-50k-fixture-suite-v1",
  "sampleCount": 100,
  "transactionsPerSample": 50000,
  "sourceCorpusPath": "historical-corpus.ndjson",
  "sourceCorpusSha256": "<normalized non-empty NDJSON lines plus LF>",
  "sourceCorpusFileSha256": "<raw corpus file SHA-256>",
  "sourceCorpusRows": 5000000,
  "sourceCorpusBindingPath": "historical-corpus-binding.json",
  "sourceCorpusBindingSha256": "<64 lowercase hex characters>",
  "sourceCorpusManifestPath": "historical-corpus.ndjson.manifest.json",
  "sourceCorpusManifestSha256": "<64 lowercase hex characters>",
  "sourceCorpusGenerationResultPath": "historical-corpus-generation-result.json",
  "sourceCorpusGenerationResultSha256": "<64 lowercase hex characters>",
  "anchor": {
    "corpusPrefixSha256": "4c08d4c17df63a8e004f4ee3ba24ca92eacbabff8ce273ac98c4be23d396b26e",
    "headerHash": "8ffd0001ced7f02bc858def1b3bd6f254a90e1ae908529985e7d7d99",
    "innerSha256": "0cad493355048c36b85c9d9998863c47b5fe8c012b4de1ae88dd91f7587603d0",
    "envelopeSha256": "d3601c2595f1ab6af5c99f297c1608d0447fd0147a07bcc277595b357e8b79d6",
    "innerBytes": 41949577,
    "envelopeBytes": 13681302
  },
  "entries": [
    {
      "sampleIndex": 0,
      "envelopePath": "envelopes/000.cbor",
      "headerHash": "<56 lowercase hex characters>",
      "envelopeSha256": "<64 lowercase hex characters>",
      "innerSha256": "<64 lowercase hex characters>",
      "transactionSetSha256": "<64 lowercase hex characters>",
      "transactionContentSha256": "<64 lowercase hex characters>",
      "envelopeBytes": 13681302,
      "innerBytes": 41949577,
      "corpusWindow": {
        "startRow": 0,
        "rowCount": 50000,
        "sha256": "<normalized window SHA-256>"
      }
    }
  ]
}
```

There must be exactly 100 entries in index order. Window `i` must start at
row `i * 50000`, contain exactly 50,000 rows, and bind to a unique decoded
transaction-set hash. Entry zero must be byte-identical to the checked anchor.
The runner streams and hashes the full corpus once, recomputes every window
hash and transaction identities, decodes every envelope, and requires unique
header, envelope, inner, key-plus-CBOR transaction-set, and key-independent
canonical-CBOR-content hashes. Relabeling identical transaction bytes under
new map keys therefore fails. For every corpus row and every transaction in
every decoded envelope, the gate also decodes the bytes with the production
Midgard native codec, requires byte-for-byte canonical re-encoding, recomputes
the transaction ID with `computeMidgardNativeTxId` from the compact native
body, and requires that body hash to equal the declared map key. Recomputing
the aggregate hashes around a false transaction ID therefore cannot make
forged evidence pass. The raw corpus is also bound through a matching source
binding, corpus manifest, and verified generation result; a suite manifest
cannot self-attest a synthetic corpus. A Phase 1 source uses
`midgard-phase1-live-corpus-binding-v1`. The retained 5M suite instead uses the
strict `midgard-phase5-historical-corpus-binding-v1`, which cryptographically
anchors the exact Phase 1 corpus/index/manifest/verification/binding/fanout
artifacts, proves the byte-identical retained prefix, and marks itself
historical-only and incompatible with Phase 1/2 benchmark claims. The runner
requires the operational 50k counts: 50k transactions, 100k UTxOs, 50k
transition steps, and 50k event-to-step entries. No structural fallback
exists.

The formal wrapper pins producer publication concurrency to the Phase 5
default of 8 (effective concurrency 3 for the three-peer committee), transport
protocol V1, threshold 2, and the unchanged V1 frame/stream/deadline limits.

The transaction-set hash sorts entries by transaction key and updates SHA-256
for each entry with an 8-byte prefix (`uint32be(key bytes)`,
`uint32be(value bytes)`) followed by the key and canonical transaction CBOR
bytes. Suite creation must use this exact definition.

The content hash sorts canonical transaction CBOR values themselves and
updates SHA-256 with `uint32be(value bytes)` followed by each value. It is
independent of the declared transaction keys and must also be unique across
all 100 windows.

## Bounded smoke check

This exercises the checked single envelope through the real three-process
path without pretending it is a distribution:

```sh
NODE_OPTIONS=--max-old-space-size=4096 \
  pnpm --dir demo/midgard-node run test:da-phase5-50k-smoke
```

The only accepted outcome is 3/3 `accepted` with
`admissionPeakActive=1` in each committee process.

## Formal benchmark image and command after the stable redeploy

The production `midgard-node` image deliberately contains production
dependencies only; it does not contain Vitest or tsup and is not the gate
runner. Build the dedicated benchmark target from the exact candidate
checkout after the stable deployment image has been frozen:

```sh
export PHASE5_GATE_IMAGE='midgard-phase5-da-gate:tx-validation-candidate'
docker build --file demo/midgard-node/Dockerfile.phase5-da-gate \
  --tag "$PHASE5_GATE_IMAGE" demo
export PHASE5_GATE_IMAGE_ID="$(docker image inspect \
  --format '{{.Id}}' "$PHASE5_GATE_IMAGE")"
test -n "$PHASE5_GATE_IMAGE_ID"
```

The Dockerfile pins Node `22.22.2`, installs the locked development workspace,
and builds `midgard-node` plus its workspace dependency closure from the clean
source context. No host `dist` artifact enters the image. The resulting
immutable image ID binds the exact candidate source, lockfile, built workspace,
fixture, tests, and runner. The host Docker CLI used below is a statically
linked binary on the benchmark host; mount it and the Docker socket read-only
so the runner can prove that `docker inspect $(hostname)` identifies its own
container and exact image.

Run with the completed suite mounted read-only and a fresh result directory:

```sh
export PHASE5_SUITE_DIR='<absolute path to the fixture suite>'
export PHASE5_RESULTS_DIR='<absolute fresh result directory>'
test ! -e "$PHASE5_RESULTS_DIR/phase5-da-50k-distribution.json"

docker run --rm --name midgard-phase5-da-gate \
  --cpuset-cpus 28-31 \
  --memory 12g \
  --volume /usr/bin/docker:/usr/local/bin/docker:ro \
  --volume /var/run/docker.sock:/var/run/docker.sock:ro \
  --volume "$PHASE5_SUITE_DIR":/bench/phase5-suite:ro \
  --volume "$PHASE5_RESULTS_DIR":/bench/results:rw \
  --env NODE_OPTIONS=--max-old-space-size=4096 \
  --env MIDGARD_DA_PHASE5_FIXTURE_SUITE=/bench/phase5-suite/manifest.json \
  --env MIDGARD_DA_PHASE5_DISTRIBUTION_REPORT=/bench/results/phase5-da-50k-distribution.json \
  --env MIDGARD_DA_PHASE5_EXPECTED_IMAGE_REFERENCE="$PHASE5_GATE_IMAGE" \
  --env MIDGARD_DA_PHASE5_EXPECTED_IMAGE_ID="$PHASE5_GATE_IMAGE_ID" \
  "$PHASE5_GATE_IMAGE"
```

The output path must not already exist. The runner inspects its own container
and fails before measuring if the Node version, image reference, immutable
image ID, corpus, suite manifest, V1 limits, or exact anchor differs. It starts
the three committee processes and producer transport once, collects 100 raw
threshold/all-peer samples, records every committee handler's duration/RSS/
admission observation, and writes the report even when the 2-second target is
missed. The wrapper then verifies the report and exits nonzero for a truthful
target miss.

Each sample begins with the already verified inner payload, includes pinned
zstd level-3 envelope creation, and stops the threshold timer synchronously
when the second `accepted` peer result arrives. Gossip announcement latency is
outside that threshold-ACK boundary. All-peer timing stops on the third peer
result. Fixture loading, process startup, and corpus verification are outside
the samples; compression is inside.

To recheck a retained report independently:

```sh
pnpm --dir demo/midgard-node run \
  verify:phase5:da-50k-distribution-report -- \
  /bench/results/phase5-da-50k-distribution.json \
  /bench/phase5-suite/manifest.json
```

This is an evidence verifier, not a report-shape check. It re-hashes the suite
manifest, Phase 1 binding/manifest/generation evidence, raw and normalized full
corpus, all 100 disjoint windows, and every envelope/inner payload. It decodes
each envelope, recomputes both transaction identities, and requires every raw
report sample to match the corresponding re-hashed fixture entry before it
recomputes the timing distribution and verdict.

Adversarial verifier coverage is:

```sh
pnpm --dir demo/midgard-node run test:phase5:distribution-verifier
```
