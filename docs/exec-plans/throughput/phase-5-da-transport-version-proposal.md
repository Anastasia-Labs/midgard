# Phase 5 DA Transport / Payload Version Proposal

> **Historical, superseded by canonical V1 consolidation:** This proposal
> analyzes pre-consolidation V2 payloads. Its version names and compatibility
> options are not supported canonical V1 behavior.

**Status:** Policy decision required; no limit change is enabled; 100k/one-hour and ≤2 s p99 remain unmet
**Date:** 2026-07-10
**Current production posture:** fail closed on `DA_TRANSPORT_LIMITS_V1.maxPayloadBytes = 64 MiB`; producer envelope emission remains `off` by default

## Decision requested

The Phase 5 measurements prove that compression and transport hardening are useful, but a single full-state `DaPayloadV2` cannot satisfy the 100k headroom gate under the pinned 64 MiB decompressed limit. Any change to that limit is a protocol-version decision, not an environment override.

The recommended decision is:

1. Keep V1 pinned and fail closed now, with the exact pre-submit size gate preventing an unpublishable header from being journaled, signed, or submitted.
2. Design a new delta/checkpoint inner payload schema as the durable solution.
3. Treat either 256 MiB option below only as an explicitly approved transitional protocol, after committee memory/concurrency limits and independent-process soak evidence exist.

## Measurement evidence

The corpus uses real Phase 1 canonical transaction CBOR. The operational scenario carries two post-state UTxOs, one canonical transition step, and one canonical event-to-step member per L2 transaction. The max-envelope scenario is a headroom model with four transition-step-sized members per transaction.

| Scenario                     | Transactions | Inner bytes | Stored envelope bytes |   Ratio | Encode + zstd | V1 inner cap             |
| ---------------------------- | -----------: | ----------: | --------------------: | ------: | ------------: | ------------------------ |
| Operational, canonical trace |       50,000 |  42,149,577 |            13,707,581 | 3.0749x |       1.653 s | Pass                     |
| Operational, canonical trace |      100,000 |  84,506,373 |            28,370,604 | 2.9787x |       4.245 s | **Fail**                 |
| Max-envelope headroom model  |       50,000 |  67,200,421 |            15,413,415 | 4.3599x |       1.633 s | **Fail by 91,557 B**     |
| Max-envelope headroom model  |      100,000 | 134,400,429 |            31,820,813 | 4.2237x |       3.106 s | **Fail by 67,291,565 B** |

The historical 50k operational row above was produced from the prior corpus
snapshot. A deterministic remeasurement on the currently retained Phase-1
snapshot (`logs/throughput-resume-20260712T154400Z/phase1-benchmark/corpus-live-4096/corpus.ndjson`)
uses 50,000 chain rows at 379 canonical-CBOR bytes each and yields 41,949,577
inner bytes and a 13,681,302-byte zstd envelope (ratio 3.0661977201x). The
current gate binds corpus-prefix SHA-256
`4c08d4c17df63a8e004f4ee3ba24ca92eacbabff8ce273ac98c4be23d396b26e`, inner
SHA-256 `0cad493355048c36b85c9d9998863c47b5fe8c012b4de1ae88dd91f7587603d0`,
and envelope SHA-256
`d3601c2595f1ab6af5c99f297c1608d0447fd0147a07bcc277595b357e8b79d6`.
The exact 200,000-byte difference from the historical 42,149,577-byte inner
row is 4 bytes per transaction in that prior corpus; its artifact is absent and
is not regenerated or silently substituted.

The deployable V1 bound was exercised through three real, separate committee processes at default producer concurrency with the valid 50k operational envelope. A retained run measured threshold ACK in 2.247 s and all peers in 2.273 s, with all three accepted; committee handler durations were 2.069 s, 2.070 s, and 2.095 s and per-process peaks were 841,252,864, 855,519,232, and 859,463,680 bytes RSS. The current exact-fixture 25/25 closeout rerun (`logs/phase5-formal-e2e-20260714T082100Z.log`) measured threshold ACK in 1.902600 s and all peers in 1.903810 s, with handler durations of 1.704–1.745 s, 655,278,080–657,321,984 bytes peak RSS, admission peak one on every process, and all three accepted. A fresh-process strict unwrap/decode/canonical check (without libp2p/store overhead) completed in 1.237 s at 591,589,376 bytes peak RSS. Before the byte-oriented decoder, that same check took 139.2 s and peaked at 3.16 GB. An additional three-node single-process diagnostic completed at 5.752 s threshold with a 1.232 GB shared-process peak; it is not used as per-peer memory evidence.

These are single-run capacity observations, not a p99 distribution. The
recorded exact-50k threshold samples span 1.903–2.493 s and clear the ~20 s
block cadence, but one sub-2-second observation does not establish the Phase 5
≤2 s p99 interface target. The current exact-fixture 25/25 rerun closes the
bounded smoke/implementation check; the formal 100-publication distribution
remains required. The retained older log that used a 400,774-byte structural
fixture is not accepted as evidence for this bound artifact.

In explicit, non-deployable `protocol-proposal` mode, unwrapping the 100k max-envelope artifact with a 256 MiB inner allowance took 1.159 s and peaked at 649,953,280 bytes before semantic verification. Under the default V1 mode the script rejects this artifact at the pinned cap.

The 100k one-hour soak cannot run truthfully under V1 because the exact inner-size gate rejects the payload before L1 mutation. It remains a blocked Phase 5 exit criterion, not a waived test, and no smaller-payload soak may be relabeled as its replacement.

## Option A — Transport V2 with one 256 MiB cap

Define `DA_TRANSPORT_PROTOCOL_VERSION = 2` and set both stored/frame bytes and decompressed inner bytes to 256 MiB.

Advantages:

- Smallest semantic change from V1.
- Both measured 100k artifacts fit.
- One limit remains easy to reason about and fail closed against.

Costs and risks:

- Permits a 256 MiB encrypted frame and a 256 MiB decompressed allocation per inbound stream.
- Multiplies memory pressure by concurrent streams before semantic verification; committee admission must reserve memory and cap concurrent payload decodes.
- Does not solve state growth. `DaPayloadV2.utxos` is the complete historical post-state, so a fixed 256 MiB cap is eventually exhausted just as 64 MiB is now.
- Requires new protocol IDs/topics, manifest schema and pinned constants, decoder-first rollout, and mixed-version compatibility tests.

This is fail-closed but not recommended as the long-term design.

## Option B — 64 MiB stored / 256 MiB inner

Introduce a new envelope/transport version with separate limits:

- stored/frame bytes: 64 MiB;
- declared and actual decompressed inner bytes: 256 MiB.

Advantages:

- Retains the current network/frame abuse bound.
- Both measured compressed 100k artifacts fit on the wire.

Costs and risks:

- Reverses Phase 5 section 2.3's deliberate rule that the same cap binds stored and decompressed bytes.
- Expands the zip-bomb and memory threat model: a small valid envelope may force up to 256 MiB output before semantic validation.
- Requires a decode-memory semaphore, per-peer/in-flight byte accounting, bounded decompression workers, and readiness/backpressure behavior based on reserved bytes.
- Still only postpones the full-state scaling failure.

If a transitional expansion is approved, this option is preferable to a 256 MiB wire cap, but only with the memory-admission controls above and a new version. It must never be implemented by changing V1 constants in place.

## Durable option — Delta payloads plus checkpoint/bootstrap

Create a new inner payload schema that commits the block delta rather than repeating the full UTxO state. It should carry:

- schema version, header hash, previous header hash, and previous UTxO root;
- spent outrefs and produced UTxOs for the block;
- transactions, source events, transition trace, and event-to-step members;
- resulting root/count commitments already bound by the header.

Auditors and committee members reconstruct post-state by applying ordered deltas from a trusted-by-hash checkpoint. Bootstrap must be explicit:

- content-addressed checkpoint manifests bind checkpoint header/root and chunk hashes;
- checkpoint chunks are independently bounded and retrievable from multiple committee members;
- retained deltas bridge the latest finalized checkpoint to the requested block;
- checkpoint generation and pruning cannot remove the last reconstructible chain.

Periodic full `DaPayloadV2` checkpoints alone are insufficient: once the state itself exceeds the single-artifact cap, every checkpoint fails. Full checkpoints must therefore be segmented/content-addressed or replaced by a database snapshot format with bounded chunks. This schema change needs fault-proof/auditor reconstruction tests, checkpoint corruption and missing-chunk recovery tests, and retention invariants before activation.

## Immediate safety and rollout contract

- V1 stays pinned at 64 MiB and envelope emission stays `off` until decoder-capable committee rollout is approved.
- The commit planner computes the exact inner size from the Phase 3 post-state UTxO tuple-byte aggregate plus current header/member arrays before journal preparation, signing, or submission.
- A cap breach discards the MPF overlay and leaves no pending journal or L1 mutation.
- Existing V2 raw payloads remain readable; V3 envelope decoders remain additive.
- No timeout, cap, or chunked-submit workaround may bypass the exact inner-size rejection.

Approval of a new protocol must name the selected cap model, committee memory budget, decode concurrency, migration epoch, mixed-version threshold policy, and rollback boundary.
