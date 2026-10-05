# Comparative recovery evidence — Cardano, OP Stack, Arbitrum Nitro

Prepared 1 October 2026 to supply the evidence the governing rule requires. The
rule is that Midgard runs unattended wherever doing so meaningfully keeps it
live, and that the test for whether automatic recovery is owed is whether a
comparable system recovers from the analogous failure without a human. Recovery
is secondary to preventing the failure.

Each section states the question, the primary-source finding, and the
consequence for a decision in [REPORT.md](REPORT.md). Findings are quoted or
closely paraphrased from the cited sources; no live experiment was run against
any of these systems.

## Q1 — How deep a rollback does Cardano absorb without a human?

Ouroboros chain selection imposes a maximum rollback depth. Candidate chains
that fork off more than `k` blocks back are never considered for adoption, where
`k` is the security parameter, currently **2,160 blocks**. `k` bounds rollback
depth in blocks, not slots. Within `k`, selection is the ordinary longest-chain
rule and the rollback is handled automatically. Under Genesis, deep candidates
are compared by density within the genesis window first. In neither case does an
operator participate: a too-deep fork is simply not adopted.

Timing follows from the same parameters: at an active slot coefficient `f` of
0.05, 2,160 blocks is about 12 hours in expectation, and the stability window
`3k/f` is 129,600 slots, or 36 hours.

**Consequence for A5.** Availability transactions settle on Cardano, so their
finality is Cardano's finality: `k`. `confirmation_depth` — 12 on the testing
profiles, 30 on public — is a liveness threshold stating when a component may
stop waiting and proceed optimistically. It is not finality, and nothing may be
discarded on reaching it. Midgard's own deployment manifest already says so: it
declares `automaticRecoveryMaxDepth` as the literal 2,160 and `deepRollbackPolicy`
as `automated_rewind_replay_incident-v1`, both frozen into the signed deployment
identity, yet only the fault-proof package reads them. The availability journal
deletes its rewind state at `confirmation_depth` and latches a permanent halt when
a later observation disagrees. Automatic recovery to `k` is therefore owed twice
over — by the comparative test, and by the deployment's own declared policy.

**Consequence for B1.** The depth chosen for the testing profiles is a response
budget question, not a safety ceiling; it is nowhere near `k`.

Sources: [Ouroboros Consensus and Storage Layer technical report](https://ouroboros-consensus.cardano.intersectmbo.org/pdfs/report.pdf),
[gouroboros consensus package](https://pkg.go.dev/github.com/blinklabs-io/gouroboros/consensus).

## Q2 — How do OP and Nitro size a batch against a hard data limit?

Both measure or deliberately under-estimate, and both reduce the batch on
overflow. Neither refuses to produce.

OP's batcher offers three compressors. `RatioCompressor` estimates fullness as
`uncompressedLength * approxCompRatio >= targetFrameSize * targetNumFrames`, and
the documentation instructs that the ratio be set "slightly smaller than average
from experiments to avoid the chances of creating a small additional leftover
frame" — the estimate is deliberately pessimistic, and its worst case is one
extra frame. `ShadowCompressor` does not estimate at all: it "contains two
compression buffers: one used for size estimation, and one used for the final
compressed output", flushing the estimation buffer on every write so the real
compressed size is known before the bound is crossed. Frame and channel sizing
are explicit configuration (`MaxFrameSize`, `TargetFrameSize`,
`TargetNumFrames`, `TargetL1TxSize`).

Nitro's batch poster caps a batch with `--node.batch-poster.max-calldata-batch-size`
(default 100,000 bytes, 90,000 for L3). When the queued transactions' compression
estimate exceeds that cap, "the batch poster will post the max size of
transactions to the L1" — it truncates the batch to fit. Brotli level is chosen
adaptively from the backlog.

**Consequence for F3.** Midgard is below this bar on both axes. Its planner
estimates optimistically (`selectedTxBytes + selectedTxCount * 128`), omitting
the base-ledger aggregate, the second transaction representation, script material
and traces; and on overflow it refuses rather than reducing, re-selecting the
same deterministic block on the next tick. The prevention fix is the
industry-standard one: measure the real framed representation incrementally, or
estimate pessimistically with a proven margin, and reduce the selection to fit.
F3's existing final refusal stays as the last guard, but it must stop being the
operating mechanism.

**Consequence for A6.** Neither system republishes unchanged state per batch,
which is what makes a fixed frame tractable for them.

Sources: [op-batcher compressor package](https://pkg.go.dev/github.com/ethereum-optimism/optimism/op-batcher/compressor),
[op-batcher package](https://pkg.go.dev/github.com/ethereum-optimism/optimism/op-batcher/batcher),
[OP batcher configuration](https://docs.optimism.io/chain-operators/guides/configuration/batcher),
[Nitro batch poster deep dive](https://docs.arbitrum.io/how-arbitrum-works/deep-dives/batchposter),
[Nitro batch poster configuration](https://docs.arbitrum.io/launch-arbitrum-chain/run-a-node/batch-poster).

## Q3 — Does a production L2 ever require manual intervention for liveness?

Yes, and the distinction is instructive.

An **L1 reorg** is handled automatically. The OP derivation pipeline resets on a
detected reorg, recovering "into a state that produces the same outputs as a full
L2 derivation process, but starting from an existing L2 chain that is traversed
back just enough to reconcile with the current L1 chain", walking the unsafe head
back only as far as the first canonical L1 origin or the finalized block. Reorg
depth is bounded by L1 finality. No operator action is involved.

A **sequencing-window expiry** is not. In OP issue #11228, the batcher stopped
submitting, the sequence window expired, op-node began producing empty blocks,
and subsequently submitted batches were dropped against those empty blocks,
re-orging repeatedly. There was no automatic recovery. Operators had to block new
user transactions, clear the sequencer's transaction pool, and run op-batcher in
singular batch mode until the chain recovered. The proposed automatic remedy — an
"incident mode" in which the sequencer builds empty blocks with `NoTxPool` set —
was filed with the caveat that it "may be a bit risky because it's touching a lot
of important features like sequencing and batch submission".

**Consequence for B4.** A standing intervention-required class is legitimate; a
mature L2 has one, and its own maintainers judged the automatic remedy risky
enough to leave unshipped. Midgard may therefore keep such a class. Two
qualifications follow from the same evidence. First, OP's intervention case is
the downstream consequence of a prevention failure — the batcher stopped — which
is why prevention outranks recovery. Second, the ordinary chain event in the same
system recovers automatically, so membership in the class has to be argued per
entry rather than assumed.

Sources: [OP Stack derivation specification](https://specs.optimism.io/protocol/derivation.html),
[optimism#11228 — recovery from sequence window expiration incident](https://github.com/ethereum-optimism/optimism/issues/11228).

## Q4 — Does anyone automatically repair a corrupted local store?

Partially, and the boundary is the useful part.

cardano-node validates its on-disk chain database at startup and **automatically
truncates the ImmutableDB to the last valid block** when validation finds an
invalid block, whether from data corruption or a clock change. That truncation is
automatic; ouroboros-network issue #1532 asks only that the user be _warned_
before a large truncation, which confirms the repair itself needs no operator.
The node then re-syncs the removed suffix from its peers. Startup also
reconstructs the ledger state at the immutable tip by replaying blocks from the
last snapshot, or from genesis if no valid snapshot exists.

Beyond what truncation can fix, the node does not self-repair. Where required
data is absent at replay time, replay fails on every boot and the immutable block
can be neither rolled back nor pruned past. For that residue the project ships
**offline operator tooling** rather than an automatic path: `db-truncater` to cut
the immutable database to a given block or slot, and `db-analyser` with
`validate-all-blocks` or `minimum-block-validation` policies. Deleting the
database volume and resyncing remains the documented last resort.

**Consequence for B4.** The intervention-required category is confirmed, but the
bar is higher than refusing with a diagnostic. The automatic path should be
_truncate to the last provably-good state and re-derive_, which is the same
capability E3 needs for rejoining after retention expiry. Only the residue that
truncation cannot bound is intervention-required, and that residue is owed
tooling, not just a message. This narrows B4's list considerably: "corrupt
records" is auto-recoverable exactly as far as a re-derivation path exists.

Sources: [ouroboros-network#1532 — warn before truncating a large part of the DB](https://github.com/input-output-hk/ouroboros-network/issues/1532),
[ouroboros-consensus cardano README (db-truncater, db-analyser)](https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus-cardano/README.md),
[cardano-node#1944 — ImmutableDB incorrectly used](https://github.com/input-output-hk/cardano-node/issues/1944).

## Summary table

| Failure                                        | Cardano                                                                     | OP / Nitro                                       | Owed in Midgard                                                  | Item   |
| ---------------------------------------------- | --------------------------------------------------------------------------- | ------------------------------------------------ | ---------------------------------------------------------------- | ------ |
| Rollback up to finality                        | Automatic, up to `k` = 2,160 blocks (~12 h expected, 36 h stability window) | Automatic pipeline reset                         | **Automatic rewind to `k`, which the manifest already declares** | A5, A1 |
| Data limit would be exceeded by the next batch | n/a                                                                         | Measure or under-estimate, then reduce the batch | **Prevention: fit the selection**                                | F3, A6 |
| Republishing unchanged state each batch        | n/a                                                                         | Nobody does it                                   | **Prevention: stop doing it**                                    | A6     |
| Producer stops and a protocol window expires   | n/a                                                                         | Manual: drain mempool, singular batch mode       | Intervention acceptable; prevent the stop                        | B4, F3 |
| Local store corrupt, bounded by truncation     | Automatic truncate and re-sync                                              | Re-sync                                          | **Automatic re-derivation**                                      | B4, E3 |
| Local store corrupt, unbounded                 | Offline tooling or delete and resync                                        | Delete and resync                                | Intervention + tooling                                           | B4     |
| Wrong chain / wrong network identity           | Refuses to connect                                                          | Refuses to start                                 | **Prevention: refuse at startup**                                | D4, A9 |

## Limitations

These are documentation and source-package findings, not executed drills against
any of the three systems. Version-specific behavior may differ from the released
documentation, and the OP compressor selection is a chain-operator configuration
choice rather than a single fixed behavior. The Cardano consensus report was
consulted through its published summaries; its PDF was not parsed in full. No
claim is made that matching these systems' behavior is sufficient for Midgard
acceptance.
