# Cardano inclusion and rollback handling

Reviewed: 2026-09-13.

Document class: Explanation and research evidence informing Midgard's
fault-proof execution policy. These sources support the design; implementation
and live acceptance evidence are tracked separately in
[automatic watcher journeys](../fault-proofs/automatic-watcher-journeys.md).

## Finding

For sequential fault-proof transactions, the simplest design supported by the
examples below is to advance from authenticated inclusion on the current chain,
retain reversible observations and submitted transaction records, and handle
rollback by reconciling against the replacement chain. A separate stability
boundary controls irreversible conclusions and deletion of recovery history.
This is an engineering inference from the sources, not a universal Cardano dapp
standard or proof that Midgard's current implementation satisfies it.

## What the primary sources actually implement

### Chain observation: apply, undo, then replay

Ogmios chain synchronization delivers both roll-forward and roll-backward
responses. Rollback identifies the common ancestor by slot and block identity.
On reconnect, a client supplies prior chain points to find an intersection.
Consequently an application following the tip needs reversible local state and
chain-point identity; checking only elapsed time or a transaction hash does not
establish that earlier observations remain canonical.
[Ogmios chain synchronization](https://ogmios.dev/mini-protocols/local-chain-sync/).

Kupo's SQLite implementation provides a compact concrete example: rollback
deletes outputs created after the rollback point, clears `spent_at` for later
spends, and removes later checkpoints. That restores the index to the ancestor;
subsequent blocks determine which outputs actually exist on the new branch.
This does not restore an application's signed intents, reservations, or completed
jobs automatically.
[Kupo SQLite source, revision 1c7346789188d7a1a05343e7888ad8f08854cbca](https://github.com/CardanoSolutions/kupo/blob/1c7346789188d7a1a05343e7888ad8f08854cbca/src/Kupo/App/Database/SQLite.hs#L802).

### Wallet: retain the transaction, reverse inclusion, retry exact bytes

The wallet lifecycle explicitly allows `in_ledger` to return to `pending` after
rollback. It also warns that forgetting a submitted transaction cannot cancel
it: the transaction can still appear in a block. The page flags its resubmission
coverage as incomplete, so the retry claim below comes from implementation.
[Cardano Wallet transaction lifecycle](https://cardano-foundation.github.io/cardano-wallet/concepts/transaction-lifecycle.html).

In current source, moving the tracked tip behind a transaction's acceptance slot
changes `InLedger` back to `InSubmission`, retaining the transaction. A distinct
`MoveFinality` operation prunes sufficiently old included or expired records.
[Wallet submission primitives, revision 10191e346bad70deeb35e89c9a32aaa33e43a633](https://github.com/cardano-foundation/cardano-wallet/blob/10191e346bad70deeb35e89c9a32aaa33e43a633/lib/wallet/src/Cardano/Wallet/Submissions/Primitives.hs#L108).

The wallet submission worker periodically retries pending transactions by passing
their stored `submittedTx` to `postSealedTx`. It does not rebuild every
transaction whenever inclusion disappears. Its source defaults to retrying
approximately every ten expected blocks; that interval is evidence of the
implementation, not a recommended Midgard setting.
[Wallet submission worker, same revision](https://github.com/cardano-foundation/cardano-wallet/blob/10191e346bad70deeb35e89c9a32aaa33e43a633/lib/wallet/src/Cardano/Wallet.hs#L3721).

The implication for Midgard is to preserve a logical action and its signed
attempt until reconciled. A rollback can invalidate local authorization to submit
without making the signed bytes intrinsically invalid forever. Fresh chain state
may justify the same transaction, or require a new one. A submission timeout or
missing observation alone does not justify assuming the original cannot land.
These are application design inferences from the wallet lifecycle and worker.

### Actual dapp example: Splash reverses funding and protocol updates

Splash's public off-chain agent implementation emits `TxApplied` as it processes
roll-forward blocks. On rollback it walks cached blocks backward and emits
`TxUnapplied` in reverse transaction order. It records block hash and slot with
the events. This is direct evidence of a dapp processing reversible recent-chain
observations.
[Splash chain event source, revision f23126425063d9d4dbf5ba07401f97737f99d3aa](https://github.com/splashprotocol/splash-offchain-multiplatform/blob/f23126425063d9d4dbf5ba07401f97737f99d3aa/cardano-chain-sync/src/event_source.rs#L94).

Its event handlers invert funding events, reverse order transitions, and emit
backward pool/entity transitions for unapplied transactions. Thus rollback is
more than removing a transaction's success flag: it corrects derived resources
and protocol state.
[Splash funding rollback handler](https://github.com/splashprotocol/splash-offchain-multiplatform/blob/f23126425063d9d4dbf5ba07401f97737f99d3aa/bloom-offchain-cardano/src/event_sink/handler.rs#L313),
[order rollback handler](https://github.com/splashprotocol/splash-offchain-multiplatform/blob/f23126425063d9d4dbf5ba07401f97737f99d3aa/bloom-offchain-cardano/src/event_sink/handler.rs#L575),
[pool/entity rollback handler](https://github.com/splashprotocol/splash-offchain-multiplatform/blob/f23126425063d9d4dbf5ba07401f97737f99d3aa/bloom-offchain-cardano/src/event_sink/handler.rs#L1053).

This inspection is limited to these source paths. It does not verify Splash's
deployment configuration, complete crash recovery, transaction replacement
policy, or production reliability. The event source also has a configurable
historical point before which rollback handling is disabled; that behavior is
not a recommendation for Midgard.

### Hydra: a useful boundary example, with acknowledged limitations

Hydra documents that ordinary chain-state changes can be reversed, while rollback
after off-chain activity introduces harder consistency problems. Its current
documentation explicitly warns that some rollbacks while a Head is open can
leave it stale and require closing it. It is therefore not evidence that all
L2 rollback problems have a simple implemented solution.
[Hydra rollback handling](https://hydra.family/head-protocol/docs/dev/rollbacks).

ADR 23 records a concrete race between chain state and higher-level Head state.
The resulting architecture keeps local chain state in the chain component and
persists a copy with Head state. This supports placing canonical-chain undo in
one owner rather than spreading independent rollback policies across workflow
steps; it does not establish that Midgard should copy Hydra's Head logic.
[Hydra ADR 23](https://hydra.family/head-protocol/adr/23).

## Recommended minimal Midgard policy

The following is a proposal inferred from the sources and scoped to sequential
fault-proof actions with authenticated L1 state. It does not change challenge
deadlines, validator semantics, withdrawal settlement, or the deployment's trust
and finality assumptions.

1. Keep the existing sequential transaction pump and one coordinated writer for
   its funding wallet. Build the next action once its predecessor is included
   and its required output/state is authenticated on the current chain. Waiting
   for mempool acceptance alone would require a more complicated dependency
   model and is unnecessary to remove per-step finality waits.
2. Give canonical observations one rollback owner. Keep the existing finalized
   cursor and rebuild the volatile suffix from the replacement chain. Re-observe
   each affected workflow and reconcile its existing signed-attempt journal;
   do not add per-action undo records or workflow snapshots. Serialize the
   correction with transaction selection and authorization so stale work cannot
   commit afterward.
3. Retain signed attempts, their exact transaction identities, inputs, validity
   bounds, and outcomes. Reconcile attempts before replacement: they may already
   be included on the new branch, remain pending and valid, or require rebuilding
   from fresh state. Invalidate stale worker authorization on rollback; issue
   fresh authorization only after reconciliation. Fresh authorization can still
   refer to the same bytes when appropriate.
4. Treat terminal inclusion as provisional. Release the job's capital and
   execution slot, while its existing journal remains monitored until the chosen
   stable boundary. Reopen the logical job if its terminal effect disappears.
   Restore funding availability from canonical UTxOs plus outstanding attempts,
   rather than blindly returning every old reservation to the available pool.
5. Reuse existing persistence, reservation, and recovery machinery where its
   invariants permit. A new rollback framework per proof family or a general
   speculative transaction dependency graph is not justified by this goal alone.

Resource use and recovery-history retention are separate concerns. There is no
general requirement here to keep the whole job's capital locked until stability.
There is a requirement to prevent selecting an input that an unresolved signed
attempt can still spend. If later jobs consume outputs produced by earlier jobs,
rollback reconciliation must invalidate those derived outputs across jobs as
well. A single wallet writer reduces the coordination required; it does not
eliminate that dependency.

If a later job owns an earlier transaction's change, reconciling the producer
must preserve the later job's unique lease. If both jobs need the same collateral
after rollback, an unresolved attempt cannot simply lose its reservation. Let
read-only reconciliation yield the execution slot; after all recorded attempts
are resolved, release idle inputs and select current free inputs when fresh
execution resumes. This recovery wait follows transaction uncertainty, not a
finality delay between ordinary proof actions.

## Finality terminology and limits

Use “included on the observed chain” and “stable under the configured policy” as
distinct facts. An application confirmation threshold is an engineering risk
policy, not unconditional absolute finality. Cardano consensus itself
distinguishes volatile chain storage from an immutable prefix and operates under
its consensus assumptions. An arbitrary small confirmation count should not be
described as that immutable boundary.
[Ouroboros consensus glossary](https://ouroboros-consensus.cardano.intersectmbo.org/docs/references/glossary/),
[Cardano consensus and storage report](https://ouroboros-consensus.cardano.intersectmbo.org/pdfs/report.pdf).

This research establishes recurring infrastructure and one concrete dapp pattern,
not how every Cardano application behaves. It does not measure rollback frequency,
prove a safe numerical confirmation depth, or assess Midgard's current recovery
coverage. Before deploying the policy, verification should cover rollback of a
middle step, terminal rollback after another job starts using the wallet, restart
before stability, and rollback during ambiguous submission. Those tests prove
the proposed integration rather than relying on another project's behavior.
