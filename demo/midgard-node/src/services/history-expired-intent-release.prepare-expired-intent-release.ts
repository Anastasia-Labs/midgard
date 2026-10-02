import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Queue, Ref } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import type { Checkpoint } from "../database/eventHistoryJournal.js";
import {
  discardPreparedHistoryRecoveryPlan,
  prepareRetainedNativeHistoryRecoveryPlan,
  retainedPreparedRecoveryPlan,
  SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
} from "../database/eventHistoryRecoveryPlans.js";
import * as MutationJobsDB from "../database/mutationJobs.js";
import * as StateQueueLeases from "../database/stateQueueMutationLeases.js";
import { recordConfirmedPendingBlock } from "../fibers/block-confirmation.js";
import { invalidateSpeculativeCommitCandidate } from "../fibers/speculative-commit-builder.js";
import {
  eventHistoryCanonicalJson,
  type EventHistorySourceBinding,
  readBoundRecoveryLedgerSnapshot,
} from "../l1-event-history-source.js";
import type { HistoryTransportOptions } from "../l1-event-history-transport.js";
import { LEDGER_SCAN_TIMEOUT_MS } from "../l1-ledger-snapshot.js";
import {
  type SerializedStateQueueUTxO,
  serializeStateQueueUTxO,
} from "../workers/utils/commit-block-header.js";
import {
  reviveReplacedCanonicalJournal,
  signedIntentReplacementDigest,
} from "./canonical-journal-recovery.js";
import type { NodeConfigDep } from "./config.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";
import { Globals } from "./globals.js";
import { executeHistoryDependentRecovery } from "./history-dependent-recovery.js";
import { scanBaseSpend } from "./history-expired-intent-release.base-spend.js";
import {
  declineBeforeTtl,
  heldBaseOutput,
} from "./history-expired-intent-release.before-ttl.js";
import { decide } from "./history-expired-intent-release.decide.js";
import {
  effective,
  openRetainedNativeOwner,
  persistedReplay,
} from "./history-expired-intent-release.open-retained-native-owner.js";
import { rederiveDecision } from "./history-expired-intent-release.rederive-decision.js";
import {
  includedInCanonicalHistory,
  replacedSiblings,
} from "./history-expired-intent-release.replaced-block-landing.js";
import {
  authenticateQueue,
  type Decision,
  journalIdentity,
  type ReleaseEvidence,
  replaceableJournal,
} from "./history-expired-intent-release.signed-commit-node.js";
import {
  activeSignedIntent,
  C,
  declinedBeforeTtl,
  deferralKey,
  deferredUntilObserved,
  failure,
  observerFingerprint,
  REPLACEMENT_EVIDENCE_DOMAIN,
  reportOnce,
  sha,
  type SignedIntentDeferral,
  signedTtl,
} from "./history-expired-intent-release.table.js";
import { reincludeStateQueueCorrectedBlocks } from "./state-queue-correction-recovery.js";
import {
  type StateQueueCorrectionRewindAuthority,
  stateQueueCorrectionRewindDisposition,
} from "./state-queue-correction-rewind.js";
import { nativeOwnerOpenWait } from "./state-queue-correction-rewind.prepare-state-queue-correction-rewind.js";

/**
 * Recovery preparation: once the active signed intent cannot land at this
 * checkpoint (its TTL is reached, or the canonical history shows its base
 * output spent by another transaction or a replaced sibling's commit
 * included), reads the exact-point queue and confirms, replaces or revives
 * (see the module comment). Defers while a correction rewind is owed
 * (it may resolve this very journal) or while another plan is retained (its
 * owner resumes it first).
 */
export const prepareExpiredIntentRelease = (input: {
  readonly binding: EventHistorySourceBinding;
  readonly checkpoint: Checkpoint;
  readonly preparation: HistoryRecoveryPreparation;
  readonly config: NodeConfigDep;
  readonly rewindAuthority: StateQueueCorrectionRewindAuthority;
  readonly transport: Omit<HistoryTransportOptions, "signal">;
  readonly contracts: Pick<SDK.MidgardValidators, "stateQueue">;
  readonly deferral: SignedIntentDeferral;
}) =>
  Effect.gen(function* () {
    const { checkpoint, preparation, config } = input;
    const reportKey = `${input.binding.digest}:decision`;
    const owned = <A, E, R>(work: Effect.Effect<A, E, R>) =>
      Authority.withRecovery(
        preparation.token,
        preparation.assertCurrent.pipe(
          Effect.zipRight(work),
          Effect.tap(() => preparation.assertCurrent),
        ),
      );
    yield* preparation.assertCurrent;
    const derived = yield* owned(
      Effect.gen(function* () {
        const intent = yield* activeSignedIntent;
        const retained = yield* retainedPreparedRecoveryPlan(
          input.binding.digest,
        );
        if (
          retained !== undefined &&
          (retained.kind !== "signed_intent_release" ||
            intent === undefined ||
            retained.headerHash !== intent.headerHash.toString("hex"))
        )
          return undefined;
        // This intent's own retained plan is resumed or discarded first: an
        // owed rewind waits for every retained plan, so waiting for the
        // rewind here would deadlock both.
        if (retained === undefined) {
          const rewind = yield* stateQueueCorrectionRewindDisposition(
            input.rewindAuthority,
          );
          if (rewind !== undefined) return undefined;
        }
        if (intent === undefined) return undefined;
        const ttl = signedTtl(intent.signedTxCbor);
        if (ttl === undefined) return undefined;
        const expired = BigInt(checkpoint.head.slot) >= ttl;
        const baseSpend = yield* scanBaseSpend({
          ...intent,
          binding: input.binding,
          fromHeight: -1,
          toHeight: checkpoint.head.height,
          declined: declinedBeforeTtl(input.deferral, deferralKey(intent)),
        });
        // Before the TTL, only evidence that its base output is gone (or its
        // own retained plan, which is resumed) reconciles the intent.
        if (!expired && baseSpend === undefined && retained === undefined)
          return undefined;
        const journal = yield* replaceableJournal(
          intent.headerHash,
          checkpoint.manifestId,
        );
        return {
          ...journal,
          ttl,
          expired,
          baseSpend,
          key: deferralKey(intent),
          identity: journalIdentity(journal.record),
          retainedPlan: retained !== undefined,
          // The retained plan's CAS moves the native root from its candidate
          // only when the journal held it there when the plan was prepared
          // (promoted or locally finalized); otherwise the CAS is base to
          // base and never moved it.
          replayRetained:
            retained !== undefined &&
            retained.expectedRoot === journal.record[C.EXPECTED_UTXOS_ROOT],
        };
      }),
    );
    if (derived === undefined) return;
    const { record } = derived;
    const headerHash = record[C.HEADER_HASH];
    const header = headerHash.toString("hex");
    const signedTx = record[C.INTENDED_TX_HASH]!.toString("hex");
    const capture = yield* Effect.tryPromise({
      try: (signal) =>
        readBoundRecoveryLedgerSnapshot({
          ...input.transport,
          timeoutMs: LEDGER_SCAN_TIMEOUT_MS,
          binding: input.binding,
          addresses: [input.contracts.stateQueue.spendingScriptAddress],
          at: checkpoint.head,
          signal,
        }),
      catch: (cause) =>
        failure(
          `Exact-point state-queue capture failed: ${formatUnknownError(cause, { includeCause: true })}`,
          cause,
        ),
    });
    yield* preparation.assertCurrent;
    const queue = yield* authenticateQueue(
      capture.ledger.outputs,
      input.contracts,
    );
    const context = `signed commit ${signedTx} of block ${header} (TTL slot ${derived.ttl.toString()}, head slot ${checkpoint.head.slot.toString()})`;
    const { baseSpend } = derived;
    const decline = (reason: string) =>
      declineBeforeTtl({
        ...derived,
        deferral: input.deferral,
        reportKey,
        context,
        reason,
      });
    const held = heldBaseOutput(derived, queue, record[C.BASE_TAIL_OUT_REF]);
    if (held !== undefined && (yield* decline(held))) return;
    const evidence: ReleaseEvidence = {
      queue,
      baseSpend: baseSpend?.kind === "spent" ? baseSpend.txHash : undefined,
      canonicalHistory: yield* owned(
        replacedSiblings(record).pipe(
          Effect.flatMap((siblings) =>
            includedInCanonicalHistory(input.binding, checkpoint, [
              signedTx,
              ...siblings.flatMap((sibling) => {
                const hash = sibling[C.INTENDED_TX_HASH];
                return hash == null ? [] : [hash.toString("hex")];
              }),
            ]),
          ),
        ),
      ),
      contracts: input.contracts,
      rewindAuthority: input.rewindAuthority,
    };
    const current = (expected: Decision["kind"]) =>
      rederiveDecision({
        headerHash,
        manifestId: checkpoint.manifestId,
        identity: derived.identity,
        evidence,
        retainedPlan: derived.retainedPlan,
        expected,
      });
    const decision = effective(
      yield* owned(decide(record, evidence)),
      derived.retainedPlan,
    );
    const globals = yield* Globals;
    // An owner that cannot open yet keeps the gate closed; others are fatal.
    const openOwner = openRetainedNativeOwner(globals, config).pipe(
      Effect.catchIf(
        (error) => nativeOwnerOpenWait(error) !== undefined,
        (error) =>
          reportOnce(
            reportKey,
            `Cannot reconcile ${context} yet: ${nativeOwnerOpenWait(error)!}. The history gate stays closed.`,
          ).pipe(Effect.as(undefined)),
      ),
    );

    if (decision.kind === "wait" && (yield* decline(decision.reason))) return;
    if (decision.kind === "wait") {
      yield* reportOnce(
        reportKey,
        `Cannot reconcile ${context} yet: ${decision.reason}. The history gate stays closed.`,
      );
      return;
    }
    if (decision.kind === "defer") {
      if (decision.sticky) input.deferral.current = derived.key;
      else
        input.deferral.untilObserved = deferredUntilObserved(
          derived.key,
          yield* owned(observerFingerprint(input.rewindAuthority)),
        );
      yield* reportOnce(
        reportKey,
        decision.sticky
          ? `Not replacing ${context}: ${decision.reason}. The history gate stays open and its journal stays active until the correction path resolves it.`
          : `Not replacing ${context} yet: ${decision.reason}. The history gate stays open, its journal stays active, and the next change of the correction observer's view decides again.`,
      );
      return;
    }
    if (decision.kind === "landed") {
      const serialized = yield* serializeStateQueueUTxO(decision.node.node);
      if (derived.replayRetained) {
        // A replacement prepared from the candidate root before the block was
        // seen to land may already have restored the base root natively (its
        // CAS ran, its SQL repair did not), so the journal is intact but the
        // native root may be at its base. Replay it to the candidate first (a
        // no-op when the CAS never ran): a locally finalized journal is not
        // replayed again at local finalization. The native root stays within
        // the retained plan's two roots, so the plan can still be resumed if
        // this attempt stops before it is discarded. A plan prepared from the
        // base root (a journal never promoted) is base to base: the native
        // root never left the base, local finalization replays the journal,
        // and replaying here would strand the plan outside its roots.
        const owner = yield* openOwner;
        if (owner === undefined) return;
        yield* preparation.assertCurrent;
        yield* Effect.tryPromise({
          try: () => owner.recover(persistedReplay(record.nativeMpfReplay!)),
          catch: (cause) =>
            failure(
              `Native replay of landed block ${header} over its discarded replacement failed`,
              cause,
            ),
        }).pipe(Effect.uninterruptible);
        yield* preparation.assertCurrent;
      }
      const requiresLocalFinalization = yield* owned(
        current("landed").pipe(
          Effect.tap(() =>
            // The native root is where the journal's status says it is again;
            // the replacement is discarded, not resumed. The plan must still
            // be the one the replay choice was made for.
            derived.retainedPlan
              ? retainedPreparedRecoveryPlan(input.binding.digest)
                  .pipe(
                    Effect.flatMap((retained) =>
                      retained?.kind === "signed_intent_release" &&
                      retained.headerHash === header &&
                      (retained.expectedRoot ===
                        record[C.EXPECTED_UTXOS_ROOT]) ===
                        derived.replayRetained
                        ? discardPreparedHistoryRecoveryPlan(
                            checkpoint,
                            SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
                            header,
                          )
                        : Effect.fail(
                            failure(
                              `The retained replacement plan of landed block ${header} changed`,
                            ),
                          ),
                    ),
                  )
                  .pipe(
                    Effect.zipRight(
                      Effect.logWarning(
                        `Discarded the prepared replacement of block ${header}: it landed.`,
                      ),
                    ),
                  )
              : Effect.void,
          ),
          Effect.flatMap((journal) =>
            recordConfirmedPendingBlock(
              journal.record,
              Buffer.from(decision.node.node.utxo.txHash, "hex"),
            ),
          ),
        ),
      );
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
      yield* Ref.set(
        globals.LOCAL_FINALIZATION_PENDING,
        requiresLocalFinalization,
      );
      yield* Ref.set(
        globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
        requiresLocalFinalization ? serialized : "",
      );
      yield* reportOnce(reportKey, undefined);
      yield* Effect.logInfo(
        `Recorded the L1 observation of ${context}: ${decision.evidence}.`,
      );
      return;
    }

    const revived = decision.kind === "revive" ? decision.revived : undefined;
    const revivedBlock: SerializedStateQueueUTxO | undefined =
      decision.kind === "revive"
        ? yield* serializeStateQueueUTxO(decision.node.node)
        : undefined;
    const replacementDigest = signedIntentReplacementDigest(record)!;
    const targetRoot = record[C.BASE_UTXOS_ROOT];
    const evidenceDigest = sha(
      eventHistoryCanonicalJson({
        domain: REPLACEMENT_EVIDENCE_DOMAIN,
        decision: decision.kind,
        headerHash: header,
        revivedHeaderHash: revived?.[C.HEADER_HASH].toString("hex") ?? null,
        queue: queue.nodes.map(({ node, headerHash }) => ({
          outRef: `${node.utxo.txHash}#${node.utxo.outputIndex.toString()}`,
          headerHash,
        })),
        point: checkpoint.head,
        snapshot: checkpoint.capture.snapshotDigest,
      }),
    );
    if (config.SPECULATIVE_COMMIT_BUILD)
      yield* invalidateSpeculativeCommitCandidate(globals, config, "T1");
    const owner = yield* openOwner;
    if (owner === undefined) return;
    yield* preparation.assertCurrent;
    const diagnostics = yield* Effect.tryPromise({
      try: () => owner.diagnostics(),
      catch: (cause) => failure("Retained native diagnostics failed", cause),
    });
    // The plan binds only the replaced journal, so a crash between native
    // restoration and the SQL repair resumes it whichever block then wins.
    const plan = yield* owned(
      current(decision.kind).pipe(
        Effect.zipRight(
          prepareRetainedNativeHistoryRecoveryPlan(
            checkpoint,
            {
              bindingDigest: input.binding.digest,
              manifestId: checkpoint.manifestId,
              headerHash: header,
              signedTransactionHash: signedTx,
              signedTransactionCborSha256: sha(record[C.SIGNED_TX_CBOR]!),
              targetRoot,
              journalDigest: derived.identity,
            },
            evidenceDigest,
            {
              durableRoot: diagnostics.durableRoot,
              candidateRoot: record[C.EXPECTED_UTXOS_ROOT],
            },
            SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
          ),
        ),
      ),
    );
    yield* executeHistoryDependentRecovery({
      checkpoint,
      preparation,
      plan,
      owner,
      repair: Effect.gen(function* () {
        const { parentAggregate } = yield* current(decision.kind);
        const sql = yield* SqlClient.SqlClient;
        // Abandons the journal under its replacement digest and reopens every
        // member (L2 transactions, deposits, withdrawals, forced
        // transactions), restoring the speculative ledger, in this
        // transaction. Its signed content is kept.
        const results = yield* reincludeStateQueueCorrectedBlocks([
          {
            headerHash: header,
            transitionDigest: replacementDigest,
            kind: "unlanded",
          },
        ]);
        if (
          results.length !== 1 ||
          !results[0]!.journalFound ||
          results[0]!.abandonedFromStatus === undefined
        )
          return yield* Effect.fail(
            failure(`Signed-intent replacement did not abandon ${header}`),
          );
        yield* MutationJobsDB.abandonLocalBlockFinalization(
          headerHash,
          `${context} can no longer land: ${decision.kind === "replace" ? decision.cause : `replaced block ${revived![C.HEADER_HASH].toString("hex")} holds its base's slot`}`,
        );
        // A replaced block can never be continued; retire only its lease.
        yield* StateQueueLeases.release(record[C.STATE_QUEUE_LEASE_TOKEN]);
        // The SQL marker follows the native root the plan's CAS proved, from
        // the replaced journal's candidate root (or already at its target
        // when a resumed plan re-runs this repair) and nothing else.
        const engine = yield* sql`UPDATE mpf_engine_state
          SET root_hex = ${targetRoot},
            utxo_payload_entry_count = ${parentAggregate?.entryCount ?? null},
            utxo_payload_encoded_tuple_bytes = ${parentAggregate?.encodedTupleBytes ?? null},
            updated_at = NOW()
          WHERE store_name = 'ledger'
            AND root_hex IN (${record[C.EXPECTED_UTXOS_ROOT]}, ${targetRoot})
          RETURNING store_name`;
        if (engine.length !== 1)
          return yield* Effect.fail(
            failure(
              "Native SQL marker changed before the signed-intent replacement",
            ),
          );
        // The winner takes its members back; its SQL marker moves to its
        // candidate root and native replay follows at local finalization.
        if (revived !== undefined)
          yield* reviveReplacedCanonicalJournal(revived[C.HEADER_HASH]);
      }),
      afterSqlCommit: Effect.gen(function* () {
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
        // Replaced: no journal is active; the commit preflight re-derives the
        // tail and boundary from L1 and selects the reopened members again.
        // Revived: local finalization replays the winner first.
        yield* Ref.set(
          globals.LOCAL_FINALIZATION_PENDING,
          revivedBlock !== undefined,
        );
        yield* Ref.set(
          globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
          revivedBlock ?? "",
        );
        yield* Queue.takeAll(globals.COMMIT_SUBMIT_WAKE_QUEUE);
        yield* Queue.takeAll(globals.SPECULATIVE_BUILD_WAKE_QUEUE);
      }),
    });
    yield* reportOnce(reportKey, undefined);
    yield* Effect.logWarning(
      decision.kind === "replace"
        ? `Replaced ${context}: ${decision.cause}. Restored native root ${targetRoot} and reopened its members for recommit.`
        : `Revived replaced block ${revived![C.HEADER_HASH].toString("hex")}: it holds the base slot of ${context}, which was abandoned; local finalization replays the winner.`,
    );
  });
