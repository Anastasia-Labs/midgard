import { SqlClient } from "@effect/sql";
import { Effect, Option, Ref } from "effect";

import type { HistoryRecoveryIntent } from "../database/eventHistoryRecoveryPlans.js";
import {
  discardPreparedHistoryRecoveryPlan,
  DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN,
  retainedPreparedRecoveryPlan,
} from "../database/eventHistoryRecoveryPlans.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import {
  eventHistoryCanonicalJson,
  readBoundRecoveryLedgerSnapshot,
} from "../l1-event-history-source.js";
import { LEDGER_SCAN_TIMEOUT_MS } from "../l1-ledger-snapshot.js";
import {
  journalAbandonment,
  SignedIntentReplacementIntegrityError,
} from "./canonical-journal-recovery.js";
import { HistoryRecoverySuperseded } from "./event-history-recovery.js";
import { Globals } from "./globals.js";
import { compensateDisplacement } from "./history-expired-intent-release.compensate-displacement.js";
import { displacement } from "./history-expired-intent-release.displacement.js";
import { openRetainedNativeOwner } from "./history-expired-intent-release.open-retained-native-owner.js";
import { ownedBy } from "./history-expired-intent-release.owned.js";
import type { ReplacedBlockRevivalInput } from "./history-expired-intent-release.prepare-replaced-block-revival.js";
import {
  displacementIdentity,
  recoverDisplacement,
} from "./history-expired-intent-release.recover-displacement.js";
import {
  canonicalEvidence,
  replacedBlockLanding,
} from "./history-expired-intent-release.replaced-block-landing.js";
import { reopenDisplaced } from "./history-expired-intent-release.revive-over.js";
import { authenticateQueue } from "./history-expired-intent-release.signed-commit-node.js";
import {
  anyActiveJournal,
  C,
  failure,
  sha,
} from "./history-expired-intent-release.table.js";
import {
  admittedRemovals,
  stateQueueCorrectionRewindDisposition,
} from "./state-queue-correction-rewind.admitted-removals.js";
import {
  loadStateQueueCorrectionObserverState,
  prepareStateQueueCorrectionRewind,
} from "./state-queue-correction-rewind.js";
import { nativeOwnerOpenWait } from "./state-queue-correction-rewind.prepare-state-queue-correction-rewind.js";

type Retained = {
  headerHash: string;
  expectedRoot: string;
  targetRoot?: string;
  recoveryId?: string;
  journalDigest?: string;
  displacedHeaderHashes?: readonly string[];
  displacementIntent?: HistoryRecoveryIntent;
};

/** The durable operation, rather than observer hints, owns its restart.
 * Its immutable roots also authorize a deterministic inverse CAS when the
 * current branch makes the original SQL repair moot. The original plan stays
 * durable until native and SQL again agree. No corrected transaction is replayed. */
export const resumeRetainedDisplacement = (
  input: ReplacedBlockRevivalInput,
  retained: Retained,
) =>
  Effect.gen(function* () {
    const owned = ownedBy(input.preparation);
    const integrity = (message: string) =>
      Effect.fail(
        new SignedIntentReplacementIntegrityError(retained.headerHash, message),
      );
    const load = Effect.gen(function* () {
      const current = yield* retainedPreparedRecoveryPlan(input.binding.digest);
      if (
        current?.kind !== "displaced_block_revival" ||
        current.recoveryId === undefined ||
        current.recoveryId !== retained.recoveryId ||
        current.targetRoot === undefined ||
        current.displacedHeaderHashes === undefined
      )
        return yield* integrity("The retained displacement operation changed");
      const records: Pending.Record[] = [];
      for (const header of [
        current.headerHash,
        ...current.displacedHeaderHashes,
      ]) {
        const found = yield* Pending.retrieveByHeaderHash(
          Buffer.from(header, "hex"),
          true,
        );
        if (Option.isNone(found))
          return yield* integrity(
            `Retained displacement journal ${header} disappeared`,
          );
        records.push(found.value);
      }
      const [winner, ...displaced] = records;
      if (
        winner === undefined ||
        winner[C.STATUS] !== Pending.Status.Abandoned ||
        journalAbandonment(winner) !== "replacement" ||
        displaced.some(
          (record) => record[C.STATUS] !== Pending.Status.Finalized,
        ) ||
        records.some(
          (record) =>
            record[C.DEPLOYMENT_MANIFEST_ID] !== input.checkpoint.manifestId,
        ) ||
        current.targetRoot !== winner[C.BASE_UTXOS_ROOT] ||
        displacementIdentity(winner, displaced) !== current.journalDigest
      )
        return yield* integrity(
          "The retained displacement no longer binds its unchanged journals",
        );
      if (yield* anyActiveJournal)
        return yield* integrity(
          "An active journal conflicts with retained displacement recovery",
        );
      return { current, winner, displaced, records };
    });
    const bound = yield* owned(load);
    const capture = yield* Effect.tryPromise({
      try: (signal) =>
        readBoundRecoveryLedgerSnapshot({
          ...input.transport,
          timeoutMs: LEDGER_SCAN_TIMEOUT_MS,
          binding: input.binding,
          addresses: [input.contracts.stateQueue.spendingScriptAddress],
          at: input.checkpoint.head,
          signal,
        }),
      catch: (cause) =>
        failure("Retained displacement exact-point capture failed", cause),
    });
    yield* input.preparation.assertCurrent;
    const queue = yield* authenticateQueue(
      capture.ledger.outputs,
      input.contracts,
    );
    const { canonicalHistory, canonicalDepth } = yield* owned(
      canonicalEvidence(
        input.binding,
        input.checkpoint,
        bound.records.flatMap((record) =>
          record[C.INTENDED_TX_HASH] === null
            ? []
            : [record[C.INTENDED_TX_HASH]!.toString("hex")],
        ),
      ),
    );
    const resolution = Effect.gen(function* () {
      const fresh = yield* load;
      const observer = yield* loadStateQueueCorrectionObserverState(
        input.rewindAuthority,
        true,
      );
      const correction = yield* stateQueueCorrectionRewindDisposition(
        input.rewindAuthority,
      );
      if (correction !== undefined) {
        const admitted = yield* admittedRemovals(input.rewindAuthority);
        const base = fresh.winner[C.BASE_TAIL_HEADER_HASH].toString("hex");
        const removal =
          admitted.kind === "admitted"
            ? admitted.transitions.get(base)
            : undefined;
        const landing = replacedBlockLanding(
          fresh.winner,
          queue,
          observer,
          canonicalHistory,
        );
        const node = landing?.onQueue;
        const priorWinner =
          removal?.previousQueue.findIndex(
            (member) => member.headerHash === fresh.current.headerHash,
          ) ?? -1;
        const stillSameNode =
          node !== undefined &&
          removal?.nextQueue.some(
            (member) =>
              member.headerHash === fresh.current.headerHash &&
              member.outRef ===
                `${node.node.utxo.txHash}#${node.node.utxo.outputIndex}`,
          ) === true;
        // The admitted link removed this exact shared base, while the same
        // successor that won its slot remains canonical. Re-prove rollback
        // displacement; a correction hint alone never reopens these journals.
        if (
          canonicalDepth !== undefined &&
          node !== undefined &&
          stillSameNode &&
          priorWinner > 0 &&
          removal?.previousQueue[priorWinner - 1]?.headerHash === base
        ) {
          const displaced = yield* displacement({
            blocking: fresh.displaced.map((record) =>
              record[C.HEADER_HASH].toString("hex"),
            ),
            node,
            queued: true,
            winner: fresh.winner,
            base,
            baseRoot: fresh.winner[C.BASE_UTXOS_ROOT],
            queue,
            observer,
            depth: canonicalDepth,
            required: input.rewindAuthority.requiredFinalityDepth,
          });
          if (
            typeof displaced !== "string" &&
            displacementIdentity(fresh.winner, displaced) ===
              fresh.current.journalDigest
          )
            return { kind: "correction" as const, ...fresh };
        }
        return { kind: "wait" as const, ...fresh };
      }
      if (
        replacedBlockLanding(
          fresh.winner,
          queue,
          observer,
          canonicalHistory,
        ) !== undefined
      )
        return { kind: "resume" as const, ...fresh };
      const returned = fresh.displaced.find(
        (record) =>
          record[C.BASE_TAIL_HEADER_HASH].equals(
            fresh.winner[C.BASE_TAIL_HEADER_HASH],
          ) &&
          record[C.BASE_UTXOS_ROOT] === fresh.winner[C.BASE_UTXOS_ROOT] &&
          replacedBlockLanding(record, queue, observer, canonicalHistory) !==
            undefined,
      );
      for (const record of fresh.displaced) {
        const signed = record[C.INTENDED_TX_HASH]?.toString("hex");
        if (
          signed !== undefined &&
          canonicalHistory.has(signed) &&
          queue.nodes.some(
            (node) =>
              `${node.node.utxo.txHash}#${node.node.utxo.outputIndex}` ===
              record[C.BASE_TAIL_OUT_REF],
          )
        )
          return yield* integrity(
            "Canonical signed commit contradicts its live exact queue input",
          );
      }
      // Undoing the CAS restores every original finalized member. A returned
      // prefix does not authorize restoring an absent finalized descendant.
      const snapshotReturned = fresh.displaced.every(
        (record) =>
          replacedBlockLanding(record, queue, observer, canonicalHistory) !==
          undefined,
      );
      return returned === undefined || !snapshotReturned
        ? { kind: "wait" as const, ...fresh }
        : { kind: "inverse" as const, ...fresh };
    });
    const decided = yield* owned(resolution);
    if (decided.kind === "resume")
      return { kind: "resume" as const, winner: decided.winner };
    if (decided.kind === "wait") {
      if (
        retained.recoveryId !== undefined &&
        retained.displacementIntent !== undefined &&
        (yield* compensateDisplacement(input, {
          recoveryId: retained.recoveryId,
          originalRecoveryId: retained.recoveryId,
          originalIntent: retained.displacementIntent,
        }))
      )
        return { kind: "resolved" as const };
      return { kind: "wait" as const };
    }
    const globals = yield* Globals;
    const clearPublication = Effect.gen(function* () {
      yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
      yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
    });
    if (decided.kind === "correction") {
      const verify = Effect.gen(function* () {
        const fresh = yield* resolution;
        if (fresh.kind !== "correction")
          return yield* Effect.fail(
            new HistoryRecoverySuperseded({
              message: "Retained displacement correction disposition changed",
            }),
          );
      });
      const completed = yield* recoverDisplacement({
        bindingDigest: input.binding.digest,
        checkpoint: input.checkpoint,
        preparation: input.preparation,
        config: input.config,
        winner: decided.winner,
        displaced: decided.displaced,
        verify,
        repair: reopenDisplaced(decided.displaced).pipe(Effect.asVoid),
        afterSqlCommit: clearPublication,
      });
      if (!completed) return { kind: "wait" as const };
      yield* prepareStateQueueCorrectionRewind({
        bindingDigest: input.binding.digest,
        checkpoint: input.checkpoint,
        preparation: input.preparation,
        config: input.config,
        authority: input.rewindAuthority,
      });
      return { kind: "resolved" as const };
    }
    const owner = yield* openRetainedNativeOwner(globals, input.config).pipe(
      Effect.catchIf(
        (error) => nativeOwnerOpenWait(error) !== undefined,
        () => Effect.succeed(undefined),
      ),
    );
    if (owner === undefined) return { kind: "wait" as const };
    const root = yield* Effect.tryPromise({
      try: () => owner.diagnostics(),
      catch: (cause) =>
        failure("Retained displacement inverse diagnostics failed", cause),
    });
    if (
      root.durableRoot !== retained.expectedRoot &&
      root.durableRoot !== decided.current.targetRoot
    )
      return yield* integrity(
        "Native inverse root is outside the retained displacement operation",
      );
    const check = Effect.gen(function* () {
      const fresh = yield* resolution;
      if (fresh.kind !== "inverse")
        return yield* Effect.fail(
          new HistoryRecoverySuperseded({
            message: "The retained displacement inverse disposition changed",
          }),
        );
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        root_hex: string;
      }>`SELECT root_hex FROM mpf_engine_state WHERE store_name = 'ledger' FOR UPDATE`;
      if (rows.length !== 1 || rows[0]!.root_hex !== retained.expectedRoot)
        return yield* integrity(
          "SQL no longer has the displacement's original native root",
        );
    });
    yield* owned(check);
    yield* input.preparation.assertCurrent;
    if (root.durableRoot !== retained.expectedRoot) {
      yield* Effect.tryPromise({
        try: () =>
          owner.restoreCanonicalRoot({
            recoveryId: sha(
              eventHistoryCanonicalJson({
                domain: "midgard-history-displacement-inverse-v1",
                recoveryId: retained.recoveryId,
                expectedRoot: decided.current.targetRoot,
                targetRoot: retained.expectedRoot,
              }),
            ),
            expectedRoot: decided.current.targetRoot!,
            targetRoot: retained.expectedRoot,
          }),
        catch: (cause) =>
          failure(
            "Retained displacement inverse CAS requires resumption",
            cause,
          ),
      });
    }
    yield* input.preparation.assertCurrent;
    const restored = yield* Effect.tryPromise({
      try: () => owner.diagnostics(),
      catch: (cause) =>
        failure("Retained displacement inverse verification failed", cause),
    });
    if (restored.durableRoot !== retained.expectedRoot)
      return yield* integrity(
        "Native inverse did not restore the original SQL root",
      );
    yield* Effect.uninterruptible(
      owned(
        check.pipe(
          Effect.zipRight(
            discardPreparedHistoryRecoveryPlan(
              input.checkpoint,
              DISPLACED_BLOCK_REVIVAL_RECOVERY_DOMAIN,
              retained.headerHash,
            ),
          ),
        ),
      ).pipe(Effect.zipRight(clearPublication)),
    );
    return { kind: "resolved" as const };
  });
