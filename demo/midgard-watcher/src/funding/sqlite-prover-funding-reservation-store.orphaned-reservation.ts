import type { DatabaseSync } from "node:sqlite";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  computeFraudProofWorkflowId,
  type FraudProofWorkflowIdentity,
  parseSignedWorkflowTransactionRetirement,
  parseWorkflowFundingAbandonmentHandoff,
  type WorkflowFundingAbandonmentHandoff,
  type WorkflowFundingPreparedTransition,
} from "@al-ft/midgard-fault-proofs";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationPlan,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "./prover-funding-reservation.js";
import { pendingProverFundingLineageParameters } from "./prover-funding-reservation.retained-signed-inputs.js";
import {
  assertPlanMatchesRecord,
  nextRecord,
} from "./sqlite-prover-funding-reservation-store.derive-signed-transition.js";
import {
  decodeProverFundingOrphanCancel,
  type ProverFundingCancelRows,
} from "./sqlite-prover-funding-reservation-store.orphan-cancel.js";
import {
  type LineageRow,
  parseLineageRow,
  type SubmissionHandoff,
} from "./sqlite-prover-funding-reservation-store.row-parsers.js";

type Abandoned = Readonly<{
  transitionDigest: string;
  transition: WorkflowFundingPreparedTransition;
  handoff: WorkflowFundingAbandonmentHandoff;
}>;

/**
 * Store operations that reclaim a reservation's funding. An unused one
 * returns its inputs to the wallet. One whose workflow will never drive it
 * again is handed over or settled, in one revision-checked transaction that
 * leaves it released, with nothing leased and no signed attempt of its own.
 */
export const createProverFundingReclaimOperations = (deps: {
  readonly database: DatabaseSync;
  readonly transaction: <T>(
    operation: () => T,
    reservationId: string | null,
  ) => T;
  readonly assertPlan: (plan: WatcherProverFundingReservationPlan) => void;
  readonly readOne: (
    reservationId: string,
  ) => WatcherProverFundingReservationRecord | null;
  readonly writeRecord: (
    currentDigest: string | null,
    next: WatcherProverFundingReservationRecord,
  ) => void;
  readonly rebuildLeases: () => void;
  readonly recordedSubmissions: (reservationId: string) => SubmissionHandoff[];
  readonly abandonmentsOf: (reservationId: string) => readonly Abandoned[];
  readonly hasSignedHistory: (reservationId: string) => boolean;
  readonly cancels: ProverFundingCancelRows;
  readonly persistHandoff: (
    record: WatcherProverFundingReservationRecord,
    kind: "submission",
    identityDigest: string,
    value: unknown,
  ) => void;
}): Pick<
  Required<WatcherProverFundingReservationStore>,
  | "releaseUnused"
  | "dropSpentUnused"
  | "handOverOrphanedAttempts"
  | "settleOrphanedReservation"
  | "readOrphanCancel"
  | "recordOrphanCancel"
  | "dropRetiredOrphanCancel"
> => {
  const { database } = deps;
  const insertAcknowledgedAbandonment = database.prepare(`
    INSERT INTO watcher_prover_funding_abandonment_v1(
      reservation_id, transition_digest, record_digest, canonical_json,
      acknowledged_revision
    ) VALUES (?, ?, ?, ?, ?)
  `);
  const insertConfirmedLineage = database.prepare(`
    INSERT INTO watcher_prover_funding_lineage_v1(
      reservation_id, action_kind, output_index, out_ref,
      resolved_output_cbor_hex, transition_digest, phase, lineage_digest
    ) VALUES (?, ?, ?, ?, ?, ?, 'confirmed', ?)
  `);
  const selectConfirmedLineage = database.prepare(`
    SELECT * FROM watcher_prover_funding_lineage_v1
    WHERE reservation_id = ? AND phase = 'confirmed'
  `);
  const deleteRows = (reservationId: string) => {
    for (const table of ["handoff", "abandonment", "lineage", "cancel"])
      database
        .prepare(
          `DELETE FROM watcher_prover_funding_${table}_v1 WHERE reservation_id = ?`,
        )
        .run(reservationId);
  };
  /** The orphan exactly as its caller classified it, still active. */
  const currentOrphan = (snapshot: WatcherProverFundingReservationRecord) => {
    const orphan = parseWatcherProverFundingReservationRecord(snapshot);
    const current = deps.readOne(orphan.reservationId);
    return current !== null &&
      current.recordDigest === orphan.recordDigest &&
      current.state === "active"
      ? current
      : null;
  };
  /** The orphan leaves nothing behind: released, with no rows of its own. */
  const settle = (orphan: WatcherProverFundingReservationRecord) => {
    deleteRows(orphan.reservationId);
    deps.writeRecord(
      orphan.recordDigest,
      nextRecord({
        current: orphan,
        state: "released",
        activeInputs: [],
        pendingTransition: null,
        lastConfirmedTransitionDigest: null,
        conflictCode: null,
      }),
    );
  };
  /** The orphan's next revision, with its leases rebuilt. */
  const revise = (orphan: WatcherProverFundingReservationRecord) => {
    const next = nextRecord({ current: orphan });
    deps.writeRecord(orphan.recordDigest, next);
    deps.rebuildLeases();
    return next;
  };
  const clearUnused = async (
    expected: WatcherProverFundingReservationRecord,
    refuseSignedHistory: boolean,
  ): Promise<boolean> => {
    const snapshot = parseWatcherProverFundingReservationRecord(expected);
    return deps.transaction(() => {
      const current = deps.readOne(snapshot.reservationId);
      if (current === null)
        throw new Error("unused prover reservation is missing");
      if (
        current.recordDigest !== snapshot.recordDigest ||
        current.state !== "active" ||
        current.activeInputs.length === 0 ||
        current.pendingTransition !== null ||
        current.lastConfirmedTransitionDigest !== null
      )
        return false;
      // Preparation persists signed bytes before the journal intent. Absence
      // from the journal alone never authorizes reclaiming those inputs.
      if (refuseSignedHistory && deps.hasSignedHistory(current.reservationId))
        return false;
      const next = nextRecord({ current, activeInputs: [] });
      deps.writeRecord(current.recordDigest, next);
      deps.rebuildLeases();
      return true;
    }, snapshot.reservationId);
  };
  return Object.freeze({
    releaseUnused: async (expected) => clearUnused(expected, true),
    dropSpentUnused: async (expected) => clearUnused(expected, false),
    handOverOrphanedAttempts: async ({
      plan,
      expectedRevision,
      orphans: snapshots,
      category,
      headerHash,
    }) => {
      deps.assertPlan(plan);
      return deps.transaction(() => {
        const current = deps.readOne(plan.reservationId);
        if (current === null) throw new Error("prover reservation is missing");
        assertPlanMatchesRecord(plan, current);
        // Only a fresh reservation of this target takes them over. A pending
        // or confirmed transition always leaves signed history.
        if (
          snapshots.length === 0 ||
          current.revision !== expectedRevision ||
          current.state !== "active" ||
          deps.hasSignedHistory(current.reservationId)
        )
          throw new Error("prover reservation hand-over mismatch");
        // Every handed-over attempt names this target; the fresh workflow
        // is the one its decision starts.
        let identity: FraudProofWorkflowIdentity | undefined;
        const funding = new Map(
          current.activeInputs.map((input) => [input.outRef, input]),
        );
        const next = () =>
          nextRecord({ current, activeInputs: [...funding.values()] });
        const moved: ((
          bound: Readonly<{
            workflowId: string;
            identity: FraudProofWorkflowIdentity;
          }>,
        ) => void)[] = [];
        for (const snapshot of snapshots) {
          const orphan = currentOrphan(snapshot);
          const submissions =
            orphan === null
              ? []
              : deps.recordedSubmissions(orphan.reservationId);
          const abandoned =
            orphan === null ? [] : deps.abandonmentsOf(orphan.reservationId);
          // Its submissions make it signed, so it is not `current`.
          // A cancelled orphan's funding is the cancel's, never a replacement's.
          if (
            orphan === null ||
            deps.cancels.has(orphan.reservationId) ||
            orphan.deploymentFingerprint !== current.deploymentFingerprint ||
            submissions.length === 0 ||
            [...submissions, ...abandoned].some(
              ({ handoff: { identity: named } }) =>
                named.deploymentFingerprint !== current.deploymentFingerprint ||
                named.category !== category ||
                named.target.kind !== "state_queue_header" ||
                named.target.headerHash !== headerHash,
            )
          )
            throw new Error("prover reservation hand-over mismatch");
          identity ??= {
            ...submissions[0]!.handoff.identity,
            decisionDigest: current.decisionDigest,
          };
          // The handed-over attempts' funding stays reserved here, so a
          // replacement must spend one of their inputs.
          for (const input of orphan.activeInputs)
            if (input.role === "funding") funding.set(input.outRef, input);
          const superseded = abandoned.map(({ transition, handoff }) => ({
            transition,
            handoff,
          }));
          const pending = orphan.pendingTransition;
          if (pending !== null) {
            const saved = submissions.find(
              ({ identityDigest }) =>
                identityDigest === pending.transitionDigest,
            );
            if (saved === undefined)
              throw new Error("prover reservation hand-over mismatch");
            // In flight when its workflow was lost: superseded from here on.
            superseded.push({
              transition: saved.transition,
              handoff: parseWorkflowFundingAbandonmentHandoff({
                workflowId: saved.handoff.workflowId,
                identity: saved.handoff.identity,
                preparedArtifactDigest: saved.handoff.preparedArtifactDigest,
                expectedJournalSequence: saved.handoff.expectedJournalSequence,
                submissionIntent: saved.handoff.submissionIntent,
                reconciliation: {
                  kind: "reconciled",
                  actionId: saved.handoff.submissionIntent.actionId,
                  outcome: "not_found",
                  txHash: saved.transition.transactionHash,
                },
              }),
            });
          }
          const lineage = (
            selectConfirmedLineage.all(orphan.reservationId) as LineageRow[]
          ).map(parseLineageRow);
          moved.push((bound) => {
            settle(orphan);
            for (const saved of submissions)
              deps.persistHandoff(current, "submission", saved.identityDigest, {
                transition: saved.transition,
                handoff: { ...saved.handoff, ...bound },
              });
            for (const { transition, handoff } of superseded) {
              const value = {
                transition,
                handoff: { ...handoff, ...bound },
              };
              insertAcknowledgedAbandonment.run(
                current.reservationId,
                computeDeploymentManifestJsonDigest(transition),
                computeDeploymentManifestJsonDigest(value),
                watcherCanonicalJson(value),
                current.revision,
              );
            }
            for (const { phase: _phase, ...row } of lineage)
              insertConfirmedLineage.run(
                ...pendingProverFundingLineageParameters({
                  ...row,
                  outRef: row.outRef as `${string}#${string}`,
                  reservationId: current.reservationId,
                }),
              );
          });
        }
        if (identity === undefined)
          throw new Error("prover reservation hand-over mismatch");
        const bound = {
          workflowId: computeFraudProofWorkflowId(identity),
          identity,
        };
        for (const move of moved) move(bound);
        const record = next();
        deps.writeRecord(current.recordDigest, record);
        deps.rebuildLeases();
        return record;
      }, plan.reservationId);
    },
    settleOrphanedReservation: async ({ orphan: snapshot, retirements }) =>
      deps.transaction(() => {
        const orphan = currentOrphan(snapshot);
        if (orphan === null) return false;
        const retired = new Set(
          deps
            .abandonmentsOf(orphan.reservationId)
            .filter(({ handoff }) => handoff.reconciliation.retirement)
            .map(({ transition }) => transition.transactionHash),
        );
        // Each retirement must be of the attempt it names.
        for (const { transactionHash, retirement } of retirements)
          retired.add(
            parseSignedWorkflowTransactionRetirement(
              retirement,
              transactionHash,
            ).transactionHash,
          );
        const attempts = [
          ...deps.recordedSubmissions(orphan.reservationId),
          ...deps.abandonmentsOf(orphan.reservationId),
          ...(orphan.pendingTransition === null
            ? []
            : [{ transition: orphan.pendingTransition }]),
        ].map(({ transition }) => transition.transactionHash);
        const cancel = deps.cancels.read(orphan.reservationId);
        if (cancel !== null) attempts.push(cancel.transactionHash);
        // Only an attempt retired past k can never land again or roll back.
        if (
          (attempts.length === 0 &&
            !deps.hasSignedHistory(orphan.reservationId) &&
            orphan.lastConfirmedTransitionDigest === null) ||
          attempts.some((hash) => !retired.has(hash))
        )
          return false;
        settle(orphan);
        deps.rebuildLeases();
        return true;
      }, null),
    readOrphanCancel: async ({ reservationId }) =>
      deps.cancels.read(reservationId),
    recordOrphanCancel: async ({
      orphan: snapshot,
      signedTransactionCborHex,
    }) => {
      const cancel = decodeProverFundingOrphanCancel(signedTransactionCborHex);
      return deps.transaction(() => {
        const orphan = currentOrphan(snapshot);
        const funding = new Set(
          orphan?.activeInputs
            .filter(({ role }) => role === "funding")
            .map(({ outRef }) => outRef),
        );
        // One live cancel at a time, of signed funding, spending nothing
        // the orphan does not lease as funding.
        if (
          orphan === null ||
          deps.cancels.has(orphan.reservationId) ||
          !deps.hasSignedHistory(orphan.reservationId) ||
          cancel.inputs.length === 0 ||
          cancel.inputs.some((outRef) => !funding.has(outRef))
        )
          throw new Error("prover funding cancel mismatch");
        deps.cancels.insert(orphan.reservationId, cancel);
        return revise(orphan);
      }, null);
    },
    dropRetiredOrphanCancel: async ({ orphan: snapshot, retirement }) =>
      deps.transaction(() => {
        const orphan = currentOrphan(snapshot);
        const cancel =
          orphan === null ? null : deps.cancels.read(orphan.reservationId);
        // A landed cancel is never dropped: the orphan settles with it.
        if (
          orphan === null ||
          cancel === null ||
          parseSignedWorkflowTransactionRetirement(
            retirement,
            cancel.transactionHash,
          ).reason === "included"
        )
          throw new Error("prover funding cancel mismatch");
        deps.cancels.remove(orphan.reservationId);
        return revise(orphan);
      }, null),
  });
};
