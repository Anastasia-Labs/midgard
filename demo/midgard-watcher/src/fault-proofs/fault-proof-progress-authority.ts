import {
  assertWorkflowActuationPermitIdentity,
  createWorkflowReconciliationPermitController,
  type HeaderFaultDecision,
  requireRunnableHeaderFault,
  revokeWorkflowActuationPermit,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs";

import { type WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import type { WatcherProofRetention } from "../l1-follower/proof-retention.js";
import type { WatcherFaultDecisionJournal } from "./fault-decision-journal.exact-record.js";
import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";
import { createWatcherProofObjectiveCleanup } from "./fault-proof-objective-cleanup.js";
import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";
import {
  canOpenWatcherProofObjective,
  completeWatcherProofObjective,
  listWatcherProofObjectives,
  matchingWatcherProofMarker,
  watcherProofJobActive,
  watcherProofJobPending,
  watcherProofMarkerMatches,
  type WatcherProofVerifiedCompletion,
} from "./fault-proof-objective-table.js";
import {
  createPinsInFlight,
  objectiveKey as keyOf,
  pruneCachedDecisions,
  releaseAtTip,
} from "./fault-proof-progress-authority.holds.js";
import { withProofMaterialsRoster } from "./fault-proof-progress-authority.proof-materials.js";
import { createProgressSupersede } from "./fault-proof-progress-authority.supersede.js";
import type {
  WatcherFaultProofProgressAuthority,
  WatcherFaultProofProgressContext,
  WatcherFaultProofProgressListeners,
  WatcherProofDepartureStep,
} from "./fault-proof-progress-authority.types.js";
import type { WatcherDecisionHold } from "./watcher-decision-hold.js";
import { openWatcherJournalDatabase } from "./watcher-journal-database.js";
import { watcherJournalOpener } from "./watcher-journal-database.opener.js";

export type {
  WatcherDecisionSupersedeHoldReason,
  WatcherDecisionSupersedeRequest,
  WatcherDecisionSupersession,
  WatcherFaultProofProgressAuthority,
  WatcherFaultProofProgressContext,
  WatcherFaultProofProgressListeners,
  WatcherFaultProofProgressRequest,
  WatcherProofDepartureStep,
  WatcherProofMaterialsRoster,
} from "./fault-proof-progress-authority.types.js";
import { assertWatcherStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import { openWatcherFaultDecisionJournal } from "./fault-decision-journal.js";

type Objective = {
  decision: HeaderFaultDecision;
  historical?: Readonly<{
    generation: string;
    permit: WorkflowActuationPermit;
  }>;
  currentPermit?: WorkflowActuationPermit;
  currentDecisionDigest?: string;
  /** Left open by an earlier process: kept only if its header has left the
   * queue, for the chain to say whether our removal landed. */
  restored?: Readonly<{ settled: boolean }>;
  /** Since its header left the queue the chain showed our removal landed,
   * short of final, and its proof step was told `confirmed`. */
  landed?: true;
};

/** What the chain shows of an objective whose header left the queue:
 * our removal final (its verified completion), landed short of final, not
 * ours or absent, or not readable now (asked again by the next
 * observation). */
export type WatcherProofDeparture =
  | Readonly<{ kind: "final"; verified: WatcherProofVerifiedCompletion }>
  | Readonly<{ kind: "landed" | "absent" | "unknown" }>;

/** Admitted faults authorize work; an objective no fault names any more is
 * driven by nothing while its header is queued. Once its header leaves the
 * queue the chain alone decides it, and no run of its family happens: our
 * removal landed keeps it until final, when it is marked completed and
 * released; anything else releases it, and a thread of ours it leaves
 * standing is the own-thread sweep's to reclaim. */
export const createWatcherFaultProofProgressAuthority = (
  input: Parameters<typeof createProgressAuthority>[0],
): WatcherFaultProofProgressAuthority =>
  createProgressAuthority(withProofMaterialsRoster(input));

const createProgressAuthority = (
  input: WatcherFaultProofProgressListeners & {
    readonly journalRoot: string;
    readonly deploymentFingerprint: string;
    readonly categories: readonly WatcherInstalledWorkflowCategory[];
    readonly authenticationKey: Uint8Array;
    readonly retention?: WatcherProofRetention; // holds open objectives' L1 history
    /** Whether a transaction this process sent for the objective can still
     * land (`WorkflowInFlight.sending`). */
    readonly sending: (objective: WatcherProofObjective) => boolean;
    /** Drops the objective's part of the in-flight set. */
    readonly forgetInFlight: (objective: WatcherProofObjective) => void;
    /** What the chain shows of the objective's removal once its header left
     * the queue, asked under `actuationPermit` (its reconciliation permit).
     * Absent, every departure releases its objective. */
    readonly departure?: (
      request: Readonly<{
        decision: HeaderFaultDecision;
        actuationPermit: WorkflowActuationPermit;
        rollbackGeneration: string;
      }>,
    ) => Promise<WatcherProofDeparture>;
    /** Told each proof step a departed objective takes from the chain. */
    readonly onDepartureStep?: (
      decision: HeaderFaultDecision,
      rollbackGeneration: string,
      step: WatcherProofDepartureStep,
    ) => void;
  },
): WatcherFaultProofProgressAuthority => {
  const database = () =>
    openWatcherJournalDatabase({
      journalRoot: input.journalRoot,
      authenticationKey: input.authenticationKey,
    });
  const cleanup = createWatcherProofObjectiveCleanup({
    database,
    ...(input.jobInProcess === undefined
      ? {}
      : { jobInProcess: input.jobInProcess }),
    onRemoved: input.onObjectiveRemoved,
  });
  // A journal failure is retried by the next use.
  const openDecisions: () => Promise<WatcherFaultDecisionJournal> =
    watcherJournalOpener(
      () =>
        openWatcherFaultDecisionJournal({
          directory: input.journalRoot,
          deploymentFingerprint: input.deploymentFingerprint,
          launchScope: input.categories,
          authenticationKey: input.authenticationKey,
        }),
      { retryInBackground: false },
    ).open;
  const k = input.retention?.securityParameter; // markers are made k deep
  const objectives = new Map<string, Objective>();
  // Objectives held by name (past their maturity with nothing landed): they
  // take no new work and clear once their header leaves the finalized queue.
  const held = new Map<
    string,
    WatcherDecisionHold & Readonly<{ kind: "objective" }>
  >();
  const decisions = new Map<string, HeaderFaultDecision>();
  // A retry is safe: initialization reads the journals before it changes any
  // state, so a latched journal throws again at its first read, and an open
  // that could not complete changed nothing.
  const initializeOnce = watcherJournalOpener(() => initialize(), {
    retryInBackground: false,
  }).open;
  let epoch = 0;
  // Open objectives whose L1 history pruning removed before a pin held it,
  // by the admission that tried: the retention names them
  // (l1_proof_history_pruned), each later new observation retries the pin
  // from current facts, and they stay open.
  const unheld = new Map<string, number>();
  // Completed objectives whose marker write failed: held until a restart.
  const unmarked = new Set<string>();
  const pins = createPinsInFlight();
  // Whether a hold is the authority's; a pass at tip releases the others.
  const owned = (key: string): boolean =>
    [objectives, held, unmarked, pins].some((set) => set.has(key));
  let admissions = 0;
  const hold = async (target: WatcherProofObjective): Promise<void> => {
    const result = await pins.track(keyOf(target), async () =>
      input.retention?.pin(target),
    );
    if (result?.kind === "already_pruned")
      unheld.set(keyOf(target), admissions);
    else unheld.delete(keyOf(target));
  };
  let lastObservation: string | undefined;
  let generation = -1n;
  const pruneDecisions = (): void =>
    pruneCachedDecisions(decisions, objectives.values());
  // The objective keeps its L1 history pinned while held; a hold is never
  // dispatched again in this process.
  const holdObjective = async (hold: WatcherDecisionHold): Promise<void> => {
    if (hold.kind !== "objective") return;
    const key = keyOf(hold);
    if (!held.has(key) && !objectives.has(key))
      await pins.track(key, async () => input.retention?.pin(hold));
    objectives.delete(key);
    held.set(key, hold);
    input.onObjectiveSettled?.(hold);
  };
  const queued = (
    observation: WatcherAuthenticatedStateQueueObservation,
    target: WatcherProofObjective,
  ): boolean =>
    observation.finalizedHeaders.some(
      ({ headerHash }) => headerHash === target.headerHash,
    );
  // An objective whose header left the finalized queue has resolved: its
  // proof or another landed and is final, or the header merged. Its pin goes,
  // and its rows once no job of it is queued or active: such a job's run
  // adopts the objective again, and a later observation releases it. Its
  // in-flight part goes once nothing in it may land (`in-flight-released.ts`).
  const release = async (target: WatcherProofObjective): Promise<void> => {
    await input.retention?.release(target);
    if (!input.sending(target)) input.forgetInFlight(target);
    await cleanup.forget(target, { final: false, departed: true });
  };
  const clearResolvedHolds = async (
    observation: WatcherAuthenticatedStateQueueObservation,
  ): Promise<void> => {
    for (const [key, hold] of held) {
      if (queued(observation, hold)) continue;
      held.delete(key);
      input.onObjectiveSettled?.(hold);
      await release(hold);
    }
  };
  // An objective left by an earlier process that nothing drives again: its
  // rows go as at startup (at once if its header has left the queue) and
  // its hold stays for a pass at tip.
  const forgetRestored = async (
    key: string,
    departed: boolean,
  ): Promise<void> => {
    const objective = objectives.get(key)!;
    objectives.delete(key);
    unheld.delete(key);
    await cleanup.forget(objective.decision, { final: false, departed });
  };
  const initialize = async (): Promise<void> => {
    const latest = new Map<string, HeaderFaultDecision>();
    for (const { decision } of await (await openDecisions()).readAll())
      if (decision.decision === "fault_detected") {
        decisions.set(decision.decisionDigest, decision);
        latest.set(keyOf(decision), decision);
      }
    // A restart remembers nothing an earlier process sent, so no row drives
    // work by itself: a marked one is final and pruned. An open one with
    // its decision is kept until the first observation: a live fault admits
    // it again and its run reads from the chain what was done; a header no
    // longer queued asks the chain whether our removal landed, as a
    // departure does; any other is forgotten as if at startup.
    const rows = listWatcherProofObjectives(database(), input.categories);
    for (const row of rows) {
      const target = row.objective;
      // No job of this process owns a queued or active row left by the last
      // one; a later run of the objective registers its job again.
      if (watcherProofJobPending(database(), target)) cleanup.markStale(target);
      // Rows of an active job are left to that job's own finish.
      const settled = !watcherProofJobActive(database(), target);
      const final =
        row.marker !== null && watcherProofMarkerMatches(row.marker, k);
      const decision = final ? undefined : latest.get(keyOf(target));
      // Its pin stays, owned again while it is kept.
      if (decision !== undefined) {
        objectives.set(keyOf(target), { decision, restored: { settled } });
        continue;
      }
      // Its hold stays: only a pass at tip releases a hold nothing owns.
      if (final && settled) await input.retention?.release(target); // a crash can leave the pin
      await cleanup.forget(target, { final, departed: settled });
    }
    pruneDecisions();
  };
  // The reconciliation permit of an objective no live fault names, minted
  // once per rollback generation.
  const reconciliationPermit = (
    objective: Objective,
    rollbackGeneration: string,
  ): WorkflowActuationPermit => {
    if (objective.historical?.generation !== rollbackGeneration)
      objective.historical = {
        generation: rollbackGeneration,
        permit: createWorkflowReconciliationPermitController({
          decision: objective.decision,
          deploymentFingerprint: input.deploymentFingerprint,
          rollbackGeneration,
        }).permit,
      };
    return objective.historical.permit;
  };
  const complete = async (
    objective: WatcherProofObjective,
    done?: WatcherProofVerifiedCompletion,
  ): Promise<void> => {
    objectives.delete(keyOf(objective));
    unheld.delete(keyOf(objective));
    held.delete(keyOf(objective));
    input.onObjectiveSettled?.(objective);
    pruneDecisions();
    // The row frees the cap slot and spares a restart one verification. A
    // failed write leaves it open, so the next start verifies it again.
    let marked = false;
    try {
      marked = completeWatcherProofObjective(database(), objective, done, k);
    } catch {
      // Verified again on the next start.
    }
    // Released once the marker holds, and forgotten once its job is done
    // and an observation no longer queues its header (a fault then names
    // the marker, not a fresh objective); unmarked, it is verified again.
    if (!input.sending(objective)) input.forgetInFlight(objective);
    if (marked) {
      unmarked.delete(keyOf(objective));
      await input.retention?.release(objective);
      await cleanup.forget(objective, { final: true, departed: false });
    } else unmarked.add(keyOf(objective));
  };
  const supersede = createProgressSupersede({
    ...input,
    initialize: initializeOnce,
    cleanup,
    objectives,
    held: (key) => held.has(key),
    forgetObjective: (key) => {
      objectives.delete(key);
      unheld.delete(key);
    },
    decisions,
    openDecisions,
    pruneDecisions,
  });
  return Object.freeze({
    supersede,
    admit: async (request) => {
      assertWatcherStateQueueObservation(request.observation);
      if (
        request.observation.deploymentIdentityDigest !==
          input.deploymentFingerprint ||
        !/^(0|[1-9][0-9]*)$/u.test(request.rollbackGeneration)
      )
        throw new Error(
          "proof progress observation has a foreign deployment or generation",
        );
      const startedEpoch = epoch;
      admissions += 1;
      await initializeOnce();
      if (
        epoch !== startedEpoch ||
        BigInt(request.rollbackGeneration) < generation
      )
        return [];
      generation = BigInt(request.rollbackGeneration);
      await clearResolvedHolds(request.observation);
      if (epoch !== startedEpoch) return [];
      await cleanup.retry(
        request.observation,
        (target) => objectives.has(keyOf(target)) || held.has(keyOf(target)),
      );
      if (epoch !== startedEpoch) return [];
      // The revision changes with every admitted queue observation; the
      // decision driver admits one per follower change, which is the wakeup.
      const queuePoint = request.observation.nativePoint;
      const observationRevision = `${request.observation.observationDigest}:${queuePoint.blockHash}`;
      const observationKey = `${request.rollbackGeneration}:${observationRevision}`;
      const changed = observationKey !== lastObservation;
      const contexts: WatcherFaultProofProgressContext[] = [];
      const fault = request.fault;
      const currentKey =
        fault === undefined ? undefined : keyOf(fault.decision);
      if (fault !== undefined) {
        const runnable = requireRunnableHeaderFault(fault.decision);
        if (
          runnable.launchScope.length !== input.categories.length ||
          runnable.launchScope.some(
            (category, index) => category !== input.categories[index],
          )
        )
          throw new Error(
            "proof progress fault changed the admitted launch scope",
          );
        const authority = assertWorkflowActuationPermitIdentity({
          permit: fault.actuationPermit,
          category: fault.decision.category,
          rollbackGeneration: request.rollbackGeneration,
        });
        if (
          authority.authority !== "submission" ||
          authority.deploymentFingerprint !== input.deploymentFingerprint ||
          authority.headerHash !== fault.decision.headerHash ||
          authority.decisionDigest !== fault.decision.decisionDigest ||
          fault.deadline.headerHash !== fault.decision.headerHash ||
          !request.observation.finalizedHeaders.some(
            ({ headerHash }) => headerHash === fault.decision.headerHash,
          )
        )
          throw new Error(
            "proof progress fault differs from its authenticated observation or authority",
          );
        // At its cap of open objectives the table refuses a new one: the
        // supervisor reports journal_capacity and a later observation retries.
        // A held objective takes no new work until it resolves.
        if (
          !held.has(currentKey!) &&
          (objectives.has(currentKey!) ||
            canOpenWatcherProofObjective(database(), fault.decision))
        ) {
          if (!objectives.has(currentKey!)) {
            await hold(fault.decision);
            if (epoch !== startedEpoch) return [];
          }
          decisions.set(fault.decision.decisionDigest, fault.decision);
          const known = objectives.get(currentKey!);
          // A live fault drives it afresh: what an earlier process or its
          // departure left is read from the chain by its run.
          const objective =
            known === undefined || known.restored !== undefined
              ? { decision: fault.decision }
              : known;
          delete objective.landed;
          objective.currentPermit = fault.actuationPermit;
          objective.currentDecisionDigest = fault.decision.decisionDigest;
          objectives.set(currentKey!, objective);
          contexts.push({
            observationRevision,
            ...fault,
            rollbackGeneration: request.rollbackGeneration,
          });
        }
      }
      if (changed) {
        for (const [key, objective] of objectives) {
          const tried = unheld.get(key);
          if (tried !== undefined && tried < admissions)
            await hold(objective.decision);
          if (key === currentKey) continue;
          if (queued(request.observation, objective.decision)) {
            delete objective.landed;
            // Left by an earlier process and named by no live fault: it is
            // forgotten as at startup; a live fault admits it again.
            if (objective.restored !== undefined) {
              await forgetRestored(key, objective.restored.settled);
              if (epoch !== startedEpoch) return [];
            }
            continue;
          }
          // Its header left the queue: the chain alone decides it, and no
          // run of its family happens. Our removal landed keeps it until
          // final, when it is marked completed against that removal and
          // released (its proof-materials needs go with it). Anything else
          // releases it as a resolved hold is: nothing drives it again until
          // its header is queued again, which admits it afresh. A
          // transaction this process sent for it keeps its coins held until
          // it lands final or dies (`in-flight-released.ts`). A marked
          // completion is final already: it is released.
          const departure: WatcherProofDeparture =
            input.departure === undefined ||
            matchingWatcherProofMarker(
              database(),
              objective.decision,
              k,
              input.categories,
            ) !== null
              ? { kind: "absent" }
              : await input.departure({
                  decision: objective.decision,
                  actuationPermit: reconciliationPermit(
                    objective,
                    request.rollbackGeneration,
                  ),
                  rollbackGeneration: request.rollbackGeneration,
                });
          if (epoch !== startedEpoch) return [];
          const step = (taken: WatcherProofDepartureStep) =>
            input.onDepartureStep?.(
              objective.decision,
              request.rollbackGeneration,
              taken,
            );
          // One seen landed whose removal is gone with nothing of ours
          // standing ends cancelled.
          if (departure.kind === "absent" && objective.landed === true)
            step("cancelled");
          // One left by an earlier process is forgotten as at startup: its
          // hold stays until a pass at tip finds the departure k deep.
          if (departure.kind === "absent" && objective.restored !== undefined) {
            await forgetRestored(key, true);
            if (epoch !== startedEpoch) return [];
            continue;
          }
          if (departure.kind === "absent") {
            objectives.delete(key);
            unheld.delete(key);
            input.onObjectiveSettled?.(objective.decision);
            await release(objective.decision);
            if (epoch !== startedEpoch) return [];
            continue;
          }
          delete objective.restored;
          if (departure.kind === "landed" && objective.landed !== true) {
            objective.landed = true;
            step("confirmed");
          }
          if (departure.kind !== "final") continue;
          step("completed");
          await complete(objective.decision, departure.verified);
          if (epoch !== startedEpoch) return [];
        }
      }
      if (epoch === startedEpoch)
        await releaseAtTip(input.retention, request.observation, owned);
      if (epoch !== startedEpoch) return [];
      assertWatcherStateQueueObservation(request.observation);
      lastObservation = observationKey;
      pruneDecisions();
      input.onAdmitted?.(request.observation, contexts);
      return Object.freeze(contexts);
    },
    unfinishedCount: () => objectives.size + held.size,
    decisionHolds: () => Object.freeze([...held.values()]),
    cleanupFailures: cleanup.failures,
    holdObjective,
    revokeAuthority: (reason) => {
      for (const objective of objectives.values()) {
        if (objective.currentPermit !== undefined)
          revokeWorkflowActuationPermit(objective.currentPermit, reason);
        if (objective.historical !== undefined)
          revokeWorkflowActuationPermit(objective.historical.permit, reason);
        delete objective.currentPermit;
        delete objective.historical;
      }
      lastObservation = undefined;
      epoch += 1;
    },
    reconcileExecution: async ({ objective: key, rollbackGeneration }) => {
      await initializeOnce();
      const objective = objectives.get(keyOf(key));
      if (objective === undefined)
        throw new Error(
          "proof progress reconciles an objective it never admitted",
        );
      const controller = createWorkflowReconciliationPermitController({
        decision: objective.decision,
        deploymentFingerprint: input.deploymentFingerprint,
        rollbackGeneration,
      });
      objective.historical = {
        generation: rollbackGeneration,
        permit: controller.permit,
      };
      return controller.permit;
    },
    markCompleted: complete,
    markResolved: async (objective) => {
      objectives.delete(keyOf(objective));
      unheld.delete(keyOf(objective));
      held.delete(keyOf(objective));
      unmarked.delete(keyOf(objective));
      input.onObjectiveSettled?.(objective);
      pruneDecisions();
      input.forgetInFlight(objective);
      await input.retention?.release(objective);
      await cleanup.forget(objective, { final: true, departed: true });
    },
    drives: (objective) => objectives.has(keyOf(objective)),
    completionMarker: (objective) =>
      matchingWatcherProofMarker(database(), objective, k, input.categories),
  });
};
