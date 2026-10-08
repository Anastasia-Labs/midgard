import {
  assertWorkflowActuationPermitIdentity,
  createWorkflowReconciliationPermitController,
  type FraudProofWorkflowJournalEntry,
  type HeaderFaultDecision,
  requireRunnableHeaderFault,
  revokeWorkflowActuationPermit,
  validateFraudProofWorkflowJournal,
  type WorkflowActuationPermit,
} from "@al-ft/midgard-fault-proofs";

import {
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import {
  assertWatcherNativeBlockAdmission,
  type WatcherNativeBlockAdmission,
} from "../l1/native-block-admission.js";
import type { WatcherProofRetention } from "../l1-follower/proof-retention.js";
import {
  openWatcherFaultDecisionJournal,
  type WatcherFaultDecisionJournal,
} from "./fault-decision-journal.js";
import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";
import {
  readWatcherProofExecution,
  type WatcherProofExecution,
  type WatcherProofObjective,
} from "./fault-proof-objective-journal.js";
import {
  canOpenWatcherProofObjective,
  completeWatcherProofObjective,
  forgetWatcherProofObjective,
  listWatcherProofObjectives,
  pruneWatcherProofObjective,
  watcherProofJobActive,
  watcherProofMarkerMatches,
} from "./fault-proof-objective-table.js";
import type { WatcherFaultProofDeadline } from "./fault-proof-supervisor.js";
import { openWatcherJournalDatabase } from "./watcher-journal-database.js";

export type WatcherFaultProofProgressRequest = Readonly<{
  observation: WatcherAuthenticatedStateQueueObservation;
  nativeProgress?: WatcherNativeBlockAdmission;
  rollbackGeneration: string;
  fault?: Readonly<{
    decision: HeaderFaultDecision;
    actuationPermit: WorkflowActuationPermit;
    deadline: WatcherFaultProofDeadline;
  }>;
}>;

export type WatcherFaultProofProgressContext = Readonly<{
  observationRevision: string;
  decision: HeaderFaultDecision;
  actuationPermit: WorkflowActuationPermit;
  deadline: WatcherFaultProofDeadline | null;
  rollbackGeneration: string;
}>;

export type WatcherFaultProofProgressAuthority = Readonly<{
  admit(
    request: WatcherFaultProofProgressRequest,
  ): Promise<readonly WatcherFaultProofProgressContext[]>;
  revokeAuthority(reason: string): void;
  unfinishedCount(): number;
  /** Records the completion; one verified beyond rollback recovery is
   * marked, so the next start skips it and prunes its workflow journal. */
  markCompleted(
    objective: WatcherProofObjective,
    verified?: Readonly<{
      execution: WatcherProofExecution;
      confirmationDepth: number;
    }>,
  ): Promise<void>;
  updateExecution(input: {
    readonly objective: WatcherProofObjective;
    readonly execution: WatcherProofExecution;
  }): Promise<void>;
  reconcileExecution(input: {
    readonly objective: WatcherProofObjective;
    readonly execution: WatcherProofExecution;
    readonly rollbackGeneration: string;
  }): Promise<WorkflowActuationPermit>;
}>;

type Objective = {
  decision: HeaderFaultDecision;
  entries?: readonly FraudProofWorkflowJournalEntry[];
  workflowId?: string;
  historical?: Readonly<{
    generation: string;
    permit: WorkflowActuationPermit;
  }>;
  currentPermit?: WorkflowActuationPermit;
  currentDecisionDigest?: string;
};
const keyOf = ({ category, headerHash }: WatcherProofObjective): string =>
  `${category}:${headerHash}`;
// Bounds the recently admitted decisions kept in memory.
const MAX_CACHED_DECISIONS = 2_048;

/** Restored decisions authorize observation of signed work only. The supervisor
 * revalidates the latest selected execution immediately before funding. */
export const createWatcherFaultProofProgressAuthority = (input: {
  readonly journalRoot: string;
  readonly deploymentFingerprint: string;
  readonly categories: readonly WatcherInstalledWorkflowCategory[];
  readonly authenticationKey: Uint8Array;
  readonly retention?: WatcherProofRetention; // holds open objectives' L1 history
}): WatcherFaultProofProgressAuthority => {
  const database = () =>
    openWatcherJournalDatabase({
      journalRoot: input.journalRoot,
      authenticationKey: input.authenticationKey,
    });
  let decisionJournal: Promise<WatcherFaultDecisionJournal> | undefined;
  const openDecisions = (): Promise<WatcherFaultDecisionJournal> =>
    (decisionJournal ??= openWatcherFaultDecisionJournal({
      directory: input.journalRoot,
      deploymentFingerprint: input.deploymentFingerprint,
      launchScope: input.categories,
      authenticationKey: input.authenticationKey,
    }));
  const k = input.retention?.securityParameter; // markers are made k deep
  const objectives = new Map<string, Objective>();
  const decisions = new Map<string, HeaderFaultDecision>();
  let initialized: Promise<void> | undefined;
  let epoch = 0;
  // Open objectives whose L1 history pruning removed before a pin held it,
  // by the admission that tried: the retention names them
  // (l1_proof_history_pruned), each later new observation retries the pin
  // from current facts, and they stay open.
  const unheld = new Map<string, number>();
  let admissions = 0;
  const hold = async (target: WatcherProofObjective): Promise<void> => {
    const result = await input.retention?.pin(target);
    if (result?.kind === "already_pruned")
      unheld.set(keyOf(target), admissions);
    else unheld.delete(keyOf(target));
  };
  let lastObservation: string | undefined;
  let generation = -1n;
  let latestNativeProgress:
    | Pick<WatcherNativeBlockAdmission, "blockHash" | "blockNo" | "slot">
    | undefined;
  const readDecision = async (
    digest: string,
  ): Promise<HeaderFaultDecision | undefined> => {
    const cached = decisions.get(digest);
    if (cached !== undefined) {
      decisions.delete(digest);
      decisions.set(digest, cached);
      return cached;
    }
    // The bridge's writer may have appended this decision since startup;
    // the journal reads the rows committed since its last read.
    const decision = (await (await openDecisions()).read(digest))?.decision;
    if (decision?.decision !== "fault_detected") return undefined;
    decisions.set(digest, decision);
    return decision;
  };
  const pruneDecisions = (): void => {
    const retained = new Set(
      [...objectives.values()].flatMap((objective) => [
        objective.decision.decisionDigest,
        objective.currentDecisionDigest,
      ]),
    );
    // A completed objective may still have a coalesced invocation waiting.
    // Keep bounded recent admitted identities so those rechecks do not scan
    // durable history, while active original/latest identities stay pinned.
    const inactive = [...decisions.keys()].filter(
      (digest) => !retained.has(digest),
    );
    for (const digest of inactive.slice(
      0,
      Math.max(0, inactive.length - MAX_CACHED_DECISIONS),
    ))
      decisions.delete(digest);
  };
  const loadExecution = async (objective: Objective): Promise<void> => {
    const execution = await readWatcherProofExecution({
      journalRoot: input.journalRoot,
      deploymentFingerprint: input.deploymentFingerprint,
      objective: objective.decision,
      selectedWorkflowId: objective.workflowId,
    });
    if (execution !== undefined) await adoptExecution(objective, execution);
  };
  const adoptExecution = async (
    objective: Pick<Objective, "entries" | "workflowId"> & {
      decision?: HeaderFaultDecision;
    },
    execution: WatcherProofExecution,
  ): Promise<void> => {
    const target = execution.entries[0]!.identity;
    const decisionDigest = execution.entries[0]!.identity.decisionDigest;
    const decision =
      decisionDigest === undefined
        ? undefined
        : await readDecision(decisionDigest);
    if (
      decision === undefined ||
      decision.category !== target.category ||
      target.target.kind !== "state_queue_header" ||
      decision.headerHash !== target.target.headerHash
    )
      throw new Error(
        "proof progress has no exact recorded execution decision",
      );
    objective.decision = decision;
    objective.entries = execution.entries;
    objective.workflowId = execution.workflowId;
  };
  const initialize = async (): Promise<void> => {
    for (const { decision } of await (await openDecisions()).readAll())
      if (decision.decision === "fault_detected")
        decisions.set(decision.decisionDigest, decision);
    // Startup lists the objective table, never the workflow directories.
    const rows = listWatcherProofObjectives(database(), input.categories);
    for (const row of rows) {
      const target = row.objective;
      const execution = await readWatcherProofExecution({
        journalRoot: input.journalRoot,
        deploymentFingerprint: input.deploymentFingerprint,
        objective: target,
      });
      // Rows of an active job are left to that job's own finish.
      const settled = !watcherProofJobActive(database(), target);
      // A completion verified beyond rollback recovery holds no work.
      if (
        settled &&
        row.marker !== null &&
        (execution === undefined ||
          watcherProofMarkerMatches(row.marker, execution, k))
      ) {
        await input.retention?.release(target); // a crash can leave the pin
        await pruneWatcherProofObjective(database(), input.journalRoot, target);
        continue;
      }
      // A job queued but never started left no execution and holds no work;
      // a live fault queues it again.
      if (execution === undefined) {
        if (settled) {
          await input.retention?.release(target); // admission pins it again
          forgetWatcherProofObjective(database(), target, { decisions: false });
        }
        continue;
      }
      const objective = {} as Objective;
      await adoptExecution(objective, execution);
      await hold(target);
      objectives.set(keyOf(target), objective);
    }
    pruneDecisions();
  };
  const updateExecution = async ({
    objective: key,
    execution,
  }: {
    readonly objective: WatcherProofObjective;
    readonly execution: WatcherProofExecution;
  }): Promise<void> => {
    await (initialized ??= initialize());
    const first = execution.entries[0];
    if (
      first === undefined ||
      first.identity.deploymentFingerprint !== input.deploymentFingerprint ||
      first.identity.category !== key.category ||
      first.identity.target.kind !== "state_queue_header" ||
      first.identity.target.headerHash !== key.headerHash
    )
      throw new Error("proof progress execution update changed its objective");
    validateFraudProofWorkflowJournal({
      workflowId: execution.workflowId,
      entries: execution.entries,
      expectedIdentity: first.identity,
    });
    const decision =
      first.identity.decisionDigest === undefined
        ? undefined
        : await readDecision(first.identity.decisionDigest);
    if (
      decision === undefined ||
      decision.category !== key.category ||
      decision.headerHash !== key.headerHash
    )
      throw new Error(
        "proof progress execution update omitted its exact recorded decision",
      );
    if (!objectives.has(keyOf(key))) await hold(key);
    const objective = objectives.get(keyOf(key)) ?? { decision };
    if (
      objective.workflowId !== undefined &&
      objective.workflowId !== execution.workflowId
    )
      throw new Error("proof progress changed its selected execution");
    if (objective.decision.decisionDigest !== decision.decisionDigest)
      delete objective.historical;
    objective.decision = decision;
    objective.entries = execution.entries;
    objective.workflowId = execution.workflowId;
    objectives.set(keyOf(key), objective);
    pruneDecisions();
  };
  return Object.freeze({
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
      await (initialized ??= initialize());
      if (
        epoch !== startedEpoch ||
        BigInt(request.rollbackGeneration) < generation
      )
        return [];
      const nextGeneration = BigInt(request.rollbackGeneration);
      if (nextGeneration !== generation) latestNativeProgress = undefined;
      generation = nextGeneration;
      const queuePoint = request.observation.nativePoint;
      const progress = request.nativeProgress;
      if (progress !== undefined) {
        assertWatcherNativeBlockAdmission(progress);
        if (
          BigInt(progress.blockNo) < BigInt(queuePoint.blockNo) ||
          BigInt(progress.slot) < BigInt(queuePoint.slot) ||
          (BigInt(progress.blockNo) > BigInt(queuePoint.blockNo) &&
            BigInt(progress.slot) <= BigInt(queuePoint.slot)) ||
          (progress.blockNo === queuePoint.blockNo &&
            (progress.blockHash !== queuePoint.blockHash ||
              progress.slot !== queuePoint.slot))
        )
          throw new Error(
            "proof progress native point is behind or differs from its queue evidence",
          );
        if (latestNativeProgress !== undefined) {
          if (BigInt(progress.blockNo) < BigInt(latestNativeProgress.blockNo))
            return [];
          if (
            (progress.blockNo === latestNativeProgress.blockNo &&
              (progress.blockHash !== latestNativeProgress.blockHash ||
                progress.slot !== latestNativeProgress.slot)) ||
            (BigInt(progress.blockNo) > BigInt(latestNativeProgress.blockNo) &&
              BigInt(progress.slot) <= BigInt(latestNativeProgress.slot))
          )
            throw new Error("proof progress native point is not monotone");
        }
        // Only a bounded wakeup marker is retained. Queue, funding and terminal
        // facts continue to come from their existing authenticated authorities.
        latestNativeProgress = {
          blockHash: progress.blockHash,
          blockNo: progress.blockNo,
          slot: progress.slot,
        };
      }
      const progressPoint =
        latestNativeProgress !== undefined &&
        BigInt(latestNativeProgress.blockNo) > BigInt(queuePoint.blockNo)
          ? latestNativeProgress
          : queuePoint;
      const observationRevision = `${request.observation.observationDigest}:${progressPoint.blockHash}`;
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
        if (
          objectives.has(currentKey!) ||
          canOpenWatcherProofObjective(database(), fault.decision)
        ) {
          if (!objectives.has(currentKey!)) {
            await hold(fault.decision);
            if (epoch !== startedEpoch) return [];
          }
          decisions.set(fault.decision.decisionDigest, fault.decision);
          const objective = objectives.get(currentKey!) ?? {
            decision: fault.decision,
          };
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
          // Newly started work acquires its durable identity on its first
          // historical observation. Already restored identities stay indexed.
          if (objective.entries === undefined) await loadExecution(objective);
          if (epoch !== startedEpoch) return [];
          if (
            objective.entries === undefined ||
            !objective.entries.some(
              ({ event }) => event.kind === "submission_intent",
            )
          )
            continue;
          if (objective.historical?.generation !== request.rollbackGeneration) {
            const controller = createWorkflowReconciliationPermitController({
              decision: objective.decision,
              deploymentFingerprint: input.deploymentFingerprint,
              rollbackGeneration: request.rollbackGeneration,
              entries: objective.entries,
            });
            objective.historical = {
              generation: request.rollbackGeneration,
              permit: controller.permit,
            };
          }
          contexts.push({
            observationRevision,
            decision: objective.decision,
            actuationPermit: objective.historical.permit,
            deadline: null,
            rollbackGeneration: request.rollbackGeneration,
          });
        }
      }
      assertWatcherStateQueueObservation(request.observation);
      lastObservation = observationKey;
      pruneDecisions();
      return Object.freeze(contexts);
    },
    unfinishedCount: () => objectives.size,
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
      latestNativeProgress = undefined;
      epoch += 1;
    },
    updateExecution,
    reconcileExecution: async ({
      objective: key,
      execution,
      rollbackGeneration,
    }) => {
      await updateExecution({ objective: key, execution });
      const objective = objectives.get(keyOf(key))!;
      const controller = createWorkflowReconciliationPermitController({
        decision: objective.decision,
        deploymentFingerprint: input.deploymentFingerprint,
        rollbackGeneration,
        entries: execution.entries,
      });
      objective.historical = {
        generation: rollbackGeneration,
        permit: controller.permit,
      };
      return controller.permit;
    },
    markCompleted: async (objective, done) => {
      objectives.delete(keyOf(objective));
      unheld.delete(keyOf(objective));
      pruneDecisions();
      // The row frees the cap slot and spares a restart one verification. A
      // failed write leaves it open, so the next start verifies it again.
      let marked = false;
      try {
        marked = completeWatcherProofObjective(database(), objective, done, k);
      } catch {
        // Verified again on the next start.
      }
      // Released once the marker holds; unmarked, it is verified again.
      if (marked) await input.retention?.release(objective);
    },
  });
};
