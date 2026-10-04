import type { ChildProcess } from "node:child_process";
import { randomUUID } from "node:crypto";
import { Duplex } from "node:stream";

import { historyChildClient } from "./history-child-client.js";
import type {
  HistoryChildActor,
  HistoryChildRole,
} from "./history-child-evidence.js";
import { HISTORY_CHILD_ROLES } from "./history-child-evidence.js";
import { HistoryConfigurationRefusal } from "./history-configuration-refusal.js";
import {
  historyProofDeadline,
  historyProofRemaining,
} from "./history-proof-deadline.js";
import {
  historyChildEnvironment,
  parseHistoryReadinessSpecification,
} from "./history-role-context.js";
import { makeHistorySignedAdmission } from "./history-signed-admission.js";
import { makeLayout, readRunEnv } from "./layout.js";
import {
  recoveryScope,
  recoveryScopeMatches,
} from "./service-recovery-scope.js";
import type { ServiceSpec, SupervisorPaths } from "./supervisor.js";

type Entry = {
  readonly actor: HistoryChildActor;
  readonly specification: ReturnType<typeof parseHistoryReadinessSpecification>;
  readonly child: ChildProcess;
  readonly pipe: Duplex;
  readonly client: ReturnType<typeof historyChildClient>;
};
/** Current process-local actual children only: persisted status is never proof. */
export const createHistoryRoleRegistry = (
  paths: SupervisorPaths,
  changed: () => void = () => undefined,
) => {
  const entries = new Map<HistoryChildRole, Entry>();
  let proving = false;
  let cohortAdmission:
    | {
        incarnation: string;
        signed: ReturnType<typeof makeHistorySignedAdmission>;
      }
    | undefined;
  let lastStage = "not_checked";
  let lastError: string | undefined;
  let lastTimings: Record<string, number> = {};
  const remove = (entry: Entry) => {
    if (entries.get(entry.actor.role) === entry) {
      entries.delete(entry.actor.role);
      changed();
    }
    entry.client.close();
  };
  const prepare = (service: ServiceSpec) => {
    const specification = parseHistoryReadinessSpecification(
      service.historyReadiness,
    );
    if (specification === null) return null;
    const scope = recoveryScope(paths);
    if (scope === undefined) return null;
    const attemptId = randomUUID();
    return {
      specification,
      scope,
      attemptId,
      env: historyChildEnvironment(specification, scope, attemptId),
    };
  };
  const register = (
    attempt: NonNullable<ReturnType<typeof prepare>>,
    child: ChildProcess,
  ) => {
    const pipe = child.stdio[3];
    if (child.pid === undefined || !(pipe instanceof Duplex))
      return () => undefined;
    const actor: HistoryChildActor = {
      role: attempt.specification.role,
      runId: attempt.specification.runId,
      deploymentFingerprint: attempt.specification.deploymentFingerprint,
      ...attempt.scope,
      attemptId: attempt.attemptId,
      childPid: child.pid,
    };
    const previous = entries.get(actor.role);
    if (previous !== undefined) remove(previous);
    const entry: Entry = {
      actor,
      specification: attempt.specification,
      child,
      pipe,
      client: historyChildClient({ actor, pipe }),
    };
    entries.set(actor.role, entry);
    changed();
    const close = () => remove(entry);
    child.once("exit", close);
    child.once("error", close);
    return close;
  };
  const check = async (
    service: ServiceSpec,
    timeoutMs: number,
    signal?: AbortSignal,
  ): Promise<Readonly<{ current: () => boolean }> | undefined> => {
    // Capture before scope hashing, run reads, or signed public admission.
    const deadline = historyProofDeadline(timeoutMs);
    if (deadline === null || proving || signal?.aborted) return undefined;
    proving = true;
    lastError = undefined;
    let stage = "service_configuration";
    let stageStarted = performance.now();
    const timings: Record<string, number> = {};
    const mark = (next: string) => {
      timings[stage] = Math.round(performance.now() - stageStarted);
      stage = next;
      stageStarted = performance.now();
    };
    try {
      const specification = parseHistoryReadinessSpecification(
        service.historyReadiness,
      );
      if (specification === null) return undefined;
      mark("runtime_scope");
      const scope = recoveryScope(paths);
      if (scope === undefined || historyProofRemaining(deadline) === 0)
        return undefined;
      mark("child_cohort");
      const current = HISTORY_CHILD_ROLES.map((role) => entries.get(role));
      if (current.some((entry) => entry === undefined)) return undefined;
      const active = () =>
        !signal?.aborted &&
        historyProofRemaining(deadline) > 0 &&
        recoveryScopeMatches(scope, recoveryScope(paths)) &&
        historyProofRemaining(deadline) > 0 &&
        current.every(
          (entry) =>
            entry !== undefined &&
            entries.get(entry.actor.role) === entry &&
            entry.child.pid === entry.actor.childPid &&
            entry.child.exitCode === null &&
            entry.child.signalCode === null &&
            !entry.pipe.destroyed &&
            entry.specification?.publicBindingDigest ===
              specification.publicBindingDigest &&
            entry.specification.expectedNetwork ===
              specification.expectedNetwork &&
            entry.actor.runId === specification.runId &&
            entry.actor.deploymentFingerprint ===
              specification.deploymentFingerprint &&
            recoveryScopeMatches(scope, entry.actor),
        );
      const currentIncarnation = current
        .map((entry) => `${entry?.actor.attemptId}:${entry?.actor.childPid}`)
        .join("|");
      // Existing cohorts must reach the signed guard even when drift also
      // makes their actor scope inactive. Only a genuinely new active cohort
      // may construct another admission closure.
      if (cohortAdmission?.incarnation !== currentIncarnation && !active())
        return undefined;
      const layout = makeLayout(paths.runDir);
      const run = readRunEnv(layout);
      if (
        historyProofRemaining(deadline) === 0 ||
        (cohortAdmission?.incarnation !== currentIncarnation &&
          run.runId !== specification.runId)
      )
        return undefined;
      mark("signed_admission");
      const incarnation = currentIncarnation;
      if (cohortAdmission?.incarnation !== incarnation) {
        const signed = makeHistorySignedAdmission({
          layout,
          run,
          deadline,
          publicBindingDigest: specification.publicBindingDigest,
          deploymentFingerprint: specification.deploymentFingerprint,
          expectedNetwork: specification.expectedNetwork,
          currentScope: () => {
            const actualRun = readRunEnv(layout);
            if (
              actualRun.runId !== run.runId ||
              actualRun.networkMagic !== run.networkMagic ||
              actualRun.portOffset !== run.portOffset
            )
              throw new HistoryConfigurationRefusal(
                "history controller public run binding changed",
              );
            const now = recoveryScope(paths);
            if (now === undefined)
              throw new Error("history runtime scope is unavailable");
            return {
              ...now,
              incarnation: HISTORY_CHILD_ROLES.map((role) => {
                const entry = entries.get(role);
                return `${entry?.actor.attemptId}:${entry?.actor.childPid}`;
              }).join("|"),
            };
          },
        });
        cohortAdmission = { incarnation, signed };
      }
      const signed = cohortAdmission.signed;
      await signed.admit(deadline);
      const guarded = () => {
        signed.current(deadline);
        return active();
      };
      if (!guarded()) return undefined;
      const recorder = entries.get("history-recorder");
      if (recorder === undefined) return undefined;
      mark("recorder_seal");
      // The preceding guard is in this same synchronous section; every
      // subsequent await is followed by another complete fresh guard.
      const sealed = await recorder.client.request(
        "seal",
        null,
        historyProofRemaining(deadline),
      );
      const offer = sealed?.offer;
      if (!guarded() || offer === undefined || offer === null) return undefined;
      for (const role of [
        "history-archive-a",
        "history-archive-b",
        "history-tunnel",
      ] as const) {
        mark(role);
        const entry = entries.get(role);
        if (entry === undefined) return undefined;
        const reply = await entry.client.request(
          "prove",
          offer,
          historyProofRemaining(deadline),
        );
        if (
          !guarded() ||
          reply?.offer === null ||
          JSON.stringify(reply?.offer) !== JSON.stringify(offer)
        )
          return undefined;
      }
      // Last await is a fresh nonce to the same current native recorder. Scope
      // and actual-child liveness are checked synchronously before the caller CAS.
      mark("recorder_revalidation");
      const revalidated = await recorder.client.request(
        "revalidate",
        offer,
        historyProofRemaining(deadline),
      );
      mark("final_scope");
      if (
        !guarded() ||
        revalidated?.offer === null ||
        JSON.stringify(revalidated?.offer) !== JSON.stringify(offer)
      )
        return undefined;
      let consumed = false;
      return Object.freeze({
        // This proof belongs to this exact nonce, deadline and child cohort.
        // The consumer checks it after its await, immediately before its CAS.
        current: () => {
          if (consumed) return false;
          consumed = true;
          try {
            return guarded();
          } catch {
            return false;
          }
        },
      });
    } catch (error) {
      // Public read/admission diagnostics only; this snapshot grants no proof.
      lastError =
        error instanceof Error ? error.message.slice(0, 300) : "unknown";
      return undefined;
    } finally {
      timings[stage] = Math.round(performance.now() - stageStarted);
      lastTimings = timings;
      lastStage = stage;
      proving = false;
    }
  };
  return {
    prepare,
    register,
    cohort: () =>
      HISTORY_CHILD_ROLES.flatMap((role) => {
        const entry = entries.get(role);
        return entry === undefined
          ? []
          : [
              {
                role,
                pid: entry.actor.childPid,
                attemptId: entry.actor.attemptId,
              },
            ];
      }),
    prove: check,
    check: async (
      service: ServiceSpec,
      timeoutMs: number,
      signal?: AbortSignal,
    ) => (await check(service, timeoutMs, signal))?.current() === true,
    diagnostic: () => ({
      stage: lastStage,
      error: lastError,
      timings: lastTimings,
    }),
    close: () => {
      for (const entry of entries.values()) remove(entry);
    },
  };
};
