import type { HeaderDecision } from "@al-ft/midgard-fault-proofs";

import { watcherUnprovenFaultMergedAlertSubject } from "../runtime/operations-observability.alert-book.js";
import { WATCHER_PACKAGE_NAME } from "../runtime/scaffold.js";
import type { BridgeDependencies } from "./fault-decision-bridge.selected-target.js";
import type { WatcherUnprovenFaultMerge } from "./watcher-unproven-fault-merges.js";

type DetectedFault = Readonly<{ category: string; decisionDigest: string }>;

/**
 * The `unproven_fault_merged` alert: a header this watcher found faulty
 * merged into confirmed state before any proof removed it. The merge is
 * recorded durably (`unprovenFaultMerges`), so every later process raises
 * the alert again from that record on its first pass, and an error line
 * names the header, its category and decision once, when it is recorded.
 * The alert is informational: it never clears, and nothing acknowledges it,
 * so it must not hold readiness forever.
 *
 * A header is faulty when its latest decision in this process is
 * `fault_detected`, or, for one this process has not classified (it merged
 * while the watcher was down), when the decision journal holds a
 * `fault_detected` decision for it.
 */
export const unprovenMergeRecorder = (
  dependencies: Pick<
    BridgeDependencies,
    "operationsSink" | "nowMs" | "readRecords" | "unprovenFaultMerges"
  >,
) => {
  const nowMs = dependencies.nowMs ?? (() => BigInt(Date.now()));
  const faults = new Map<string, DetectedFault>();
  let reraised = false;
  const raise = (merge: WatcherUnprovenFaultMerge): void =>
    dependencies.operationsSink?.setAlert({
      code: "unproven_fault_merged",
      subjectDigest: watcherUnprovenFaultMergedAlertSubject(merge.headerHash),
      active: true,
      observedAtMs: merge.observedAtMs,
    });
  const detected = async (
    headerHash: string,
  ): Promise<DetectedFault | undefined> =>
    faults.get(headerHash) ??
    (await dependencies.readRecords())
      .map(({ decision }) => decision)
      .filter(
        (decision): decision is Extract<HeaderDecision, DetectedFault> =>
          decision.decision === "fault_detected" &&
          decision.headerHash === headerHash,
      )
      .at(-1);
  return Object.freeze({
    /** Raises the recorded merges again, once per process. */
    reraise: (): void => {
      if (reraised) return;
      for (const merge of dependencies.unprovenFaultMerges?.().list() ?? [])
        raise(merge);
      reraised = true;
    },
    /** The header's latest classification decision in this process. */
    decided: (headerHash: string, decision: HeaderDecision): void => {
      if (decision.decision === "fault_detected")
        faults.set(headerHash, {
          category: decision.category,
          decisionDigest: decision.decisionDigest,
        });
      else faults.delete(headerHash);
    },
    /** The header merged at release finality without a healthy verdict. */
    merged: async (
      headerHash: string,
      mergeTransactionHash: string,
    ): Promise<void> => {
      const fault = await detected(headerHash);
      if (fault === undefined) return;
      const merge = Object.freeze({
        headerHash,
        category: fault.category,
        decisionDigest: fault.decisionDigest,
        mergeTransactionHash,
        observedAtMs: nowMs().toString(),
      });
      const fresh = dependencies.unprovenFaultMerges?.().record(merge) ?? true;
      raise(merge);
      if (fresh)
        process.stderr.write(
          `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, level: "error", event: "unproven_fault_merged", ...merge })}\n`,
        );
    },
    /** A rollback may change what a decision read: they are taken afresh. */
    reset: (): void => faults.clear(),
  });
};
