import {
  COMPLETE_CANONICAL_REPLAY,
  HEADER_CLASSIFIER,
  HEADER_DECISION,
  type HeaderDecision,
} from "@al-ft/midgard-fault-proofs";

import {
  canonicalDigest,
  DETECTION_IDENTIFIER,
  DIGEST,
  exactLaunchScope,
  exactRecord,
  exactString,
  HEADER_HASH,
  IDENTIFIER,
  NATURAL,
} from "./fault-decision-journal.exact-record.js";
import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";

export const parseDecision = (
  value: unknown,
  deploymentFingerprint: string,
  launchScope: readonly WatcherInstalledWorkflowCategory[],
): HeaderDecision => {
  const common = [
    "schemaVersion",
    "classifierVersion",
    "deploymentFingerprint",
    "headerHash",
    "authenticatedObservationDigest",
    "payloadEnvelopeSha256",
    "payloadSha256",
    "replayVersion",
    "replayDigest",
    "launchScope",
    "launchScopeDigest",
    "classificationDigest",
    "decisionDigest",
    "decision",
  ] as const;
  const candidate = exactRecord(
    value,
    (() => {
      const decision = (value as { readonly decision?: unknown } | null)
        ?.decision;
      if (decision === "fault_detected") {
        return [
          ...common,
          "category",
          "violationId",
          "detectionId",
          "position",
        ];
      }
      if (decision === "unprovable") {
        return [...common, "reason", "violationId", "detectionId", "position"];
      }
      return common;
    })(),
    "persisted production decision",
  );
  if (
    candidate.schemaVersion !== HEADER_DECISION ||
    candidate.classifierVersion !== HEADER_CLASSIFIER ||
    candidate.replayVersion !== COMPLETE_CANONICAL_REPLAY ||
    candidate.deploymentFingerprint !== deploymentFingerprint
  ) {
    throw new Error("persisted production decision identity is invalid");
  }
  const parsedScope = exactLaunchScope(candidate.launchScope, launchScope);
  if (
    exactString(
      candidate.launchScopeDigest,
      DIGEST,
      "persisted production decision launch-scope digest",
    ) !== canonicalDigest(parsedScope)
  ) {
    throw new Error(
      "persisted production decision launch-scope digest mismatch",
    );
  }
  const base = {
    schemaVersion: HEADER_DECISION,
    classifierVersion: HEADER_CLASSIFIER,
    deploymentFingerprint,
    headerHash: exactString(
      candidate.headerHash,
      HEADER_HASH,
      "persisted production decision header hash",
    ),
    authenticatedObservationDigest: exactString(
      candidate.authenticatedObservationDigest,
      DIGEST,
      "persisted production decision observation digest",
    ),
    payloadEnvelopeSha256: exactString(
      candidate.payloadEnvelopeSha256,
      DIGEST,
      "persisted production decision envelope digest",
    ),
    payloadSha256: exactString(
      candidate.payloadSha256,
      DIGEST,
      "persisted production decision payload digest",
    ),
    replayVersion: COMPLETE_CANONICAL_REPLAY,
    replayDigest: exactString(
      candidate.replayDigest,
      DIGEST,
      "persisted production decision replay digest",
    ),
    launchScope: parsedScope,
    launchScopeDigest: candidate.launchScopeDigest as string,
    classificationDigest: exactString(
      candidate.classificationDigest,
      DIGEST,
      "persisted production decision classification digest",
    ),
  } as const;
  const decision = (() => {
    if (candidate.decision === "healthy") {
      return Object.freeze({ ...base, decision: "healthy" as const });
    }
    const violationId = exactString(
      candidate.violationId,
      IDENTIFIER,
      "persisted production decision violation id",
    );
    const detectionId = exactString(
      candidate.detectionId,
      DETECTION_IDENTIFIER,
      "persisted production decision detection id",
    );
    const position = exactString(
      candidate.position,
      NATURAL,
      "persisted production decision position",
    );
    if (candidate.decision === "unprovable") {
      if (
        candidate.reason !== "unregistered_violation" &&
        candidate.reason !== "category_not_installed" &&
        candidate.reason !== "predecessor_context_unavailable"
      ) {
        throw new Error("persisted unprovable decision reason is invalid");
      }
      return Object.freeze({
        ...base,
        decision: "unprovable" as const,
        reason: candidate.reason,
        violationId,
        detectionId,
        position,
      });
    }
    if (
      candidate.decision !== "fault_detected" ||
      !launchScope.includes(
        candidate.category as WatcherInstalledWorkflowCategory,
      )
    ) {
      throw new Error(
        "persisted production decision kind or category is invalid",
      );
    }
    return Object.freeze({
      ...base,
      decision: "fault_detected" as const,
      category: candidate.category as WatcherInstalledWorkflowCategory,
      violationId,
      detectionId,
      position,
    });
  })();
  const decisionDigest = exactString(
    candidate.decisionDigest,
    DIGEST,
    "persisted production decision digest",
  );
  if (decisionDigest !== canonicalDigest(decision)) {
    throw new Error("persisted production decision digest mismatch");
  }
  return Object.freeze({ ...decision, decisionDigest });
};
