import { existsSync } from "node:fs";
import { readdir } from "node:fs/promises";
import { join } from "node:path";

import {
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { expect } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import { readJourneyWorkflowEntries } from "./correction.verify-journey-corrected-scheduler.js";

export type JourneyAnchoredTerminal = {
  readonly kind: "completed" | "terminal_included";
  readonly terminal: FraudProofWorkflowTerminal;
};

/**
 * The workflow journals `completed` only beyond the recovery horizon, far past
 * any journey budget, so the anchor is the release depth: the last
 * `terminal_included`, which the caller's native recorder must authenticate at
 * that depth. An already journaled `completed` event is used as is.
 */
export const journeyAnchoredEvidence = (
  records: readonly FraudProofWorkflowJournalEntry[],
): JourneyAnchoredTerminal | undefined => {
  const completed = records.filter(({ event }) => event.kind === "completed");
  expect(completed.length).toBeLessThanOrEqual(1);
  const event =
    completed[0]?.event ??
    [...records]
      .reverse()
      .find(({ event }) => event.kind === "terminal_included")?.event;
  return event?.kind === "completed" || event?.kind === "terminal_included"
    ? { kind: event.kind, terminal: event.terminal }
    : undefined;
};

export interface JourneyPendingEvidenceStamp {
  readonly category: SDK.FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly deploymentFingerprint: string;
  readonly completedAtConfirmationDepth: number;
  readonly terminalObservedAt: string;
  readonly successorTxHash: string;
  readonly releaseFinalityPolicyDigest: string;
  readonly finalityDepth: number;
}

export interface JourneyFinalizedEvidenceStamp
  extends JourneyPendingEvidenceStamp {
  readonly anchoredAt: string;
  readonly nativeEvidencePath: string;
  /** Absent in stamps written before the release-depth anchor: "completed". */
  readonly terminalKind?: JourneyAnchoredTerminal["kind"];
  readonly terminal: FraudProofWorkflowTerminal;
}

/**
 * Sweep durable pending stamps while the shared watcher reconciles terminals.
 * Earlier families need no live harness process of their own. A restart reads
 * these same requests; already stamped families and older run evidence are left
 * intact. The last selected family polls this sweep until nothing remains.
 */
export const finalizePendingJourneyEvidence = async ({
  journeysDirectory,
  workflowJournalDirectory,
  deploymentFingerprint,
  releaseFinalityPolicyDigest,
  finalityDepth,
  nativeEvidencePath,
  authenticate,
}: {
  journeysDirectory: string;
  workflowJournalDirectory: string;
  deploymentFingerprint: string;
  releaseFinalityPolicyDigest: string;
  finalityDepth: number;
  nativeEvidencePath: string;
  authenticate(
    request: JourneyPendingEvidenceStamp,
    terminal: FraudProofWorkflowTerminal,
  ): Promise<boolean>;
}): Promise<number> => {
  let pending = 0;
  for (const directoryEntry of await readdir(journeysDirectory, {
    withFileTypes: true,
  })) {
    if (!directoryEntry.isDirectory()) continue;
    const directory = join(journeysDirectory, directoryEntry.name);
    const requestPath = join(directory, "pending-evidence-stamp.json");
    if (
      !existsSync(requestPath) ||
      existsSync(join(directory, "finalized-evidence-stamp.json"))
    )
      continue;
    const request =
      await readJourneyArtifact<JourneyPendingEvidenceStamp>(requestPath);
    expect(request.deploymentFingerprint).toBe(deploymentFingerprint);
    expect(request.releaseFinalityPolicyDigest).toBe(
      releaseFinalityPolicyDigest,
    );
    expect(request.finalityDepth).toBe(finalityDepth);
    const records = await readJourneyWorkflowEntries({
      workflowJournalDirectory,
      category: request.category,
      headerHash: request.headerHash,
    });
    const anchored = journeyAnchoredEvidence(records);
    if (anchored === undefined) {
      pending += 1;
      continue;
    }
    const { kind: terminalKind, terminal } = anchored;
    expect(terminal.category).toBe(request.category);
    expect(terminal.headerHash).toBe(request.headerHash);
    // An included terminal was observed shallow; its release depth is proven
    // only by `authenticate` against the native recorder.
    if (terminalKind === "completed")
      expect(terminal.observedAt.confirmationDepth).toBeGreaterThanOrEqual(
        finalityDepth,
      );
    if (!(await authenticate(request, terminal))) {
      pending += 1;
      continue;
    }
    await writeJourneyArtifact(
      join(directory, "finalized-workflow.json"),
      records,
    );
    // The terminal may have been rebuilt or included at a new point after a
    // rollback; `authenticate` re-checks its effects at the release depth.
    await writeJourneyArtifact(
      join(directory, "finalized-evidence-stamp.json"),
      {
        ...request,
        anchoredAt: new Date().toISOString(),
        nativeEvidencePath,
        terminalKind,
        terminal,
      } satisfies JourneyFinalizedEvidenceStamp,
    );
  }
  return pending;
};
