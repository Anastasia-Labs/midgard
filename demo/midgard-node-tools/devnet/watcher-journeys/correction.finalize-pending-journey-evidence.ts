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

/** Final evidence comes only from the production verifier's completed event. */
export const journeyAnchoredEvidence = (
  records: readonly FraudProofWorkflowJournalEntry[],
): FraudProofWorkflowTerminal | undefined => {
  const completed = records.filter(({ event }) => event.kind === "completed");
  expect(completed.length).toBeLessThanOrEqual(1);
  const event = completed[0]?.event;
  return event?.kind === "completed" ? event.terminal : undefined;
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
    const terminal = journeyAnchoredEvidence(records);
    if (terminal === undefined) {
      pending += 1;
      continue;
    }
    expect(terminal.category).toBe(request.category);
    expect(terminal.headerHash).toBe(request.headerHash);
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
    // rollback. The completed event independently reauthenticates its effects.
    await writeJourneyArtifact(
      join(directory, "finalized-evidence-stamp.json"),
      {
        ...request,
        anchoredAt: new Date().toISOString(),
        nativeEvidencePath,
        terminal,
      },
    );
  }
  return pending;
};
