import { existsSync } from "node:fs";
import { readdir } from "node:fs/promises";
import { join } from "node:path";

import type * as SDK from "@al-ft/midgard-sdk/core";
import { expect } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { JourneyChainCorrection } from "./correction.chain-correction.js";

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
  /** The correction as the chain held it at the release depth. */
  readonly terminal: JourneyChainCorrection;
}

/**
 * Sweep durable pending stamps while earlier families' corrections deepen,
 * re-reading each correction from the chain: a rollback may have rebuilt or
 * re-included it at a new point. Earlier families need no live harness
 * process of their own. A restart reads
 * these same requests; already stamped families and older run evidence are left
 * intact. The last selected family polls this sweep until nothing remains.
 */
export const finalizePendingJourneyEvidence = async ({
  journeysDirectory,
  readCorrection,
  deploymentFingerprint,
  releaseFinalityPolicyDigest,
  finalityDepth,
  nativeEvidencePath,
  authenticate,
}: {
  journeysDirectory: string;
  /** The request's correction as the chain now holds it, if it does. */
  readCorrection(
    request: JourneyPendingEvidenceStamp,
  ): Promise<JourneyChainCorrection | undefined>;
  deploymentFingerprint: string;
  releaseFinalityPolicyDigest: string;
  finalityDepth: number;
  nativeEvidencePath: string;
  authenticate(
    request: JourneyPendingEvidenceStamp,
    terminal: JourneyChainCorrection,
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
    const terminal = await readCorrection(request);
    if (terminal === undefined) {
      pending += 1;
      continue;
    }
    expect(terminal.category).toBe(request.category);
    expect(terminal.headerHash).toBe(request.headerHash);
    // Its release depth is proven only by `authenticate` against the native
    // recorder.
    if (!(await authenticate(request, terminal))) {
      pending += 1;
      continue;
    }
    // The correction may have been rebuilt or included at a new point after
    // a rollback; `authenticate` re-checks its effects at the release depth.
    await writeJourneyArtifact(
      join(directory, "finalized-evidence-stamp.json"),
      {
        ...request,
        anchoredAt: new Date().toISOString(),
        nativeEvidencePath,
        terminal,
      } satisfies JourneyFinalizedEvidenceStamp,
    );
  }
  return pending;
};
