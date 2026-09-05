import { describe, expect, it } from "vitest";

import {
  DISTINCT_ASSET_PROOF_PUBLICATION,
  distinctAssetProofPublicationRecovery,
} from "../src/distinct-asset-accumulation-limit/proof-carriage.js";
import type { JournalJsonObject } from "../src/workflow/journal.js";

const recovery: JournalJsonObject = {
  schemaVersion: DISTINCT_ASSET_PROOF_PUBLICATION,
  directCapacityFailure: {
    kind: "max_tx_size",
    maximumTransactionBytes: 16384,
    actualTransactionBytes: 20000,
    errorSha256: "aa".repeat(32),
  },
  publication: { schemaVersion: "raw-publication-validated-by-shared-port" },
};

const capacityChanges: readonly JournalJsonObject[] = [
  { maximumTransactionBytes: 32768 },
  { actualTransactionBytes: 16384 },
  { actualTransactionBytes: 20000.5 },
  { errorSha256: "AA".repeat(32) },
  { kind: "script_failure" },
  { extra: true },
];

describe("distinct-asset durable source publication recovery", () => {
  it("admits the release-bound size refusal and delegates exact outputs to the authenticated port", () => {
    expect(distinctAssetProofPublicationRecovery(recovery, 16384)).toEqual(
      recovery.publication,
    );
  });

  it.each([
    { ...recovery, extra: true },
    { ...recovery, schemaVersion: "changed" },
    { ...recovery, publication: [] },
    ...capacityChanges.map((change) => ({
      ...recovery,
      directCapacityFailure: {
        ...(recovery.directCapacityFailure as JournalJsonObject),
        ...change,
      },
    })),
  ])("refuses mutated recovery %# before publication lookup", (changed) => {
    expect(() =>
      distinctAssetProofPublicationRecovery(changed, 16384),
    ).toThrow();
  });
});
