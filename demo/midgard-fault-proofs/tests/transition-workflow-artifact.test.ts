import { describe, expect, it } from "vitest";

import { requireTransitionTraceWorkflowArtifact } from "../src/transition-trace/workflow-artifact.js";
import type {
  JournalJsonObject,
  JournalJsonValue,
} from "../src/workflow/journal.js";

// Admission compares already verified immutable facts. These bytes are not an
// L1 authority; on-chain reopening separately authenticates the actual hashes.
const artifact = (): JournalJsonObject => ({
  schemaVersion: "midgard-transition-trace-workflow-artifact-v1",
  headerHash: "aa",
  detectionId: "deposit-0",
  payloadEnvelopeCbor: "bb",
  predecessorEnvelopeCbor: null,
  proofCbor: "cc",
  eventOutRef: "old#0",
  depositOpening: { commitmentCbor: "dd", openingCbor: "ee" },
  l1Snapshot: { evidence: "retained" },
});

describe("transition staged opening journal identity", () => {
  it("survives serialization and pointer-only changes with the same immutable opening", () => {
    const prior = JSON.parse(JSON.stringify(artifact())) as JournalJsonObject;
    const fresh = {
      ...artifact(),
      eventOutRef: "continued#1",
      l1Snapshot: { evidence: "current" },
    };
    expect(() =>
      requireTransitionTraceWorkflowArtifact(prior, fresh),
    ).not.toThrow();
  });
  it.each(["commitmentCbor", "openingCbor"])(
    "rejects altered %s, including later reuse of the same ID",
    (key) => {
      const prior = artifact();
      const fresh = {
        ...artifact(),
        depositOpening: {
          commitmentCbor: "dd",
          openingCbor: "ee",
          [key]: "ff",
        },
      };
      expect(() =>
        requireTransitionTraceWorkflowArtifact(prior, fresh),
      ).toThrow(/deposit opening/);
    },
  );
  it.each<JournalJsonValue>([
    null,
    {},
    { commitmentCbor: "dd", openingCbor: "ee", extra: "00" },
  ])("rejects omitted or extended opening shape %j", (opening) => {
    expect(() =>
      requireTransitionTraceWorkflowArtifact(
        { ...artifact(), depositOpening: opening },
        artifact(),
      ),
    ).toThrow(/deposit opening/);
  });
  it("preserves exact reference binding for direct proofs without captured openings", () => {
    const prior = { ...artifact(), depositOpening: null };
    expect(() =>
      requireTransitionTraceWorkflowArtifact(prior, {
        ...prior,
        eventOutRef: "changed#0",
      }),
    ).toThrow(/event reference/);
    expect(() =>
      requireTransitionTraceWorkflowArtifact(prior, prior),
    ).not.toThrow();
  });
  it("still rejects a changed frozen proof or removed raw evidence", () => {
    expect(() =>
      requireTransitionTraceWorkflowArtifact(
        { ...artifact(), proofCbor: "ff" },
        artifact(),
      ),
    ).toThrow(/proofCbor/);
    expect(() =>
      requireTransitionTraceWorkflowArtifact(
        { ...artifact(), l1Snapshot: null },
        artifact(),
      ),
    ).toThrow(/raw L1 snapshot/);
  });
});
