import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
  createWorkflowReconciliationPermitController,
  revokeWorkflowActuationPermit,
  workflowJournalIsReconciliationOnly,
} from "../src/workflow/actuation-permit.js";
import type { HeaderFaultDecision } from "../src/workflow/header-classifier.js";
import {
  computeFraudProofWorkflowId,
  type FraudProofWorkflowJournalEntry,
  journalJsonDigest,
  type JournalJsonObject,
  MemoryFraudProofWorkflowJournalStore,
  normalizeJournalJson,
} from "../src/workflow/journal.js";

type Fixture = {
  decision: HeaderFaultDecision;
  entries: FraudProofWorkflowJournalEntry[];
};
// Public decision and transaction journal from the observed restart failure.
// No filesystem/runtime dependency on the retained live deployment.
const fixture = (): Fixture =>
  JSON.parse(
    readFileSync(
      new URL("./fixtures/mint-reconciliation-retained.json", import.meta.url),
      "utf8",
    ),
  );
const controller = ({ decision, entries }: Fixture) =>
  createWorkflowReconciliationPermitController({
    decision,
    entries,
    deploymentFingerprint: decision.deploymentFingerprint,
    rollbackGeneration: "0",
  });
const reseal = (value: Fixture): void => {
  const { decisionDigest: _, ...unsealed } = value.decision;
  value.decision = {
    ...unsealed,
    decisionDigest: journalJsonDigest(normalizeJournalJson(unsealed)),
  };
  value.entries = value.entries.map((entry) => {
    const identity = {
      ...entry.identity,
      decisionDigest: value.decision.decisionDigest,
    };
    return {
      ...entry,
      identity,
      workflowId: computeFraudProofWorkflowId(identity),
    };
  });
};
const replacePrepared = (
  value: Fixture,
  update: (artifact: JournalJsonObject) => JournalJsonObject,
): void => {
  value.entries = value.entries.map((entry) => {
    if (entry.event.kind !== "prepared") return entry;
    const artifact = update(entry.event.artifact);
    return {
      ...entry,
      event: {
        ...entry.event,
        artifact,
        artifactDigest: journalJsonDigest(artifact),
      },
    };
  });
};

describe("mint retained prepared evidence reconciliation", () => {
  it("admits the recorded proof for observation only, preserving revocation", () => {
    const value = fixture();
    const admitted = controller(value);
    const journal = bindWorkflowActuationJournal({
      journal: new MemoryFraudProofWorkflowJournalStore(),
      permit: admitted.permit,
      decisionDigest: value.decision.decisionDigest,
      deploymentFingerprint: value.decision.deploymentFingerprint,
      category: "mintItemNonCanonical",
      headerHash: value.decision.headerHash,
    });
    const check = (
      checkpoint: Parameters<
        typeof assertWorkflowJournalActuation
      >[0]["checkpoint"],
    ) =>
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: value.decision.deploymentFingerprint,
        category: "mintItemNonCanonical",
        headerHash: value.decision.headerHash,
        checkpoint,
      });
    expect(workflowJournalIsReconciliationOnly(journal)).toBe(true);
    expect(() => check("before_reconcile")).not.toThrow();
    expect(() => check("before_terminal_verify")).not.toThrow();
    expect(() => check("before_preflight")).toThrow("actuation revoked");
    expect(() => check("before_submit")).toThrow("actuation revoked");
    revokeWorkflowActuationPermit(admitted.permit, "rollback");
    expect(() => check("before_reconcile")).toThrow("rollback");
  });

  it("rejects lifecycle revocation of an unadmitted permit", () => {
    expect(() =>
      revokeWorkflowActuationPermit(
        { permitVersion: "midgard-production-workflow-actuation-permit-v1" },
        "rollback",
      ),
    ).toThrow("was not admitted");
  });

  it.each([
    "category",
    "headerHash",
    "payloadSha256",
    "decisionDigest",
  ] as const)("rejects a changed sealed %s", (field) => {
    const value = fixture();
    value.decision = { ...value.decision, [field]: "ff".repeat(32) };
    expect(() => controller(value)).toThrow();
  });

  it.each([
    ["detectionId", "mint-item-non-canonical:0:wrong:0"],
    ["position", "1"],
    ["position", "00"],
    ["violationId", "other-violation"],
  ])("rejects a sealed but inconsistent %s=%s", (field, replacement) => {
    const value = fixture();
    value.decision = { ...value.decision, [field!]: replacement };
    reseal(value);
    expect(() => controller(value)).toThrow("prepared mint fault evidence");
  });

  it.each([
    ["category", "other-family"],
    ["familyIdentity", "00".repeat(32)],
    ["extra", true],
  ] as const)(
    "rejects a re-digested prepared %s substitution",
    (field, replacement) => {
      const value = fixture();
      replacePrepared(value, (artifact) => ({
        ...artifact,
        [String(field)]: replacement,
      }));
      expect(() => controller(value)).toThrow("prepared mint fault evidence");
    },
  );

  it("rejects a re-digested family coordinate inconsistent with the detection", () => {
    const value = fixture();
    replacePrepared(value, (artifact) => ({
      ...artifact,
      familyIdentity: String(artifact.familyIdentity).replace(
        /^[^:]+/u,
        "00".repeat(32),
      ),
    }));
    expect(() => controller(value)).toThrow("prepared mint fault evidence");
  });

  it("rejects changed field commitments even when the detection coordinate agrees", () => {
    const value = fixture();
    replacePrepared(value, (artifact) => {
      const parts = String(artifact.familyIdentity).split(":");
      parts[3] = "00".repeat(32);
      return { ...artifact, familyIdentity: parts.join(":") };
    });
    expect(() => controller(value)).toThrow("prepared mint fault evidence");
  });

  it("rejects a missing durable recovery identity", () => {
    const value = fixture();
    value.entries = value.entries.map((entry) =>
      entry.event.kind === "submission_intent"
        ? { ...entry, event: { ...entry.event, durableRecovery: {} } }
        : entry,
    );
    expect(() => controller(value)).toThrow("prepared mint fault evidence");
  });

  it("rejects a prepared artifact with an invalid digest", () => {
    const value = fixture();
    value.entries = value.entries.map((entry) =>
      entry.event.kind === "prepared"
        ? {
            ...entry,
            event: { ...entry.event, artifactDigest: "00".repeat(32) },
          }
        : entry,
    );
    expect(() => controller(value)).toThrow("artifact digest");
  });

  it.each(["durableRecovery", "actionInput"] as const)(
    "checks %s identity on every recorded attempt",
    (field) => {
      const value = fixture();
      const lastIntent = [...value.entries]
        .reverse()
        .find(({ event }) => event.kind === "submission_intent")!.sequence;
      value.entries = value.entries.map((entry, index) =>
        index === lastIntent && entry.event.kind === "submission_intent"
          ? {
              ...entry,
              event: {
                ...entry.event,
                [field]: { ...entry.event[field], familyIdentity: "different" },
              },
            }
          : entry,
      );
      expect(() => controller(value)).toThrow("prepared mint fault evidence");
    },
  );
});
