import {
  buildMidgardValidationMerkleMembershipV1,
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardScriptExecutionLeafV1,
  hashMidgardScriptPurposeLeafV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  classifyReceivePurposeLanguageFindingV1,
  prepareReceivePurposeLanguageEvidenceV1,
  RECEIVE_PURPOSE_LANGUAGE_CATEGORY_ID_V1,
  type ReceivePurposeLanguageDescriptorV1,
  receivePurposeLanguageEvidenceClosesV1,
} from "../src/receive-purpose-language/family-v1.js";
import {
  createManifestBoundReceivePurposeLanguageWorkflowV1,
  createReceivePurposeLanguageProductionWorkflowRunnerSurfaceV1,
  RECEIVE_PURPOSE_LANGUAGE_PRODUCTION_CONFIG_KEYS_V1,
  RECEIVE_PURPOSE_LANGUAGE_STEP_DATUM_SCHEMAS_V1,
  runOrResumeManifestBoundReceivePurposeLanguageWorkflowV1,
} from "../src/receive-purpose-language/manifest-workflow-v1.js";
import { createReceivePurposeLanguageActuatorV1 } from "../src/receive-purpose-language/production-actuator-v1.js";
import {
  createReceivePurposeLanguageProductionWorkflowV1,
  RECEIVE_PURPOSE_LANGUAGE_DIRECT_CONFIG_KEYS_V1,
} from "../src/receive-purpose-language/production-v1.js";
import { detectReceivePurposeLanguageAcceptedReplayV1 } from "../src/receive-purpose-language/replay-v1.js";
import { AuthenticatedReceiveLanguageV1Schema } from "../src/receive-purpose-language/schemas-v1.js";
import {
  cancelReceivePurposeLanguageWorkflowV1,
  type ReceivePurposeLanguageCursorV1,
  type ReceivePurposeLanguageJournalEntryV1,
  runReceivePurposeLanguageWorkflowV1,
} from "../src/receive-purpose-language/workflow-v1.js";

const txId = "00".repeat(32);
const scriptHash = Buffer.from("22".repeat(28), "hex");
const commitment = Buffer.from("33".repeat(32), "hex");
const accepted = acceptedVerdictSubjectV1(txId);
const forced = (executionIndex = 0) =>
  forcedVerdictSubjectV1({
    transactionId: txId,
    sourceKey: { transactionId: "44".repeat(32), outputIndex: 0n },
    rejectionReason: {
      ReceivePurposePlutusV3Forbidden: {
        execution_index: BigInt(executionIndex),
      },
    },
  });

const descriptor = (
  languageTag: 0 | 3 | 128,
): ReceivePurposeLanguageDescriptorV1 => {
  const purposeLeaf = hashMidgardScriptPurposeLeafV1({
    purposeKind: 3,
    purposeIndex: 0n,
    scriptHash,
    subject: Buffer.from("aa", "hex"),
  });
  const sourceLeaf = hashMidgardInlineScriptSourceLeafV1({
    sourceIndex: 0n,
    scriptLanguageTag: languageTag,
    scriptHash,
    scriptTotalLength: 42,
    itemCommitment: commitment,
  });
  const redeemerLeaf = Buffer.from("55".repeat(32), "hex");
  const executionLeaf = hashMidgardScriptExecutionLeafV1({
    languageTag,
    purposeLeaf,
    sourceLeaf,
    redeemerLeaf,
  });
  return {
    sourceIndex: 0,
    originKind: 0,
    sourceKeyHex: "00",
    languageTag,
    scriptHashHex: scriptHash.toString("hex"),
    scriptTotalLength: 42,
    scriptItemCommitmentHex: commitment.toString("hex"),
    purposeKind: 3,
    purposeIndex: 0,
    purposeSubjectHex: "aa",
    redeemerLeafHex: redeemerLeaf.toString("hex"),
    purposeMembership: buildMidgardValidationMerkleMembershipV1(
      [purposeLeaf],
      0,
    ),
    sourceMembership: buildMidgardValidationMerkleMembershipV1([sourceLeaf], 0),
    executionMembership: buildMidgardValidationMerkleMembershipV1(
      [executionLeaf],
      0,
    ),
  };
};
const evidence = (languageTag: 0 | 3 | 128, subject = accepted) =>
  prepareReceivePurposeLanguageEvidenceV1({
    finding: { subject, executionIndex: 0 },
    descriptor: descriptor(languageTag),
  });

describe("receivePurposeLanguage V1", () => {
  it("freezes ID 34 and binds the exact typed execution coordinate", () => {
    expect(RECEIVE_PURPOSE_LANGUAGE_CATEGORY_ID_V1).toBe("00000034");
    expect(
      classifyReceivePurposeLanguageFindingV1({
        subject: forced(),
        executionIndex: 0,
      }),
    ).toBeTruthy();
    expect(() =>
      classifyReceivePurposeLanguageFindingV1({
        subject: forced(1),
        executionIndex: 0,
      }),
    ).toThrow(/coordinate changed/u);
  });
  it("has a callback-free package-owned real Lucid lifecycle surface", () => {
    expect(RECEIVE_PURPOSE_LANGUAGE_DIRECT_CONFIG_KEYS_V1).not.toContain(
      "submit",
    );
    const workflow = createReceivePurposeLanguageProductionWorkflowV1(
      {} as never,
    );
    expect(Object.keys(workflow)).toEqual(["run"]);
    expect(RECEIVE_PURPOSE_LANGUAGE_PRODUCTION_CONFIG_KEYS_V1).toEqual([
      "manifest",
      "blueprintJson",
      "deploymentInfo",
      "headerHash",
      "lucid",
      "signer",
      "source",
      "decisionDigest",
      "stateQueueMutationLeaseCoordinator",
      "referenceScripts",
    ]);
    expect(RECEIVE_PURPOSE_LANGUAGE_STEP_DATUM_SCHEMAS_V1).toHaveLength(3);
    expect(
      Object.keys(createReceivePurposeLanguageActuatorV1({} as never)),
    ).toEqual(["capture"]);
  });
  it("rejects callback/evidence authority at manifest and runner boundaries", async () => {
    await expect(
      createManifestBoundReceivePurposeLanguageWorkflowV1({
        submit: async () => "00".repeat(32),
      } as never),
    ).rejects.toThrow(/callback authority/u);
    await expect(
      runOrResumeManifestBoundReceivePurposeLanguageWorkflowV1({
        workflow: {} as never,
        sources: [],
        journal: {} as never,
        evidence: evidence(3),
      } as never),
    ).rejects.toThrow(/caller-authored evidence/u);
    const runner =
      createReceivePurposeLanguageProductionWorkflowRunnerSurfaceV1({
        loadRuntimeConfig: async () => ({}) as never,
      });
    expect(runner.runnerVersion).toBe(
      "midgard-production-fraud-proof-workflow-runner-v1",
    );
    await expect(
      runner.runOrResume({ category: "doubleSpend" } as never),
    ).rejects.toThrow(/category changed/u);
  });
  it("round-trips the canonical authenticated state wire encoding", () => {
    const value = {
      bound: {
        subject: accepted,
        validation_traces_root: "11".repeat(32),
        validation_trace_count: 1n,
        execution_index: 0n,
      },
      prior_ledger_root: "22".repeat(32),
      purpose_kind: 3n,
      purpose_index: 0n,
      source_index: 0n,
      origin_kind: 0n,
      source_key: "00",
      language_tag: 3n,
      script_hash: scriptHash.toString("hex"),
    };
    const encoded = Data.to(
      value as never,
      AuthenticatedReceiveLanguageV1Schema as never,
    );
    expect(
      Data.to(
        Data.from(
          encoded,
          AuthenticatedReceiveLanguageV1Schema as never,
        ) as never,
        AuthenticatedReceiveLanguageV1Schema as never,
      ),
    ).toBe(encoded);
  });
  it("convicts only accepted receive PlutusV3 and contradicts allowed languages", () => {
    expect(receivePurposeLanguageEvidenceClosesV1(evidence(3))).toBe(true);
    expect(receivePurposeLanguageEvidenceClosesV1(evidence(0))).toBe(false);
    expect(receivePurposeLanguageEvidenceClosesV1(evidence(0, forced()))).toBe(
      true,
    );
    expect(
      receivePurposeLanguageEvidenceClosesV1(evidence(128, forced())),
    ).toBe(true);
    expect(receivePurposeLanguageEvidenceClosesV1(evidence(3, forced()))).toBe(
      false,
    );
  });
  it("rejects purpose, source, execution, language and coordinate substitution", () => {
    const exact = descriptor(3);
    expect(() =>
      prepareReceivePurposeLanguageEvidenceV1({
        finding: { subject: accepted, executionIndex: 0 },
        descriptor: { ...exact, purposeKind: 2 } as never,
      }),
    ).toThrow(/not receive/u);
    for (const field of [
      "purposeMembership",
      "sourceMembership",
      "executionMembership",
    ] as const)
      expect(() =>
        prepareReceivePurposeLanguageEvidenceV1({
          finding: { subject: accepted, executionIndex: 0 },
          descriptor: {
            ...exact,
            [field]: {
              ...exact[field],
              frontier: {
                ...exact[field].frontier,
                peaks: exact[field].frontier.peaks.map((peak) => ({
                  ...peak,
                  hash: Buffer.alloc(32, 0xff),
                })),
              },
            },
          },
        }),
      ).toThrow(/membership is invalid/u);
    expect(() =>
      prepareReceivePurposeLanguageEvidenceV1({
        finding: { subject: accepted, executionIndex: 1 },
        descriptor: exact,
      }),
    ).toThrow(/coordinate changed/u);
  });
  it("replays authenticated descriptors deterministically", () => {
    const findings = detectReceivePurposeLanguageAcceptedReplayV1({
      headerHash: "ab".repeat(28),
      descriptors: [
        {
          transactionId: txId,
          position: 2n,
          executionIndex: 0,
          descriptor: descriptor(3),
        },
        {
          transactionId: txId,
          position: 1n,
          executionIndex: 0,
          descriptor: descriptor(0),
        },
      ],
    });
    expect(findings).toHaveLength(1);
    expect(findings[0]!.detection).toMatchObject({
      violationId: "receive-purpose-plutus-v3-forbidden",
      position: 2n,
    });
  });
  it("journals exact transaction identity, resumes and cancels", async () => {
    const rows: ReceivePurposeLanguageJournalEntryV1[] = [];
    let cursor: ReceivePurposeLanguageCursorV1 = {
      stage: "none",
      threadOutRef: "",
    };
    const txHash = "aa".repeat(32);
    const journal = {
      load: async () => rows,
      append: async (row: ReceivePurposeLanguageJournalEntryV1) => {
        rows.push(row);
      },
    };
    const transactions = {
      observe: async () => cursor,
      capture: async ({ action }: { action: string }) => ({
        txHash,
        target: {
          stage:
            action === "cancel" ? ("cancelled" as const) : ("step01" as const),
          threadOutRef: `${txHash}#0`,
        },
        submit: async () => {
          cursor = {
            stage: action === "cancel" ? "cancelled" : "step01",
            threadOutRef: `${txHash}#0`,
          };
          return txHash;
        },
      }),
      transactionConfirmed: async () => true,
    };
    expect(
      await runReceivePurposeLanguageWorkflowV1({
        evidence: evidence(3),
        journal,
        transactions,
      }),
    ).toBe("none");
    expect(
      await runReceivePurposeLanguageWorkflowV1({
        evidence: evidence(3),
        journal,
        transactions,
      }),
    ).toBe("step01");
    rows.length = 0;
    cursor = { stage: "step01", threadOutRef: `${txHash}#0` };
    expect(
      await cancelReceivePurposeLanguageWorkflowV1({
        evidence: evidence(3),
        journal,
        transactions,
      }),
    ).toBe("cancelled");
  });
});
