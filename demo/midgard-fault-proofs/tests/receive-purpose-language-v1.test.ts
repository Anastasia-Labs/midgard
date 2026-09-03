import {
  buildMidgardValidationMerkleMembership,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  classifyReceivePurposeLanguageFinding,
  prepareReceivePurposeLanguageEvidence,
  RECEIVE_PURPOSE_LANGUAGE_CATEGORY_ID,
  type ReceivePurposeLanguageDescriptor,
  receivePurposeLanguageEvidenceCloses,
} from "../src/receive-purpose-language/family-v1.js";
import {
  createManifestBoundReceivePurposeLanguageWorkflow,
  createReceivePurposeLanguageWorkflowRunnerSurface,
  RECEIVE_PURPOSE_LANGUAGE_CONFIG_KEYS,
  RECEIVE_PURPOSE_LANGUAGE_STEP_DATUM_SCHEMAS,
  runOrResumeManifestBoundReceivePurposeLanguageWorkflow,
} from "../src/receive-purpose-language/manifest-workflow-v1.js";
import { createReceivePurposeLanguageActuator } from "../src/receive-purpose-language/production-actuator-v1.js";
import {
  createReceivePurposeLanguageWorkflow,
  RECEIVE_PURPOSE_LANGUAGE_DIRECT_CONFIG_KEYS,
} from "../src/receive-purpose-language/production-v1.js";
import { detectReceivePurposeLanguageAcceptedReplay } from "../src/receive-purpose-language/replay-v1.js";
import { AuthenticatedReceiveLanguageSchema } from "../src/receive-purpose-language/schemas-v1.js";
import {
  cancelReceivePurposeLanguageWorkflow,
  type ReceivePurposeLanguageCursor,
  type ReceivePurposeLanguageJournalEntry,
  runReceivePurposeLanguageWorkflow,
} from "../src/receive-purpose-language/workflow-v1.js";

const txId = "00".repeat(32);
const scriptHash = Buffer.from("22".repeat(28), "hex");
const commitment = Buffer.from("33".repeat(32), "hex");
const accepted = acceptedVerdictSubject(txId);
const forced = (executionIndex = 0) =>
  forcedVerdictSubject({
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
): ReceivePurposeLanguageDescriptor => {
  const purposeLeaf = hashMidgardScriptPurposeLeaf({
    purposeKind: 3,
    purposeIndex: 0n,
    scriptHash,
    subject: Buffer.from("aa", "hex"),
  });
  const sourceLeaf = hashMidgardInlineScriptSourceLeaf({
    sourceIndex: 0n,
    scriptLanguageTag: languageTag,
    scriptHash,
    scriptTotalLength: 42,
    itemCommitment: commitment,
  });
  const redeemerLeaf = Buffer.from("55".repeat(32), "hex");
  const executionLeaf = hashMidgardScriptExecutionLeaf({
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
    purposeMembership: buildMidgardValidationMerkleMembership([purposeLeaf], 0),
    sourceMembership: buildMidgardValidationMerkleMembership([sourceLeaf], 0),
    executionMembership: buildMidgardValidationMerkleMembership(
      [executionLeaf],
      0,
    ),
  };
};
const evidence = (languageTag: 0 | 3 | 128, subject = accepted) =>
  prepareReceivePurposeLanguageEvidence({
    finding: { subject, executionIndex: 0 },
    descriptor: descriptor(languageTag),
  });

describe("receivePurposeLanguage V1", () => {
  it("freezes ID 34 and binds the exact typed execution coordinate", () => {
    expect(RECEIVE_PURPOSE_LANGUAGE_CATEGORY_ID).toBe("00000034");
    expect(
      classifyReceivePurposeLanguageFinding({
        subject: forced(),
        executionIndex: 0,
      }),
    ).toBeTruthy();
    expect(() =>
      classifyReceivePurposeLanguageFinding({
        subject: forced(1),
        executionIndex: 0,
      }),
    ).toThrow(/coordinate changed/u);
  });
  it("has a callback-free package-owned real Lucid lifecycle surface", () => {
    expect(RECEIVE_PURPOSE_LANGUAGE_DIRECT_CONFIG_KEYS).not.toContain("submit");
    const workflow = createReceivePurposeLanguageWorkflow({} as never);
    expect(Object.keys(workflow)).toEqual(["run"]);
    expect(RECEIVE_PURPOSE_LANGUAGE_CONFIG_KEYS).toEqual([
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
    expect(RECEIVE_PURPOSE_LANGUAGE_STEP_DATUM_SCHEMAS).toHaveLength(3);
    expect(
      Object.keys(createReceivePurposeLanguageActuator({} as never)),
    ).toEqual(["capture"]);
  });
  it("rejects callback/evidence authority at manifest and runner boundaries", async () => {
    await expect(
      createManifestBoundReceivePurposeLanguageWorkflow({
        submit: async () => "00".repeat(32),
      } as never),
    ).rejects.toThrow(/callback authority/u);
    await expect(
      runOrResumeManifestBoundReceivePurposeLanguageWorkflow({
        workflow: {} as never,
        sources: [],
        journal: {} as never,
        evidence: evidence(3),
      } as never),
    ).rejects.toThrow(/caller-authored evidence/u);
    const runner = createReceivePurposeLanguageWorkflowRunnerSurface({
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
      AuthenticatedReceiveLanguageSchema as never,
    );
    expect(
      Data.to(
        Data.from(
          encoded,
          AuthenticatedReceiveLanguageSchema as never,
        ) as never,
        AuthenticatedReceiveLanguageSchema as never,
      ),
    ).toBe(encoded);
  });
  it("convicts only accepted receive PlutusV3 and contradicts allowed languages", () => {
    expect(receivePurposeLanguageEvidenceCloses(evidence(3))).toBe(true);
    expect(receivePurposeLanguageEvidenceCloses(evidence(0))).toBe(false);
    expect(receivePurposeLanguageEvidenceCloses(evidence(0, forced()))).toBe(
      true,
    );
    expect(receivePurposeLanguageEvidenceCloses(evidence(128, forced()))).toBe(
      true,
    );
    expect(receivePurposeLanguageEvidenceCloses(evidence(3, forced()))).toBe(
      false,
    );
  });
  it("rejects purpose, source, execution, language and coordinate substitution", () => {
    const exact = descriptor(3);
    expect(() =>
      prepareReceivePurposeLanguageEvidence({
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
        prepareReceivePurposeLanguageEvidence({
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
      prepareReceivePurposeLanguageEvidence({
        finding: { subject: accepted, executionIndex: 1 },
        descriptor: exact,
      }),
    ).toThrow(/coordinate changed/u);
  });
  it("replays authenticated descriptors deterministically", () => {
    const findings = detectReceivePurposeLanguageAcceptedReplay({
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
    const rows: ReceivePurposeLanguageJournalEntry[] = [];
    let cursor: ReceivePurposeLanguageCursor = {
      stage: "none",
      threadOutRef: "",
    };
    const txHash = "aa".repeat(32);
    const journal = {
      load: async () => rows,
      append: async (row: ReceivePurposeLanguageJournalEntry) => {
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
      await runReceivePurposeLanguageWorkflow({
        evidence: evidence(3),
        journal,
        transactions,
      }),
    ).toBe("none");
    expect(
      await runReceivePurposeLanguageWorkflow({
        evidence: evidence(3),
        journal,
        transactions,
      }),
    ).toBe("step01");
    rows.length = 0;
    cursor = { stage: "step01", threadOutRef: `${txHash}#0` };
    expect(
      await cancelReceivePurposeLanguageWorkflow({
        evidence: evidence(3),
        journal,
        transactions,
      }),
    ).toBe("cancelled");
  });
});
