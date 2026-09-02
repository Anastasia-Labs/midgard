import { readFile } from "node:fs/promises";

import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { Constr } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { applyScriptIntegrityHashMismatchScriptsV1 } from "../src/script-integrity-hash-mismatch/contracts-v1.js";
import {
  languagesForIntegrityBitmapV1,
  prepareScriptIntegrityHashMismatchEvidenceV1,
  SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_ID_V1,
  scriptIntegrityHashMismatchEvidenceClosesV1,
} from "../src/script-integrity-hash-mismatch/family-v1.js";
import { scriptIntegrityHashMismatchDetectionFromEvidenceV1 } from "../src/script-integrity-hash-mismatch/production-replay-v1.js";
import {
  createManifestBoundScriptIntegrityHashMismatchWorkflowV1,
  createScriptIntegrityHashMismatchProductionWorkflowRunnerSurfaceV1,
  SCRIPT_INTEGRITY_HASH_MISMATCH_PRODUCTION_CONFIG_KEYS_V1,
  type ScriptIntegrityHashMismatchProductionActuatorV1,
} from "../src/script-integrity-hash-mismatch/production-v1.js";
import {
  cancelScriptIntegrityHashMismatchWorkflowV1,
  runScriptIntegrityHashMismatchWorkflowV1,
  type ScriptIntegrityHashMismatchCursorV1,
  type ScriptIntegrityHashMismatchJournalEntryV1,
} from "../src/script-integrity-hash-mismatch/workflow-v1.js";
import { PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1 } from "../src/workflow/production-adapters-v1.js";

const txId = "00".repeat(32);
const redeemerHash = "11".repeat(32);
const expected = [
  "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53",
  "d7239eb1bd8b7376dedfbf7e6201815b225c023d11c975cd99d25d5236b199a1",
  "71201d25ea11e4104eda108782a7d67b37b4ae97df6dc3258b06d9c98e58bbcb",
  "6d49b4f24c60bec1cb34a2538278252059ec0601b7f675ef73fe2b48e24317d8",
] as const;
const accepted = acceptedVerdictSubjectV1(txId);
const forced = forcedVerdictSubjectV1({
  transactionId: txId,
  sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
  rejectionReason: "ScriptIntegrityHashMismatch",
});
const evidence = (
  bitmap: 0 | 1 | 2 | 3,
  subject = accepted,
  committed = "ff".repeat(32),
) =>
  prepareScriptIntegrityHashMismatchEvidenceV1({
    finding: { subject },
    scriptIntegrityHash: committed,
    redeemerWitnessHash: redeemerHash,
    selectedLanguageBitmap: bitmap,
    executionCount: 2n,
  });

describe("scriptIntegrityHashMismatch V1", () => {
  it("freezes ID 33 and the canonical two-bit language order", () => {
    expect(SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_ID_V1).toBe("00000033");
    expect(languagesForIntegrityBitmapV1(0)).toEqual([]);
    expect(languagesForIntegrityBitmapV1(1)).toEqual(["PlutusV3"]);
    expect(languagesForIntegrityBitmapV1(2)).toEqual(["MidgardV1"]);
    expect(languagesForIntegrityBitmapV1(3)).toEqual(["PlutusV3", "MidgardV1"]);
  });
  it("matches authoritative Cardano language-view vectors", () => {
    for (const bitmap of [0, 1, 2, 3] as const)
      expect(evidence(bitmap).expectedHash).toBe(expected[bitmap]);
  });
  it("closes both direction polarities and refuses equality/mismatch inversions", () => {
    expect(scriptIntegrityHashMismatchEvidenceClosesV1(evidence(3))).toBe(true);
    expect(
      scriptIntegrityHashMismatchEvidenceClosesV1(
        evidence(3, accepted, expected[3]),
      ),
    ).toBe(false);
    expect(
      scriptIntegrityHashMismatchEvidenceClosesV1(
        evidence(2, forced, expected[2]),
      ),
    ).toBe(true);
    expect(
      scriptIntegrityHashMismatchEvidenceClosesV1(evidence(2, forced)),
    ).toBe(false);
  });
  it("detects only honest accepted/forced integrity contradictions", () => {
    const acceptedFault = scriptIntegrityHashMismatchDetectionFromEvidenceV1({
      headerHash: "77".repeat(32),
      position: 2n,
      source: "accepted",
      evidence: evidence(3),
    });
    expect(acceptedFault).toMatchObject({
      violationId: "script-integrity-hash-mismatch",
      position: 2n,
    });
    expect(
      scriptIntegrityHashMismatchDetectionFromEvidenceV1({
        headerHash: "77".repeat(32),
        position: 2n,
        source: "accepted",
        evidence: evidence(3, accepted, expected[3]),
      }),
    ).toBeNull();
    expect(
      scriptIntegrityHashMismatchDetectionFromEvidenceV1({
        headerHash: "77".repeat(32),
        position: 3n,
        source: "forced",
        evidence: evidence(2, forced, expected[2]),
      }),
    ).toMatchObject({ violationId: "script-integrity-hash-mismatch" });
    expect(
      scriptIntegrityHashMismatchDetectionFromEvidenceV1({
        headerHash: "77".repeat(32),
        position: 3n,
        source: "forced",
        evidence: evidence(2, forced),
      }),
    ).toBeNull();
  });
  it("rejects reason, bitmap, and hash substitution", () => {
    expect(() => evidence(4 as never)).toThrow(/bitmap/u);
    expect(() => evidence(1, accepted, "00")).toThrow(/hash32/u);
    expect(() => evidence(1, accepted, "00".repeat(32))).not.toThrow();
    expect(() =>
      evidence(1, { ...forced, rejection_reason: "InvalidRange" } as never),
    ).toThrow(/reason/u);
  });
  it("journals a captured signed identity before submission and resumes exactly", async () => {
    const rows: ScriptIntegrityHashMismatchJournalEntryV1[] = [];
    let cursor: ScriptIntegrityHashMismatchCursorV1 = {
      stage: "none",
      threadOutRef: "",
    };
    const txHash = "aa".repeat(32);
    const journal = {
      load: async () => rows,
      append: async (row: ScriptIntegrityHashMismatchJournalEntryV1) => {
        rows.push(row);
      },
    };
    const transactions = {
      observe: async () => cursor,
      capture: async () => ({
        txHash,
        target: { stage: "step01" as const, threadOutRef: `${txHash}#0` },
        submit: async () => {
          cursor = { stage: "step01", threadOutRef: `${txHash}#0` };
          return txHash;
        },
      }),
      transactionConfirmed: async () => true,
    };
    expect(
      await runScriptIntegrityHashMismatchWorkflowV1({
        evidence: evidence(3),
        journal,
        transactions,
      }),
    ).toBe("none");
    expect(rows.map(({ phase }) => phase)).toEqual(["intent", "submitted"]);
    expect(
      await runScriptIntegrityHashMismatchWorkflowV1({
        evidence: evidence(3),
        journal,
        transactions,
      }),
    ).toBe("step01");
    expect(rows.at(-1)?.phase).toBe("confirmed");
  });
  it("cancels every nonterminal family state", async () => {
    for (const stage of [
      "step01",
      "step02",
      "step03",
      "step04:0",
      "step04:1",
      "step05",
    ] as const) {
      const rows: ScriptIntegrityHashMismatchJournalEntryV1[] = [];
      const cursor = { stage, threadOutRef: `${"bb".repeat(32)}#0` };
      await expect(
        cancelScriptIntegrityHashMismatchWorkflowV1({
          evidence: evidence(3),
          journal: {
            load: async () => rows,
            append: async (row) => {
              rows.push(row);
            },
          },
          transactions: {
            observe: async () => cursor,
            capture: async () => ({
              txHash: "cc".repeat(32),
              target: { stage: "cancelled" as const, threadOutRef: "" },
              submit: async () => "cc".repeat(32),
            }),
            transactionConfirmed: async () => true,
          },
        }),
      ).resolves.toBe("cancelled");
    }
  });
  it("exposes no caller verdict callback on the production actuator", () => {
    const keys: readonly (keyof ScriptIntegrityHashMismatchProductionActuatorV1)[] =
      [
        "observe",
        "captureSignedTransaction",
        "submitSignedTransaction",
        "transactionConfirmed",
        "acquireRemovalLease",
      ];
    expect(keys).not.toContain("verdict");
    expect(keys).not.toContain("evidence");
  });
  it("exposes a strict shared-runtime loader and refuses category substitution", async () => {
    expect(
      SCRIPT_INTEGRITY_HASH_MISMATCH_PRODUCTION_CONFIG_KEYS_V1,
    ).not.toEqual(
      expect.arrayContaining(["evidence", "actuator", "verdict", "submit"]),
    );
    await expect(
      createManifestBoundScriptIntegrityHashMismatchWorkflowV1({
        manifest: {},
        blueprintJson: "{}",
        deploymentInfo: {},
        headerHash: "55".repeat(32),
        lucid: {},
        signer: {},
        source: {},
        decisionDigest: "66".repeat(32),
        stateQueueMutationLeaseCoordinator: {},
        referenceScripts: {},
        evidence: evidence(3),
      } as never),
    ).rejects.toThrow(/callback authority/u);
    const loadRuntimeConfig = vi.fn();
    const runner =
      createScriptIntegrityHashMismatchProductionWorkflowRunnerSurfaceV1({
        loadRuntimeConfig,
      });
    expect(runner.runnerVersion).toBe(PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1);
    await expect(
      runner.runOrResume({ category: "unusedRedeemer" } as never),
    ).rejects.toThrow(/category mismatch/u);
    expect(loadRuntimeConfig).not.toHaveBeenCalled();
  });
  it("keeps every fully applied testnet validator below the publication target", async () => {
    const blueprint = JSON.parse(
      await readFile(
        new URL("../../../onchain/aiken/plutus.json", import.meta.url),
        "utf8",
      ),
    ) as never;
    const steps = applyScriptIntegrityHashMismatchScriptsV1({
      blueprint,
      network: "Preview",
      computationThreadPolicyId: "11".repeat(28),
      fraudProofPolicyId: "22".repeat(28),
      fraudProofTokenAddressData: new Constr(0, []),
      hubOracleScriptHash: "33".repeat(28),
    });
    expect(
      steps.map(({ spendingScript }) => spendingScript.script.length / 2),
    ).toEqual([14692, 11817, 1603, 5401, 1957]);
    expect(
      steps.every(
        ({ spendingScript }) => spendingScript.script.length / 2 < 15_872,
      ),
    ).toBe(true);
  });
});
