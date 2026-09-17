import { readFile } from "node:fs/promises";

import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { Constr } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { applyScriptIntegrityHashMismatchScripts } from "../src/script-integrity-hash-mismatch/contracts.js";
import {
  languagesForIntegrityBitmap,
  prepareScriptIntegrityHashMismatchEvidence,
  SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_ID,
  scriptIntegrityHashMismatchEvidenceCloses,
} from "../src/script-integrity-hash-mismatch/family.js";
import { scriptIntegrityHashMismatchDetectionFromEvidence } from "../src/script-integrity-hash-mismatch/replay.js";
import {
  createManifestBoundScriptIntegrityHashMismatchWorkflow,
  createScriptIntegrityHashMismatchWorkflowRunnerSurface,
  SCRIPT_INTEGRITY_HASH_MISMATCH_CONFIG_KEYS,
} from "../src/script-integrity-hash-mismatch/v1.js";
import {
  cancelScriptIntegrityHashMismatchWorkflow,
  runScriptIntegrityHashMismatchWorkflow,
  type ScriptIntegrityHashMismatchCursor,
  type ScriptIntegrityHashMismatchJournalEntry,
} from "../src/script-integrity-hash-mismatch/workflow.js";
import { WORKFLOW_ADAPTER_RUNNER } from "../src/workflow/adapters.js";

const txId = "00".repeat(32);
/**
 * The redeemer-witness hash and the four expected script-integrity hashes are
 * the cross-implementation vectors the on-chain validator is pinned to:
 * `onchain/aiken/lib/midgard/script-language-views-v1.test.ak`
 * (`script_integrity_language_view_vectors_match_typescript`) asserts exactly
 * these four bytes for bitmaps 0..3 over the same all-`0x11` redeemer hash,
 * and `demo/midgard-core/tests/script-language-views.test.ts` asserts them for
 * the codec that produces them. Their provenance is therefore a second,
 * independently written implementation of the same rule — not this module's
 * own output — which is what makes the bitmap-to-hash wiring below checkable
 * rather than self-confirming.
 */
const redeemerHash = "11".repeat(32);
const expected = [
  "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53",
  "d7239eb1bd8b7376dedfbf7e6201815b225c023d11c975cd99d25d5236b199a1",
  "71201d25ea11e4104eda108782a7d67b37b4ae97df6dc3258b06d9c98e58bbcb",
  "6d49b4f24c60bec1cb34a2538278252059ec0601b7f675ef73fe2b48e24317d8",
] as const;
const accepted = acceptedVerdictSubject(txId);
const forced = forcedVerdictSubject({
  transactionId: txId,
  sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
  rejectionReason: "ScriptIntegrityHashMismatch",
});
const evidence = (
  bitmap: 0 | 1 | 2 | 3,
  subject = accepted,
  committed = "ff".repeat(32),
) =>
  prepareScriptIntegrityHashMismatchEvidence({
    finding: { subject },
    scriptIntegrityHash: committed,
    redeemerWitnessHash: redeemerHash,
    selectedLanguageBitmap: bitmap,
    executionCount: 2n,
  });

describe("scriptIntegrityHashMismatch V1", () => {
  it("freezes ID 33 and the canonical two-bit language order", () => {
    expect(SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_ID).toBe("00000033");
    expect(languagesForIntegrityBitmap(0)).toEqual([]);
    expect(languagesForIntegrityBitmap(1)).toEqual(["PlutusV3"]);
    expect(languagesForIntegrityBitmap(2)).toEqual(["MidgardV1"]);
    expect(languagesForIntegrityBitmap(3)).toEqual(["PlutusV3", "MidgardV1"]);
  });
  it("derives the on-chain expected hash for every language bitmap", () => {
    for (const bitmap of [0, 1, 2, 3] as const)
      expect(evidence(bitmap).expectedHash, `bitmap ${bitmap.toString()}`).toBe(
        expected[bitmap],
      );
  });
  it("closes both direction polarities and refuses equality/mismatch inversions", () => {
    expect(scriptIntegrityHashMismatchEvidenceCloses(evidence(3))).toBe(true);
    expect(
      scriptIntegrityHashMismatchEvidenceCloses(
        evidence(3, accepted, expected[3]),
      ),
    ).toBe(false);
    expect(
      scriptIntegrityHashMismatchEvidenceCloses(
        evidence(2, forced, expected[2]),
      ),
    ).toBe(true);
    expect(scriptIntegrityHashMismatchEvidenceCloses(evidence(2, forced))).toBe(
      false,
    );
  });
  it("detects only honest accepted/forced integrity contradictions", () => {
    const acceptedFault = scriptIntegrityHashMismatchDetectionFromEvidence({
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
      scriptIntegrityHashMismatchDetectionFromEvidence({
        headerHash: "77".repeat(32),
        position: 2n,
        source: "accepted",
        evidence: evidence(3, accepted, expected[3]),
      }),
    ).toBeNull();
    expect(
      scriptIntegrityHashMismatchDetectionFromEvidence({
        headerHash: "77".repeat(32),
        position: 3n,
        source: "forced",
        evidence: evidence(2, forced, expected[2]),
      }),
    ).toMatchObject({ violationId: "script-integrity-hash-mismatch" });
    expect(
      scriptIntegrityHashMismatchDetectionFromEvidence({
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
    const rows: ScriptIntegrityHashMismatchJournalEntry[] = [];
    let cursor: ScriptIntegrityHashMismatchCursor = {
      stage: "none",
      threadOutRef: "",
    };
    const txHash = "aa".repeat(32);
    const journal = {
      load: async () => rows,
      append: async (row: ScriptIntegrityHashMismatchJournalEntry) => {
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
      await runScriptIntegrityHashMismatchWorkflow({
        evidence: evidence(3),
        journal,
        transactions,
      }),
    ).toBe("none");
    expect(rows.map(({ phase }) => phase)).toEqual(["intent", "submitted"]);
    expect(
      await runScriptIntegrityHashMismatchWorkflow({
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
      const rows: ScriptIntegrityHashMismatchJournalEntry[] = [];
      const cursor = { stage, threadOutRef: `${"bb".repeat(32)}#0` };
      await expect(
        cancelScriptIntegrityHashMismatchWorkflow({
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
  it("exposes a strict shared-runtime loader and refuses category substitution", async () => {
    // Each forbidden key on its own: `not.arrayContaining` over the four is
    // satisfied by omitting any single one of them, so it does not state the
    // claim that none of them may appear.
    for (const forbidden of ["evidence", "actuator", "verdict", "submit"])
      expect(SCRIPT_INTEGRITY_HASH_MISMATCH_CONFIG_KEYS).not.toContain(
        forbidden,
      );
    await expect(
      createManifestBoundScriptIntegrityHashMismatchWorkflow({
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
    const runner = createScriptIntegrityHashMismatchWorkflowRunnerSurface({
      loadRuntimeConfig,
    });
    expect(runner.runnerVersion).toBe(WORKFLOW_ADAPTER_RUNNER);
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
    const steps = applyScriptIntegrityHashMismatchScripts({
      blueprint,
      network: "Preview",
      computationThreadPolicyId: "11".repeat(28),
      fraudProofPolicyId: "22".repeat(28),
      fraudProofTokenAddressData: new Constr(0, []),
      hubOracleScriptHash: "33".repeat(28),
    });
    expect(
      steps.every(
        ({ spendingScript }) => spendingScript.script.length / 2 < 15_872,
      ),
    ).toBe(true);
  });
});
