import "./workflow-raw-l1-family-derivation.distinct-asset-computation-datum-recovery.js";

import { FraudProofTokenDatum } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  computeFraudProofRawL1PointId,
  computeFraudProofWorkflowId,
  deriveFraudProofRawL1CompletedTerminal,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  verifyCompletedFraudProofWorkflow,
} from "../src/workflow/index.js";
import {
  DEPLOYMENT,
  fixture,
  hash32,
  policy,
  releaseEconomics,
  releaseFinality,
} from "./support/raw-l1-terminal-fixture.js";
import { rollBackTerminalFixture } from "./support/raw-l1-terminal-fixture.js";

describe("read-only completed workflow verification", () => {
  const completed = async () => {
    const value = await fixture({
      proofCreation: true,
      confirmationDepth: 2162,
    });
    const terminal = await deriveFraudProofRawL1CompletedTerminal({
      ...value,
      releaseEconomics,
    });
    if (terminal === null) throw new Error("fixture must be completed");
    const identity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend" as const,
      target: {
        kind: "state_queue_header" as const,
        headerHash: value.definition.headerHash,
      },
      decisionDigest: hash32("92"),
    };
    const workflowId = computeFraudProofWorkflowId(identity);
    const entries: FraudProofWorkflowJournalEntry[] = [];
    const append = (event: FraudProofWorkflowJournalEntry["event"]) =>
      entries.push({
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId,
        identity,
        sequence: entries.length,
        recordedAt: "2026-09-16T12:00:00.000Z",
        event,
      });
    append({ kind: "started" });
    append({
      kind: "prepared",
      artifact: {},
      artifactDigest: journalJsonDigest({}),
    });
    for (const txHash of [
      terminal.proofToken.createdByTxHash,
      terminal.correction.removalTxHash,
    ]) {
      append({
        kind: "preflight_passed",
        actionId: txHash,
        txHash,
        localEvaluator: "lucid",
        referenceScripts: [],
      });
      append({
        kind: "submission_intent",
        actionId: txHash,
        txHash,
        attempt: 1,
        actionInput: {},
      });
      append({ kind: "submitted", actionId: txHash, txHash, attempt: 1 });
      append({
        kind: "reconciled",
        actionId: txHash,
        txHash,
        outcome: "confirmed",
      });
      append({ kind: "confirmed", actionId: txHash, txHash });
    }
    append({
      kind: "completed",
      terminal,
      terminalDigest: journalJsonDigest(terminal),
    });
    const definition = {
      ...value.definition,
      computationThread: {
        policyId: value.definition.computationThread.policyId,
        steps: value.definition.computationThread.steps.map(
          ({ role, address }) => ({ role, address }),
        ),
      },
    };
    const binding = {
      deploymentFingerprint: DEPLOYMENT,
      definition,
      releaseFinality,
      releaseEconomics,
    };
    return {
      ...value,
      terminal,
      entries,
      verify: (
        snapshot = value.snapshot,
        candidate: FraudProofWorkflowTerminal = terminal,
      ) =>
        verifyCompletedFraudProofWorkflow({
          binding,
          entries,
          terminal: candidate,
          decisionDigest: identity.decisionDigest,
          authority: {
            authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
            capture: async () => snapshot,
          },
        }),
    };
  };

  it("authenticates exact completed bytes and economics without thread decoders or journal mutations", async () => {
    const value = await completed();
    const saved = structuredClone(value.entries);
    await expect(value.verify()).resolves.toEqual({
      kind: "applicable",
      terminal: value.terminal,
    });
    expect(value.entries).toEqual(saved);
  });
  it("invalidates completion when an authenticated rollback restores the target", async () => {
    const value = await completed();
    await expect(value.verify(rollBackTerminalFixture(value))).resolves.toEqual(
      { kind: "pending", reason: "target_live" },
    );
  });
  it("keeps completed verification pending when authenticated release depth regresses", async () => {
    const value = await completed();
    const point = value.snapshot.cursor.point;
    const shallow = {
      ...value.snapshot,
      provenance: { ...value.snapshot.provenance, ogmiosTip: point },
      cursor: { ...value.snapshot.cursor, tip: point, confirmationDepth: 1 },
      transactions: value.snapshot.transactions.map((tx) => ({
        ...tx,
        confirmationDepth: 1,
      })),
    };
    await expect(value.verify(shallow)).resolves.toEqual({
      kind: "pending",
      reason: "release_finality",
    });
  });
  it.each(["headerHash", "economics", "proofToken"] as const)(
    "rejects substituted durable %s",
    async (field) => {
      const value = await completed();
      const changed =
        field === "headerHash"
          ? { ...value.terminal, headerHash: policy("99") }
          : field === "economics"
            ? {
                ...value.terminal,
                economics: {
                  ...value.terminal.economics,
                  proverRewardLovelace: "1",
                },
              }
            : {
                ...value.terminal,
                proofToken: {
                  ...value.terminal.proofToken,
                  unit: policy("99"),
                },
              };
      await expect(value.verify(value.snapshot, changed)).rejects.toThrow(
        "durable terminal",
      );
    },
  );
  it.each(["economics", "proofToken", "anchor"] as const)(
    "rejects self-consistent journal %s that contradicts authenticated L1",
    async (field) => {
      const value = await completed();
      const changed =
        field === "anchor"
          ? {
              ...value.terminal,
              observedAt: {
                ...value.terminal.observedAt,
                blockHash: hash32("99"),
              },
            }
          : field === "economics"
            ? {
                ...value.terminal,
                economics: {
                  ...value.terminal.economics,
                  proverRewardLovelace: "1",
                },
              }
            : {
                ...value.terminal,
                proofToken: {
                  ...value.terminal.proofToken,
                  unit: policy("99"),
                },
              };
      const last = value.entries.at(-1)!;
      value.entries[value.entries.length - 1] = {
        ...last,
        event: {
          kind: "completed",
          terminal: changed,
          terminalDigest: journalJsonDigest(changed),
        },
      };
      await expect(value.verify(value.snapshot, changed)).rejects.toThrow(
        "authenticated L1 facts",
      );
    },
  );
  it("rejects changed raw proof output bytes rather than treating integrity failure as pending", async () => {
    const value = await completed();
    const tampered = {
      ...value.snapshot,
      scopes: value.snapshot.scopes.map((scope) =>
        scope.role === "permanent_proof_token"
          ? {
              ...scope,
              utxos: scope.utxos.map((utxo) => ({
                ...utxo,
                datumCbor: Data.to(
                  { fraud_prover: policy("99") },
                  FraudProofTokenDatum,
                ),
              })),
            }
          : scope,
      ),
    };
    await expect(value.verify(tampered)).rejects.toThrow();
  });
  it.each([30, 2161] as const)(
    "keeps a completed receipt pending after bounded rollback to canonical depth %s",
    async (confirmationDepth) => {
      const value = await completed();
      const saved = structuredClone(value.entries);
      const point = value.snapshot.cursor.point;
      const tipInput = {
        slot: (BigInt(point.slot) + BigInt(confirmationDepth)).toString(),
        blockNo: (
          BigInt(point.blockNo) +
          BigInt(confirmationDepth) -
          1n
        ).toString(),
        blockHash: hash32("93"),
      };
      const tip = {
        ...tipInput,
        pointId: computeFraudProofRawL1PointId(tipInput),
      };
      const shallow = {
        ...value.snapshot,
        provenance: { ...value.snapshot.provenance, ogmiosTip: tip },
        cursor: { ...value.snapshot.cursor, tip, confirmationDepth },
        transactions: value.snapshot.transactions.map((tx) => ({
          ...tx,
          confirmationDepth,
        })),
      };
      expect(BigInt(tip.blockNo) - BigInt(point.blockNo) + 1n).toBe(
        BigInt(confirmationDepth),
      );
      expect(
        BigInt(value.snapshot.cursor.tip.blockNo) - BigInt(tip.blockNo),
      ).toBeLessThanOrEqual(2160n);
      await expect(value.verify(shallow)).resolves.toEqual({
        kind: "pending",
        reason: "release_finality",
      });
      expect(value.entries).toEqual(saved);
    },
  );

  it("rejects scalar confirmation depth inconsistent with the canonical tip", async () => {
    const value = await completed();
    const forged = {
      ...value.snapshot,
      cursor: { ...value.snapshot.cursor, confirmationDepth: 2161 },
      transactions: value.snapshot.transactions.map((tx) => ({
        ...tx,
        confirmationDepth: 2161,
      })),
    };
    await expect(value.verify(forged)).rejects.toThrow(
      "confirmation depth disagrees with chain points",
    );
  });
});
