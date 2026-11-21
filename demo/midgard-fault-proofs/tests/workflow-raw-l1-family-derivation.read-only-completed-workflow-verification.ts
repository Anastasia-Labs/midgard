import { FraudProofTokenDatum } from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { verifyCompletedFraudProofWorkflow } from "../src/workflow/completed-verification.js";
import type { FraudProofWorkflowTerminal } from "../src/workflow/journal.fraud-proof-workflow-terminal.js";
import { deriveFraudProofRawL1CompletedTerminal } from "../src/workflow/raw-l1-family-derivation.derive-fraud-proof-raw-l1-family-stage.js";
import { computeFraudProofRawL1PointId } from "../src/workflow/raw-l1-snapshot.admit-fraud-proof-raw-l1-transaction.js";
import { FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY } from "../src/workflow/raw-l1-snapshot.compute-fraud-proof-raw-l1-snapshot-evidence-digest.js";
import { fixture } from "./support/raw-l1-terminal-fixture.fixture.js";
import {
  DEPLOYMENT,
  hash32,
  policy,
  releaseEconomics,
  releaseFinality,
} from "./support/raw-l1-terminal-fixture.output.js";
import { rollBackTerminalFixture } from "./support/raw-l1-terminal-fixture.roll-back-terminal-fixture.js";

describe("read-only completed workflow verification", () => {
  const completed = async () => {
    const value = await fixture({
      proofCreation: true,
      confirmationDepth: 2162,
    });
    const derived = await deriveFraudProofRawL1CompletedTerminal({
      ...value,
      releaseEconomics,
    });
    if (derived.kind !== "removed")
      throw new Error("fixture must be completed");
    const terminal = derived.terminal;
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
      verify: (
        snapshot = value.snapshot,
        /** `null` verifies without a runner's terminal. */
        candidate: FraudProofWorkflowTerminal | null = terminal,
      ) =>
        verifyCompletedFraudProofWorkflow({
          binding,
          ...(candidate === null ? {} : { terminal: candidate }),
          decisionDigest: hash32("92"),
          authority: {
            authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
            capture: async () => snapshot,
          },
        }),
    };
  };

  it("authenticates exact completed bytes and economics without thread decoders", async () => {
    const value = await completed();
    await expect(value.verify()).resolves.toEqual({
      kind: "applicable",
      terminal: value.terminal,
    });
    // Without a runner's terminal, the completion is the one the chain shows.
    await expect(value.verify(value.snapshot, null)).resolves.toEqual({
      kind: "applicable",
      terminal: value.terminal,
    });
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
      provenance: { ...value.snapshot.provenance, tipPoint: point },
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
  it("rejects a terminal for another header than the workflow's identity", async () => {
    const value = await completed();
    await expect(
      value.verify(value.snapshot, {
        ...value.terminal,
        headerHash: policy("99"),
      }),
    ).rejects.toThrow("does not match workflow identity");
  });
  it.each(["economics", "proofToken", "anchor"] as const)(
    "rejects a self-consistent terminal %s that contradicts authenticated L1",
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
        provenance: { ...value.snapshot.provenance, tipPoint: tip },
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
