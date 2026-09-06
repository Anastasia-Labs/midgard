import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { afterAll, describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import {
  buildForgedOperatorSuccessorValidationDisputeFixture,
  runForcedValidationDisputeScenario as runScenario,
} from "./support/submit-init-emulator-shared.js";

const rows: VanRossemFitMeasurement[] = [];
let completed = 0;
const runForcedValidationDisputeScenario = async (
  ...[fixture, options]: Parameters<typeof runScenario>
) => {
  const shape =
    [
      "native-selection",
      "plutus-selection",
      "165-node-graph",
      "data-repeated-graph",
      "json-restart",
      "cancel",
    ][completed] ?? "honest-refusal";
  const captured: VanRossemFitMeasurement[] = [];
  const result = await runScenario(fixture, {
    ...options,
    onSubmittedTransaction: (m) =>
      captured.push({
        name: `${shape}/transaction-${captured.length}`,
        maximumShape: shape,
        kind:
          m.executionMemory === 0n && m.executionSteps === 0n
            ? "publication"
            : "lifecycle",
        signedBytes: m.completeSignedBytes,
        memoryUnits: m.executionMemory,
        cpuUnits: m.executionSteps,
      }),
  });
  rows.push(...captured);
  completed++;
  return result;
};
afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect(completed).toBe(6);
  const bytes = readFileSync(realBlueprintPath);
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/validation-trace-cek-selection-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger({
      category: "validationTraceDispute/CEK selection",
      blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
      compilerVersion: JSON.parse(bytes.toString()).preamble.compiler.version,
      measurements: rows,
    }),
  );
});

describe("CEK selection authenticated yields", () => {
  it("proves a forged native execution selection and removes its block", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          cekSelection: true,
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);

  it("proves a forged Plutus execution selection with both material yields", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);

  it("proves a Plutus selection with 160 reachable lambda nodes", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekProgramLambdaCount: 160,
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);

  it("traverses Data blobs and a repeated constant reference", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekProgramLambdaCount: 160,
          cekDataGraph: true,
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);

  it("reconstructs the exact live checkpoint after process loss and a JSON checkpoint reload", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekProgramLambdaCount: 160,
        }),
      { restartCekMaterialTraversal: true },
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);

  it("cancels a live traversal checkpoint without minting a fraud proof", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekProgramLambdaCount: 160,
        }),
      { cancelCekMaterialTraversal: true },
    );
    expect(result.cancellation?.txHash).toHaveLength(64);
    expect(result.awardResult).toBeUndefined();
    expect(result.removal).toBeUndefined();
  }, 900_000);

  it("refuses a forged native successor against an honest block", async () => {
    await expect(
      runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          cekSelection: true,
          dishonestChallenger: true,
        }),
      ),
    ).rejects.toThrow(/semantic-resolution/);
  }, 900_000);
  it("refuses a forged Plutus successor against an honest block", async () => {
    await expect(
      runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekProgramLambdaCount: 160,
          dishonestChallenger: true,
        }),
      ),
    ).rejects.toThrow(/semantic-resolution/);
  }, 900_000);
});
