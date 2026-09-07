import { createHash } from "node:crypto";
import { appendFileSync, readFileSync } from "node:fs";
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
  const shape = expect.getState().currentTestName ?? "core";
  const captured: VanRossemFitMeasurement[] = [];
  const blueprintSha256 = createHash("sha256")
    .update(readFileSync(realBlueprintPath))
    .digest("hex");
  const result = await runScenario(fixture, {
    ...options,
    onSubmittedTransaction: (m) => {
      appendFileSync(
        `/tmp/midgard-cek-context-fit-raw-${blueprintSha256}.jsonl`,
        JSON.stringify({ blueprintSha256, shape, ...m }, (_key, value) =>
          typeof value === "bigint" ? value.toString() : value,
        ) + "\n",
      );
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
      });
    },
  });
  for (const row of captured) {
    expect(row.memoryUnits, row.name).toBeLessThanOrEqual(13_200_000n);
    expect(row.cpuUnits, row.name).toBeLessThanOrEqual(8_000_000_000n);
  }
  rows.push(...captured);
  completed++;
  return result;
};
afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect(completed).toBe(24);
  for (const row of rows) {
    expect(row.memoryUnits, row.name).toBeLessThanOrEqual(13_200_000n);
    expect(row.cpuUnits, row.name).toBeLessThanOrEqual(8_000_000_000n);
  }
  const bytes = readFileSync(realBlueprintPath);
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/validation-trace-cek-context-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger({
      category: "validationTraceDispute/CEK context",
      blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
      compilerVersion: JSON.parse(bytes.toString()).preamble.compiler.version,
      measurements: rows,
    }),
  );
});

describe("bounded CEK context registered lifecycle", () => {
  it.each([0, 1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 13])(
    "proves canonical context stage %s, mints its proof and removes the forged block",
    async (cekContextStage) => {
      const result = await runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "cek",
            plutusSelection: true,
            cekContextStage,
          }),
      );
      expect(result.awardResult?.txHash).toHaveLength(64);
      expect(result.removal?.transactions.length).toBeGreaterThan(0);
    },
    900_000,
  );
  it.each(["restart", "cancel"])(
    "%s restores exact JSON context evidence after an accepted checkpoint",
    async (action) => {
      const result = await runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "cek",
            plutusSelection: true,
            cekContextStage: 6,
          }),
        action === "restart"
          ? { restartCekContext: true }
          : { cancelCekContext: true },
      );
      if (action === "restart") {
        expect(result.awardResult?.txHash).toHaveLength(64);
        expect(result.removal?.transactions.length).toBeGreaterThan(0);
      } else {
        expect(result.cancellation?.txHash).toHaveLength(64);
        expect(result.awardResult).toBeUndefined();
        expect(result.removal).toBeUndefined();
      }
    },
    900_000,
  );
  it("refuses a forged context successor against an honest block", async () => {
    await expect(
      runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekContextStage: 6,
          dishonestChallenger: true,
        }),
      ),
    ).rejects.toThrow(/semantic-resolution/);
  }, 900_000);

  it.each([6, 8])(
    "proves context stage %s at the exact 1,304-asset mint maximum",
    async (cekContextStage) => {
      const result = await runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "cek",
            plutusSelection: true,
            cekSelection: true,
            assetCount: 1304,
            cekContextStage,
            ...(cekContextStage === 8 ? { cekContextMintCursor: 1303 } : {}),
          }),
      );
      expect(result.awardResult?.txHash).toHaveLength(64);
      expect(result.removal?.transactions.length).toBeGreaterThan(0);
    },
    900_000,
  );

  // Negative polarity of the mixed-width closure (the size plan's
  // "Mixed-width mint ordering closure"): the challenger's trace is honest
  // except the disputed stage-8 item's own permutation witness, so every
  // local builder gate passes and the refusal is the on-chain stage-8
  // membership / head-opening clause the selector tests pin
  // (context_mint_item_refuses_* in cek-split-v1.test.ak).
  it.each(["foreignIndex", "forgedHead", "omittedHead"] as const)(
    "refuses a %s mutation of the mixed-width mint permutation witness",
    async (permutationWitnessMutation) => {
      await expect(
        runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "cek",
            plutusSelection: true,
            cekSelection: true,
            assetCount: 1304,
            cekContextStage: 8,
            cekContextMintCursor: 1303,
            permutationWitnessMutation,
          }),
        ),
      ).rejects.toThrow(/failed script execution/);
    },
    900_000,
  );

  it.each(["openHeader", "openTail", "traverseData"])(
    "proves shared context item action %s through the registered return chain",
    async (cekContextItemAction) => {
      const result = await runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "cek",
            plutusSelection: true,
            cekContextStage: cekContextItemAction === "traverseData" ? 9 : 0,
            cekContextItemAction,
          }),
      );
      expect(result.awardResult?.txHash).toHaveLength(64);
      expect(result.removal?.transactions.length).toBeGreaterThan(0);
    },
    900_000,
  );

  it("proves the 224-observer context maximum with authenticated raw field carriage", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekContextStage: 5,
          cekObserverCount: 224,
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);

  it.each(["restart", "cancel"])(
    "%s from the shared item checkpoint using exact retained bytes",
    async (action) => {
      const result = await runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "cek",
            plutusSelection: true,
            cekContextStage: 0,
            cekContextItemAction: "openHeader",
          }),
        {
          cekContextCheckpointStage: "item",
          ...(action === "restart"
            ? { restartCekContext: true }
            : { cancelCekContext: true }),
        },
      );
      if (action === "restart")
        expect(result.awardResult?.txHash).toHaveLength(64);
      else expect(result.cancellation?.txHash).toHaveLength(64);
    },
    900_000,
  );
  it.each(["restart", "cancel"])(
    "%s from the verified context settlement checkpoint",
    async (action) => {
      const result = await runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "cek",
            plutusSelection: true,
            cekContextStage: 6,
          }),
        {
          cekContextCheckpointStage: "settle",
          ...(action === "restart"
            ? { restartCekContext: true }
            : { cancelCekContext: true }),
        },
      );
      if (action === "restart")
        expect(result.awardResult?.txHash).toHaveLength(64);
      else expect(result.cancellation?.txHash).toHaveLength(64);
    },
    900_000,
  );
});
