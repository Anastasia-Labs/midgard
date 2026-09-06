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
      if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1")
        appendFileSync(
          `/tmp/midgard-cek-core-fit-raw-${blueprintSha256}.jsonl`,
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
  rows.push(...captured);
  completed++;
  return result;
};
afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect(completed).toBe(17);
  for (const row of rows) {
    expect(row.memoryUnits, row.name).toBeLessThanOrEqual(13_200_000n);
    expect(row.cpuUnits, row.name).toBeLessThanOrEqual(8_000_000_000n);
  }
  const bytes = readFileSync(realBlueprintPath);
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/validation-trace-cek-core-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger({
      category: "validationTraceDispute/CEK core",
      blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
      compilerVersion: JSON.parse(bytes.toString()).preamble.compiler.version,
      measurements: rows,
    }),
  );
});

describe("bounded CEK core published lifecycle", () => {
  it.each([42, 29, 33, 35, 36, 40, 47])(
    "proves context-derived semantic builtin %s through its published leaf",
    async (cekSemanticTag) => {
      const result = await runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "cek",
            plutusSelection: true,
            cekSemanticTag,
            cekCoreArm: "executeBuiltinSemantic",
          }),
      );
      expect(result.awardResult?.txHash).toHaveLength(64);
      expect(result.removal?.transactions.length).toBeGreaterThan(0);
    },
    900_000,
  );
  it.each(["stepBuiltinMapToList", "finishBuiltinMapConversion"])(
    "proves map conversion control %s",
    async (cekCoreArm) => {
      const result = await runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "cek",
            plutusSelection: true,
            cekSemanticTag: 43,
            cekCoreArm,
          }),
      );
      expect(result.awardResult?.txHash).toHaveLength(64);
      expect(result.removal?.transactions.length).toBeGreaterThan(0);
    },
    900_000,
  );
  it("proves context map conversion through roots, budget and nodes", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekSemanticTag: 43,
          cekCoreArm: "startBuiltinMapConversion",
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);
  it("proves the exact application step, mints the proof and removes the forged block", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekCoreArm: "computeApplication",
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);
  it("proves a direct integer builtin through roots, budget and scalar semantics", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekDirectBuiltin: true,
          cekCoreArm: "executeBuiltinDirect",
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);

  it("proves the terminal core hand-off", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekCoreArm: "returnEmptyContinuation",
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);
  it("resumes a JSON-restored builtin checkpoint after accepted submission and process loss", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekDirectBuiltin: true,
          cekCoreArm: "executeBuiltinDirect",
        }),
      { restartCekCore: true },
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);
  it("cancels an accepted builtin checkpoint without an award", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekDirectBuiltin: true,
          cekCoreArm: "executeBuiltinDirect",
        }),
      { cancelCekCore: true },
    );
    expect(result.cancellation?.txHash).toHaveLength(64);
    expect(result.awardResult).toBeUndefined();
    expect(result.removal).toBeUndefined();
  }, 900_000);
  it("refuses a forged core successor against an honest block", async () => {
    await expect(
      runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekDirectBuiltin: true,
          cekCoreArm: "executeBuiltinDirect",
          dishonestChallenger: true,
        }),
      ),
    ).rejects.toThrow(/semantic-resolution/);
  }, 900_000);

  it("proves the maximum ten-leaf BLS final verification through published stages", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekBlsFinal: true,
          cekCoreArm: "executeBuiltinBlsFinal",
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);

  it("proves the maximum 9,215-byte direct revealed payload", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekMaximumDirect: true,
          cekCoreArm: "executeBuiltinDirect",
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);
});
