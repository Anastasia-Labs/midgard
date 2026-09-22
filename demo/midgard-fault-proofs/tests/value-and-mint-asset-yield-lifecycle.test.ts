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
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

const measurements: VanRossemFitMeasurement[] = [];
let completed = 0;
const runMeasured = async (
  kind: "replayAsset" | "outputAsset" | "mintAsset",
  assetCount: number,
  maximumAssetProof = false,
) => {
  const shape = `${kind}/${maximumAssetProof ? "maximum-proof" : assetCount}`;
  let index = 0;
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "valueAndMint",
        disputedValueKind: kind,
        assetCount,
        maximumAssetProof,
      }),
    {
      onSubmittedTransaction: (m) => {
        measurements.push({
          name: `${shape}/${(index++).toString().padStart(3, "0")}`,
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
    },
  );
  completed++;
  return result;
};
afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect(completed).toBe(9);
  const blueprint = readFileSync(realBlueprintPath);
  const ledger = buildVanRossemFitLedger({
    category: "validationTraceDispute/ValueAndMint",
    blueprintSha256: createHash("sha256").update(blueprint).digest("hex"),
    compilerVersion: JSON.parse(blueprint.toString()).preamble.compiler.version,
    measurements,
  });
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/validation-trace-value-and-mint-fit-ledger.json",
        import.meta.url,
      ),
    ),
    ledger,
  );
});

describe("ValueAndMint authenticated asset yield lifecycle", () => {
  it.each(["replayAsset", "outputAsset", "mintAsset"] as const)(
    "proves %s and removes the forged block",
    async (kind) => {
      const result = await runMeasured(kind, 1);
      expect(result.awardResult?.txHash).toHaveLength(64);
      expect(result.removal?.transactions.length).toBeGreaterThan(0);
    },
    900_000,
  );
  it.each(["replayAsset", "outputAsset", "mintAsset"] as const)(
    "refuses a forged %s successor on an honest block",
    async (kind) => {
      await expect(
        runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "valueAndMint",
            disputedValueKind: kind,
            assetCount: 1,
            dishonestChallenger: true,
          }),
        ),
      ).rejects.toThrow(/semantic-resolution/);
    },
    900_000,
  );
  it.each(["replayAsset", "outputAsset", "mintAsset"] as const)(
    "proves dense %s asset frontiers",
    async (kind) => {
      const result = await runMeasured(kind, 1304);
      expect(result.awardResult?.txHash).toHaveLength(64);
      expect(result.removal?.transactions.length).toBeGreaterThan(0);
    },
    900_000,
  );
  it.each(["replayAsset", "outputAsset", "mintAsset"] as const)(
    "proves maximum 14-sibling and 16-step %s witnesses",
    async (kind) => {
      const result = await runMeasured(kind, 1, true);
      expect(result.awardResult?.txHash).toHaveLength(64);
      expect(result.removal?.transactions.length).toBeGreaterThan(0);
    },
    900_000,
  );
});
