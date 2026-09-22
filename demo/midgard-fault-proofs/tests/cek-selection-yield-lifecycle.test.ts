import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { afterAll, describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
  VAN_ROSSEM_MAX_SIGNED_TX_BYTES,
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
    onSubmittedTransaction: (m) => {
      const name = `${shape}/transaction-${captured.length.toString()}`;
      // Every authenticated submission is gated here, on every run: the
      // dispute is only actuatable on L1 if each of its signed transactions
      // fits the Van Rossem envelope. Recording without a criterion (a
      // ledger written only under MIDGARD_WRITE_FIT_LEDGER=1) let a
      // regression past this file silently.
      expect(m.completeSignedBytes, `${name} signed bytes`).toBeLessThan(
        VAN_ROSSEM_MAX_SIGNED_TX_BYTES,
      );
      expect(m.executionMemory, `${name} execution memory`).toBeLessThan(
        VAN_ROSSEM_MAX_MEMORY_UNITS,
      );
      expect(m.executionSteps, `${name} execution steps`).toBeLessThan(
        VAN_ROSSEM_MAX_CPU_UNITS,
      );
      captured.push({
        name,
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

type DisputeScenarioResult = Awaited<ReturnType<typeof runScenario>>;

/**
 * The authenticated end state of a won dispute: the fraud proof the award
 * minted carries the computation-thread identity `init` opened against this
 * exact fraudulent header, the removal spends that same proof, and the
 * challenged block is really gone from the state queue. Asserting only
 * `txHash.toHaveLength(64)` and `transactions.length > 0` accepts any
 * transaction at all, including one that removed a different block.
 */
const expectAuthenticatedRemoval = async (
  result: DisputeScenarioResult,
): Promise<void> => {
  if (
    !("removal" in result) ||
    result.removal === undefined ||
    !("awardResult" in result) ||
    result.awardResult === undefined ||
    !("setup" in result)
  ) {
    throw new Error("dispute scenario did not reach authenticated removal");
  }
  const { setup, initResult, awardResult, removal, contracts } = result;
  expect(initResult.fraudCategoryName).toBe("validationTraceDispute");
  expect(initResult.fraudulentHeaderHash).toBe(setup.headerHash);
  expect(awardResult.fraudProofUnit.slice(56)).toBe(
    initResult.computationThreadAssetName,
  );
  expect(awardResult.fraudProofUnit.slice(0, 56)).not.toBe(
    initResult.computationThreadPolicyId,
  );
  expect(removal.fraudCategory).toBe("validationTraceDispute");
  expect(removal.fraudCategoryId).toBe(initResult.fraudCategoryId);
  expect(removal.fraudulentHeaderHash).toBe(setup.headerHash);
  expect(removal.fraudProofOutRef).toBe(awardResult.fraudProofOutRef);
  expect(removal.fraudProver).toBe(initResult.fraudProver);
  expect(removal.transactions.map((tx) => tx.kind)).toContain("remove-target");
  await expect(
    result.challengerLucid.utxosAtWithUnit(
      contracts.stateQueue.spendingScriptAddress,
      setup.stateQueueBlockUnit,
    ),
  ).resolves.toHaveLength(0);
};

afterAll(async () => {
  // The Van Rossem envelope is gated per submission above, on every run; this
  // block only regenerates the checked-in evidence ledger.
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect(completed).toBe(6);
  const bytes = readFileSync(realBlueprintPath);
  const ledger = buildVanRossemFitLedger({
    category: "validationTraceDispute/CEK selection",
    blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
    compilerVersion: JSON.parse(bytes.toString()).preamble.compiler.version,
    measurements: rows,
  });
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/validation-trace-cek-selection-fit-ledger.json",
        import.meta.url,
      ),
    ),
    ledger,
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
    await expectAuthenticatedRemoval(result);
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
    await expectAuthenticatedRemoval(result);
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
    await expectAuthenticatedRemoval(result);
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
    await expectAuthenticatedRemoval(result);
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
    await expectAuthenticatedRemoval(result);
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
