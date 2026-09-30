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
  expectOnchainRefusal,
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

  // The transaction's one Plutus script spends and mints, and its witness
  // list holds the Mint redeemer before the Spend redeemer. Ledger order
  // puts the Spend first, so the honest fold selects the Mint (purpose
  // frontier 1, witness item 0) and then the Spend (frontier 0, item 1).
  // These cases call the unmeasured runner, so the fit ledger's measured
  // set stays the 24 shapes above.
  it.each([0, 1])(
    "proves redeemer select %s of a spend-and-mint context whose witness list is out of ledger order",
    async (cekRedeemerSelectOrdinal) => {
      const result = await runScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekPlutusMint: true,
          cekContextStage: 9,
          cekRedeemerSelectOrdinal,
        }),
      );
      expect(result.awardResult?.txHash).toHaveLength(64);
      expect(result.removal?.transactions.length).toBeGreaterThan(0);
    },
    900_000,
  );

  // Negative polarity: after the honest Mint select has lowered the purpose
  // bound to frontier 1, the challenger selects the Mint again instead of
  // the Spend. Its redeemer item and successor are genuine, but the
  // execution leaf at frontier 0 names the Spend, so select-authenticate
  // refuses it.
  it("refuses a redeemer select at the purpose bound against an honest block", async () => {
    const refusal = await expectOnchainRefusal(() =>
      runScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekPlutusMint: true,
          cekContextStage: 9,
          cekRedeemerSelectOrdinal: 1,
          cekRedeemerSelectDonorOrdinal: 0,
        }),
      ),
    );
    expect(refusal).toMatch(/redeemerSelectAuthenticate transaction failed/);
  }, 900_000);

  // A select could name any purpose below the bound, so from the first
  // state the challenger could skip the Mint and select the Spend, reach a
  // successor the honest block does not contain, and win against it. The
  // execution leaf at `purpose_bound - 1` names the Mint, so there is one
  // successor and this select is refused.
  it("refuses a redeemer select that skips a purpose against an honest block", async () => {
    const refusal = await expectOnchainRefusal(() =>
      runScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekPlutusMint: true,
          cekContextStage: 9,
          cekRedeemerSelectOrdinal: 0,
          cekRedeemerSelectDonorOrdinal: 1,
        }),
      ),
    );
    expect(refusal).toMatch(/redeemerSelectAuthenticate transaction failed/);
  }, 900_000);

  // The native mint policy sits above the Plutus spend in ledger order, so
  // the honest fold first skips the mint's native execution leaf and then
  // selects the spend. The skip settles directly from select-authenticate.
  it("proves the redeemer skip over a native execution above a Plutus spend", async () => {
    const result = await runScenario(({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "cek",
        plutusSelection: true,
        cekSelection: true,
        cekContextStage: 9,
        cekRedeemerSkipOrdinal: 0,
      }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  }, 900_000);

  // The honest select at ordinal 0 is proved above; a skip of the same
  // purpose from the same state names a native execution leaf the frontier
  // does not hold.
  it("refuses a redeemer skip in place of an honest select", async () => {
    const refusal = await expectOnchainRefusal(() =>
      runScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekPlutusMint: true,
          cekContextStage: 9,
          cekRedeemerSelectOrdinal: 0,
          cekRedeemerSkipForgery: true,
        }),
      ),
    );
    expect(refusal).toMatch(/redeemerSelectAuthenticate transaction failed/);
  }, 900_000);

  // At the skip point the challenger selects the Plutus spend instead: the
  // execution leaf there is the native mint's, so the select is refused.
  it("refuses a redeemer select in place of an honest skip", async () => {
    const refusal = await expectOnchainRefusal(() =>
      runScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "cek",
          plutusSelection: true,
          cekSelection: true,
          cekContextStage: 9,
          cekRedeemerSkipOrdinal: 0,
          cekRedeemerSelectDonorOrdinal: 0,
        }),
      ),
    );
    expect(refusal).toMatch(/redeemerSelectAuthenticate transaction failed/);
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
