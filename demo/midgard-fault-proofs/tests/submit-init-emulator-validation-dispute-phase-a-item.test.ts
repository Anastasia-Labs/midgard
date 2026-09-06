import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { afterAll, expect, it } from "vitest";

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
  const shape = expect.getState().currentTestName!;
  const captured: VanRossemFitMeasurement[] = [];
  const result = await runScenario(fixture, {
    ...options,
    onSubmittedTransaction: (m, cbor) => {
      const body = CML.Transaction.from_cbor_hex(cbor).body();
      const outputs = body.outputs();
      let publication = m.executionMemory === 0n && m.executionSteps === 0n;
      for (let i = 0; i < outputs.len(); i++)
        publication ||= outputs.get(i).script_ref() !== undefined;
      const mint = body.mint();
      if (mint !== undefined) {
        const policies = mint.keys();
        const name = CML.AssetName.from_hex(
          FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
        );
        for (let i = 0; i < policies.len(); i++)
          publication ||= (mint.get(policies.get(i), name) ?? 0n) > 0n;
      }
      captured.push({
        name: `${shape}/attempt-${completed}/transaction-${captured.length}`,
        maximumShape: shape,
        kind: publication ? "publication" : "lifecycle",
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
  const bytes = readFileSync(realBlueprintPath);
  const evidence = {
    category: "validationTraceDispute/phase-A native item",
    blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
    compilerVersion: JSON.parse(bytes.toString()).preamble.compiler.version,
    measurements: rows,
  };
  writeFileSync(
    "/tmp/nip-phase-a-item-measurements.json",
    JSON.stringify(
      { diagnosticOnly: true, ...evidence },
      (_, v: unknown) => (typeof v === "bigint" ? v.toString() : v),
      2,
    ),
  );
  expect(completed).toBe(6);
  for (const row of rows) {
    expect(row.memoryUnits, row.name).toBeLessThanOrEqual(13_200_000n);
    expect(row.cpuUnits, row.name).toBeLessThanOrEqual(8_000_000_000n);
  }
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/validation-trace-phase-a-native-item-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger(evidence),
  );
});

it.each(["native", "foreign"] as const)(
  "proves the %s phase-A item successor through permanent proof and removal",
  async (kind) => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "phaseANativeScripts",
          plutusSelection: kind === "foreign",
        }),
      { phaseANativeItemYieldKind: kind },
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  },
  180_000,
);

it.each(["native", "foreign"] as const)(
  "refuses a forged %s successor against an honest trace",
  async (kind) => {
    await expect(
      runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "phaseANativeScripts",
            plutusSelection: kind === "foreign",
            dishonestChallenger: true,
          }),
        { phaseANativeItemYieldKind: kind },
      ),
    ).rejects.toThrow(/semantic-resolution/);
  },
  180_000,
);

it("resumes the exact late native continuation through permanent proof and removal", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "phaseANativeScripts",
        lateNativeItem: true,
      }),
    { phaseANativeItemYieldKind: "native" },
  );
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
}, 180_000);

it.each(["native", "foreign"] as const)(
  "refuses the opposite yield for a %s item",
  async (kind) => {
    await expect(
      runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "phaseANativeScripts",
            plutusSelection: kind === "foreign",
          }),
        { phaseANativeItemYieldKind: kind === "native" ? "foreign" : "native" },
      ),
    ).rejects.toThrow(/semantic-resolution/);
  },
  180_000,
);

it("proves the maximum 32KiB certified native item through removal", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now, prepareFieldCarriage }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        prepareFieldCarriage,
        disputedPhase: "phaseANativeScripts",
        nativeItemWidth: 10918,
      }),
    { phaseANativeItemYieldKind: "native", phaseANativeItemMaximum: true },
  );
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
}, 900_000);

it("cancels the prepared native item and completes a fresh out-ref-driven attempt", async () => {
  const build = ({
    operatorVkey,
    now,
  }: {
    operatorVkey: string;
    now: number;
  }) =>
    buildForgedOperatorSuccessorValidationDisputeFixture({
      operatorVkey,
      now,
      disputedPhase: "phaseANativeScripts",
    });
  const cancelled = await runForcedValidationDisputeScenario(build, {
    phaseANativeItemYieldKind: "native",
    cancelPreparedSemantic: true,
  });
  expect(cancelled.cancellation?.txHash).toHaveLength(64);
  expect(cancelled.awardResult).toBeUndefined();
  const resumed = await runForcedValidationDisputeScenario(build, {
    phaseANativeItemYieldKind: "native",
  });
  expect(resumed.awardResult?.txHash).toHaveLength(64);
}, 180_000);
