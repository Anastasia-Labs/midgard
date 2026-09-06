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
    category: "validationTraceDispute/phase-A preconditions",
    blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
    compilerVersion: JSON.parse(bytes.toString()).preamble.compiler.version,
    measurements: rows,
  };
  writeFileSync(
    "/tmp/nip-preconditions-measurements.json",
    JSON.stringify(
      { diagnosticOnly: true, ...evidence },
      (_, v: unknown) => (typeof v === "bigint" ? v.toString() : v),
      2,
    ),
  );
  expect(completed).toBe(12);
  for (const row of rows) {
    expect(row.memoryUnits, row.name).toBeLessThanOrEqual(13_200_000n);
    expect(row.cpuUnits, row.name).toBeLessThanOrEqual(8_000_000_000n);
  }
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/validation-trace-phase-a-preconditions-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger(evidence),
  );
});

it.each([0, 3])(
  "proves phase-A finalization after %i observers through removal",
  async (observerCount) => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "phaseAScriptPreconditions",
          observerCount,
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  },
  180_000,
);

it("proves an intermediate observer item through removal", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "phaseAScriptPreconditions",
        observerCount: 3,
        preconditionsItemIndex: 1,
      }),
  );
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
}, 180_000);

it.each([undefined, 1])(
  "refuses an honest preconditions successor at item %s",
  async (preconditionsItemIndex) => {
    await expect(
      runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "phaseAScriptPreconditions",
          observerCount: 3,
          preconditionsItemIndex,
          dishonestChallenger: true,
        }),
      ),
    ).rejects.toThrow(/semantic-resolution/);
  },
  180_000,
);

it("proves the last of 1092 observers using maximum certified field carriage", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now, prepareFieldCarriage }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        prepareFieldCarriage,
        disputedPhase: "phaseAScriptPreconditions",
        observerCount: 1092,
        preconditionsItemIndex: 1091,
        rejectAfterPreconditions: true,
      }),
    { phaseAObserverItemMaximum: true },
  );
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
}, 900_000);

it.each(["missingIntegrity", "untaggedObservers", "observerOrder"] as const)(
  "proves the %s preconditions rejection through removal",
  async (preconditionsRejection) => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "phaseAScriptPreconditions",
          observerCount: preconditionsRejection === "missingIntegrity" ? 0 : 3,
          preconditionsRejection,
          ...(preconditionsRejection === "observerOrder"
            ? { preconditionsItemIndex: 1 }
            : {}),
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  },
  180_000,
);

it("proves finalization after the maximum observer frontier", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "phaseAScriptPreconditions",
        observerCount: 1092,
        rejectAfterPreconditions: true,
      }),
  );
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
}, 180_000);

it.each([undefined, 1])(
  "cancels and restarts preconditions at item %s",
  async (preconditionsItemIndex) => {
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
        disputedPhase: "phaseAScriptPreconditions",
        observerCount: 3,
        preconditionsItemIndex,
      });
    const cancelled = await runForcedValidationDisputeScenario(build, {
      cancelPreparedSemantic: true,
    });
    expect(cancelled.cancellation?.txHash).toHaveLength(64);
    expect(cancelled.awardResult).toBeUndefined();
    const resumed = await runForcedValidationDisputeScenario(build);
    expect(resumed.awardResult?.txHash).toHaveLength(64);
    expect(resumed.removal?.transactions.length).toBeGreaterThan(0);
  },
  180_000,
);
