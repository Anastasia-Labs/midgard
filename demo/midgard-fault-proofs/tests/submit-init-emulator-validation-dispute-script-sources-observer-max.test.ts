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
    category: "validationTraceDispute/ScriptSources observer maximum",
    blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
    compilerVersion: JSON.parse(bytes.toString()).preamble.compiler.version,
    measurements: rows,
  };
  writeFileSync(
    "/tmp/nip-script-sources-observer-max-measurements.json",
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
        "../../../docs/fault-proofs/size-plans/validation-trace-script-sources-observer-max-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger(evidence),
  );
});

const build =
  (scriptSourcesItemIndex: number, dishonestChallenger = false) =>
  ({
    operatorVkey,
    now,
    prepareFieldCarriage,
  }: Parameters<Parameters<typeof runScenario>[0]>[0]) =>
    buildForgedOperatorSuccessorValidationDisputeFixture({
      operatorVkey,
      now,
      prepareFieldCarriage,
      disputedPhase: "scriptSources",
      scriptSourcesSemanticIndex: 25,
      scriptSourcesItemIndex,
      observerCount: 1092,
      scriptSourcesRejection: "missingObserver",
      dishonestChallenger,
    });
it.each([0, 1091])(
  "proves observer%d of1092 with certified32763-byte carriage",
  async (index) => {
    const result = await runForcedValidationDisputeScenario(build(index), {
      phaseAObserverItemMaximum: true,
    });
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  },
  600000,
);
it.each([0, 1091])(
  "refuses forged observer%d successor at maximum carriage",
  async (index) => {
    await expect(
      runForcedValidationDisputeScenario(build(index, true), {
        phaseAObserverItemMaximum: true,
      }),
    ).rejects.toThrow(/semantic-resolution/);
  },
  600000,
);
it.each([0, 1091])(
  "cancels and restarts observer%d at maximum carriage",
  async (index) => {
    const cancelled = await runForcedValidationDisputeScenario(build(index), {
      phaseAObserverItemMaximum: true,
      cancelPreparedSemantic: true,
    });
    expect(cancelled.cancellation?.txHash).toHaveLength(64);
    const result = await runForcedValidationDisputeScenario(build(index), {
      phaseAObserverItemMaximum: true,
    });
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  },
  600000,
);
