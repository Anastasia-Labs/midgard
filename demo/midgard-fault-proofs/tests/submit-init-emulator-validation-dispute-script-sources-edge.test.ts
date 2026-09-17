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
    category: "validationTraceDispute/ScriptSources edge",
    blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
    compilerVersion: JSON.parse(bytes.toString()).preamble.compiler.version,
    measurements: rows,
  };
  writeFileSync(
    "/tmp/nip-script-sources-edge-measurements.json",
    JSON.stringify(
      { diagnosticOnly: true, ...evidence },
      (_, v: unknown) => (typeof v === "bigint" ? v.toString() : v),
      2,
    ),
  );
  expect(completed).toBe(9);
  for (const row of rows) {
    expect(row.memoryUnits, row.name).toBeLessThanOrEqual(13_200_000n);
    expect(row.cpuUnits, row.name).toBeLessThanOrEqual(8_000_000_000n);
  }
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/validation-trace-script-sources-edge-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger(evidence),
  );
});

const cases = [
  {
    scriptSourcesSemanticIndex: 20,
    scriptSourcesRejection: "missingRedeemer" as const,
    plutusSelection: true,
  },
  {
    scriptSourcesSemanticIndex: 26,
    scriptSourcesRejection: "missingReceive" as const,
  },
  {
    scriptSourcesSemanticIndex: 21,
    scriptSourcesRejection: "unusedRedeemer" as const,
    scriptSourcesDescriptorAction: "tail" as const,
    plutusSelection: true,
  },
] as const;
it.each(cases)(
  "proves $scriptSourcesRejection at slot $scriptSourcesSemanticIndex",
  async (options) => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "scriptSources",
          ...options,
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  },
  180000,
);
it.each(cases)(
  "refuses forged $scriptSourcesRejection successor at slot $scriptSourcesSemanticIndex",
  async (options) => {
    await expect(
      runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "scriptSources",
          ...options,
          dishonestChallenger: true,
        }),
      ),
    ).rejects.toThrow(/semantic-resolution/);
  },
  180000,
);

it.each(cases)(
  "cancels and restarts $scriptSourcesRejection at slot $scriptSourcesSemanticIndex",
  async (options) => {
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
        disputedPhase: "scriptSources",
        ...options,
      });
    const cancelled = await runForcedValidationDisputeScenario(build, {
      cancelPreparedSemantic: true,
    });
    expect(cancelled.cancellation?.txHash).toHaveLength(64);
    expect(cancelled.awardResult).toBeUndefined();
    const restarted = await runForcedValidationDisputeScenario(build);
    expect(restarted.awardResult?.txHash).toHaveLength(64);
    expect(restarted.removal?.transactions.length).toBeGreaterThan(0);
  },
  180000,
);
