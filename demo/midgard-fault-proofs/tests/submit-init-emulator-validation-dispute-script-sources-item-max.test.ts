import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  buildMidgardRedeemerItemProofTrace,
  encodeCbor,
  encodeMidgardFieldPreimageForField,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core";
import { FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX } from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { afterAll, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { redeemerItemExecutor } from "../src/redeemer-item-plan.js";
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
    category: "validationTraceDispute/ScriptSources item executor maximum",
    blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
    compilerVersion: JSON.parse(bytes.toString()).preamble.compiler.version,
    measurements: rows,
  };
  writeFileSync(
    "/tmp/nip-script-sources-item-max-measurements.json",
    JSON.stringify(
      { diagnosticOnly: true, ...evidence },
      (_, v: unknown) => (typeof v === "bigint" ? v.toString() : v),
      2,
    ),
  );
  expect(completed).toBe(20);
  for (const row of rows) {
    expect(row.memoryUnits, row.name).toBeLessThanOrEqual(13_200_000n);
    expect(row.cpuUnits, row.name).toBeLessThanOrEqual(8_000_000_000n);
  }
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/validation-trace-script-sources-item-max-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger(evidence),
  );
});

const redeemerFieldLength = (redeemerCbor: Uint8Array) =>
  encodeMidgardFieldPreimageForField({
    fieldIndex: 8,
    items: [
      {
        purpose: "Spend",
        index: 0n,
        redeemerCbor,
        executionUnits: { memory: 1_000_000_000n, steps: 1_000_000_000n },
      },
    ],
  }).length;

// One redeemer whose field preimage is exactly the 32,768-byte consensus
// maximum and whose Data reaches every one of the seventeen shared item
// executor arms. Every chunk-reading arm therefore runs against maximum-width
// bounded-item chunk proofs -- the dominant cost of an item-route step -- and
// the structural arms run against real list, map and large-constructor frames.
// The trailing canonical byte string absorbs the remaining width without
// adding traversal breadth.
const maximumItemRedeemer = (): Uint8Array => {
  for (let padding = 30_000; padding < 32_800; padding++) {
    const candidate = Buffer.concat([
      Buffer.from([0x9f]),
      Buffer.from("9f0001ff", "hex"),
      Buffer.from("a200010203", "hex"),
      Buffer.from("d8668218809f01ff", "hex"),
      Buffer.from(Data.to("ab".repeat(padding)), "hex"),
      Buffer.from([0xff]),
    ]);
    if (redeemerFieldLength(candidate) === 32_768) return candidate;
  }
  throw new Error("Could not construct an exact 32,768-byte redeemer field");
};

const SHARED_ITEM_EXECUTOR_ARMS = Array.from({ length: 17 }, (_, i) => i);

const build =
  (scriptSourcesItemExecutor: number, dishonestChallenger = false) =>
  ({ operatorVkey, now }: Parameters<Parameters<typeof runScenario>[0]>[0]) =>
    buildForgedOperatorSuccessorValidationDisputeFixture({
      operatorVkey,
      now,
      disputedPhase: "scriptSources",
      scriptSourcesSemanticIndex: 28,
      scriptSourcesItemExecutor,
      worstCaseWitness: true,
      plutusSelection: true,
      redeemerDataCbor: maximumItemRedeemer(),
      dishonestChallenger,
    });

it("pins the maximum item redeemer at the exact consensus field width", () => {
  expect(redeemerFieldLength(maximumItemRedeemer())).toBe(
    MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  );
});

it("reaches every shared item executor arm from the maximum redeemer", () => {
  const trace = buildMidgardRedeemerItemProofTrace({
    itemIndex: 0,
    itemCount: 1,
    itemBytes: encodeCbor([
      0n,
      0n,
      Buffer.from(maximumItemRedeemer()),
      [10n, 20n],
    ]),
    mode: 1,
  });
  const reached = new Set(
    trace.steps.map(
      (step) => redeemerItemExecutor(step.control, step.witness).index,
    ),
  );
  expect([...reached].sort((a, b) => a - b)).toStrictEqual(
    SHARED_ITEM_EXECUTOR_ARMS,
  );
});

it("refuses the field width one byte above the item-route bound", () => {
  const owner = Buffer.alloc(28, 0x11);
  const txId = Buffer.alloc(32, 0x22);
  expect(() =>
    planMidgardFieldCarriage({
      owner,
      txId,
      fieldIndex: 8,
      preimage: Buffer.alloc(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES),
    }),
  ).not.toThrow();
  expect(() =>
    planMidgardFieldCarriage({
      owner,
      txId,
      fieldIndex: 8,
      preimage: Buffer.alloc(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES + 1),
    }),
  ).toThrow();
});

it.each(SHARED_ITEM_EXECUTOR_ARMS)(
  "proves shared item executor %s at the maximum redeemer field",
  async (scriptSourcesItemExecutor) => {
    const result = await runForcedValidationDisputeScenario(
      build(scriptSourcesItemExecutor),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  },
  600_000,
);

it.each([2, 10, 12])(
  "refuses a forged shared item executor %s claim at the maximum redeemer field",
  async (scriptSourcesItemExecutor) => {
    await expect(
      runForcedValidationDisputeScenario(
        build(scriptSourcesItemExecutor, true),
      ),
    ).rejects.toThrow(/semantic-resolution/);
  },
  600_000,
);

it("resumes the shared item chain at the maximum redeemer field", async () => {
  const result = await runForcedValidationDisputeScenario(build(12), {
    scriptSourcesItemCheckpoint: 3,
  });
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
}, 600_000);

it("cancels and restarts the shared item chain at the maximum redeemer field", async () => {
  const cancelled = await runForcedValidationDisputeScenario(build(12), {
    scriptSourcesItemCheckpoint: 3,
    cancelScriptSourcesItem: true,
  });
  expect(cancelled.cancellation?.txHash).toHaveLength(64);
  const result = await runForcedValidationDisputeScenario(build(12));
  expect(result.awardResult?.txHash).toHaveLength(64);
}, 600_000);
