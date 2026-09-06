import {
  buildMidgardRedeemerItemProofTrace,
  encodeCbor,
  encodeMidgardFieldPreimageForField,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { redeemerItemExecutor } from "../src/redeemer-item-plan.js";
import {
  buildForgedOperatorSuccessorValidationDisputeFixture,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

it.each([15, 28])(
  "proves ScriptSources item slot %s through permanent mint and removal",
  async (scriptSourcesSemanticIndex) => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "scriptSources",
          scriptSourcesSemanticIndex,
          plutusSelection: true,
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  },
  240_000,
);
it("resumes the shared item chain from retained JSON after an accepted checkpoint", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "scriptSources",
        scriptSourcesSemanticIndex: 28,
        plutusSelection: true,
      }),
    { scriptSourcesItemCheckpoint: 3 },
  );
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
}, 240_000);
it("cancels a shared item checkpoint and burns its computation token", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "scriptSources",
        scriptSourcesSemanticIndex: 28,
        plutusSelection: true,
      }),
    { scriptSourcesItemCheckpoint: 3, cancelScriptSourcesItem: true },
  );
  expect(result.cancellation?.txHash).toHaveLength(64);
  expect(result.awardResult).toBeUndefined();
}, 240_000);

const executorValues = new Map<number, string>();
for (const value of [
  "00",
  Data.to(-(2n ** 90n)),
  Data.to("ab".repeat(4500)),
  "9f0001ff",
  "a200010203",
  "d8799f0102ff",
  "d8668218809f01ff",
]) {
  const trace = buildMidgardRedeemerItemProofTrace({
    itemIndex: 0,
    itemCount: 1,
    itemBytes: encodeCbor([0n, 0n, Buffer.from(value, "hex"), [10n, 20n]]),
    mode: 1,
  });
  for (const step of trace.steps) {
    const { index } = redeemerItemExecutor(step.control, step.witness);
    if (!executorValues.has(index)) executorValues.set(index, value);
  }
}
it.each(Array.from({ length: 17 }, (_, i) => i))(
  "proves registered shared executor %s through mint and removal",
  async (scriptSourcesItemExecutor) => {
    expect(executorValues.size).toBe(17);
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "scriptSources",
          scriptSourcesSemanticIndex: 28,
          scriptSourcesItemExecutor,
          plutusSelection: true,
          redeemerDataCbor: Buffer.from(
            executorValues.get(scriptSourcesItemExecutor)!,
            "hex",
          ),
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  },
  240_000,
);
it.each([15, 28])(
  "refuses a forged ScriptSources item claim at slot %s",
  async (scriptSourcesSemanticIndex) => {
    await expect(
      runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "scriptSources",
          scriptSourcesSemanticIndex,
          plutusSelection: true,
          dishonestChallenger: true,
        }),
      ),
    ).rejects.toThrow(/semantic-resolution/);
  },
  240_000,
);

const maximumRedeemerData = () => {
  for (let bytes = 31700; bytes < 31800; bytes++) {
    const encoded = Data.to("ab".repeat(bytes));
    for (let padding = 0; padding < 3; padding++) {
      const data = Buffer.from(
        padding === 0 ? encoded : "9f" + encoded + "00".repeat(padding) + "ff",
        "hex",
      );
      const field = encodeMidgardFieldPreimageForField({
        fieldIndex: 8,
        items: [
          {
            purpose: "Spend",
            index: 0n,
            redeemerCbor: data,
            executionUnits: { memory: 1_000_000_000n, steps: 1_000_000_000n },
          },
        ],
      });
      if (field.length === 32768) return data;
    }
  }
  throw new Error("Could not construct exact32768-byte redeemer field");
};
it.each([15, 28])(
  "proves maximum32768-byte redeemer field at slot %s",
  async (scriptSourcesSemanticIndex) => {
    const redeemerDataCbor = maximumRedeemerData();
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now, prepareFieldCarriage }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          prepareFieldCarriage,
          disputedPhase: "scriptSources",
          scriptSourcesSemanticIndex,
          plutusSelection: true,
          redeemerDataCbor,
        }),
      { scriptSourcesItemMaximum: scriptSourcesSemanticIndex === 15 },
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  },
  600_000,
);
