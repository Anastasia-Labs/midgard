/**
 * `distinctAssetAccumulationLimit` at the consensus maximum shape, from a
 * genuine Init: the spend-input, output and mint fields each hold as many
 * items as their byte cap admits, every output's value is near its own byte
 * cap, and the running asset entry count reaches the bound only at the last
 * mint asset, so the fold walks every input, output and mint policy. One
 * entry past the bound convicts; the same shape at the bound is refused at
 * the terminal. A third shape crosses in the last of as many spends as the
 * field admits, the heaviest values last, with the reference-input field at
 * its cap below every spend, so the rank places the crossing past the last
 * reference input. A fourth runs the third with the heavy spends' out-refs
 * deep in a populated ledger trie, so each resolve page opens proofs at
 * production depth, and records the page size the fit chooses.
 */
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { encodeMidgardValue } from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import {
  DistinctAssetAccumulationLimitHonestLeaf,
  requireConvictingDistinctAssetAccumulationLimitProof,
} from "../src/distinct-asset-accumulation-limit/index.js";
import { scriptDecodeLedgerLimits } from "../src/script-decode/record.js";
import { pathStarts } from "./support/deep-ledger-trie.js";
import {
  DEEP_CLUSTER,
  deepLedger,
  HEAVY_SPEND_NONCES,
} from "./support/distinct-asset-accumulation-limit-emulator.deep-ledger.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_STEP_BOUNDS,
  driveDistinctAssetAccumulationLimitThread,
  makeDistinctAssetAccumulationLimitHarness,
  publishDistinctAssetAccumulationLimitRoles,
  type StepMeasurement,
  stepTable,
} from "./support/distinct-asset-accumulation-limit-emulator.js";
import {
  buildDistinctAssetSubject,
  type DistinctAssetSubjectShape,
  ENTRY_BOUND,
  policiesValue,
  POLICY_ASSETS,
  setupDistinctAssetScenario,
} from "./support/distinct-asset-accumulation-limit-emulator.subject.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

const FIELD_CAP = MIDGARD_CONSENSUS_LIMITS.maxTransactionAggregateFieldBytes;
const VALUE_CAP = MIDGARD_CONSENSUS_LIMITS.maxOutputValueCborBytes;

/** Six full policies: the most one-byte-named policies a value holds below
 * its byte cap. */
const OUTPUT_POLICIES = Array.from({ length: 6 }, () => POLICY_ASSETS);

type Dimensions = {
  readonly inputs: number;
  readonly outputs: number;
  readonly mintPolicies: number;
};

/** `entries` mint assets over `policies` policies, as even as possible. */
const spread = (entries: number, policies: number): number[] =>
  Array.from(
    { length: policies },
    (_, index) =>
      Math.floor(entries / policies) + (index < entries % policies ? 1 : 0),
  );

/** One-entry spends and full outputs; the mint carries the entries left
 * below the bound, plus `excess`. */
const shapeOf = (
  { inputs, outputs, mintPolicies }: Dimensions,
  excess: number,
): DistinctAssetSubjectShape => {
  const mintEntries =
    ENTRY_BOUND - inputs - outputs * OUTPUT_POLICIES.length * POLICY_ASSETS;
  return {
    inputs: Array.from({ length: inputs }, () => 1),
    outputs: Array.from({ length: outputs }, () => OUTPUT_POLICIES),
    mint: spread(mintEntries + excess, mintPolicies),
  };
};

const fieldBytes = (dimensions: Dimensions) => {
  const tx = buildDistinctAssetSubject(shapeOf(dimensions, 1));
  return {
    inputs: tx.body.spendInputsPreimageCbor.length,
    outputs: tx.body.outputsPreimageCbor.length,
    mintPolicies: tx.body.mintPreimageCbor.length,
  };
};

/** Whether `dimensions`' `key` field is within its cap; a shape with no
 * entries left for the mint does not fit. */
const fits = (dimensions: Dimensions, key: keyof Dimensions): boolean => {
  const mintEntries =
    ENTRY_BOUND -
    dimensions.inputs -
    dimensions.outputs * OUTPUT_POLICIES.length * POLICY_ASSETS;
  if (mintEntries < dimensions.mintPolicies) return false;
  return fieldBytes(dimensions)[key] <= FIELD_CAP;
};

/** The largest value in [low, high) for which `fits` holds (`fits(low)`
 * must). */
const largest = (
  low: number,
  high: number,
  fitsAt: (value: number) => boolean,
): number => {
  let [ok, bad] = [low, high];
  while (bad - ok > 1) {
    const mid = Math.floor((ok + bad) / 2);
    if (fitsAt(mid)) ok = mid;
    else bad = mid;
  }
  return ok;
};

/** Spends and outputs filled to their caps, then as many mint policies as
 * the mint cap admits for the entries left. */
const maximumShape = (): Dimensions => {
  const base: Dimensions = { inputs: 1, outputs: 1, mintPolicies: 64 };
  const inputs = largest(1, ENTRY_BOUND, (value) =>
    fits({ ...base, inputs: value }, "inputs"),
  );
  const outputs = largest(1, ENTRY_BOUND, (value) =>
    fits({ ...base, outputs: value }, "outputs"),
  );
  const mintPolicies = largest(64, ENTRY_BOUND, (value) =>
    fits({ inputs, outputs, mintPolicies: value }, "mintPolicies"),
  );
  return { inputs, outputs, mintPolicies };
};

const expectWithinBounds = (steps: readonly StepMeasurement[]): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      DISTINCT_ASSET_ACCUMULATION_LIMIT_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      DISTINCT_ASSET_ACCUMULATION_LIMIT_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(DISTINCT_ASSET_ACCUMULATION_LIMIT_STEP_BOUNDS.bytes);
  }
};

/** Each page's size (its label's parenthesised count), by label prefix. */
const pages = (steps: readonly StepMeasurement[], prefix: string) => {
  const sizes = steps.flatMap(({ label }) =>
    label.startsWith(`${prefix} `) && label.endsWith(")")
      ? [Number(label.slice(label.lastIndexOf("(") + 1, -1))]
      : [],
  );
  return {
    pages: sizes.length,
    max: Math.max(...sizes),
    min: Math.min(...sizes),
  };
};

const summary = (steps: readonly StepMeasurement[]) => {
  const table = stepTable(steps);
  const max = (key: "memory" | "cpu") =>
    table.reduce(
      (best, row) => (BigInt(row[key]) > BigInt(best[key]) ? row : best),
      table[0]!,
    );
  return {
    transactions: table.length,
    maxMemory: max("memory"),
    maxCpu: max("cpu"),
    maxBytes: Math.max(...table.map((row) => row.bytes)),
    inputsTake: pages(steps, "inputs take"),
    inputsResolve: pages(steps, "inputs resolve"),
    outputs: pages(steps, "outputs"),
    mint: pages(steps, "mint"),
  };
};

/** Most reference inputs the field admits. */
const maximumReferences = (): number =>
  largest(
    1,
    ENTRY_BOUND,
    (references) =>
      buildDistinctAssetSubject({
        inputs: [1],
        references,
        outputs: [[1]],
        mint: [],
      }).body.referenceInputsPreimageCbor.length <= FIELD_CAP,
  );

/** Entries a full spent value carries: six full policies. */
const HEAVY_SPEND = OUTPUT_POLICIES.length * POLICY_ASSETS;

/** `spends` spends whose last ones carry full values, enough of them that
 * the running count crosses in the last spend; the references sort below
 * every spend. */
const inputCrossingShape = (
  spends: number,
  references: number,
): {
  readonly shape: DistinctAssetSubjectShape;
  readonly assetIndex: bigint;
} => {
  const heavy = Math.ceil((ENTRY_BOUND + 1 - spends) / (HEAVY_SPEND - 1));
  const inputs = Array.from({ length: spends }, (_, index) =>
    index < spends - heavy ? 1 : HEAVY_SPEND,
  );
  const before = inputs.slice(0, -1).reduce((sum, count) => sum + count, 0);
  expect(before).toBeLessThanOrEqual(ENTRY_BOUND);
  expect(before + HEAVY_SPEND).toBeGreaterThan(ENTRY_BOUND);
  return {
    shape: { inputs, references, outputs: [[1]], mint: [] },
    assetIndex: BigInt(ENTRY_BOUND - before),
  };
};

describe("distinct-asset-accumulation-limit consensus-maximum shape", () => {
  it("fills the spend, output and mint fields to their caps with the entries at the bound", () => {
    const dimensions = maximumShape();
    for (const excess of [0, 1]) {
      const shape = shapeOf(dimensions, excess);
      const tx = buildDistinctAssetSubject(shape);
      for (const bytes of [
        tx.body.spendInputsPreimageCbor,
        tx.body.outputsPreimageCbor,
        tx.body.mintPreimageCbor,
      ])
        expect(bytes.length).toBeLessThanOrEqual(FIELD_CAP);
      const entries =
        shape.inputs.reduce((sum, count) => sum + count, 0) +
        shape.outputs.flat().reduce((sum, count) => sum + count, 0) +
        shape.mint.reduce((sum, count) => sum + count, 0);
      expect(entries).toBe(ENTRY_BOUND + excess);
      expect(Math.max(...shape.mint)).toBeLessThanOrEqual(POLICY_ASSETS);
    }
    // Each output's value is within its own cap; one more full policy is not.
    expect(
      encodeMidgardValue(policiesValue(50_000_000n, OUTPUT_POLICIES, 0x80))
        .length,
    ).toBeLessThanOrEqual(VALUE_CAP);
    expect(
      encodeMidgardValue(
        policiesValue(50_000_000n, [...OUTPUT_POLICIES, POLICY_ASSETS], 0x80),
      ).length,
    ).toBeGreaterThan(VALUE_CAP);
    for (const key of Object.keys(dimensions) as (keyof Dimensions)[])
      expect(fits({ ...dimensions, [key]: dimensions[key] + 1 }, key)).toBe(
        false,
      );
  }, 120_000);

  it("convicts an accepted transaction whose last mint asset is one past the bound", async () => {
    const harness = await makeDistinctAssetAccumulationLimitHarness();
    const references =
      await publishDistinctAssetAccumulationLimitRoles(harness);
    const dimensions = maximumShape();
    const scenario = await setupDistinctAssetScenario({
      harness,
      shape: shapeOf(dimensions, 1),
      source: { kind: "normal" },
    });
    const run = await driveDistinctAssetAccumulationLimitThread({
      harness,
      scenario,
      references,
    });
    const mintEntries =
      ENTRY_BOUND -
      dimensions.inputs -
      dimensions.outputs * OUTPUT_POLICIES.length * POLICY_ASSETS;
    expect(run.prepared.convicts).toBe(true);
    expect(run.prepared.crossing).toStrictEqual({
      kind: "mint",
      mintIndex: BigInt(mintEntries),
    });
    const outcome = await run.terminal();
    console.log(
      "DAAL max-shape conviction",
      JSON.stringify({ dimensions, ...summary(run.steps) }),
    );
    expectWithinBounds(run.steps);
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.distinctAssetAccumulationLimit.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
  }, 3_600_000);

  it("refuses the same shape with the entries at the bound", async () => {
    const harness = await makeDistinctAssetAccumulationLimitHarness();
    const references =
      await publishDistinctAssetAccumulationLimitRoles(harness);
    const dimensions = maximumShape();
    const scenario = await setupDistinctAssetScenario({
      harness,
      shape: shapeOf(dimensions, 0),
      source: { kind: "normal" },
    });
    await expect(
      requireConvictingDistinctAssetAccumulationLimitProof({
        material: scenario.material,
        contracts: harness.distinctAssetAccumulationLimit,
        prover: harness.proverSigner.paymentKeyHash,
      }),
    ).rejects.toThrow(DistinctAssetAccumulationLimitHonestLeaf);
    const run = await driveDistinctAssetAccumulationLimitThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(false);
    expect(run.prepared.crossing).toBe(null);
    console.log(
      "DAAL max-shape honest",
      JSON.stringify({ dimensions, ...summary(run.steps) }),
    );
    expectWithinBounds(run.steps);
    await expectOnchainRefusal(() => run.terminal(false), {
      refusedBy: "fraud_proofs/distinct_asset_accumulation_limit/terminal",
      check: /^Validator returned false$/u,
    });
  }, 3_600_000);

  it("convicts a crossing in the last spend with the reference-input field at its cap", async () => {
    const harness = await makeDistinctAssetAccumulationLimitHarness();
    const references =
      await publishDistinctAssetAccumulationLimitRoles(harness);
    const { inputs: spends } = maximumShape();
    const referenceCount = maximumReferences();
    const { shape, assetIndex } = inputCrossingShape(spends, referenceCount);
    const tx = buildDistinctAssetSubject(shape);
    expect(tx.body.spendInputsPreimageCbor.length).toBeLessThanOrEqual(
      FIELD_CAP,
    );
    expect(
      buildDistinctAssetSubject({ ...shape, references: referenceCount + 1 })
        .body.referenceInputsPreimageCbor.length,
    ).toBeGreaterThan(FIELD_CAP);
    const scenario = await setupDistinctAssetScenario({
      harness,
      shape,
      source: { kind: "normal" },
    });
    const run = await driveDistinctAssetAccumulationLimitThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.crossing).toStrictEqual({
      kind: "input",
      inputIndex: BigInt(spends - 1 + referenceCount),
      assetIndex,
    });
    const outcome = await run.terminal();
    console.log(
      "DAAL max-shape input crossing",
      JSON.stringify({
        spends,
        references: referenceCount,
        heavySpends: shape.inputs.filter((count) => count > 1).length,
        ...summary(run.steps),
        rank: stepTable(run.steps).find((row) => row.label === "input rank"),
      }),
    );
    expectWithinBounds(run.steps);
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.distinctAssetAccumulationLimit.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
  }, 3_600_000);

  it("fits each inputs-resolve page with the heavy spends deep in a populated ledger", async () => {
    const harness = await makeDistinctAssetAccumulationLimitHarness();
    const references =
      await publishDistinctAssetAccumulationLimitRoles(harness);
    const { inputs: spends } = maximumShape();
    const referenceCount = maximumReferences();
    const crossing = inputCrossingShape(spends, referenceCount);
    const heavySpends = crossing.shape.inputs.filter((count) => count > 1);
    expect(heavySpends).toHaveLength(HEAVY_SPEND_NONCES.length);
    const deep = deepLedger(spends - heavySpends.length);
    for (const outRef of deep.spentOutRefs)
      expect(pathStarts(outRef, DEEP_CLUSTER)).toBe(true);
    const shape: DistinctAssetSubjectShape = { ...crossing.shape, deep };
    expect(
      buildDistinctAssetSubject(shape).body.spendInputsPreimageCbor.length,
    ).toBeLessThanOrEqual(FIELD_CAP);
    const scenario = await setupDistinctAssetScenario({
      harness,
      shape,
      source: { kind: "normal" },
    });
    const run = await driveDistinctAssetAccumulationLimitThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.crossing).toStrictEqual({
      kind: "input",
      inputIndex: BigInt(spends - 1 + referenceCount),
      assetIndex: crossing.assetIndex,
    });
    const outcome = await run.terminal();
    const resolve = run.steps.filter(({ label }) =>
      label.startsWith("inputs resolve "),
    );
    const peak = (key: "executionMemory" | "executionSteps") =>
      resolve.reduce((best, step) =>
        step.measurement[key] > best.measurement[key] ? step : best,
      );
    console.log(
      "DAAL max-shape deep-ledger input crossing",
      JSON.stringify({
        spends,
        references: referenceCount,
        heavySpends: heavySpends.length,
        cluster: DEEP_CLUSTER,
        inputsResolve: pages(run.steps, "inputs resolve"),
        peakMemory: stepTable([peak("executionMemory")])[0],
        peakCpu: stepTable([peak("executionSteps")])[0],
      }),
    );
    expectWithinBounds(run.steps);
    // The fit keeps every page inside the 20% reserve below the ledger's
    // limits.
    const limits = scriptDecodeLedgerLimits(harness.proverLucid);
    for (const { label, measurement } of resolve) {
      expect(measurement.executionMemory * 5n, label).toBeLessThanOrEqual(
        BigInt(limits.mem) * 4n,
      );
      expect(measurement.executionSteps * 5n, label).toBeLessThanOrEqual(
        BigInt(limits.steps) * 4n,
      );
    }
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.distinctAssetAccumulationLimit.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
  }, 3_600_000);
});
