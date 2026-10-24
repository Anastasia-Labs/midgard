/**
 * `missingRedeemer` at the consensus maximum observer shape, from a genuine
 * Init: the required-observer field holds as many observers as its byte cap
 * admits, and the script-witness and redeemer fields are filled to their
 * caps. The missing redeemer sits at the last observer coordinate, so every
 * scan walks its whole field. The same shape with every redeemer present is
 * refused at the terminal.
 */
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  MissingRedeemerHonestLeaf,
  requireConvictingMissingRedeemerProof,
} from "../src/missing-redeemer/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveMissingRedeemerThread,
  makeMissingRedeemerHarness,
  MISSING_REDEEMER_STEP_BOUNDS,
  publishMissingRedeemerRoles,
  type StepMeasurement,
  stepTable,
} from "./support/missing-redeemer-emulator.js";
import {
  buildMissingRedeemerSubject,
  type MissingRedeemerSubjectShape,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

const FIELD_CAP = MIDGARD_CONSENSUS_LIMITS.maxTransactionAggregateFieldBytes;

const fieldBytes = (shape: MissingRedeemerSubjectShape) => {
  const { nativeTx } = buildMissingRedeemerSubject(shape);
  return {
    observers: nativeTx.body.requiredObserversPreimageCbor.length,
    witnesses: nativeTx.witnessSet.scriptTxWitsPreimageCbor.length,
    redeemers: nativeTx.witnessSet.redeemerTxWitsPreimageCbor.length,
  };
};

/** The largest value in [low, ∞) for which `fits` holds (`fits(low)` must). */
const largest = (low: number, fits: (value: number) => boolean): number => {
  let value = low;
  while (fits(value + 1)) value += 1;
  return value;
};

const shapeOf = ({
  count,
  scriptBytes,
  dataBytes,
  missing,
}: {
  readonly count: number;
  readonly scriptBytes: number;
  readonly dataBytes: number;
  /** Whether the observer at the last coordinate lacks its redeemer. */
  readonly missing: boolean;
}): MissingRedeemerSubjectShape => {
  const scripts = Array.from({ length: count }, (_, seed) =>
    plutusScript(seed + 1, scriptBytes),
  );
  const order = buildMissingRedeemerSubject({
    observers: scripts.map((script) => ({ script, redeemer: true })),
  }).observerIndex;
  const last = BigInt(count - 1);
  return {
    observers: scripts.map((script, position) => ({
      script,
      redeemer: !(missing && order[position] === last),
    })),
    redeemerData: () => Buffer.from(Data.to("ab".repeat(dataBytes)), "hex"),
  };
};

/** Observer count at the field-7 cap, then the largest uniform script and
 * redeemer-data sizes that keep fields 6 and 8 within their caps. */
const maximumShape = (missing: boolean) => {
  const count = largest(
    1,
    (value) =>
      fieldBytes(
        shapeOf({ count: value, scriptBytes: 2, dataBytes: 0, missing }),
      ).observers <= FIELD_CAP,
  );
  const scriptBytes = largest(
    2,
    (value) =>
      fieldBytes(shapeOf({ count, scriptBytes: value, dataBytes: 0, missing }))
        .witnesses <= FIELD_CAP,
  );
  const dataBytes = largest(
    0,
    (value) =>
      fieldBytes(shapeOf({ count, scriptBytes, dataBytes: value, missing }))
        .redeemers <= FIELD_CAP,
  );
  return { count, scriptBytes, dataBytes };
};

const expectWithinBounds = (steps: readonly StepMeasurement[]): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      MISSING_REDEEMER_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      MISSING_REDEEMER_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(MISSING_REDEEMER_STEP_BOUNDS.bytes);
  }
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
  };
};

describe("missing-redeemer consensus-maximum observer shape", () => {
  // An emulator max-shape row: the cap search builds a full transaction per
  // step. Measured alone at 4.6-6.6 s under load, so the bound is 3x the
  // slowest run.
  it("fills every observer-route field to its cap", () => {
    const { count, scriptBytes, dataBytes } = maximumShape(true);
    const at = fieldBytes(
      shapeOf({ count, scriptBytes, dataBytes, missing: true }),
    );
    expect(at.observers).toBeLessThanOrEqual(FIELD_CAP);
    expect(at.witnesses).toBeLessThanOrEqual(FIELD_CAP);
    expect(at.redeemers).toBeLessThanOrEqual(FIELD_CAP);
    expect(
      fieldBytes(
        shapeOf({
          count: count + 1,
          scriptBytes: 2,
          dataBytes: 0,
          missing: true,
        }),
      ).observers,
    ).toBeGreaterThan(FIELD_CAP);
    expect(
      fieldBytes(
        shapeOf({
          count,
          scriptBytes: scriptBytes + 1,
          dataBytes,
          missing: true,
        }),
      ).witnesses,
    ).toBeGreaterThan(FIELD_CAP);
    expect(
      fieldBytes(
        shapeOf({
          count,
          scriptBytes,
          dataBytes: dataBytes + 1,
          missing: true,
        }),
      ).redeemers,
    ).toBeGreaterThan(FIELD_CAP);
  }, 20_000);

  it("convicts an accepted transaction whose last observer has no redeemer", async () => {
    const harness = await makeMissingRedeemerHarness();
    const references = await publishMissingRedeemerRoles(harness);
    const dimensions = maximumShape(true);
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: shapeOf({ ...dimensions, missing: true }),
      source: { kind: "normal" },
    });
    const run = await driveMissingRedeemerThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(true);
    expect(run.prepared.final.context.observed?.redeemer_missing).toBe(true);
    const outcome = await run.terminal();
    console.log(
      "MR max-shape conviction",
      JSON.stringify({ dimensions, ...summary(run.steps) }),
    );
    expectWithinBounds(run.steps);
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.missingRedeemer.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
  }, 3_600_000);

  it("refuses the same shape when every observer has its redeemer", async () => {
    const harness = await makeMissingRedeemerHarness();
    const references = await publishMissingRedeemerRoles(harness);
    const dimensions = maximumShape(false);
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: shapeOf({ ...dimensions, missing: false }),
      source: { kind: "normal" },
    });
    await expect(
      requireConvictingMissingRedeemerProof({
        material: scenario.material,
        contracts: harness.missingRedeemer,
        prover: harness.proverSigner.paymentKeyHash,
      }),
    ).rejects.toThrow(MissingRedeemerHonestLeaf);
    const run = await driveMissingRedeemerThread({
      harness,
      scenario,
      references,
      observed: { kind: 2n, index: BigInt(dimensions.count - 1) },
    });
    expect(run.prepared.convicts).toBe(false);
    expect(run.prepared.final.context.observed?.redeemer_missing).toBe(false);
    console.log(
      "MR max-shape honest",
      JSON.stringify({ dimensions, ...summary(run.steps) }),
    );
    expectWithinBounds(run.steps);
    await expectOnchainRefusal(() => run.terminal(false), {
      refusedBy: "fraud_proofs/missing_redeemer/step_05",
      check: /^Validator returned false$/u,
    });
  }, 3_600_000);
});
