/**
 * `missingScriptSource` at the consensus maximum observer shape, from a
 * genuine Init: the required-observer field holds as many observers as its
 * byte cap admits, and the script-witness and redeemer fields are filled to
 * their caps. The observer at the last coordinate has no source, so the
 * purpose selection and the source lookup walk their whole fields. Committed
 * as accepted it convicts; the same transaction committed with the node's
 * own forced verdict, which rejects it before its purposes, is refused at
 * the terminal after the same observed walk.
 */
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  MissingScriptSourceHonestLeaf,
  requireConvictingMissingScriptSourceProof,
} from "../src/missing-script-source/index.js";
import {
  buildMissingRedeemerSubject,
  machineForcedVerdict,
  type MissingRedeemerSubjectShape,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  driveMissingScriptSourceThread,
  makeMissingScriptSourceHarness,
  publishMissingScriptSourceRoles,
  type StepMeasurement,
  stepTable,
} from "./support/missing-script-source-emulator.js";
import {
  convictAndCheck,
  expectTerminalRefusal,
} from "./support/missing-script-source-emulator.rows.js";

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

/** Every observer has a redeemer; every observer but the one at the last
 * coordinate carries its script as a witness. */
const shapeOf = ({
  count,
  scriptBytes,
  dataBytes,
}: {
  readonly count: number;
  readonly scriptBytes: number;
  readonly dataBytes: number;
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
      witness: order[position] !== last,
      redeemer: true,
    })),
    redeemerData: () => Buffer.from(Data.to("ab".repeat(dataBytes)), "hex"),
  };
};

/** Observer count at the field-7 cap, then the largest uniform script and
 * redeemer-data sizes that keep fields 6 and 8 within their caps. */
const maximumShape = () => {
  const count = largest(
    1,
    (value) =>
      fieldBytes(shapeOf({ count: value, scriptBytes: 2, dataBytes: 0 }))
        .observers <= FIELD_CAP,
  );
  const scriptBytes = largest(
    2,
    (value) =>
      fieldBytes(shapeOf({ count, scriptBytes: value, dataBytes: 0 }))
        .witnesses <= FIELD_CAP,
  );
  const dataBytes = largest(
    0,
    (value) =>
      fieldBytes(shapeOf({ count, scriptBytes, dataBytes: value })).redeemers <=
      FIELD_CAP,
  );
  return { count, scriptBytes, dataBytes };
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

describe("missing-script-source consensus-maximum observer shape", () => {
  // The cap search builds a full transaction per step.
  it("fills every observer-route field to its cap", () => {
    const { count, scriptBytes, dataBytes } = maximumShape();
    const at = fieldBytes(shapeOf({ count, scriptBytes, dataBytes }));
    expect(at.observers).toBeLessThanOrEqual(FIELD_CAP);
    expect(at.witnesses).toBeLessThanOrEqual(FIELD_CAP);
    expect(at.redeemers).toBeLessThanOrEqual(FIELD_CAP);
    expect(
      fieldBytes(shapeOf({ count: count + 1, scriptBytes: 2, dataBytes: 0 }))
        .observers,
    ).toBeGreaterThan(FIELD_CAP);
    expect(
      fieldBytes(shapeOf({ count, scriptBytes: scriptBytes + 1, dataBytes }))
        .witnesses,
    ).toBeGreaterThan(FIELD_CAP);
    expect(
      fieldBytes(shapeOf({ count, scriptBytes, dataBytes: dataBytes + 1 }))
        .redeemers,
    ).toBeGreaterThan(FIELD_CAP);
  }, 20_000);

  it("convicts an accepted transaction whose last observer has no source", async () => {
    const harness = await makeMissingScriptSourceHarness();
    const dimensions = maximumShape();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: shapeOf(dimensions),
      source: { kind: "normal" },
    });
    const { run } = await convictAndCheck(harness, scenario, "max-shape");
    expect(
      run.prepared.final.context.observed?.authenticated.selection.coordinate,
    ).toStrictEqual({ kind: 2n, index: BigInt(dimensions.count - 1) });
    console.log(
      "MSS max-shape conviction",
      JSON.stringify({ dimensions, ...summary(run.steps) }),
    );
  }, 3_600_000);

  it("refuses the same transaction under the node's own forced verdict", async () => {
    const dimensions = maximumShape();
    const shape = shapeOf(dimensions);
    const verdict = await machineForcedVerdict(shape);
    // The subject spends nothing: the node rejects it before its purposes.
    expect(verdict).toStrictEqual({
      ForcedTxInvalid: { reason: "EmptyInputs" },
    });
    const harness = await makeMissingScriptSourceHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape,
      source: { kind: "forced", verdict },
    });
    await expect(
      requireConvictingMissingScriptSourceProof({
        material: scenario.material,
        contracts: harness.missingScriptSource,
        prover: harness.proverSigner.paymentKeyHash,
      }),
    ).rejects.toThrow(MissingScriptSourceHonestLeaf);
    const references = await publishMissingScriptSourceRoles(harness);
    // The prover finds no route; the observed route is forced through the
    // last observer's missing source to show the terminal refuse it.
    const last = { kind: 2n, index: BigInt(dimensions.count - 1) };
    const run = await driveMissingScriptSourceThread({
      harness,
      scenario,
      references,
      observed: last,
    });
    expect(run.prepared.final.claim).toBe("NotRequired");
    expect(
      run.prepared.final.context.observed?.authenticated.selection.coordinate,
    ).toStrictEqual(last);
    console.log(
      "MSS max-shape honest",
      JSON.stringify({ dimensions, ...summary(run.steps) }),
    );
    await expectTerminalRefusal(harness, run);
  }, 3_600_000);
});
