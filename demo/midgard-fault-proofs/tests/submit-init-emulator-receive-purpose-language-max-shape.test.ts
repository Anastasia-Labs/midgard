/**
 * `receivePurposeLanguage` at the consensus maximum receive shape, from a
 * genuine Init: the output field holds as many receive outputs as its byte
 * cap admits, each paying its own PlutusV3 script's protected address, and
 * the script-witness and redeemer fields are filled to their caps. The
 * observed receive purpose is the last ordinal, so the receive route, the
 * source lookup and the claimed redeemer scan walk their whole fields.
 * Committed as accepted it convicts. The refusal polarity is fixture-only:
 * the same transaction committed with the node's own forced verdict, which
 * rejects it before its purposes, is refused at the terminal after the same
 * observed walk, but the forced admission screen never admits it (it spends
 * nothing and carries redeemers and Plutus witnesses), and no admissible
 * forced transaction fills the witness and redeemer fields.
 */
import {
  MIDGARD_CONSENSUS_LIMITS,
  screenMidgardForcedTxAdmission,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  ReceivePurposeLanguageHonestLeaf,
  requireConvictingReceivePurposeLanguageProof,
} from "../src/receive-purpose-language/index.js";
import {
  buildMissingRedeemerSubject,
  machineForcedVerdict,
  type MissingRedeemerSubjectShape,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  driveReceivePurposeLanguageThread,
  makeReceivePurposeLanguageHarness,
  publishReceivePurposeLanguageRoles,
  receiveCoordinate,
  type StepMeasurement,
  stepTable,
} from "./support/receive-purpose-language-emulator.js";
import {
  convictAndCheck,
  expectTerminalRefusal,
} from "./support/receive-purpose-language-emulator.rows.js";

const FIELD_CAP = MIDGARD_CONSENSUS_LIMITS.maxTransactionAggregateFieldBytes;

const fieldBytes = (shape: MissingRedeemerSubjectShape) => {
  const { nativeTx } = buildMissingRedeemerSubject(shape);
  return {
    outputs: nativeTx.body.outputsPreimageCbor.length,
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

/** Every receive output pays its own PlutusV3 script, carried as a witness
 * and reached by a redeemer. */
const shapeOf = ({
  count,
  scriptBytes,
  dataBytes,
}: {
  readonly count: number;
  readonly scriptBytes: number;
  readonly dataBytes: number;
}): MissingRedeemerSubjectShape => ({
  observers: [],
  receives: Array.from({ length: count }, (_, seed) => ({
    script: plutusScript(seed + 1, scriptBytes),
    redeemer: true,
  })),
  redeemerData: () => Buffer.from(Data.to("ab".repeat(dataBytes)), "hex"),
});

/** Receive count at the field-2 cap, then the largest uniform script and
 * redeemer-data sizes that keep fields 6 and 8 within their caps. */
const maximumShape = () => {
  const count = largest(
    1,
    (value) =>
      fieldBytes(shapeOf({ count: value, scriptBytes: 2, dataBytes: 0 }))
        .outputs <= FIELD_CAP,
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

describe("receive-purpose-language consensus-maximum receive shape", () => {
  // The cap search builds a full transaction per step.
  it("fills every receive-route field to its cap", () => {
    const { count, scriptBytes, dataBytes } = maximumShape();
    const at = fieldBytes(shapeOf({ count, scriptBytes, dataBytes }));
    expect(at.outputs).toBeLessThanOrEqual(FIELD_CAP);
    expect(at.witnesses).toBeLessThanOrEqual(FIELD_CAP);
    expect(at.redeemers).toBeLessThanOrEqual(FIELD_CAP);
    expect(
      fieldBytes(shapeOf({ count: count + 1, scriptBytes: 2, dataBytes: 0 }))
        .outputs,
    ).toBeGreaterThan(FIELD_CAP);
    expect(
      fieldBytes(shapeOf({ count, scriptBytes: scriptBytes + 1, dataBytes }))
        .witnesses,
    ).toBeGreaterThan(FIELD_CAP);
    expect(
      fieldBytes(shapeOf({ count, scriptBytes, dataBytes: dataBytes + 1 }))
        .redeemers,
    ).toBeGreaterThan(FIELD_CAP);
  }, 60_000);

  it("convicts an accepted transaction at its last receive purpose", async () => {
    const harness = await makeReceivePurposeLanguageHarness();
    const dimensions = maximumShape();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: shapeOf(dimensions),
      source: { kind: "normal" },
    });
    const last = receiveCoordinate(BigInt(dimensions.count - 1));
    const { run } = await convictAndCheck(harness, scenario, "max-shape", last);
    expect(run.prepared.final.claim).toHaveProperty("RedeemerPresent");
    console.log(
      "RPL max-shape conviction",
      JSON.stringify({ dimensions, ...summary(run.steps) }),
    );
  }, 3_600_000);

  it("fixture-only: refuses the same transaction under the node's own forced verdict, which the admission screen never admits", async () => {
    const dimensions = maximumShape();
    const shape = shapeOf(dimensions);
    const verdict = await machineForcedVerdict(shape);
    // The subject spends nothing: the node rejects it before its purposes.
    expect(verdict).toStrictEqual({
      ForcedTxInvalid: { reason: "EmptyInputs" },
    });
    expect(
      screenMidgardForcedTxAdmission(
        buildMissingRedeemerSubject(shape).nativeTx,
      ),
    ).not.toBeNull();
    const harness = await makeReceivePurposeLanguageHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape,
      source: { kind: "forced", verdict },
    });
    await expect(
      requireConvictingReceivePurposeLanguageProof({
        material: scenario.material,
        contracts: harness.receivePurposeLanguage,
        prover: harness.proverSigner.paymentKeyHash,
      }),
    ).rejects.toThrow(ReceivePurposeLanguageHonestLeaf);
    const references = await publishReceivePurposeLanguageRoles(harness);
    const last = receiveCoordinate(BigInt(dimensions.count - 1));
    const run = await driveReceivePurposeLanguageThread({
      harness,
      scenario,
      references,
      observed: last,
    });
    expect(run.prepared.final.claim).toHaveProperty("RedeemerPresent");
    console.log(
      "RPL max-shape honest",
      JSON.stringify({ dimensions, ...summary(run.steps) }),
    );
    await expectTerminalRefusal(harness, run);
  }, 3_600_000);
});
