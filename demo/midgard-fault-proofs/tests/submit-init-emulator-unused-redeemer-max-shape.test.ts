/**
 * `unusedRedeemer` at the consensus maximum redeemer shape, from a genuine
 * Init: the redeemer field holds as many redeemers as its byte cap admits,
 * each a distinct Reward pointer with the smallest redeemer data, and none
 * names a purpose. The proof walks to the last redeemer, so grammar
 * certification and the pointer walk cover the whole field. Committed as
 * accepted it convicts; the same transaction committed with the node's own
 * forced verdict, which rejects it before the unused audit, is refused at
 * the terminal after the same walk.
 */
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  requireConvictingUnusedRedeemerProof,
  UnusedRedeemerHonestLeaf,
} from "../src/unused-redeemer/index.js";
import {
  buildMissingRedeemerSubject,
  machineForcedVerdict,
  type MissingRedeemerSubjectShape,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  driveUnusedRedeemerThread,
  makeUnusedRedeemerHarness,
  publishUnusedRedeemerRoles,
  type StepMeasurement,
  stepTable,
} from "./support/unused-redeemer-emulator.js";
import {
  convictAndCheck,
  expectTerminalRefusal,
} from "./support/unused-redeemer-emulator.rows.js";

const FIELD_CAP = MIDGARD_CONSENSUS_LIMITS.maxTransactionAggregateFieldBytes;

const DATA = Buffer.from(Data.to(0n), "hex");

/** `count` Reward redeemers at indices 0..count-1, no observer. */
const shapeOf = (count: number): MissingRedeemerSubjectShape => ({
  observers: [],
  paddingRedeemers: Array.from({ length: count }, () => DATA),
});

const redeemerBytes = (count: number) =>
  buildMissingRedeemerSubject(shapeOf(count)).nativeTx.witnessSet
    .redeemerTxWitsPreimageCbor.length;

/** The largest redeemer count whose field fits the cap. */
const maximumCount = (): number => {
  let low = 1;
  let high = FIELD_CAP;
  while (low < high) {
    const middle = Math.ceil((low + high) / 2);
    if (redeemerBytes(middle) <= FIELD_CAP) low = middle;
    else high = middle - 1;
  }
  return low;
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

describe("unused-redeemer consensus-maximum redeemer shape", () => {
  it("fills the redeemer field to its cap", () => {
    const count = maximumCount();
    expect(redeemerBytes(count)).toBeLessThanOrEqual(FIELD_CAP);
    expect(redeemerBytes(count + 1)).toBeGreaterThan(FIELD_CAP);
  }, 20_000);

  it("convicts an accepted transaction through its last redeemer", async () => {
    const count = maximumCount();
    const harness = await makeUnusedRedeemerHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape: shapeOf(count),
      source: { kind: "normal" },
    });
    expect(scenario.material.fields[8]).toHaveLength(count);
    const { run } = await convictAndCheck(
      harness,
      scenario,
      "max-shape",
      BigInt(count - 1),
    );
    expect(run.prepared.final.claim).toStrictEqual({
      ImpossiblePurpose: { coordinate: { kind: 2n, index: BigInt(count - 1) } },
    });
    console.log(
      "UR max-shape conviction",
      JSON.stringify({ count, ...summary(run.steps) }),
    );
  }, 3_600_000);

  it("refuses the same transaction under the node's own forced verdict", async () => {
    const count = maximumCount();
    const shape = shapeOf(count);
    const verdict = await machineForcedVerdict(shape);
    // The subject spends nothing: the node rejects it before the audit.
    expect(verdict).toStrictEqual({
      ForcedTxInvalid: { reason: "EmptyInputs" },
    });
    const harness = await makeUnusedRedeemerHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape,
      source: { kind: "forced", verdict },
    });
    await expect(
      requireConvictingUnusedRedeemerProof({
        material: scenario.material,
        contracts: harness.unusedRedeemer,
        prover: harness.proverSigner.paymentKeyHash,
      }),
    ).rejects.toThrow(UnusedRedeemerHonestLeaf);
    const references = await publishUnusedRedeemerRoles(harness);
    const run = await driveUnusedRedeemerThread({
      harness,
      scenario,
      references,
      redeemerIndex: BigInt(count - 1),
    });
    console.log(
      "UR max-shape honest",
      JSON.stringify({ count, ...summary(run.steps) }),
    );
    await expectTerminalRefusal(harness, run);
  }, 3_600_000);
});
