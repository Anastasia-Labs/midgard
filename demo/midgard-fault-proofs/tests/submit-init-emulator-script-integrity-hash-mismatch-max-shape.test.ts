/**
 * `scriptIntegrityHashMismatch` at the consensus maximum purpose shape, from a
 * genuine Init: the spend-input, output, required-observer and mint fields
 * each hold as many purposes as their byte cap admits, with the script-witness
 * and redeemer fields within theirs. Every spend is script-locked, every
 * output pays a protected script credential, and every observer and mint
 * policy has its own source, so the walk resolves an execution for every item
 * of every purpose field. A wrong hash convicts; the same shape with the
 * expected hash is refused at the terminal.
 */
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  MIDGARD_CONSENSUS_LIMITS,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  requireConvictingScriptIntegrityHashMismatchProof,
  ScriptIntegrityHashMismatchHonestLeaf,
} from "../src/script-integrity-hash-mismatch/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  driveScriptIntegrityHashMismatchThread,
  makeScriptIntegrityHashMismatchHarness,
  publishScriptIntegrityHashMismatchRoles,
  SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_BOUNDS,
  type StepMeasurement,
  stepTable,
} from "./support/script-integrity-hash-mismatch-emulator.js";
import {
  buildIntegritySubject,
  expectedHash,
  type IntegritySubjectShape,
  nativeScript,
  plutusScript,
  setupIntegrityScenario,
  WRONG_HASH,
} from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

const FIELD_CAP = MIDGARD_CONSENSUS_LIMITS.maxTransactionAggregateFieldBytes;

type Dimensions = {
  readonly spends: number;
  readonly outputs: number;
  readonly observers: number;
  readonly mints: number;
};

/** The one Plutus V3 script every spent output is locked by, and the one
 * every output pays. */
const SPENT = plutusScript(1);
const RECEIVED = plutusScript(2);

const shapeOf = (
  { spends, outputs, observers, mints }: Dimensions,
  integrity: IntegritySubjectShape["integrity"],
): IntegritySubjectShape => ({
  spend: Array.from({ length: spends }, (_, index) => ({
    outRef: encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x41),
      outputIndex: index,
    }),
    output: encodeMidgardTxOutput({
      address: scriptAddress(SPENT),
      value: { lovelace: 5_000_000n, assets: new Map() },
    }),
  })),
  spendRedeemers: Array.from({ length: spends }, (_, index) => BigInt(index)),
  receives: Array.from({ length: outputs }, () => ({
    script: RECEIVED,
    witness: false,
    redeemer: true,
  })),
  observers: Array.from({ length: observers }, (_, index) => ({
    script: nativeScript(BigInt(1_000_000 + index)),
    redeemer: false,
  })),
  mints: Array.from({ length: mints }, (_, index) => ({
    script: nativeScript(BigInt(index)),
    redeemer: false,
  })),
  witnesses: [SPENT, RECEIVED],
  integrity,
});

const fieldBytes = (dimensions: Dimensions) => {
  const tx = buildIntegritySubject(shapeOf(dimensions, WRONG_HASH));
  return {
    spends: tx.body.spendInputsPreimageCbor.length,
    outputs: tx.body.outputsPreimageCbor.length,
    observers: tx.body.requiredObserversPreimageCbor.length,
    mints: tx.body.mintPreimageCbor.length,
    witnesses: tx.witnessSet.scriptTxWitsPreimageCbor.length,
    redeemers: tx.witnessSet.redeemerTxWitsPreimageCbor.length,
  };
};

/** The largest value in [low, high) for which `fits` holds (`fits(low)`
 * must). */
const largest = (
  low: number,
  high: number,
  fits: (value: number) => boolean,
): number => {
  let [ok, bad] = [low, high];
  while (bad - ok > 1) {
    const mid = Math.floor((ok + bad) / 2);
    if (fits(mid)) ok = mid;
    else bad = mid;
  }
  return ok;
};

const ONE: Dimensions = { spends: 1, outputs: 1, observers: 1, mints: 1 };

/** Each purpose field filled to its own cap. */
const maximumShape = (): Dimensions => {
  const fill = (key: keyof Dimensions) =>
    largest(
      1,
      FIELD_CAP,
      (value) => fieldBytes({ ...ONE, [key]: value })[key] <= FIELD_CAP,
    );
  return {
    spends: fill("spends"),
    outputs: fill("outputs"),
    observers: fill("observers"),
    mints: fill("mints"),
  };
};

const expectWithinBounds = (steps: readonly StepMeasurement[]): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_BOUNDS.bytes);
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

describe("script-integrity-hash-mismatch consensus-maximum purpose shape", () => {
  it("fills every purpose field to its cap, with witnesses and redeemers within theirs", () => {
    const dimensions = maximumShape();
    const at = fieldBytes(dimensions);
    for (const value of Object.values(at))
      expect(value).toBeLessThanOrEqual(FIELD_CAP);
    for (const key of Object.keys(dimensions) as (keyof Dimensions)[])
      expect(
        fieldBytes({ ...dimensions, [key]: dimensions[key] + 1 })[key],
      ).toBeGreaterThan(FIELD_CAP);
  }, 120_000);

  it("convicts an accepted transaction carrying a wrong hash", async () => {
    const harness = await makeScriptIntegrityHashMismatchHarness();
    const references = await publishScriptIntegrityHashMismatchRoles(harness);
    const dimensions = maximumShape();
    const scenario = await setupIntegrityScenario({
      harness,
      shape: shapeOf(dimensions, WRONG_HASH),
      source: { kind: "normal" },
    });
    const run = await driveScriptIntegrityHashMismatchThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(true);
    expect(run.prepared.final.facts.language_bitmap).toBe(1n);
    const outcome = await run.terminal();
    console.log(
      "SIHM max-shape conviction",
      JSON.stringify({ dimensions, ...summary(run.steps) }),
    );
    expectWithinBounds(run.steps);
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.scriptIntegrityHashMismatch.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
  }, 3_600_000);

  it("refuses the same shape carrying the expected hash", async () => {
    const harness = await makeScriptIntegrityHashMismatchHarness();
    const references = await publishScriptIntegrityHashMismatchRoles(harness);
    const dimensions = maximumShape();
    const scenario = await setupIntegrityScenario({
      harness,
      shape: shapeOf(dimensions, expectedHash("PlutusV3")),
      source: { kind: "normal" },
    });
    await expect(
      requireConvictingScriptIntegrityHashMismatchProof({
        material: scenario.material,
        contracts: harness.scriptIntegrityHashMismatch,
        prover: harness.proverSigner.paymentKeyHash,
      }),
    ).rejects.toThrow(ScriptIntegrityHashMismatchHonestLeaf);
    const run = await driveScriptIntegrityHashMismatchThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(false);
    console.log(
      "SIHM max-shape honest",
      JSON.stringify({ dimensions, ...summary(run.steps) }),
    );
    expectWithinBounds(run.steps);
    await expectOnchainRefusal(() => run.terminal(false), {
      refusedBy: "fraud_proofs/script_integrity_hash_mismatch/terminal",
      check: /^Validator returned false$/u,
    });
  }, 3_600_000);
});
