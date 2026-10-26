/**
 * `missingScriptSource` at the consensus maximum observer shape, from a
 * genuine Init: the required-observer field holds as many observers as its
 * byte cap admits, and the script-witness and redeemer fields are filled to
 * their caps. The observer at the last coordinate has no source, so the
 * purpose selection and the source lookup walk their whole fields. Committed
 * as accepted it convicts. The same shape with the last observer witnessed,
 * committed as accepted, is honest: the prover finds no route, and the
 * observed route through the last observer, claiming its source absent, is
 * refused at the source lookup after the same source walk and selection.
 */
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildDirectPurposeTransition,
  directPurposeBindTransitions,
  directPurposeFieldPlans,
  type DirectPurposeSourceEvidence,
  directPurposeSourceTransitions,
} from "../src/direct-purpose-family/index.js";
import { buildDirectSourceIndexEvidence } from "../src/direct-script-derivation/index.js";
import {
  MISSING_SCRIPT_SOURCE_FAMILY,
  missingScriptSourceContractRoles,
  MissingScriptSourceHonestLeaf,
  missingScriptSourceObservedRoute,
  requireConvictingMissingScriptSourceProof,
} from "../src/missing-script-source/index.js";
import { driveDirectPurposeThread } from "./support/direct-purpose-emulator.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  buildMissingRedeemerSubject,
  type MissingRedeemerSubjectShape,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  makeMissingScriptSourceHarness,
  publishMissingScriptSourceRoles,
  type StepMeasurement,
  stepTable,
} from "./support/missing-script-source-emulator.js";
import { convictAndCheck } from "./support/missing-script-source-emulator.rows.js";

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

type Dimensions = {
  readonly count: number;
  readonly scriptBytes: number;
  readonly dataBytes: number;
};

/** Every observer has a redeemer; every observer but the one at the last
 * coordinate carries its script as a witness, and that one too when
 * `lastWitnessed`. */
const shapeOf = (
  { count, scriptBytes, dataBytes }: Dimensions,
  lastWitnessed = false,
): MissingRedeemerSubjectShape => {
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
      witness: lastWitnessed || order[position] !== last,
      redeemer: true,
    })),
    redeemerData: () => Buffer.from(Data.to("ab".repeat(dataBytes)), "hex"),
  };
};

/** Observer count at the field-7 cap, then the largest uniform script and
 * redeemer-data sizes that keep fields 6 and 8 within their caps. */
const maximumShape = (lastWitnessed = false): Dimensions => {
  const count = largest(
    1,
    (value) =>
      fieldBytes(
        shapeOf({ count: value, scriptBytes: 2, dataBytes: 0 }, lastWitnessed),
      ).observers <= FIELD_CAP,
  );
  const scriptBytes = largest(
    2,
    (value) =>
      fieldBytes(
        shapeOf({ count, scriptBytes: value, dataBytes: 0 }, lastWitnessed),
      ).witnesses <= FIELD_CAP,
  );
  const dataBytes = largest(
    0,
    (value) =>
      fieldBytes(
        shapeOf({ count, scriptBytes, dataBytes: value }, lastWitnessed),
      ).redeemers <= FIELD_CAP,
  );
  return { count, scriptBytes, dataBytes };
};

/** Source evidence that reports every script's source absent while keeping
 * the source index's own proof, which shows the source present. */
const claimingAbsent = (
  evidence: DirectPurposeSourceEvidence,
): DirectPurposeSourceEvidence => ({
  ...evidence,
  lookup: async (hash) => ({
    ...(await evidence.lookup(hash)),
    selected: null,
  }),
});

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

  it("refuses the same shape with the last observer witnessed", async () => {
    const dimensions = maximumShape(true);
    const shape = shapeOf(dimensions, true);
    const at = fieldBytes(shape);
    expect(at.observers).toBeLessThanOrEqual(FIELD_CAP);
    expect(at.witnesses).toBeLessThanOrEqual(FIELD_CAP);
    expect(at.redeemers).toBeLessThanOrEqual(FIELD_CAP);
    expect(dimensions.count).toBe(maximumShape().count);
    const harness = await makeMissingScriptSourceHarness();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape,
      source: { kind: "normal" },
    });
    await expect(
      requireConvictingMissingScriptSourceProof({
        material: scenario.material,
        contracts: harness.missingScriptSource,
        prover: harness.proverSigner.paymentKeyHash,
      }),
    ).rejects.toThrow(MissingScriptSourceHonestLeaf);
    const references = await publishMissingScriptSourceRoles(harness);
    // The prover finds no route; the observed route is planned through the
    // last observer as if its source were absent, up to the source lookup.
    const last = { kind: 2n, index: BigInt(dimensions.count - 1) };
    const run = await driveDirectPurposeThread({
      harness,
      family: MISSING_SCRIPT_SOURCE_FAMILY,
      scenario,
      references,
      prepare: async ({ material, contracts, prover }) => {
        const roles = missingScriptSourceContractRoles(contracts);
        const evidence = await buildDirectSourceIndexEvidence(material);
        const plan = directPurposeFieldPlans(material, prover);
        const route = await missingScriptSourceObservedRoute(
          { material, evidence: claimingAbsent(evidence), prover, plan },
          last,
        );
        const at = route.transitions.findIndex(
          ({ role }) => role === "sourceLookup",
        );
        return {
          plan,
          lookup: route.transitions[at]!,
          // The thread is driven up to the lookup.
          transitions: [
            ...directPurposeBindTransitions(material, prover),
            ...directPurposeSourceTransitions({
              material,
              evidence,
              prover,
              plan,
              resolvedScriptHash: roles.sourceResolved.spendingScriptHash,
              witnessesScriptHash: roles.sourceWitnesses.spendingScriptHash,
            }),
            ...route.transitions.slice(0, at),
          ],
        };
      },
    });
    const { lookup } = run.prepared;
    expect(lookup.label).toBe("observe lookup");
    expect(run.thread.datum).toBe(run.prepared.transitions.at(-1)!.datum);
    console.log(
      "MSS max-shape honest",
      JSON.stringify({ dimensions, ...summary(run.steps) }),
    );
    await expectOnchainRefusal(async () =>
      (
        await buildDirectPurposeTransition(run.custody, run.thread, lookup)
      ).complete(),
    );
  }, 3_600_000);
});
