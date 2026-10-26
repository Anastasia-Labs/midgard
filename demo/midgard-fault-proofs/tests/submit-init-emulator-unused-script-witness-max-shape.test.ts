/**
 * `unusedScriptWitness` at the consensus maximum shape, from a genuine Init:
 * the spend-input, output, required-observer and mint fields each hold as
 * many purposes as their byte cap admits, every observer and mint policy
 * brings its own field-6 witness, and field 6 is filled to its own byte cap.
 * The audited witness is the last one, so the witness walk crosses the whole
 * field. Over a deep predecessor ledger and source map an unused last
 * witness convicts; the same purposes with every witness used, field 6
 * filled by one large used script, are refused at the terminal.
 */
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  hashMidgardVersionedScript,
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import type { LedgerEntry } from "../src/ledger-at.js";
import { keyValuePhasMembershipProofs } from "../src/transition-trace/phas.js";
import {
  requireConvictingUnusedScriptWitnessProof,
  UnusedScriptWitnessHonestLeaf,
} from "../src/unused-script-witness/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  buildIntegritySubject,
  expectedHash,
  type IntegritySubjectShape,
  nativeScript,
  plutusScript,
  setupIntegrityScenario,
} from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";
import {
  CLUSTER_DIGITS,
  groundNatives,
  groundOutRef,
  KEY_LOCKED,
  largest,
  leavesAt,
  LEDGER_CLUSTER,
  pathStarts,
} from "./support/unused-script-witness-emulator.deep.js";
import {
  driveUnusedScriptWitnessThread,
  makeUnusedScriptWitnessHarness,
  publishUnusedScriptWitnessRoles,
  type StepMeasurement,
  stepTable,
  UNUSED_SCRIPT_WITNESS_STEP_BOUNDS,
} from "./support/unused-script-witness-emulator.js";

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

/** Every purpose used; field 6 lists the observer and mint natives, then
 * `witnesses`. */
const shapeOf = (
  { spends, outputs, observers, mints }: Dimensions,
  witnesses: readonly MidgardVersionedScript[] = [SPENT, RECEIVED],
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
    script: witnesses[1]!,
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
  witnesses,
  integrity: expectedHash("PlutusV3"),
});

const fieldBytes = (shape: IntegritySubjectShape) => {
  const tx = buildIntegritySubject(shape);
  return {
    spends: tx.body.spendInputsPreimageCbor.length,
    outputs: tx.body.outputsPreimageCbor.length,
    observers: tx.body.requiredObserversPreimageCbor.length,
    mints: tx.body.mintPreimageCbor.length,
    witnesses: tx.witnessSet.scriptTxWitsPreimageCbor.length,
    redeemers: tx.witnessSet.redeemerTxWitsPreimageCbor.length,
  };
};

/** The number of field-6 items of `shape`. */
const witnessCount = (shape: IntegritySubjectShape) =>
  (shape.observers ?? []).length +
  (shape.mints ?? []).length +
  (shape.witnesses ?? []).length;

const ONE: Dimensions = { spends: 1, outputs: 1, observers: 1, mints: 1 };

/** Each purpose field filled to its own cap. */
const maximumShape = (): Dimensions => {
  const fill = (key: keyof Dimensions) =>
    largest(
      1,
      FIELD_CAP,
      (value) =>
        fieldBytes(shapeOf({ ...ONE, [key]: value }))[key] <= FIELD_CAP,
    );
  return {
    spends: fill("spends"),
    outputs: fill("outputs"),
    observers: fill("observers"),
    mints: fill("mints"),
  };
};

/** Unused native witnesses with a four-byte slot, one per slot from 2^30. */
const FILLER_SLOT = 1 << 30;
const fillers = (count: number) =>
  Array.from({ length: count }, (_, index) =>
    nativeScript(BigInt(FILLER_SLOT + index)),
  );

/** `shape` with `extra` appended to field 6 after as many unused fillers
 * as field 6 still admits. */
const fillWitnesses = (
  shape: IntegritySubjectShape,
  extra: readonly MidgardVersionedScript[],
): IntegritySubjectShape => {
  const withFillers = (count: number): IntegritySubjectShape => ({
    ...shape,
    witnesses: [...(shape.witnesses ?? []), ...fillers(count), ...extra],
  });
  return withFillers(
    largest(
      0,
      FIELD_CAP,
      (count) => fieldBytes(withFillers(count)).witnesses <= FIELD_CAP,
    ),
  );
};

/** The purposes at their caps with every witness used: the received script
 * is enlarged until field 6 reaches its cap. */
const honestMaximum = (dimensions: Dimensions) => {
  const received = (bytes: number): MidgardVersionedScript => ({
    language: "PlutusV3",
    scriptBytes: Buffer.alloc(bytes, 0x5b),
  });
  const size = largest(
    2,
    FIELD_CAP,
    (bytes) =>
      fieldBytes(shapeOf(dimensions, [SPENT, received(bytes)])).witnesses <=
      FIELD_CAP,
  );
  return shapeOf(dimensions, [SPENT, received(size)]);
};

/**
 * The maximum shape over a deep predecessor ledger and a deep source map:
 * every spent out-ref, every purpose script hash and the audited witness's
 * hash are ground into one three-digit path cluster per trie, and one
 * padding key leaves each cluster at each of its digits, so every opened key
 * sits at least five branches deep. Field 6 is then filled to its cap, the
 * audited (unused) witness last.
 */
const deepShapeOf = (dimensions: Dimensions) => {
  const base = shapeOf(dimensions);
  const sourceCluster = computeHash32(
    Buffer.from(hashMidgardVersionedScript(SPENT), "hex"),
  )
    .toString("hex")
    .slice(0, CLUSTER_DIGITS);
  const inSourceCluster = (hash: Buffer) => pathStarts(hash, sourceCluster);
  let seed = 2;
  while (
    !inSourceCluster(
      Buffer.from(hashMidgardVersionedScript(plutusScript(seed)), "hex"),
    )
  )
    seed += 1;
  const received = plutusScript(seed);
  const purposes = groundNatives(
    dimensions.observers + dimensions.mints + 1,
    1 << 24,
    inSourceCluster,
  );
  const audited = purposes.scripts.at(-1)!;
  let next = purposes.next;
  const sourcePadding = Array.from({ length: CLUSTER_DIGITS }, (_, depth) => {
    const ground = groundNatives(1, next, (hash) =>
      leavesAt(hash, sourceCluster, depth),
    );
    next = ground.next;
    return ground.scripts[0]!;
  });
  const ledgerPadding: LedgerEntry[] = Array.from(
    { length: CLUSTER_DIGITS },
    (_, depth) => ({
      outRef: groundOutRef(0, 0x50 + depth, (key) =>
        leavesAt(key, LEDGER_CLUSTER, depth),
      ),
      output: KEY_LOCKED,
    }),
  );
  const shape = fillWitnesses(
    {
      ...base,
      spend: (base.spend ?? []).map((entry, index) => ({
        ...entry,
        outRef: groundOutRef(index, 0x41, (key) =>
          pathStarts(key, LEDGER_CLUSTER),
        ),
      })),
      padding: ledgerPadding,
      receives: (base.receives ?? []).map((entry) => ({
        ...entry,
        script: received,
      })),
      observers: purposes.scripts
        .slice(0, dimensions.observers)
        .map((script) => ({ script, redeemer: false })),
      mints: purposes.scripts
        .slice(dimensions.observers, -1)
        .map((script) => ({ script, redeemer: false })),
      witnesses: [SPENT, received, ...sourcePadding],
    },
    [audited],
  );
  return { shape, audited, received };
};

const expectWithinBounds = (steps: readonly StepMeasurement[]): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      UNUSED_SCRIPT_WITNESS_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      UNUSED_SCRIPT_WITNESS_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(UNUSED_SCRIPT_WITNESS_STEP_BOUNDS.bytes);
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

/** The family's own field-6 audit transactions. */
const auditSteps = (steps: readonly StepMeasurement[]) =>
  steps.filter(({ label }) => label.startsWith("audit "));

/** The number of transactions of each kind (the label without its page
 * position and size). */
const kinds = (steps: readonly StepMeasurement[]) => {
  const counts: Record<string, number> = {};
  for (const { label } of steps) {
    const kind = label.replace(/ \(\d+\)$/u, "").replace(/ \d+$/u, "");
    counts[kind] = (counts[kind] ?? 0) + 1;
  }
  return counts;
};

describe("unused-script-witness consensus-maximum shape", () => {
  it("fills every purpose field and field 6 to their caps", () => {
    const dimensions = maximumShape();
    const base = shapeOf(dimensions);
    for (const value of Object.values(fieldBytes(base)))
      expect(value).toBeLessThanOrEqual(FIELD_CAP);
    for (const key of Object.keys(dimensions) as (keyof Dimensions)[])
      expect(
        fieldBytes(shapeOf({ ...dimensions, [key]: dimensions[key] + 1 }))[key],
      ).toBeGreaterThan(FIELD_CAP);
    const filled = fillWitnesses(base, []);
    expect(fieldBytes(filled).witnesses).toBeLessThanOrEqual(FIELD_CAP);
    expect(
      fieldBytes({
        ...filled,
        witnesses: [
          ...filled.witnesses!,
          nativeScript(BigInt(FILLER_SLOT - 1)),
        ],
      }).witnesses,
    ).toBeGreaterThan(FIELD_CAP);
    const honest = honestMaximum(dimensions);
    const honestBytes = fieldBytes(honest).witnesses;
    expect(honestBytes).toBeLessThanOrEqual(FIELD_CAP);
    expect(FIELD_CAP - honestBytes).toBeLessThan(4);
  }, 120_000);

  it("convicts the last witness of a capped field 6 over a deep ledger and source map", async () => {
    const harness = await makeUnusedScriptWitnessHarness();
    const references = await publishUnusedScriptWitnessRoles(harness);
    const dimensions = maximumShape();
    const { shape, audited, received } = deepShapeOf(dimensions);
    for (const bytes of Object.values(fieldBytes(shape)))
      expect(bytes).toBeLessThanOrEqual(FIELD_CAP);
    const scriptIndex = BigInt(witnessCount(shape) - 1);
    const scenario = await setupIntegrityScenario({
      harness,
      shape,
      source: { kind: "normal" },
    });
    const run = await driveUnusedScriptWitnessThread({
      harness,
      scenario,
      references,
      scriptIndex,
    });
    expect(run.prepared.scriptIndex).toBe(scriptIndex);
    expect(run.prepared.unused).toBe(true);
    expect(run.prepared.convicts).toBe(true);
    const { ledger } = scenario.material;
    const ledgerDepths = (
      await keyValuePhasMembershipProofs(
        ledger.root,
        scenario.material.fields[0]!.map(
          (item) =>
            ledger.root.entries.find((entry) => entry.key.equals(item))!,
        ),
      )
    ).map((proof) => proof.length);
    // Every script an execution opens in the source map, and the audited
    // witness's selection; the fillers are never opened.
    const opened = new Set(
      [
        SPENT,
        received,
        audited,
        ...[...(shape.observers ?? []), ...(shape.mints ?? [])].map(
          (entry) => entry.script,
        ),
      ].map(hashMidgardVersionedScript),
    );
    const sourceDepths = await Promise.all(
      [...opened].map(
        async (hash) => (await run.prepared.evidence.lookup(hash)).proof.length,
      ),
    );
    expect(Math.min(...ledgerDepths)).toBeGreaterThanOrEqual(5);
    expect(Math.min(...sourceDepths)).toBeGreaterThanOrEqual(5);
    const outcome = await run.terminal();
    console.log(
      "USW deep max-shape conviction",
      JSON.stringify({
        dimensions,
        witnesses: witnessCount(shape),
        field6Bytes: fieldBytes(shape).witnesses,
        ledgerDepth: [Math.min(...ledgerDepths), Math.max(...ledgerDepths)],
        sourceDepth: [Math.min(...sourceDepths), Math.max(...sourceDepths)],
        audit: summary(auditSteps(run.steps)),
        kinds: kinds(run.steps),
        ...summary(run.steps),
      }),
    );
    expectWithinBounds(run.steps);
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.unusedScriptWitness.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
  }, 3_600_000);

  it("refuses the same purposes when every witness is used, auditing the last", async () => {
    const harness = await makeUnusedScriptWitnessHarness();
    const references = await publishUnusedScriptWitnessRoles(harness);
    const dimensions = maximumShape();
    const shape = honestMaximum(dimensions);
    const scriptIndex = BigInt(witnessCount(shape) - 1);
    const scenario = await setupIntegrityScenario({
      harness,
      shape,
      source: { kind: "normal" },
    });
    await expect(
      requireConvictingUnusedScriptWitnessProof({
        material: scenario.material,
        contracts: harness.unusedScriptWitness,
        prover: harness.proverSigner.paymentKeyHash,
        scriptIndex,
      }),
    ).rejects.toThrow(UnusedScriptWitnessHonestLeaf);
    const run = await driveUnusedScriptWitnessThread({
      harness,
      scenario,
      references,
      scriptIndex,
    });
    expect(run.prepared.unused).toBe(false);
    expect(run.prepared.convicts).toBe(false);
    console.log(
      "USW max-shape honest",
      JSON.stringify({
        dimensions,
        witnesses: witnessCount(shape),
        field6Bytes: fieldBytes(shape).witnesses,
        audit: summary(auditSteps(run.steps)),
        kinds: kinds(run.steps),
        ...summary(run.steps),
      }),
    );
    expectWithinBounds(run.steps);
    await expectOnchainRefusal(() => run.terminal(false), {
      refusedBy: "fraud_proofs/unused_script_witness/terminal",
      check: /^Validator returned false$/u,
    });
  }, 3_600_000);
});
