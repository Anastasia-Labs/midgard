/**
 * `executionNativeScriptInvalid` at the consensus maximum shape, from a
 * genuine Init. Field 7 holds as many address witnesses as its byte cap
 * admits, and the observer's native source is the one field-6 witness,
 * filled to field 6's byte cap: a single-child `all` chain 256 deep around
 * an `atLeast` over every signer's key and as many absent keys as fit. Every
 * spend and reference input sits at least five branches deep in the
 * predecessor ledger. An accepted transaction whose `atLeast` asks for one
 * more signature than it carries convicts; the same shape asking for exactly
 * the signatures it carries is refused at the terminal. Each signer and
 * evaluation page is sized to one transaction's budget by the shared page
 * fit.
 */
import {
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import type { MidgardNativeScript } from "@al-ft/midgard-core/codec";
import type { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  ExecutionNativeScriptInvalidHonestLeaf,
  requireConvictingExecutionNativeScriptInvalidProof,
} from "../src/execution-native-script-invalid/index.js";
import { keyValuePhasMembershipProofs } from "../src/transition-trace/phas.js";
import { clusterPadding, intoCluster } from "./support/deep-ledger-trie.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  driveExecutionNativeScriptInvalidThread,
  EXECUTION_NATIVE_SCRIPT_INVALID_STEP_BOUNDS,
  makeExecutionNativeScriptInvalidHarness,
  type PageSize,
  publishExecutionNativeScriptInvalidRoles,
  type StepMeasurement,
  stepTable,
} from "./support/execution-native-script-invalid-emulator.js";
import {
  buildNativeSubject,
  keyHashOf,
  keyLockedEntries,
  native,
  type NativeSubjectShape,
  PAYER,
  setupNativeScenario,
  signingKey,
} from "./support/execution-native-script-invalid-emulator.subject.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

const FIELD_CAP = MIDGARD_CONSENSUS_LIMITS.maxTransactionAggregateFieldBytes;

/** The deep-ledger path clusters and recorded first nonces of the direct
 * purpose deep-source test: the spend inputs share the reference cluster's
 * first five digits, behind a full branch at each of its eight. */
const REFERENCE_CLUSTER = "a5c3e9b7";
const SPEND_CLUSTER = REFERENCE_CLUSTER.slice(0, 5);
const FIRST_NONCES = {
  spend: [
    80679n,
    449231n,
    426674n,
    2156817n,
    4192642n,
    1144979n,
    222137n,
    421296n,
    1357776n,
    1725562n,
    502928n,
    243121n,
    555734n,
    343954n,
    421234n,
    489208n,
  ],
  reference: [
    3707647191n,
    4034171828n,
    12422552239n,
    1577775210n,
    5695688245n,
    8777524122n,
    681379952n,
    1889196097n,
  ],
  padding: [
    [37n, 58n, 14n, 13n, 1n, 1n, 16n, 14n, 24n, 20n, 84n, 26n, 9n, 1n, 46n],
    [
      338n,
      241n,
      146n,
      738n,
      409n,
      15n,
      282n,
      36n,
      50n,
      421n,
      656n,
      7n,
      410n,
      274n,
      34n,
    ],
    [
      7257n,
      4353n,
      1124n,
      686n,
      4031n,
      3183n,
      22193n,
      12876n,
      3548n,
      4515n,
      685n,
      5240n,
      15279n,
      6412n,
      6552n,
    ],
    [
      6222n,
      45986n,
      21945n,
      287199n,
      1634n,
      18426n,
      67769n,
      39518n,
      226882n,
      42432n,
      12286n,
      37441n,
      207722n,
      42054n,
      149866n,
    ],
    [
      342197n,
      2315350n,
      467883n,
      2149793n,
      23372n,
      510121n,
      625989n,
      1073605n,
      1266971n,
      1721988n,
      213599n,
      118950n,
      654347n,
      95861n,
      178498n,
    ],
    [
      12257465n,
      43455662n,
      9906622n,
      13451730n,
      22878142n,
      23513872n,
      10116551n,
      9055863n,
      24801526n,
      42796369n,
      23132861n,
      15565669n,
      26227855n,
      10555011n,
      13158450n,
    ],
    [
      1657357456n,
      463873695n,
      320120793n,
      67936308n,
      251964577n,
      14406750n,
      36209683n,
      91906087n,
      381624244n,
      970885261n,
      309455690n,
      35213651n,
      47037005n,
      450083137n,
      225808194n,
    ],
    [
      2188913258n,
      939845396n,
      11386368700n,
      1806025541n,
      2958304247n,
      2047004260n,
      2647107311n,
      3705177192n,
      1848344279n,
      14199330n,
      8199965037n,
      516480476n,
      2788212509n,
      2228844141n,
      180077104n,
    ],
  ],
};

/** Every spend and reference input, key-locked by the payer. */
const SPENDS = intoCluster(
  keyLockedEntries(0x41, PAYER, Array(FIRST_NONCES.spend.length).fill(null)),
  0x41,
  SPEND_CLUSTER,
  FIRST_NONCES.spend,
);
const REFERENCES = intoCluster(
  keyLockedEntries(
    0x31,
    PAYER,
    Array(FIRST_NONCES.reference.length).fill(null),
  ),
  0x31,
  REFERENCE_CLUSTER,
  FIRST_NONCES.reference,
);

/** The `all` chain's depth around the `atLeast`. */
const CHAIN_DEPTH = 256;

/** The `count` co-signers after the payer. */
const cosigners = (count: number) =>
  Array.from({ length: count }, (_, index) => signingKey(1_000 + index));

/** Keys that sign nothing. */
const absent = (count: number) =>
  Array.from({ length: count }, (_, index) => signingKey(100_000 + index));

/** The observer's source: every signer's key, then `absentCount` absent
 * keys, under an `atLeast` asking for `required` of them, inside the chain. */
const sourceOf = (
  signers: readonly CML.PrivateKey[],
  absentCount: number,
  required: bigint,
): MidgardVersionedScript => {
  const keys = [...signers, ...absent(absentCount)];
  let script: MidgardNativeScript = {
    type: "atLeast",
    required,
    scripts: keys.map((key) => ({ type: "sig", keyHash: keyHashOf(key) })),
  };
  for (let level = 0; level < CHAIN_DEPTH; level += 1)
    script = { type: "all", scripts: [script] };
  return native(script);
};

/** An accepted subject requiring one observer whose source is the one
 * field-6 witness, signed by the payer and `signers`. */
const shapeOf = (
  signers: readonly CML.PrivateKey[],
  source: MidgardVersionedScript,
): NativeSubjectShape => ({
  observers: [{ script: source, witness: true, redeemer: false }],
  spend: SPENDS,
  reference: REFERENCES,
  padding: clusterPadding(REFERENCE_CLUSTER, FIRST_NONCES.padding),
  signers,
});

const fieldBytes = (shape: NativeSubjectShape) => {
  const tx = buildNativeSubject(shape);
  return {
    witnesses: tx.witnessSet.scriptTxWitsPreimageCbor.length,
    signers: tx.witnessSet.addrTxWitsPreimageCbor.length,
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

/** Field 7 and field 6 at their caps; `extra` more signatures required
 * than the transaction carries. */
const maximumShape = (extra: bigint) => {
  const small = native({ type: "all", scripts: [] });
  const signerCount = largest(
    1,
    1_000,
    (count) =>
      fieldBytes(shapeOf([PAYER, ...cosigners(count - 1)], small)).signers <=
      FIELD_CAP,
  );
  const signers = [PAYER, ...cosigners(signerCount - 1)];
  const required = BigInt(signerCount) + extra;
  const absentCount = largest(
    0,
    2_000, // a signature leaf takes 32 bytes: past field 6 long before
    (count) =>
      fieldBytes(shapeOf(signers, sourceOf(signers, count, required)))
        .witnesses <= FIELD_CAP,
  );
  const shape = shapeOf(signers, sourceOf(signers, absentCount, required));
  return { shape, signerCount, absentCount, signers, required };
};

const expectWithinBounds = (steps: readonly StepMeasurement[]): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      EXECUTION_NATIVE_SCRIPT_INVALID_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      EXECUTION_NATIVE_SCRIPT_INVALID_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(EXECUTION_NATIVE_SCRIPT_INVALID_STEP_BOUNDS.bytes);
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

/** Each paged role's page sizes: minimum, maximum and count. */
const pageSizes = (pages: readonly PageSize[]) => {
  const byRole: Record<string, number[]> = {};
  for (const { role, size } of pages) (byRole[role] ??= []).push(size);
  return Object.fromEntries(
    Object.entries(byRole).map(([role, sizes]) => [
      role,
      { min: Math.min(...sizes), max: Math.max(...sizes), pages: sizes.length },
    ]),
  );
};

/** The transactions of the family's own walk roles. */
const walkSteps = (steps: readonly StepMeasurement[], role: string) =>
  steps.filter(({ label }) => label.startsWith(`${role} `));

/** The proof depth of every spend and reference input in the predecessor
 * ledger. */
const ledgerDepths = async (
  material: Awaited<ReturnType<typeof setupNativeScenario>>["material"],
) => {
  const { ledger } = material;
  return (
    await keyValuePhasMembershipProofs(
      ledger.root,
      [...material.fields[0]!, ...material.fields[1]!].map(
        (item) => ledger.root.entries.find((entry) => entry.key.equals(item))!,
      ),
    )
  ).map((proof) => proof.length);
};

const OBSERVER = { kind: 2n, index: 0n };

describe("execution-native-script-invalid consensus-maximum shape", () => {
  it("fills field 7 and field 6 to their caps", () => {
    const { shape, signers, absentCount, required } = maximumShape(1n);
    const bytes = fieldBytes(shape);
    expect(bytes.signers).toBeLessThanOrEqual(FIELD_CAP);
    expect(bytes.witnesses).toBeLessThanOrEqual(FIELD_CAP);
    expect(
      fieldBytes(
        shapeOf(
          [PAYER, ...cosigners(signers.length)],
          native({ type: "all", scripts: [] }),
        ),
      ).signers,
    ).toBeGreaterThan(FIELD_CAP);
    expect(
      fieldBytes(shapeOf(signers, sourceOf(signers, absentCount + 1, required)))
        .witnesses,
    ).toBeGreaterThan(FIELD_CAP);
  }, 600_000);

  it("convicts an accepted transaction whose capped source is false over a deep ledger", async () => {
    const harness = await makeExecutionNativeScriptInvalidHarness();
    const references = await publishExecutionNativeScriptInvalidRoles(harness);
    const { shape, signerCount, absentCount } = maximumShape(1n);
    const scenario = await setupNativeScenario({
      harness,
      shape,
      source: { kind: "normal" },
    });
    const depths = await ledgerDepths(scenario.material);
    expect(depths).toHaveLength(SPENDS.length + REFERENCES.length);
    expect(Math.min(...depths)).toBeGreaterThanOrEqual(5);
    const run = await driveExecutionNativeScriptInvalidThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.entry).toStrictEqual({ Observe: OBSERVER });
    expect(run.prepared.final.fact).toStrictEqual({
      Evaluated: { coordinate: OBSERVER, satisfied: false },
    });
    expect(run.prepared.convicts).toBe(true);
    const outcome = await run.terminal();
    console.log(
      "ENSI max-shape conviction",
      JSON.stringify({
        signerCount,
        absentCount,
        chainDepth: CHAIN_DEPTH,
        field: fieldBytes(shape),
        ledgerDepth: [Math.min(...depths), Math.max(...depths)],
        pageSizes: pageSizes(run.pages),
        signers: summary(walkSteps(run.steps, "signers")),
        evaluate: summary(walkSteps(run.steps, "evaluate")),
        ...summary(run.steps),
      }),
    );
    expectWithinBounds(run.steps);
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.executionNativeScriptInvalid.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
  }, 3_600_000);

  it("refuses the same shape when its source holds", async () => {
    const harness = await makeExecutionNativeScriptInvalidHarness();
    const references = await publishExecutionNativeScriptInvalidRoles(harness);
    const { shape, signerCount, absentCount } = maximumShape(0n);
    const scenario = await setupNativeScenario({
      harness,
      shape,
      source: { kind: "normal" },
    });
    await expect(
      requireConvictingExecutionNativeScriptInvalidProof({
        material: scenario.material,
        contracts: harness.executionNativeScriptInvalid,
        prover: harness.proverSigner.paymentKeyHash,
        entry: { Observe: OBSERVER },
      }),
    ).rejects.toThrow(ExecutionNativeScriptInvalidHonestLeaf);
    const run = await driveExecutionNativeScriptInvalidThread({
      harness,
      scenario,
      references,
      entry: { Observe: OBSERVER },
    });
    expect(run.prepared.convicts).toBe(false);
    expect(run.prepared.final.fact).toStrictEqual({
      Evaluated: { coordinate: OBSERVER, satisfied: true },
    });
    console.log(
      "ENSI max-shape honest",
      JSON.stringify({
        signerCount,
        absentCount,
        field: fieldBytes(shape),
        pageSizes: pageSizes(run.pages),
        signers: summary(walkSteps(run.steps, "signers")),
        evaluate: summary(walkSteps(run.steps, "evaluate")),
        ...summary(run.steps),
      }),
    );
    expectWithinBounds(run.steps);
    await expectOnchainRefusal(() => run.terminal(false), {
      refusedBy: "fraud_proofs/execution_native_script_invalid/terminal",
      check: /^Validator returned false$/u,
    });
  }, 3_600_000);
});
