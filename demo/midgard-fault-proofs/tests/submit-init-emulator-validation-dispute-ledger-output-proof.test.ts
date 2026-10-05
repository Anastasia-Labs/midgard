/**
 * Applied emulator paths for ResolveInputs and ScriptSources LOP step/finalize.
 * Descriptor facts are attached in groups [[2, 3], [1], [0]], then finalized
 * without descriptor yields. A span-attach step pins the consumer-derived
 * window; subsequent readers authenticate against that window. Positive paths
 * enforce the 13,200,000-memory / 8,000,000,000-step fit basis.
 */
import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import { Constr, Data } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  buildForgedOperatorSuccessorValidationDisputeFixture,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

const POSITIVE_PATH_MEMORY_BASIS = 13_200_000n;
const POSITIVE_PATH_STEP_BASIS = 8_000_000_000n;

const maximumOutputDatum = (): Buffer => {
  // The fixture uses a 29-byte enterprise address and the same lovelace
  // amount for its spent and produced outputs. Search the serialized output,
  // including Plutus Data chunk framing, instead of subtracting an estimate.
  for (let scalarCount = 0; scalarCount < 3; scalarCount += 1) {
    let low = 0;
    let high = 16_384;
    while (low <= high) {
      const width = Math.floor((low + high) / 2);
      const datum = Buffer.from(
        Data.to([
          ...Array.from({ length: scalarCount }, () => 0n),
          "ab".repeat(width),
        ]),
        "hex",
      );
      const output = encodeMidgardTxOutput({
        address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 1)]),
        value: { lovelace: 100_000_000n, assets: new Map() },
        datum: { kind: "inline", cbor: datum },
      });
      if (output.length === 16_384) return datum;
      if (output.length < 16_384) low = width + 1;
      else high = width - 1;
    }
  }
  throw new Error("Could not construct the exact 16,384-byte ledger output");
};

/**
 * Inline datum for the multi-yield lifecycles: a wide integer, a 150-byte
 * bytestring, a list and a constructor drive the ledger-output-proof datum
 * traversal through the span-attach (role 23), advance-integer (role 7:
 * scalar-integer yield) and advance-bytes (role 18: scalar-bytes yield)
 * stages the plain address+value output never reaches. The
 * `disputedMatchOrdinal` values below were mapped by scanning every matching
 * step's derived plan: with this datum on both fixture outputs, ordinal 9 is
 * the span-attach step, ordinal 12 an advance-integer step and ordinal 31 an
 * advance-bytes step in both families.
 */
const MULTI_YIELD_DATUM_CBOR = Buffer.from(
  Data.to(
    new Constr(0, [
      123_456_789_012_345_678_901_234_567_890n,
      "ab".repeat(150),
      [1n, 2n, 3n],
      new Constr(1, []),
    ]),
  ),
  "hex",
);

const expectPositiveBasis = (
  result: Awaited<ReturnType<typeof runForcedValidationDisputeScenario>>,
  label: string,
): void => {
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
  console.info(
    `${label}: semantic tx memory ${result.semanticMeasurement!.executionMemory.toString()} cpu ${result.semanticMeasurement!.executionSteps.toString()}`,
  );
  if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
    console.info(
      JSON.stringify(
        {
          ledgerOutputNecessityMeasurement: {
            label,
            ...result.semanticMeasurement,
          },
        },
        (_key, value: unknown) =>
          typeof value === "bigint" ? value.toString() : value,
      ),
    );
  }
  expect(result.semanticMeasurement!.executionMemory).toBeLessThanOrEqual(
    POSITIVE_PATH_MEMORY_BASIS,
  );
  expect(result.semanticMeasurement!.executionSteps).toBeLessThanOrEqual(
    POSITIVE_PATH_STEP_BASIS,
  );
};

it.each(["resolveInputs", "scriptSources"] as const)(
  "proves the widest %s datum step at the exact 16,384-byte output maximum",
  async (disputedPhase) => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase,
          ...(disputedPhase === "resolveInputs"
            ? { resolveInputsKind: "membershipStep" as const }
            : { scriptSourcesSemanticIndex: 2 }),
          outputDatumCbor: maximumOutputDatum(),
          outputLovelace: 100_000_000n,
          worstCaseWitness: true,
        }),
    );
    expectPositiveBasis(result, `${disputedPhase} exact 16,384-byte output`);
  },
  600_000,
);

it("proves resolve-inputs membershipStep through permanent proof and removal", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "resolveInputs",
        resolveInputsKind: "membershipStep",
      }),
  );
  expectPositiveBasis(result, "resolve-inputs membershipStep");
}, 300_000);

// The four finalize-shaped steps of one resolve-inputs membership proof, in
// machine order: the datum+value summary attach, the reference-script attach,
// the scan-facts attach and the thin terminal (no descriptor yields).
it.each([
  [0, "datum+value summary attach [2,3]"],
  [1, "reference-script attach [1]"],
  [2, "scan-facts attach [0]"],
  [3, "thin terminal []"],
])(
  "proves resolve-inputs membershipFinalize ordinal %i (%s) through permanent proof and removal",
  async (ordinal, label) => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "resolveInputs",
          resolveInputsKind: "membershipFinalize",
          disputedMatchOrdinal: ordinal,
        }),
    );
    expectPositiveBasis(result, `resolve-inputs finalize ${label}`);
  },
  300_000,
);

// A finalize step over a datum-bearing output: the finalize plan reads the
// datum traversal's folded result, which the datum-less rows above never
// carry.
it("proves a script-sources datum+value summary attach over a datum-bearing output through permanent proof and removal", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "scriptSources",
        scriptSourcesSemanticIndex: 3,
        disputedMatchOrdinal: 0,
        outputDatumCbor: MULTI_YIELD_DATUM_CBOR,
      }),
  );
  expectPositiveBasis(result, "script-sources datum-bearing finalize");
}, 300_000);

it("proves script-sources output-proof semantic 2 through permanent proof and removal", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "scriptSources",
        scriptSourcesSemanticIndex: 2,
      }),
  );
  expectPositiveBasis(result, "script-sources output-proof step");
}, 300_000);

it.each([
  [0, "datum+value summary attach [2,3]"],
  [1, "reference-script attach [1]"],
  [2, "scan-facts attach [0]"],
  [3, "thin terminal []"],
])(
  "proves script-sources output-proof semantic 3 ordinal %i (%s) through permanent proof and removal",
  async (ordinal, label) => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "scriptSources",
          scriptSourcesSemanticIndex: 3,
          disputedMatchOrdinal: ordinal,
        }),
    );
    expectPositiveBasis(result, `script-sources finalize ${label}`);
  },
  300_000,
);

it("refuses a forged resolve-inputs membershipStep successor against an honest trace", async () => {
  await expect(
    runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "resolveInputs",
        resolveInputsKind: "membershipStep",
        dishonestChallenger: true,
      }),
    ),
  ).rejects.toThrow(/semantic-resolution failed: EvaluatorError/);
}, 300_000);

it("refuses a forged script-sources output-proof step successor against an honest trace", async () => {
  await expect(
    runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "scriptSources",
        scriptSourcesSemanticIndex: 2,
        dishonestChallenger: true,
      }),
    ),
  ).rejects.toThrow(/semantic-resolution failed: EvaluatorError/);
}, 300_000);

// The fact-attach step's successor records the fact commitment its
// descriptor yields attest: a forged continuation dies at the dispatcher's
// successor equality after the on-chain `fact_attach` recomputation.
it("refuses a forged resolve-inputs fact-attach successor against an honest trace", async () => {
  await expect(
    runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "resolveInputs",
        resolveInputsKind: "membershipFinalize",
        disputedMatchOrdinal: 0,
        dishonestChallenger: true,
      }),
    ),
  ).rejects.toThrow(/semantic-resolution failed: EvaluatorError/);
}, 300_000);

// The thin terminal requires the recorded scan fact to commit exactly the
// redeemer's descriptor: a forged terminal claim dies at the
// facts/authorization conjunction with no descriptor yields present to
// launder it.
it("refuses a forged resolve-inputs thin-terminal successor against an honest trace", async () => {
  await expect(
    runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "resolveInputs",
        resolveInputsKind: "membershipFinalize",
        disputedMatchOrdinal: 3,
        dishonestChallenger: true,
      }),
    ),
  ).rejects.toThrow(/semantic-resolution failed: EvaluatorError/);
}, 300_000);

it("proves the span-attach step through its span stage yield", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "scriptSources",
        scriptSourcesSemanticIndex: 2,
        disputedMatchOrdinal: 9,
        outputDatumCbor: MULTI_YIELD_DATUM_CBOR,
      }),
  );
  // Dispute spend + span stage yield.
  expect(result.semanticMeasurement!.redeemerCount).toBeGreaterThanOrEqual(2);
  expectPositiveBasis(result, "script-sources span-attach step");
}, 300_000);

it("proves a resolve-inputs advance-integer step through its scalar-integer yield", async () => {
  // Under the positive-path basis since the sanctioned dispatcher-decode
  // remediation: the ResolveInputs step dispatcher decodes the pending
  // descriptor once for both carrier predicates and derives the successor
  // work witness by splicing the pending tail instead of re-encoding the
  // whole control (semantic transaction 13,100,448 memory against the
  // 13,200,000 basis, down from 14,226,253).
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "resolveInputs",
        resolveInputsKind: "membershipStep",
        disputedMatchOrdinal: 12,
        outputDatumCbor: MULTI_YIELD_DATUM_CBOR,
      }),
  );
  // Dispute spend + stage yield + scalar-integer yield; the span yield is
  // gone from the step path — the step binds its redeemer window bytes to
  // the span-attach step's recorded commitment inline.
  expect(result.semanticMeasurement!.redeemerCount).toBeGreaterThanOrEqual(3);
  expectPositiveBasis(result, "resolve-inputs advance-integer step");
}, 300_000);

it("proves a script-sources advance-bytes step through its scalar-bytes yield", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "scriptSources",
        scriptSourcesSemanticIndex: 2,
        disputedMatchOrdinal: 31,
        outputDatumCbor: MULTI_YIELD_DATUM_CBOR,
      }),
  );
  // Dispute spend + stage yield + scalar-bytes yield.
  expect(result.semanticMeasurement!.redeemerCount).toBeGreaterThanOrEqual(3);
  expectPositiveBasis(result, "script-sources advance-bytes step");
}, 300_000);

it("refuses a forged span-attach successor against an honest trace", async () => {
  await expect(
    runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "scriptSources",
        scriptSourcesSemanticIndex: 2,
        disputedMatchOrdinal: 9,
        outputDatumCbor: MULTI_YIELD_DATUM_CBOR,
        dishonestChallenger: true,
      }),
    ),
  ).rejects.toThrow(/semantic-resolution failed: EvaluatorError/);
}, 300_000);

it("refuses a forged advance-integer successor against an honest trace", async () => {
  await expect(
    runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "resolveInputs",
        resolveInputsKind: "membershipStep",
        disputedMatchOrdinal: 12,
        outputDatumCbor: MULTI_YIELD_DATUM_CBOR,
        dishonestChallenger: true,
      }),
    ),
  ).rejects.toThrow(/semantic-resolution failed: EvaluatorError/);
}, 300_000);

it("refuses a forged advance-bytes successor against an honest trace", async () => {
  await expect(
    runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "scriptSources",
        scriptSourcesSemanticIndex: 2,
        disputedMatchOrdinal: 31,
        outputDatumCbor: MULTI_YIELD_DATUM_CBOR,
        dishonestChallenger: true,
      }),
    ),
  ).rejects.toThrow(/semantic-resolution failed: EvaluatorError/);
}, 300_000);

// One successor per attach step. Each adversary claims, from the honest
// pre-state, a successor the redeemer-chosen attach used to admit: a span
// window one byte before the demanded span (sized and digested as an honest
// window at that start), and leaf-summary facts committed together with a
// prover-chosen descriptor. The derived attach refuses both.
it.each([
  ["scriptSources", { scriptSourcesSemanticIndex: 2 }],
  ["resolveInputs", { resolveInputsKind: "membershipStep" as const }],
] as const)(
  "refuses a %s span-attach successor recording an off-demand window",
  async (disputedPhase, selector) => {
    await expect(
      runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase,
          ...selector,
          disputedMatchOrdinal: 9,
          outputDatumCbor: MULTI_YIELD_DATUM_CBOR,
          dishonestChallenger: true,
          ledgerOutputProofForgery: "offDemandSpanWindow",
        }),
      ),
    ).rejects.toThrow(/semantic-resolution failed: EvaluatorError/);
  },
  300_000,
);

it.each([
  ["scriptSources", { scriptSourcesSemanticIndex: 3 }],
  ["resolveInputs", { resolveInputsKind: "membershipFinalize" as const }],
] as const)(
  "refuses a %s leaf-summary attach successor with descriptor-bound facts",
  async (disputedPhase, selector) => {
    await expect(
      runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase,
          ...selector,
          disputedMatchOrdinal: 0,
          dishonestChallenger: true,
          ledgerOutputProofForgery: "descriptorBoundLeafFacts",
        }),
      ),
    ).rejects.toThrow(/semantic-resolution failed: EvaluatorError/);
  },
  300_000,
);

// One successor per head step. The nested `[1, 2, 3]` list of the multi-yield
// datum is its second head-sequence step (after the outer constructor).
// Every head takes no argument: the pushed frame is open-ended, its
// `expected_children` pinned to zero and its close left to the authenticated
// break, so the honest head proves and a successor recording a child count
// (what a head taking that count from the redeemer admitted) is refused.
const NESTED_HEAD_SEQUENCE = {
  ledgerOutputDatumAction: "headSequence",
  disputedMatchOrdinal: 1,
  outputDatumCbor: MULTI_YIELD_DATUM_CBOR,
} as const;

it.each([
  ["scriptSources", { scriptSourcesSemanticIndex: 2 }],
  ["resolveInputs", { resolveInputsKind: "membershipStep" as const }],
] as const)(
  "proves a %s nested datum head-sequence step through permanent proof and removal",
  async (disputedPhase, selector) => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase,
          ...selector,
          ...NESTED_HEAD_SEQUENCE,
        }),
    );
    expectPositiveBasis(result, `${disputedPhase} nested head-sequence step`);
  },
  300_000,
);

it.each([
  ["scriptSources", { scriptSourcesSemanticIndex: 2 }],
  ["resolveInputs", { resolveInputsKind: "membershipStep" as const }],
] as const)(
  "refuses a %s head-sequence successor recording an open frame child count",
  async (disputedPhase, selector) => {
    await expect(
      runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase,
          ...selector,
          ...NESTED_HEAD_SEQUENCE,
          dishonestChallenger: true,
          ledgerOutputProofForgery: "openFrameChildCount",
        }),
      ),
    ).rejects.toThrow(/semantic-resolution failed: EvaluatorError/);
  },
  300_000,
);
