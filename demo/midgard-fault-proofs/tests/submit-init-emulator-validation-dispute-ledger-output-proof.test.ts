/**
 * Emulator lifecycles for the shared ledger-output-proof semantic family:
 * the ResolveInputs membership step/finalize pair (resolver 7, semantics 3/4)
 * and the ScriptSources output-proof step/finalize pair (resolver 8,
 * semantics 2/3), each resolved through the shared LOP yield conjunction
 * (stage yield plus scalar attestation yields on the step path, the attach
 * group's descriptor yields on the finalize path).
 *
 * The monolithic five-execution finalize was restructured into three
 * descriptor fact-attach steps and a thin terminal: each finalize-shaped
 * machine step attaches one fact group (`[[2, 3], [0], [1]]` — datum+value
 * summaries, then scan facts, then reference script) with only that group's
 * descriptor yields in the transaction, and the terminal carries no
 * descriptor yields at all — it requires all four recorded facts to match
 * the redeemer's descriptor exactly. Likewise the per-step span yield was
 * replaced by a single span-attach step (stage role 23) that records the
 * window commitment every later consumer binds its redeemer bytes to. Every
 * positive path below is asserted under the 13,200,000-memory /
 * 8,000,000,000-step basis the fit ledger enforces.
 */
import { Constr, Data } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  buildForgedOperatorSuccessorValidationDisputeFixture,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

const POSITIVE_PATH_MEMORY_BASIS = 13_200_000n;
const POSITIVE_PATH_STEP_BASIS = 8_000_000_000n;

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
  expect(result.semanticMeasurement!.executionMemory).toBeLessThanOrEqual(
    POSITIVE_PATH_MEMORY_BASIS,
  );
  expect(result.semanticMeasurement!.executionSteps).toBeLessThanOrEqual(
    POSITIVE_PATH_STEP_BASIS,
  );
};

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
// machine order: the datum+value summary attach, the scan-facts attach, the
// reference-script attach and the thin terminal (no descriptor yields).
it.each([
  [0, "datum+value summary attach [2,3]"],
  [1, "scan-facts attach [0]"],
  [2, "reference-script attach [1]"],
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
  [1, "scan-facts attach [0]"],
  [2, "reference-script attach [1]"],
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

// The thin terminal requires all four recorded facts to match the redeemer's
// descriptor exactly: a forged terminal claim dies at the facts/authorization
// conjunction with no descriptor yields present to launder it.
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
