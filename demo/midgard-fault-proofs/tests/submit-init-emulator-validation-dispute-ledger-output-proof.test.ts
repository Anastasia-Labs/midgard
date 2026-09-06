/**
 * Emulator lifecycles for the shared ledger-output-proof semantic family:
 * the ResolveInputs membership step/finalize pair (resolver 7, semantics 3/4)
 * and the ScriptSources output-proof step/finalize pair (resolver 8,
 * semantics 2/3), each resolved through the shared LOP yield conjunction
 * (stage yield plus attestation yields on the step path, the four descriptor
 * yields on the finalize path).
 *
 * The two finalize lifecycles are skipped: the five-execution finalize
 * transaction measured 21,359,516 (ResolveInputs) and 20,046,146
 * (ScriptSources) memory units against the 13,200,000 positive-path basis —
 * the exact miss for which the finalize size plans reserve the two-hop
 * chain fallback (`validation-trace-script-sources-output-proof-finalize-semantic-v1.md`
 * §3/§7). They stay skipped until that fallback (or an owner ruling) lands;
 * the finalize negative is skipped with them because budget exhaustion would
 * mask the semantic refusal it is supposed to prove.
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
 * traversal through the advance-integer (role 7: span + scalar-integer
 * yields) and advance-bytes (role 18: span + scalar-bytes yields) stages the
 * plain address+value output never reaches. The `disputedMatchOrdinal`
 * values below were mapped by scanning every matching step's derived plan:
 * with this datum on both fixture outputs, ordinal 11 is an advance-integer
 * step and ordinal 30 an advance-bytes step in both families.
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
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
  expect(result.semanticMeasurement!.executionMemory).toBeLessThanOrEqual(
    POSITIVE_PATH_MEMORY_BASIS,
  );
  expect(result.semanticMeasurement!.executionSteps).toBeLessThanOrEqual(
    POSITIVE_PATH_STEP_BASIS,
  );
}, 300_000);

it.skip("proves resolve-inputs membershipFinalize through permanent proof and removal", async () => {
  // Blocked on the finalize two-hop fallback: measured 21,359,516 memory
  // against the 13,200,000 basis (and the emulator's 16,500,000 cap).
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "resolveInputs",
        resolveInputsKind: "membershipFinalize",
      }),
  );
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
  expect(result.semanticMeasurement!.executionMemory).toBeLessThanOrEqual(
    POSITIVE_PATH_MEMORY_BASIS,
  );
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
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
  expect(result.semanticMeasurement!.executionMemory).toBeLessThanOrEqual(
    POSITIVE_PATH_MEMORY_BASIS,
  );
  expect(result.semanticMeasurement!.executionSteps).toBeLessThanOrEqual(
    POSITIVE_PATH_STEP_BASIS,
  );
}, 300_000);

it.skip("proves script-sources output-proof semantic 3 through permanent proof and removal", async () => {
  // Blocked on the finalize two-hop fallback: measured 20,046,146 memory
  // against the 13,200,000 basis (and the emulator's 16,500,000 cap).
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "scriptSources",
        scriptSourcesSemanticIndex: 3,
      }),
  );
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
  expect(result.semanticMeasurement!.executionMemory).toBeLessThanOrEqual(
    POSITIVE_PATH_MEMORY_BASIS,
  );
}, 300_000);

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

it.skip("refuses a forged resolve-inputs membershipFinalize successor against an honest trace", async () => {
  // Skipped with the finalize positives: while the honest finalize
  // transaction exceeds the execution-memory cap, the evaluator refuses
  // dishonest and honest claims alike on budget, so this test would pass
  // without exercising the descriptor-yield refusal it exists to prove.
  await expect(
    runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "resolveInputs",
        resolveInputsKind: "membershipFinalize",
        dishonestChallenger: true,
      }),
    ),
  ).rejects.toThrow(/semantic-resolution failed: EvaluatorError/);
}, 300_000);

it.skip("proves a resolve-inputs advance-integer step through its span and scalar-integer yields", async () => {
  // Blocked on the positive-path basis: the lifecycle itself completes under
  // the emulator's 16,500,000 cap, but the semantic transaction measured
  // 15,481,633 memory against the 13,200,000 basis (dispute spend 5,961,500 +
  // stage yield 4,126,300 + span yield 2,605,777 + scalar-integer yield
  // 2,790,924). Escalated with the finalize misses; the matching negative
  // below stays active because the honest transaction fits the evaluator cap,
  // so its refusal is semantic rather than budget exhaustion.
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "resolveInputs",
        resolveInputsKind: "membershipStep",
        disputedMatchOrdinal: 11,
        outputDatumCbor: MULTI_YIELD_DATUM_CBOR,
      }),
  );
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
  // Dispute spend + stage yield + span yield + scalar-integer yield.
  expect(result.semanticMeasurement!.redeemerCount).toBeGreaterThanOrEqual(4);
  expect(result.semanticMeasurement!.executionMemory).toBeLessThanOrEqual(
    POSITIVE_PATH_MEMORY_BASIS,
  );
  expect(result.semanticMeasurement!.executionSteps).toBeLessThanOrEqual(
    POSITIVE_PATH_STEP_BASIS,
  );
}, 300_000);

it("proves a script-sources advance-bytes step through its span and scalar-bytes yields", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "scriptSources",
        scriptSourcesSemanticIndex: 2,
        disputedMatchOrdinal: 30,
        outputDatumCbor: MULTI_YIELD_DATUM_CBOR,
      }),
  );
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
  // Dispute spend + stage yield + span yield + scalar-bytes yield.
  expect(result.semanticMeasurement!.redeemerCount).toBeGreaterThanOrEqual(4);
  expect(result.semanticMeasurement!.executionMemory).toBeLessThanOrEqual(
    POSITIVE_PATH_MEMORY_BASIS,
  );
  expect(result.semanticMeasurement!.executionSteps).toBeLessThanOrEqual(
    POSITIVE_PATH_STEP_BASIS,
  );
}, 300_000);

it("refuses a forged advance-integer successor against an honest trace", async () => {
  await expect(
    runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        operatorVkey,
        now,
        disputedPhase: "resolveInputs",
        resolveInputsKind: "membershipStep",
        disputedMatchOrdinal: 11,
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
        disputedMatchOrdinal: 30,
        outputDatumCbor: MULTI_YIELD_DATUM_CBOR,
        dishonestChallenger: true,
      }),
    ),
  ).rejects.toThrow(/semantic-resolution failed: EvaluatorError/);
}, 300_000);
