/** The validation-trace dispute runner at a phase step whose semantic
 * resolver reads one item of a field the prepare and semantic moves carry
 * whole, inline, with the field grown one unit at a time: by default an
 * item, or as the caller grows it. At the largest field within the phase's
 * inline limit the runner drives the step to the award, its prepare move
 * within the transaction size limit less its reserve even when our wallet
 * pays it, and one more unit would pass it. One unit over the limit, and at
 * the inline carriage's largest, it does not drive the step, and the thread
 * waits at ReadyForOneStep under the one-step-required alarm
 * (validation-trace-dispute-one-step-phase-drive.ts). */
import { beforeAll, describe, expect, it } from "vitest";

import type { ValidationTracePhase } from "../../src/validation-trace-dispute/one-step-phase.js";
import { VALIDATION_TRACE_ONE_STEP_REQUIRED } from "../../src/validation-trace-dispute/threads.js";
import { validationTraceRunState } from "../../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  defendTwoThreads,
  type TwoThreadCase,
} from "./validation-trace-dispute-one-step-case.js";
import {
  phaseDrive,
  type PhaseEvidence,
} from "./validation-trace-dispute-one-step-phase-drive.js";
import {
  isPendingFor,
  playThreads,
  threadPlace,
} from "./validation-trace-dispute-one-step-play.js";
import { threadOf } from "./validation-trace-dispute-scripted-defender.js";

/** The ledger's transaction size limit, and the reserve a move keeps under
 * it. */
const MAX_TX_SIZE = 16_384;
const SIZE_RESERVE = 512;

/** The tier-1 inline carriage limit of a field's preimage. */
const TIER1_MAX_PREIMAGE_BYTES = 14_336;

/** The bytes of a field's CBOR array header over `items` items. */
const headerBytes = (items: number) => (items < 24 ? 1 : items < 256 ? 2 : 3);

type Installs = Pick<
  Parameters<typeof defendTwoThreads>[0],
  | "failingReferenceInputs"
  | "failingSigners"
  | "failingScript"
  | "failingRedeemerData"
>;

/** How the field grows: an item of `itemBytes` bytes at a time, or a
 * `unit` at a time, its preimage `fieldBytes(units)` bytes at `units`
 * units. */
type Growth =
  | { readonly itemBytes: number }
  | {
      readonly unit: string;
      readonly fieldBytes: (units: number) => number;
    };

/**
 * The rows of a step of `phase` whose semantic resolver `semantic` (held at
 * `at`) reads an item of a field that grows as `growth` says: the failing
 * transaction `installs(units)` carries the field at `units` units, our
 * trace reads the item at the step after its state `stepOf(units)`
 * (`evidence`), and the phase's inline limit is `limit` bytes. `field`
 * names the field in the rows.
 */
export const describeInlineCap = ({
  field,
  phase,
  semantic,
  at,
  evidence,
  limit,
  stepOf,
  installs,
  ...growth
}: {
  readonly field: string;
  readonly phase: ValidationTracePhase;
  readonly semantic: number;
  readonly at: Parameters<typeof phaseDrive>[0]["at"];
  readonly evidence: (units: number) => PhaseEvidence;
  readonly limit: number;
  readonly stepOf: (units: number) => number;
  readonly installs: (units: number) => Installs;
} & Growth) => {
  const { unit, fieldBytes } =
    "itemBytes" in growth
      ? {
          unit: "item",
          fieldBytes: (items: number) =>
            items * growth.itemBytes + headerBytes(items),
        }
      : growth;
  const fits = (items: number) => fieldBytes(items) <= limit;
  let largest = 1;
  while (fits(largest + 1)) largest += 1;
  let tier1 = largest;
  while (fieldBytes(tier1 + 1) <= TIER1_MAX_PREIMAGE_BYTES) tier1 += 1;
  const prepareLabel = `PrepareSelected ${phase}`;
  const verifyLabel = `Verify ${phase} ${semantic.toString()}`;

  describe(`validation-trace dispute runner: a step of ${phase}, its ${field} at the inline ${phase} limit`, () => {
    // A raised per-byte fee prices the prepare move over the per-move cap,
    // so our wallet pays it: its largest shape, with the wallet's input and
    // change output.
    const drive = phaseDrive({
      from: stepOf(largest),
      phase,
      semantic,
      evidence: evidence(largest),
      at,
      minFeeA: 80,
      ...installs(largest),
    });

    beforeAll(drive.setUp, 1_800_000);

    it(
      "routes the step to the semantic resolver, committing the transition beside the item and its field",
      drive.routes,
      300_000,
    );

    it(
      "moves the thread through the step's resolution to the award, and closes it",
      drive.awards,
      1_200_000,
    );

    it(`lands the prepare move within the transaction size limit less its reserve, and one more ${unit} would pass it`, () => {
      const bytes = drive.bytesOf(prepareLabel);
      expect(bytes).toBeLessThanOrEqual(MAX_TX_SIZE - SIZE_RESERVE);
      expect(
        bytes + fieldBytes(largest + 1) - fieldBytes(largest),
      ).toBeGreaterThan(MAX_TX_SIZE - SIZE_RESERVE);
      expect(drive.bytesOf(verifyLabel)).toBeLessThanOrEqual(bytes);
    });

    it(
      "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
      drive.pays,
    );
  });

  /** Whether `result` waits under the one-step-required state on the
   * thread holding `unit`. */
  const waitsOn = (result: FraudProofWorkflowRunResult, unit: string) =>
    isPendingFor(result, VALIDATION_TRACE_ONE_STEP_REQUIRED) &&
    "reason" in result &&
    result.reason.includes(`thread ${unit} at `);

  describe.each([
    [`one ${unit} over the inline ${phase} limit`, largest + 1],
    ["at the inline carriage's largest", tier1],
  ])(
    `validation-trace dispute runner: a step of ${phase}, its ${field} %s`,
    (_, items) => {
      let dispute: TwoThreadCase;
      let waited: FraudProofWorkflowRunResult;

      beforeAll(async () => {
        dispute = await defendTwoThreads({
          withRemoval: false,
          successorFrom: stepOf(items),
          phases: true,
          ...installs(items),
        });
        const { scene, runner, a, b } = dispute;
        ({ result: waited } = await playThreads({
          scene,
          runner,
          sides: [a, b],
          until: async (run) => waitsOn(run, b.unit),
          limit: 80,
        }));
      }, 1_800_000);

      it("does not drive the step: the thread waits at ReadyForOneStep under the one-step-required alarm, naming the limit", async () => {
        const { scene, deployment, b } = dispute;
        const place = await threadPlace(scene, deployment, b.unit);
        expect(place!.holder).toBe("game");
        expect(threadOf(place!.utxo).turn).toBe("ReadyForOneStep");
        expect(threadOf(place!.utxo).low_index).toBe(BigInt(stepOf(items)));
        const state = validationTraceRunState(waited);
        expect("namedState" in state && state.namedState?.alarm).toBe(true);
        expect("reason" in waited && waited.reason).toContain(
          `a ${phase} item of a ${fieldBytes(items).toString()}-byte field, over the ${limit.toString()}-byte inline ${phase} limit`,
        );
      }, 300_000);

      it("never enters the step's resolution", async () => {
        const { scene, deployment, runner, prepared, a, b } = dispute;
        const before = await threadPlace(scene, deployment, b.unit);
        await playThreads({
          scene,
          runner,
          sides: [a, b],
          until: async (run) => waitsOn(run, b.unit),
          limit: 1,
        });
        const after = await threadPlace(scene, deployment, b.unit);
        expect(after!.utxo.txHash).toBe(before!.utxo.txHash);
        expect(after!.utxo.outputIndex).toBe(before!.utxo.outputIndex);
        // Our other thread parts from ours at a step of another phase, which
        // the runner may resolve; this thread's place is never spent into one.
        expect(
          prepared.filter(
            ({ stage, consumes }) =>
              stage === "enter_resolution" &&
              consumes ===
                `${before!.utxo.txHash}#${before!.utxo.outputIndex.toString()}`,
          ),
        ).toStrictEqual([]);
      }, 300_000);
    },
  );
};
