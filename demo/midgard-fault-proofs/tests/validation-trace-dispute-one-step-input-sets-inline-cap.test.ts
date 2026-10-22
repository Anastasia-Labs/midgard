/** The validation-trace dispute runner at an input-sets step whose item
 * belongs to the failing transaction's reference-input field, a field the
 * prepare and semantic moves carry whole, inline. A field of 40-byte items
 * grows 40 bytes at a time; at the largest that fits the inline input-sets
 * limit the runner drives the step to the award, its prepare move within the
 * transaction size limit less its reserve even when our wallet pays it, and
 * one more item would pass it. One item over the limit, and at the inline
 * carriage's largest, it does not drive the step, and the thread waits at
 * ReadyForOneStep under the one-step-required alarm
 * (support/validation-trace-dispute-one-step-phase-drive.ts). */
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_INPUT_SETS_INLINE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-phase.js";
import { VALIDATION_TRACE_ONE_STEP_REQUIRED } from "../src/validation-trace-dispute/threads.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  defendTwoThreads,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import { phaseDrive } from "./support/validation-trace-dispute-one-step-phase-drive.js";
import {
  isPendingFor,
  playThreads,
  threadPlace,
} from "./support/validation-trace-dispute-one-step-play.js";
import { threadOf } from "./support/validation-trace-dispute-scripted-defender.js";

/** The ledger's transaction size limit, and the reserve a move keeps under
 * it. */
const MAX_TX_SIZE = 16_384;
const SIZE_RESERVE = 512;

/** The bytes of one input-sets item, and of the field's length header past
 * 255 items. */
const ITEM_BYTES = 40;
const HEADER_BYTES = 3;

/** The tier-1 inline carriage limit of a field's preimage. */
const TIER1_MAX_PREIMAGE_BYTES = 14_336;

const LIMIT = VALIDATION_TRACE_INPUT_SETS_INLINE_MAX_PREIMAGE_BYTES;

/** The field of `items` items, and the trace state after which the
 * input-sets walk reads its last item first. */
const fieldBytes = (items: number) => items * ITEM_BYTES + HEADER_BYTES;
const stepOf = (items: number) => items + 10;

const LARGEST = Math.floor((LIMIT - HEADER_BYTES) / ITEM_BYTES);

describe("validation-trace dispute runner: an input-sets step, its reference-input field at the inline input-sets limit", () => {
  // A raised per-byte fee prices the prepare move over the per-move cap, so
  // our wallet pays it: its largest shape, with the wallet's input and
  // change output.
  const drive = phaseDrive({
    from: stepOf(LARGEST),
    phase: "inputSets",
    semantic: 1,
    evidence: { fieldIndex: 1, itemIndex: LARGEST - 1 },
    at: "inputSetsItemSemantic",
    minFeeA: 80,
    failingReferenceInputs: LARGEST,
  });

  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the item semantic resolver, committing the transition beside the item and its field",
    drive.routes,
    300_000,
  );

  it(
    "moves the thread through the step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it("lands the prepare move within the transaction size limit less its reserve, and one more item would pass it", () => {
    const bytes = drive.bytesOf("PrepareSelected inputSets");
    expect(bytes).toBeLessThanOrEqual(MAX_TX_SIZE - SIZE_RESERVE);
    expect(bytes + ITEM_BYTES).toBeGreaterThan(MAX_TX_SIZE - SIZE_RESERVE);
    expect(drive.bytesOf("Verify inputSets 1")).toBeLessThanOrEqual(bytes);
  });

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});

/** Whether `result` waits under the one-step-required state on the thread
 * holding `unit`. */
const waitsOn = (result: FraudProofWorkflowRunResult, unit: string) =>
  isPendingFor(result, VALIDATION_TRACE_ONE_STEP_REQUIRED) &&
  "reason" in result &&
  result.reason.includes(`thread ${unit} at `);

describe.each([
  ["one item over the inline input-sets limit", LARGEST + 1],
  [
    "at the inline carriage's largest",
    Math.floor((TIER1_MAX_PREIMAGE_BYTES - HEADER_BYTES) / ITEM_BYTES),
  ],
])(
  "validation-trace dispute runner: an input-sets step, its reference-input field %s",
  (_, items) => {
    let dispute: TwoThreadCase;
    let waited: FraudProofWorkflowRunResult;

    beforeAll(async () => {
      dispute = await defendTwoThreads({
        withRemoval: false,
        successorFrom: stepOf(items),
        phases: true,
        failingReferenceInputs: items,
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
        `a inputSets item of a ${fieldBytes(items).toString()}-byte field, over the ${LIMIT.toString()}-byte inline inputSets limit`,
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
      expect(
        prepared.filter(({ stage }) => stage === "enter_resolution"),
      ).toStrictEqual([]);
    }, 300_000);
  },
);
