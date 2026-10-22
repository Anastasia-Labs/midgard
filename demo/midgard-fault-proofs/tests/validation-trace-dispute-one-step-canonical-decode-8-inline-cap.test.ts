/** The validation-trace dispute runner at the canonical-decode step after
 * state 8, an item of the failing transaction's redeemer field, whose
 * observe move carries the whole field preimage inline. At the inline
 * observe limit the runner drives the step to the award, its observe move
 * within the transaction size limit less its reserve even when our wallet
 * pays it; one byte over the limit, and at the inline carriage's largest,
 * it does not drive the step, and the thread waits at ReadyForOneStep under
 * the one-step-required alarm. A one-step move that does not build raises
 * that alarm too, and is built again once it can be
 * (support/validation-trace-dispute-one-step-canonical-decode-drive.ts). */
import { MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES } from "@al-ft/midgard-core/codec/native-tx-field-access";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-canonical-decode.js";
import {
  VALIDATION_TRACE_ONE_STEP_REQUIRED,
  validationTraceCaseExpiryMs,
} from "../src/validation-trace-dispute/threads.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { canonicalDecodeDrive } from "./support/validation-trace-dispute-one-step-canonical-decode-drive.js";
import {
  defendTwoThreads,
  liveCase,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import {
  isPendingFor,
  playThreads,
  threadPlace,
} from "./support/validation-trace-dispute-one-step-play.js";
import { redeemerDataForFieldPreimage } from "./support/validation-trace-dispute-redeemer-field.js";
import { threadOf } from "./support/validation-trace-dispute-scripted-defender.js";

/** The ledger's transaction size limit, and the reserve a move keeps under
 * it. */
const MAX_TX_SIZE = 16_384;
const SIZE_RESERVE = 512;

const LIMIT = VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES;

describe("validation-trace dispute runner: the canonical-decode step after state 8, its redeemer field at the inline observe limit", () => {
  // A raised per-byte fee prices the observe move over the per-move cap, so
  // our wallet pays it: its largest shape, with the wallet's input and
  // change output.
  const drive = canonicalDecodeDrive({
    from: 8,
    kind: "item",
    at: "canonicalDecodeItemObserve",
    minFeeA: 80,
    failingRedeemerData: redeemerDataForFieldPreimage(LIMIT),
  });

  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the item semantic resolver, committing the transition alone",
    drive.routes,
    300_000,
  );

  it("raises the one-step alarm, naming the thread, the case and the deadline, when the observe move does not build", async () => {
    const dispute = drive.dispute();
    const place = await threadPlace(
      dispute.scene,
      dispute.deployment,
      dispute.b.unit,
    );
    const caseDatum = Data.from((await liveCase(dispute)).datum!, SDK.Case);
    const reason = await drive.unbuilt(MAX_TX_SIZE - 2 * SIZE_RESERVE);
    expect(reason).toContain(
      `${VALIDATION_TRACE_ONE_STEP_REQUIRED}: the resolve move on thread ${place!.utxo.txHash}#${place!.utxo.outputIndex.toString()} of case ${dispute.caseUnit} does not build, and must land by ${validationTraceCaseExpiryMs(caseDatum).toString()}: `,
    );
  }, 600_000);

  it(
    "moves the thread through the step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it("lands the observe move at the transaction size limit less its reserve: a byte longer field would pass it", () => {
    expect(drive.bytesOf("Observe canonicalDecode item")).toBe(
      MAX_TX_SIZE - SIZE_RESERVE,
    );
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
  ["one byte over the inline observe limit", LIMIT + 1],
  [
    "at the inline carriage's largest",
    MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  ],
])(
  "validation-trace dispute runner: the canonical-decode step after state 8, its redeemer field %s",
  (_, bytes) => {
    let dispute: TwoThreadCase;
    let waited: FraudProofWorkflowRunResult;

    beforeAll(async () => {
      dispute = await defendTwoThreads({
        withRemoval: false,
        successorFrom: 8,
        canonicalDecode: true,
        failingRedeemerData: redeemerDataForFieldPreimage(bytes),
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
      expect(threadOf(place!.utxo).low_index).toBe(8n);
      const state = validationTraceRunState(waited);
      expect("namedState" in state && state.namedState?.alarm).toBe(true);
      expect("reason" in waited && waited.reason).toContain(
        `a canonical-decode item of a ${bytes.toString()}-byte field, over the ${LIMIT.toString()}-byte inline observe limit`,
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
