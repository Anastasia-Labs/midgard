/** The validation-trace dispute runner with two threads of our case at the
 * same script-sources redeemer step, whose redeemer field is one byte over
 * the inline limit: both read our one publication of the field. One thread
 * is played to its award first, its read of the publication final; the
 * other has committed the publication's position among its reader's
 * reference inputs when its next move does not build for a while. The
 * publication is kept while that thread stands, so once its move builds it
 * reads the same publication, and the publication is spent back by key
 * only after both reads (carried-field.ts). While the runner cannot tell
 * which publications the standing thread reads, as its case history has not
 * yet seen that thread or the publication is not yet authenticated, it
 * keeps every one. */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import type { ValidationTraceCaseTransaction } from "../src/validation-trace-dispute/case-history.js";
import type { ValidationTraceOneStepHolder } from "../src/validation-trace-dispute/one-step-holders.js";
import { VALIDATION_TRACE_SCRIPT_SOURCES_REDEEMER_INLINE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-phase-evidence.js";
import { VALIDATION_TRACE_THREAD_COUNT_HOLD } from "../src/validation-trace-dispute/threads.js";
import { sceneCaseHistory } from "./support/validation-trace-dispute-case-history.js";
import {
  copiesOf,
  expectReclaimedByKey,
  landed,
  readersOf,
  spenderOf,
} from "./support/validation-trace-dispute-observe-copies.js";
import {
  defendTwoThreads,
  threadClosed,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import {
  playThreads,
  threadPlace,
  withLedgerParameters,
} from "./support/validation-trace-dispute-one-step-play.js";
import { redeemerDataForFieldPreimage } from "./support/validation-trace-dispute-redeemer-field.js";
import { makeRunner } from "./support/validation-trace-dispute-runner.js";

const BYTES =
  VALIDATION_TRACE_SCRIPT_SOURCES_REDEEMER_INLINE_MAX_PREIMAGE_BYTES + 1;

/** The state both threads' traces agree with ours up to. */
const STEP = 48;

/** The holder whose move reads the field by reference. */
const READER: ValidationTraceOneStepHolder =
  "scriptSourcesStageOneRedeemerSemantic";

/** A transaction size limit under which the reader's move does not build
 * and a spend back by key does. */
const SMALL_MAX_TX_SIZE = 1_000;

let dispute: TwoThreadCase;
let copy: string;
/** The thread still at the reader. */
let open: string;

const holderOf = async (unit: string) =>
  (await threadPlace(dispute.scene, dispute.deployment, unit))?.holder;

/** Whether `transaction` leaves an output carrying `unit`. */
const carries = (
  unit: string,
  { bodyCbor }: ValidationTraceCaseTransaction,
) => {
  const outputs = CML.TransactionBody.from_cbor_hex(bodyCbor).outputs();
  for (let index = 0; index < outputs.len(); index += 1)
    if (coreToTxOutput(outputs.get(index)).assets[unit] === 1n) return true;
  return false;
};

/** Our runner over the same journal, restarted against a source that lags:
 * a case history that has not yet seen the Defend opening the standing
 * thread, or a publication observer that does not yet authenticate our
 * publication though the provider shows it. */
const lagging = (lag: "thread" | "publication") => {
  const { scene, deployment, runner, caseUnit } = dispute;
  const authentic = sceneCaseHistory(scene);
  return makeRunner(scene, {
    deployment,
    journal: runner.journal,
    caseHistory: {
      transactions: async (input) => {
        const transactions = await authentic.transactions(input);
        return lag === "thread" && input.caseUnit === caseUnit
          ? transactions.filter((transaction) => !carries(open, transaction))
          : transactions;
      },
    },
    publications: (observer) => ({
      ...observer,
      observeExact: async (input) =>
        lag === "publication" && input.expectedOutRef === copy
          ? { kind: "not_found" }
          : await observer.observeExact(input),
    }),
  });
};

/** Runs the lagging runner a few blocks, each run waiting for the reason
 * `waits` matches; the publication stays unspent and the thread at the
 * reader. */
const holdsWhileLagging = async (
  lag: "thread" | "publication",
  waits: RegExp,
) => {
  const { scene } = dispute;
  const runner = lagging(lag);
  for (let run = 0; run < 3; run += 1) {
    expect(await runner.runOnce(scene.failing)).toMatchObject({
      kind: "pending",
      reason: expect.stringMatching(waits),
    });
  }
  expect(await holderOf(open)).toBe(READER);
  expect(readersOf(scene, copy)).toHaveLength(1);
  expect(spenderOf(scene, copy)).toBeUndefined();
};

describe("validation-trace dispute runner: two threads at one step whose field it carries by reference", () => {
  beforeAll(async () => {
    dispute = await defendTwoThreads({
      withRemoval: false,
      phases: true,
      operatorFrom: STEP,
      successorFrom: STEP,
      failingRedeemerData: redeemerDataForFieldPreimage(BYTES),
    });
    const { scene, runner, a, b } = dispute;
    // One thread is played to its award and closed; the other's evidence
    // names the publication, and it stands at the reader.
    await playThreads({
      scene,
      runner,
      sides: [a, b],
      until: async () => {
        const [atA, atB] = [await holderOf(a.unit), await holderOf(b.unit)];
        open = atA === undefined ? b.unit : a.unit;
        return (
          (atA === undefined && atB === READER) ||
          (atB === undefined && atA === READER)
        );
      },
      limit: 80,
    });
  }, 1_800_000);

  it("publishes the field once, and the closed thread's reader reads it", () => {
    const copies = copiesOf(dispute.scene, BYTES);
    expect(copies).toHaveLength(1);
    copy = copies[0]!;
    expect(readersOf(dispute.scene, copy)).toHaveLength(1);
  });

  it("keeps the publication while the other thread's reader does not build, though the first read of it is final", async () => {
    const { scene, runner, a, b } = dispute;
    let runs = 0;
    await withLedgerParameters(
      scene,
      { maxTxSize: SMALL_MAX_TX_SIZE },
      async () =>
        await playThreads({
          scene,
          runner,
          sides: [a, b],
          until: async () => (runs += 1) >= 4,
          limit: 4,
        }),
    );
    expect(await holderOf(open)).toBe(READER);
    expect(readersOf(scene, copy)).toHaveLength(1);
    expect(spenderOf(scene, copy)).toBeUndefined();
  }, 600_000);

  it("keeps the publication while its case history has not yet seen the standing thread, as the threads it finds differ from the case's count", async () => {
    await holdsWhileLagging(
      "thread",
      new RegExp(`^${VALIDATION_TRACE_THREAD_COUNT_HOLD}:`),
    );
  }, 600_000);

  it("keeps the publication while it is not yet authenticated, as the standing thread's reads of it cannot be resolved", async () => {
    await holdsWhileLagging(
      "publication",
      /is not authenticated on the current chain$/,
    );
  }, 600_000);

  it("moves the other thread through the reader over the same publication, to its award, and spends the publication back by key after both reads", async () => {
    const { scene, runner, a, b } = dispute;
    await playThreads({
      scene,
      runner,
      sides: [a, b],
      until: async () =>
        (await threadClosed(dispute, a.unit)) &&
        (await threadClosed(dispute, b.unit)) &&
        spenderOf(scene, copy) !== undefined,
      limit: 40,
    });
    expect(copiesOf(scene, BYTES)).toStrictEqual([copy]);
    const readers = readersOf(scene, copy);
    expect(readers).toHaveLength(2);
    const spender = spenderOf(scene, copy)!;
    expectReclaimedByKey(scene, spender);
    const order = landed(scene).map(({ hash }) => hash);
    for (const { hash } of readers)
      expect(order.indexOf(spender.hash)).toBeGreaterThan(order.indexOf(hash));
  }, 1_200_000);
});
