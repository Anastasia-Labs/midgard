/** The validation-trace dispute runner with two threads of our case at the
 * same script-sources redeemer step, whose redeemer field is one byte over
 * the inline limit: both read our one publication of the field. One thread
 * is played to its award first, its read of the publication final; the
 * other has committed the publication's position among its reader's
 * reference inputs when its next move does not build for a while. The
 * publication is kept while that thread stands, so once its move builds it
 * reads the same publication, and the publication is spent back by key
 * only after both reads (carried-field.ts). */
import { beforeAll, describe, expect, it } from "vitest";

import type { ValidationTraceOneStepHolder } from "../src/validation-trace-dispute/one-step-holders.js";
import { VALIDATION_TRACE_SCRIPT_SOURCES_REDEEMER_INLINE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-phase-evidence.js";
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
