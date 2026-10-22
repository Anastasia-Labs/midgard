/** The validation-trace dispute runner at the canonical-decode step after
 * state 8, an item of the failing transaction's redeemer field. At the
 * inline observe limit the observe move carries the whole field preimage
 * inline, within the transaction size limit less its reserve even when our
 * wallet pays it, and nothing else is published. A longer field, from one
 * byte over that limit to the inline carriage's largest, is first published
 * as a bytes-only copy at our key address; the observe move reads the copy
 * as a reference input, and once the move is final at release depth the
 * copy is spent back by key; a copy spent before the move lands is
 * published again under the prover funding permit
 * (validation-trace-dispute-funded-one-step-canonical-decode-copy.test.ts).
 * A one-step move that does not build raises the one-step-required alarm,
 * and is built again once it can be
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
import {
  blockOf,
  copiesOf,
  expectReclaimedByKey,
  landed,
  outRefsOf,
  readersOf,
  spenderOf,
} from "./support/validation-trace-dispute-observe-copies.js";
import { canonicalDecodeDrive } from "./support/validation-trace-dispute-one-step-canonical-decode-drive.js";
import {
  liveCase,
  type TwoThreadCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import {
  playThreads,
  threadPlace,
} from "./support/validation-trace-dispute-one-step-play.js";
import { redeemerDataForFieldPreimage } from "./support/validation-trace-dispute-redeemer-field.js";
import { reportFit } from "./support/validation-trace-dispute-scripted-defender.js";

/** The ledger's transaction size limit, and the reserve a move keeps under
 * it. */
const MAX_TX_SIZE = 16_384;
const SIZE_RESERVE = 512;

const LIMIT = VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES;
const OBSERVE = "Observe canonicalDecode item";

/** Plays both threads until `done` holds of the chain. */
const playUntil = async (
  dispute: TwoThreadCase,
  done: () => Promise<boolean> | boolean,
  limit = 40,
) => {
  const { scene, runner, a, b } = dispute;
  await playThreads({
    scene,
    runner,
    sides: [a, b],
    until: async () => await done(),
    limit,
  });
};

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
    expect(drive.bytesOf(OBSERVE)).toBe(MAX_TX_SIZE - SIZE_RESERVE);
  });

  it("publishes no copy of the field", () => {
    expect(copiesOf(drive.dispute().scene, LIMIT)).toStrictEqual([]);
  });

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});

describe("validation-trace dispute runner: the canonical-decode step after state 8, its redeemer field at the inline carriage's largest", () => {
  const bytes = MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES;
  const drive = canonicalDecodeDrive({
    from: 8,
    kind: "item",
    at: "canonicalDecodeItemObserve",
    failingRedeemerData: redeemerDataForFieldPreimage(bytes),
  });

  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the item semantic resolver, committing the transition alone",
    drive.routes,
    300_000,
  );

  it(
    "moves the thread through the step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it("publishes one bytes-only copy of the field at our key address, which the observe move reads without spending", () => {
    const dispute = drive.dispute();
    const [copy, ...others] = copiesOf(dispute.scene, bytes);
    expect(others).toStrictEqual([]);
    const readers = readersOf(dispute.scene, copy!);
    expect(readers).toHaveLength(1);
    const [reader] = readers;
    expect(outRefsOf(reader!.tx.body().inputs())).not.toContain(copy);
    expect(reader!.cbor.length / 2).toBe(drive.bytesOf(OBSERVE));
    expect(drive.bytesOf(OBSERVE)).toBeLessThan(bytes);
    reportFit(`${OBSERVE} (RawUtxo, ${bytes.toString()}-byte field)`, [
      reader!.cbor,
    ]);
    const publish = landed(dispute.scene).find(
      ({ hash }) => hash === copy!.split("#")[0],
    )!;
    expect(publish.cbor.length / 2).toBeLessThanOrEqual(MAX_TX_SIZE);
    reportFit(`Publish field copy (${bytes.toString()}-byte field)`, [
      publish.cbor,
    ]);
  });

  it("spends the copy back to our key address by key once the observe move is final", async () => {
    const dispute = drive.dispute();
    const [copy] = copiesOf(dispute.scene, bytes);
    await playUntil(
      dispute,
      () => spenderOf(dispute.scene, copy!) !== undefined,
    );
    const spender = spenderOf(dispute.scene, copy!)!;
    expectReclaimedByKey(dispute.scene, spender);
    reportFit("Reclaim field copy", [spender.cbor]);
    const [reader] = readersOf(dispute.scene, copy!);
    expect(await blockOf(dispute.scene, spender.hash)).toBeGreaterThan(
      await blockOf(dispute.scene, reader!.hash),
    );
  }, 600_000);

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});

describe("validation-trace dispute runner: the canonical-decode step after state 8, its redeemer field one byte over the inline observe limit, its release-final point blocks below the tip", () => {
  const bytes = LIMIT + 1;
  /** How many blocks below the tip the release-final point stands. */
  const depth = 10;
  const drive = canonicalDecodeDrive({
    from: 8,
    kind: "item",
    at: "canonicalDecodeItemObserve",
    failingRedeemerData: redeemerDataForFieldPreimage(bytes),
    releaseDepth: depth,
  });

  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the item semantic resolver, committing the transition alone",
    drive.routes,
    300_000,
  );

  it(
    "moves the thread through the step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it("publishes one copy of the field, which the observe move reads", () => {
    const { scene } = drive.dispute();
    const [copy, ...others] = copiesOf(scene, bytes);
    expect(others).toStrictEqual([]);
    const readers = readersOf(scene, copy!);
    expect(readers).toHaveLength(1);
    expect(readers[0]!.cbor.length / 2).toBe(drive.bytesOf(OBSERVE));
  });

  it("spends the copy back by key only once the observe move is final at release depth", async () => {
    const dispute = drive.dispute();
    const { scene } = dispute;
    const [copy] = copiesOf(scene, bytes);
    await playUntil(dispute, () => spenderOf(scene, copy!) !== undefined);
    const spender = spenderOf(scene, copy!)!;
    expectReclaimedByKey(scene, spender);
    const [reader] = readersOf(scene, copy!);
    expect(
      (await blockOf(scene, spender.hash)) -
        (await blockOf(scene, reader!.hash)),
    ).toBeGreaterThanOrEqual(depth);
  }, 600_000);

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
