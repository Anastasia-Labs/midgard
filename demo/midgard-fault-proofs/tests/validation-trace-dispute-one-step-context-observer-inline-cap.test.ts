/** The validation-trace dispute runner at the CEK context step that opens
 * the first observer the fold takes of a failing transaction requiring more
 * and more native scripts it witnesses, a field of 28-byte script hashes.
 * At the largest field within the inline CEK observer limit the context
 * moves carry it whole, inline, and the runner drives the step to the
 * award, its longest move within the transaction size limit less its
 * reserve even when our wallet pays it, and within two observers' bytes of
 * it: what the moves carry beside the field varies from one observer count
 * to the next, and one more observer than the limit carries more.
 * Past the limit the runner publishes the field at our key address first
 * (past one publication, under the field-preimage certificate); the
 * observer authenticate opens it from them, the observer fold and the
 * context settle read them too, and each publication is spent back by key
 * once the thread has left them behind
 * (support/validation-trace-dispute-one-step-context-observer-drive.ts). */
import {
  decodeMidgardFieldPreimage,
  MIDGARD_CHUNK_BYTES_K,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_CEK_OBSERVER_INLINE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-plan-carried.js";
import {
  expectReclaimedByKey,
  landed,
  ownReadsOf,
  spenderOf,
} from "./support/validation-trace-dispute-observe-copies.js";
import {
  contextObserverDrive,
  observedTrace,
  observerItemOpening,
  observerItemStepFrom,
} from "./support/validation-trace-dispute-one-step-context-observer-drive.js";
import { playThreads } from "./support/validation-trace-dispute-one-step-play.js";
import { reportFit } from "./support/validation-trace-dispute-scripted-defender.js";

/** The ledger's transaction size limit, and the reserve a move keeps under
 * it. */
const MAX_TX_SIZE = 16_384;
const SIZE_RESERVE = 512;

/** The longest field our raw-datum publication carries in one output. */
const RAW_PUBLICATION_MAX_BYTES = 15_000;

/** A 28-byte script hash in the field, and the field's bytes over
 * `observers` of them. */
const ITEM_BYTES = 30;
const fieldBytes = (observers: number) =>
  observers * ITEM_BYTES + (observers < 24 ? 1 : observers < 256 ? 2 : 3);

/** The bytes a move carries a field of `bytes` bytes in, inline: a Plutus
 * Data byte string, in 64-byte chunks past 64 bytes. */
const carriedBytes = (bytes: number) =>
  bytes <= 64 ? bytes + 2 : bytes + 2 + 2 * Math.ceil(bytes / 64);

let largest = 1;
while (
  fieldBytes(largest + 1) <=
  VALIDATION_TRACE_CEK_OBSERVER_INLINE_MAX_PREIMAGE_BYTES
)
  largest += 1;

/** The moves of the context step's resolution that may carry the field. */
const CARRIERS = [
  "PrepareSelected context",
  "ContextBound",
  "ContextStaged",
  "ObserverPending",
  "ContextVerified observerFold",
  "ContextSettle",
] as const;

/** The moves that read our publications of the field, once it is carried
 * by reference: the observer authenticate's, which opens it, and the two
 * past it. */
const READERS = [
  "ObserverPending",
  "ContextVerified observerFold",
  "ContextSettle",
] as const;

const driveOf = (observers: number) =>
  contextObserverDrive({
    from: observerItemStepFrom(observers),
    installs: {
      ...observedTrace(observers),
      ...(fieldBytes(observers) > RAW_PUBLICATION_MAX_BYTES
        ? { certifies: true }
        : {}),
    },
    opening: observerItemOpening(observers),
    // A raised per-byte fee prices the longest moves over the per-move cap,
    // so our wallet pays them: their largest shape.
    minFeeA: 80,
  });

describe("validation-trace dispute runner: a CEK context step opening a required observer, its field at the inline CEK observer limit", () => {
  const drive = driveOf(largest);

  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the observer authenticate with the observer the fold takes first",
    drive.routes,
    300_000,
  );

  it(
    "moves the thread through the observer fold and the context step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it("lands its longest move within the transaction size limit less its reserve, within two observers' bytes of it, reading no publication", () => {
    const longest = Math.max(...CARRIERS.map(drive.bytesOf));
    expect(longest).toBeLessThanOrEqual(MAX_TX_SIZE - SIZE_RESERVE);
    expect(
      longest +
        carriedBytes(fieldBytes(largest + 2)) -
        carriedBytes(fieldBytes(largest)),
    ).toBeGreaterThan(MAX_TX_SIZE - SIZE_RESERVE);
    for (const label of READERS)
      expect(
        ownReadsOf(drive.dispute().scene, drive.cborOf(label)),
      ).toStrictEqual([]);
  });

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});

describe.each([
  ["one observer over the inline CEK observer limit", largest + 1],
  [
    "past one publication's bytes",
    Math.floor(MIDGARD_CHUNK_BYTES_K / ITEM_BYTES) + 1,
  ],
])(
  "validation-trace dispute runner: a CEK context step opening a required observer, its field %s",
  (_, observers) => {
    const bytes = fieldBytes(observers);
    const drive = driveOf(observers);

    beforeAll(drive.setUp, 1_800_000);

    it(
      "routes the step to the observer authenticate with the observer the fold takes first, beside how many there are",
      drive.routes,
      300_000,
    );

    it(
      "moves the thread through the observer fold and the context step's resolution to the award, and closes it",
      drive.awards,
      1_200_000,
    );

    it(`reads the field by reference: the observer authenticate, the fold and the settle read our publication${bytes > MIDGARD_CHUNK_BYTES_K ? "s" : ""} of its ${bytes.toString()} bytes, and no move carries it`, () => {
      const { scene } = drive.dispute();
      const [opener, ...others] = READERS.map((label) =>
        ownReadsOf(scene, drive.cborOf(label)),
      );
      for (const reads of others) expect(reads).toStrictEqual(opener);
      const chunks = opener!.map(({ datumCbor }) =>
        Buffer.from(Data.from<string>(datumCbor), "hex"),
      );
      expect(chunks).toHaveLength(Math.ceil(bytes / MIDGARD_CHUNK_BYTES_K));
      expect(chunks.reduce((total, chunk) => total + chunk.length, 0)).toBe(
        bytes,
      );
      if (chunks.length === 1)
        expect(decodeMidgardFieldPreimage(chunks[0]!)).toHaveLength(observers);
      for (const label of CARRIERS) {
        expect(drive.bytesOf(label)).toBeLessThan(bytes);
        if (!(READERS as readonly string[]).includes(label))
          expect(ownReadsOf(scene, drive.cborOf(label))).toStrictEqual([]);
      }
      const publishes = landed(scene).filter(({ hash }) =>
        opener!.some(({ outRef }) => outRef.startsWith(`${hash}#`)),
      );
      for (const { cbor } of publishes)
        expect(cbor.length / 2).toBeLessThanOrEqual(MAX_TX_SIZE);
      reportFit(
        `Publish observer field (${bytes.toString()} bytes)`,
        publishes.map(({ cbor }) => cbor),
      );
    });

    it("spends each publication back to our key address by key once the thread has left them behind", async () => {
      const dispute = drive.dispute();
      const { scene, runner, a, b } = dispute;
      const reads = ownReadsOf(scene, drive.cborOf("ObserverPending"));
      await playThreads({
        scene,
        runner,
        sides: [a, b],
        until: async () =>
          reads.every(({ outRef }) => spenderOf(scene, outRef) !== undefined),
        limit: 40,
      });
      for (const { outRef } of reads)
        expectReclaimedByKey(scene, spenderOf(scene, outRef)!);
    }, 600_000);

    it(
      "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
      drive.pays,
    );
  },
);
