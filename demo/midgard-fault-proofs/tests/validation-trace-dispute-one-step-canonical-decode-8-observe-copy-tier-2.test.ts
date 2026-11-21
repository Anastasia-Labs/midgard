/** The validation-trace dispute runner at a canonical-decode step whose
 * field is longer than an inline carriage takes: the observe move reads our
 * publications of the field as reference inputs, and the thread plays
 * through to the award. The redeemer field one byte past the inline
 * carriage, after state 8, is published bytes-only; an address-witness
 * field, after state 7, around the longest one raw-datum publication holds
 * (15,000 bytes), the longest one publication of the field carriage holds
 * (15,148 bytes), and at the consensus bound on a field, past which it is
 * published under the field-preimage certificate; each publication is
 * spent back by key once the observe move is final
 * (validation-trace-dispute-one-step-canonical-decode-8-observe-copy.test.ts
 * covers fields an inline carriage takes). */
import {
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  copiesOf,
  expectReclaimedByKey,
  landed,
  ownReadsOf,
  readersOf,
  spenderOf,
} from "./support/validation-trace-dispute-observe-copies.js";
import { canonicalDecodeDrive } from "./support/validation-trace-dispute-one-step-canonical-decode-drive.js";
import { playThreads } from "./support/validation-trace-dispute-one-step-play.js";
import { redeemerDataForFieldPreimage } from "./support/validation-trace-dispute-redeemer-field.js";
import { reportFit } from "./support/validation-trace-dispute-scripted-defender.js";

/** The ledger's transaction size limit. */
const MAX_TX_SIZE = 16_384;

const OBSERVE = "Observe canonicalDecode item";

describe("validation-trace dispute runner: the canonical-decode step after state 8, its redeemer field one byte longer than an inline carriage takes", () => {
  const bytes = MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES + 1;
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

  it("publishes one bytes-only copy of the field, which the observe move reads without spending", () => {
    const { scene } = drive.dispute();
    const [copy, ...others] = copiesOf(scene, bytes);
    expect(others).toStrictEqual([]);
    const readers = readersOf(scene, copy!);
    expect(readers).toHaveLength(1);
    expect(readers[0]!.cbor.length / 2).toBe(drive.bytesOf(OBSERVE));
    reportFit(`${OBSERVE} (RawUtxo, ${bytes.toString()}-byte field)`, [
      readers[0]!.cbor,
    ]);
    const publish = landed(scene).find(
      ({ hash }) => hash === copy!.split("#")[0],
    )!;
    expect(publish.cbor.length / 2).toBeLessThanOrEqual(MAX_TX_SIZE);
    reportFit(`Publish field copy (${bytes.toString()}-byte field)`, [
      publish.cbor,
    ]);
  });

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});

/** An address witness in the field, and the field's bytes over `witnesses`
 * of them. */
const WITNESS_BYTES = 103;
const witnessFieldBytes = (witnesses: number) =>
  witnesses * WITNESS_BYTES + (witnesses < 256 ? 2 : 3);

describe.each([
  ["just within one raw-datum publication", 145],
  ["just past one raw-datum publication", 146],
  ["within five bytes of one field-carriage publication's largest", 147],
  ["just past one field-carriage publication", 148],
  ["at the consensus bound on a field", 318],
])(
  "validation-trace dispute runner: the canonical-decode step after state 7, its address-witness field %s",
  (_, witnesses) => {
    const bytes = witnessFieldBytes(witnesses);
    const drive = canonicalDecodeDrive({
      from: 7,
      kind: "item",
      at: "canonicalDecodeItemObserve",
      failingSigners: { extraSigners: witnesses - 1 },
      ...(bytes > 15_000 ? { certifies: true } : {}),
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

    it(`reads the field by reference: the observe move reads our publication${bytes > MIDGARD_CHUNK_BYTES_K ? "s" : ""} of its ${bytes.toString()} bytes, each published within the transaction size limit`, () => {
      const { scene } = drive.dispute();
      const reads = ownReadsOf(scene, drive.cborOf(OBSERVE));
      const chunks = reads.map(({ datumCbor }) =>
        Buffer.from(Data.from<string>(datumCbor), "hex"),
      );
      expect(chunks).toHaveLength(Math.ceil(bytes / MIDGARD_CHUNK_BYTES_K));
      expect(chunks.reduce((total, chunk) => total + chunk.length, 0)).toBe(
        bytes,
      );
      expect(drive.bytesOf(OBSERVE)).toBeLessThan(MIDGARD_CHUNK_BYTES_K);
      const publishes = landed(scene).filter(({ hash }) =>
        reads.some(({ outRef }) => outRef.startsWith(`${hash}#`)),
      );
      for (const { cbor } of publishes)
        expect(cbor.length / 2).toBeLessThanOrEqual(MAX_TX_SIZE);
      reportFit(`Publish field (${bytes.toString()}-byte field)`, [
        ...publishes.map(({ cbor }) => cbor),
      ]);
      reportFit(`${OBSERVE} (${bytes.toString()}-byte field)`, [
        drive.cborOf(OBSERVE),
      ]);
    });

    it("spends each publication back to our key address by key once the observe move is final", async () => {
      const dispute = drive.dispute();
      const { scene, runner, a, b } = dispute;
      const reads = ownReadsOf(scene, drive.cborOf(OBSERVE));
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
