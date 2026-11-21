/** The validation-trace dispute runner at the canonical-decode step after
 * state 8, the one item of the failing transaction's redeemer field at the
 * consensus bound on a field: the largest item the item route walks. The
 * field is past one publication, so the runner publishes it in chunks under
 * the field-preimage certificate, the observe move opens the item from
 * those publications (Certified), the on-chain stages derive the same
 * observation and proof our plan names, and the thread plays through to the
 * award
 * (validation-trace-dispute-one-step-canonical-decode-8-observe-copy-tier-2.test.ts
 * covers the redeemer field one publication carries, and the address-witness
 * field at the same bound). */
import {
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { Constr, Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  landed,
  ownReadsOf,
} from "./support/validation-trace-dispute-observe-copies.js";
import { canonicalDecodeDrive } from "./support/validation-trace-dispute-one-step-canonical-decode-drive.js";
import { redeemerDataForFieldPreimage } from "./support/validation-trace-dispute-redeemer-field.js";
import { reportFit } from "./support/validation-trace-dispute-scripted-defender.js";

/** The ledger's transaction size limit. */
const MAX_TX_SIZE = 16_384;

const OBSERVE = "Observe canonicalDecode item";

/** The field's one-item count header and its item's `59 LLLL` byte-string
 * header. */
const FIELD_HEADER_BYTES = 1;
const ITEM_HEADER_BYTES = 3;

describe("validation-trace dispute runner: the canonical-decode step after state 8, its one redeemer at the consensus bound on a field", () => {
  const bytes = MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES;
  const itemBytes = bytes - FIELD_HEADER_BYTES - ITEM_HEADER_BYTES;
  const drive = canonicalDecodeDrive({
    from: 8,
    kind: "item",
    at: "canonicalDecodeItemObserve",
    failingRedeemerData: redeemerDataForFieldPreimage(bytes),
    certifies: true,
  });

  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the item semantic resolver, committing the transition alone",
    drive.routes,
    300_000,
  );

  it("plans the item's observation and proof over the whole field: one item, its length, and the field's length past it", async () => {
    const { canonicalDecode } = await drive.plan();
    const item = canonicalDecode.item!;
    expect(item.carried?.fieldIndex).toBe(8);
    expect(item.carried?.preimage.length).toBe(bytes);
    expect(Data.from(item.observationCbor)).toStrictEqual(
      new Constr(0, [1n, BigInt(itemBytes), new Constr(1, [])]),
    );
    expect(Data.from(item.proofCbor)).toStrictEqual(
      new Constr(0, [1n, new Constr(1, []), BigInt(bytes)]),
    );
  }, 300_000);

  it(
    "moves the thread through the step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it("observes the item from our publications of the field under its certificate, each published within the transaction size limit", () => {
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
    expect(publishes.length).toBeGreaterThan(0);
    for (const { cbor } of publishes)
      expect(cbor.length / 2).toBeLessThanOrEqual(MAX_TX_SIZE);
    reportFit(`Publish field (${bytes.toString()}-byte redeemer field)`, [
      ...publishes.map(({ cbor }) => cbor),
    ]);
    reportFit(`${OBSERVE} (Certified, ${bytes.toString()}-byte field)`, [
      drive.cborOf(OBSERVE),
    ]);
  });

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
