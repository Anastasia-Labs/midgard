import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardCekBlobChunk,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  hashMidgardCekProgramMaterialPreimage,
  materializeMidgardForcedTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import type { MidgardFieldCarriage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  carriageFieldPreimages,
  publishedProgramMaterialEntries,
  reconstructTxOrderMaterial,
} from "../src/forced-orders/index.js";

const transactionCbor = ({
  addrTxWitsPreimageCbor = EMPTY_CBOR_LIST,
  scriptTxWitsPreimageCbor = EMPTY_CBOR_LIST,
  outputFills = [0x11, 0x22],
}: {
  readonly addrTxWitsPreimageCbor?: Buffer;
  readonly scriptTxWitsPreimageCbor?: Buffer;
  /**
   * One 5 kB-datum output per fill byte. Two puts field 2 under §8.3's `K` and
   * therefore in tier 1/2 territory; four puts it above `K`, where §8.4's
   * partition makes tier 3 the *only* admissible carriage.
   */
  readonly outputFills?: readonly number[];
} = {}): Buffer =>
  encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: encodeCbor(
          outputFills.map((fill) =>
            encodeMidgardTxOutput({
              address: Buffer.concat([
                Buffer.from([0x60]),
                Buffer.alloc(28, fill),
              ]),
              value: { lovelace: 2_000_000n, assets: new Map() },
              datum: {
                kind: "inline",
                cbor: Buffer.from(
                  aikenSerialisedPlutusDataCborPreservingMapOrder(
                    encodeCbor(Buffer.alloc(5_000, fill)).toString("hex"),
                  ),
                  "hex",
                ),
              },
            }),
          ),
        ),
        fee: 0n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor: EMPTY_CBOR_LIST,
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        auxiliaryDataHash: EMPTY_NULL_ROOT,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor,
        scriptTxWitsPreimageCbor,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    }),
  );

const emptyTransactionCbor = (): Buffer =>
  encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: EMPTY_CBOR_LIST,
        fee: 0n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor: EMPTY_CBOR_LIST,
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        auxiliaryDataHash: EMPTY_NULL_ROOT,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    }),
  );

const payloadFor = (submittedTxCbor: Buffer): SDK.TxOrderPayload => {
  const material = SDK.deriveTxOrderMaterial({
    submittedTxCbor,
    owner: Buffer.alloc(28, 0x66),
  });
  return {
    tx_id: material.transactionId,
    transaction_commitment: material.transactionCommitment,
    submitted_source: material.submitted_source,
  };
};

/**
 * Rebuilds an order's transaction the way ingestion does: the carriage
 * yields the nine field preimages, each hashed against the payload's own
 * commitment, and the preimages rebuild the transaction. Reference input
 * `i` holds `referenceDatums[i]` (raw carriage as its publication datum).
 */
const reconstruct = (
  payload: SDK.TxOrderPayload,
  carriage: readonly MidgardFieldCarriage[] = [],
  referenceDatums: readonly (Uint8Array | null)[] = [],
): Buffer =>
  reconstructTxOrderMaterial({
    payload,
    fieldPreimages: carriageFieldPreimages({
      payload,
      carriage,
      referenceInputs: referenceDatums.map((_datum, index) => ({
        txHash: Buffer.alloc(32, index + 1),
        index,
      })),
      datumOf: (outRef) => {
        const bytes = referenceDatums[outRef.index];
        return bytes == null
          ? null
          : Buffer.from(SDK.fieldPreimagePublicationDatumCbor(bytes), "hex");
      },
    }),
  });

/**
 * The mint redeemer's carriage vector for an order, read back the way ingestion
 * receives it: positional over the non-empty fields in ascending field index.
 *
 * Built from `deriveTxOrderMaterial`'s own per-field plans rather than from a
 * hand-listed set of field indices, so the vector cannot drift out of the order
 * the on-chain walk reads the nine commitments in.
 */
const inlineCarriageFor = (
  material: SDK.TxOrderMaterial,
): readonly MidgardFieldCarriage[] =>
  material.carriage.map((field) => ({
    carriage: "Inline",
    preimage: field.preimage,
  }));

const FIELD_HASH_REFUSAL =
  /field preimage does not match the committed field hash/u;

describe("V1 tx-order §8 field carriage", () => {
  it("reconstructs a canonically-empty forced order, which consumes no carriage", () => {
    const submittedTxCbor = emptyTransactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });

    // Nine empty fields carry nothing, so the redeemer's vector is empty and the
    // walk opens the door at no slot at all (§7.1: an untouched field is never
    // authenticated).
    expect(material.carriage).toEqual([]);
    expect(reconstruct(payloadFor(submittedTxCbor))).toEqual(submittedTxCbor);
  });

  it("reassembles a material-bearing forced order from inline carriage", () => {
    // `transactionCbor()` puts two 5 kB-datum outputs in field 2: one field
    // preimage, authenticated once against the flat commitment the payload's
    // compact body carries.
    const submittedTxCbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    expect(material.carriage.map((field) => field.fieldIndex)).toEqual([2]);
    expect(
      reconstruct(payloadFor(submittedTxCbor), inlineCarriageFor(material)),
    ).toEqual(submittedTxCbor);
  });

  it("reassembles the same order from tier-2 predeployed carriage", () => {
    // The tier is an encoding detail: the same bytes reached from a reference
    // input's nothing-but-bytes inline datum authenticate identically (§8.11).
    const submittedTxCbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    const [field] = material.carriage;
    expect(field).toBeDefined();
    expect(
      reconstruct(
        payloadFor(submittedTxCbor),
        [{ carriage: "RawUtxo", refInputIndex: 1 }],
        [Buffer.from("deadbeef", "hex"), field!.preimage],
      ),
    ).toEqual(submittedTxCbor);
  });

  it("refuses a tier-2 reference input that is not raw carriage", () => {
    const submittedTxCbor = transactionCbor();
    expect(() =>
      carriageFieldPreimages({
        payload: payloadFor(submittedTxCbor),
        carriage: [{ carriage: "RawUtxo", refInputIndex: 0 }],
        referenceInputs: [{ txHash: Buffer.alloc(32, 1), index: 0 }],
        datumOf: () => Buffer.from("d87980", "hex"),
      }),
    ).toThrow(/nothing-but-bytes/u);
  });

  it("reassembles the same order from tier-3 certified carriage", () => {
    // The only carriage whose bytes arrive split. Four 5 kB-datum outputs put
    // field 2 above §8.3's `K`, so §8.4's partition leaves tier 3 as the only
    // admissible carriage. The node never reads the certificate: the chunks'
    // concatenation is hashed whole against the field's commitment.
    const submittedTxCbor = transactionCbor({
      outputFills: [0x11, 0x22, 0x33, 0x44],
    });
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    expect(material.carriage.map((field) => field.fieldIndex)).toEqual([2]);
    const [field] = material.carriage;
    expect(field!.plan.tier).toBe("Certified");
    expect(field!.plan.publications.length).toBeGreaterThan(1);

    const chunks = field!.plan.publications.map(
      (publication) => publication.bytes,
    );
    const certifiedCarriage: readonly MidgardFieldCarriage[] = [
      {
        carriage: "Certified",
        certRefInputIndex: 0,
        chunkRefInputIndices: chunks.map((_chunk, index) => index + 1),
      },
    ];
    expect(
      reconstruct(payloadFor(submittedTxCbor), certifiedCarriage, [
        null,
        ...chunks,
      ]),
    ).toEqual(submittedTxCbor);

    // Tier 3's wrong-bytes refusal: only the last chunk's content differs, at
    // the same length, so the whole-field commitment is what refuses it.
    const lastChunk = Buffer.from(chunks[chunks.length - 1]!);
    lastChunk[lastChunk.length - 1] ^= 0xff;
    expect(() =>
      reconstruct(payloadFor(submittedTxCbor), certifiedCarriage, [
        null,
        ...chunks.slice(0, -1),
        lastChunk,
      ]),
    ).toThrow(FIELD_HASH_REFUSAL);
  });

  it("fails closed on a material-bearing forced order with no carriage supplied", () => {
    expect(() => reconstruct(payloadFor(transactionCbor()))).toThrow(
      /no §8 carriage for it/u,
    );
  });

  it("fails closed on carriage whose bytes are not the committed preimage", () => {
    const submittedTxCbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    const [field] = material.carriage;
    expect(field).toBeDefined();
    // One byte of the §5.1 payload changed, the envelope and the length intact —
    // so only the flat §4 commitment refuses it.
    const corrupted = Buffer.from(field!.preimage);
    corrupted[corrupted.length - 1] ^= 0xff;
    expect(() =>
      reconstruct(payloadFor(submittedTxCbor), [
        { carriage: "Inline", preimage: corrupted },
      ]),
    ).toThrow(FIELD_HASH_REFUSAL);
  });

  it("fails closed on a carriage vector the nine commitments do not exhaust", () => {
    // The mint's exhaustion rule, re-derived: a spare entry means the vector being
    // read is not the one the mint authenticated.
    const submittedTxCbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    expect(() =>
      reconstruct(payloadFor(submittedTxCbor), [
        ...inlineCarriageFor(material),
        { carriage: "Inline", preimage: Buffer.from("80", "hex") },
      ]),
    ).toThrow(/carriage entries more than its commitments name/u);
  });

  it("fails closed when the committed field lengths do not describe the source", () => {
    const payload = payloadFor(emptyTransactionCbor());
    const lengths = Buffer.from(
      payload.submitted_source.field_preimage_lengths_cbor,
      "hex",
    );
    // Nine one-byte fields encode as nine `01`s behind a `89` header; claiming
    // two bytes for field 0 leaves every length still "empty enough" to pass the
    // header check and wrong against the source.
    const index = lengths.indexOf(0x01);
    expect(index).toBeGreaterThan(0);
    const mutated = Buffer.from(lengths);
    mutated[index] = 0x02;
    expect(() =>
      reconstruct({
        ...payload,
        submitted_source: {
          ...payload.submitted_source,
          field_preimage_lengths_cbor: mutated.toString("hex"),
        },
      }),
    ).toThrow();
  });

  it("fails closed when the payload's transaction id is not the source's", () => {
    const payload = payloadFor(emptyTransactionCbor());
    expect(() => reconstruct({ ...payload, tx_id: "11".repeat(32) })).toThrow();
  });

  it("fails closed when the payload's commitment is not the source's", () => {
    const payload = payloadFor(emptyTransactionCbor());
    expect(() =>
      reconstruct({ ...payload, transaction_commitment: "22".repeat(32) }),
    ).toThrow();
  });
});

describe("V1 CEK program-material publication ingestion", () => {
  const materialUtxo = (datum: string, outputIndex = 0): UTxO =>
    ({
      txHash: "aa".repeat(32),
      outputIndex,
      address: "addr_test1vprogrammaterial",
      assets: { lovelace: 2_000_000n },
      datum,
    }) as UTxO;

  it("accepts one exact typed hash and skips wrong roots, kinds, and encodings", () => {
    const preimage = encodeMidgardCekBlobChunk(Buffer.from("material"));
    const root = hashMidgardCekProgramMaterialPreimage("blobChunk", preimage);
    const datum: SDK.CekProgramMaterialDatum = {
      kind: 3n,
      root: Buffer.from(root).toString("hex"),
      preimage: preimage.toString("hex"),
    };
    const datumCbor = Data.to(datum, SDK.CekProgramMaterialDatum);

    const exact = publishedProgramMaterialEntries([materialUtxo(datumCbor)]);
    expect(exact.ignoredCount).toBe(0);
    expect(exact.entries).toEqual([{ kind: "blobChunk", root, preimage }]);

    const wrongRoot = Data.to(
      { ...datum, root: "00".repeat(32) },
      SDK.CekProgramMaterialDatum,
    );
    const wrongKind = Data.to(
      { ...datum, kind: 4n },
      SDK.CekProgramMaterialDatum,
    );
    const unknownKind = Data.to(
      { ...datum, kind: 8n },
      SDK.CekProgramMaterialDatum,
    );
    const noncanonicalKind = datumCbor.replace("d8799f03", "d8799f1803");
    expect(Data.from(noncanonicalKind, SDK.CekProgramMaterialDatum)).toEqual(
      datum,
    );

    const hostile = publishedProgramMaterialEntries([
      materialUtxo(wrongRoot, 1),
      materialUtxo(wrongKind, 2),
      materialUtxo(unknownKind, 3),
      materialUtxo(noncanonicalKind, 4),
    ]);
    expect(hostile.entries).toEqual([]);
    expect(hostile.ignoredCount).toBe(4);
  });

  it("does not let foreign outputs under the shared credential hide valid material", () => {
    const preimage = encodeMidgardCekBlobChunk(Buffer.from("material"));
    const root = hashMidgardCekProgramMaterialPreimage("blobChunk", preimage);
    const datumCbor = Data.to(
      {
        kind: 3n,
        root: Buffer.from(root).toString("hex"),
        preimage: preimage.toString("hex"),
      },
      SDK.CekProgramMaterialDatum,
    );
    // What the always-fails credential holds on a public network: bare Ada,
    // reference-script parking, and other protocols' datums.
    const noDatum = { ...materialUtxo(datumCbor, 1), datum: undefined };
    const foreignDatum = materialUtxo("d87980", 2);
    const notCbor = materialUtxo("ff", 3);

    const snapshot = publishedProgramMaterialEntries([
      noDatum,
      materialUtxo(datumCbor, 0),
      foreignDatum,
      notCbor,
    ]);
    expect(snapshot.entries).toEqual([{ kind: "blobChunk", root, preimage }]);
    expect(snapshot.ignoredCount).toBe(3);
  });
});
