import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardCekBlobChunk,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
  hashMidgardCekProgramMaterialPreimage,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { MidgardCekProgramMaterialMissingRootError } from "@al-ft/midgard-core/cek-proof";
import type { MidgardFieldCarriage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  isDeferrablePublishedProgramMaterialError,
  publishedProgramMaterialEntries,
  publishedProgramMaterialSnapshotError,
  reconstructTxOrderMaterial,
} from "../src/fibers/fetch-and-insert-tx-order-utxos.js";

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
  encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
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
  encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
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

const payloadFor = (nativeTxCbor: Buffer): SDK.TxOrderPayload => {
  const material = SDK.deriveTxOrderMaterial({
    nativeTxCbor,
    owner: Buffer.alloc(28, 0x66),
  });
  return {
    tx_id: material.transactionId,
    transaction_commitment: material.transactionCommitment,
    source: material.source,
  };
};

const RECONSTRUCTION_FAILURE = {
  message:
    "Failed to reconstruct the authenticated V1 tx-order material from its §8 carriage",
} as const;

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

describe("V1 tx-order §8 field carriage", () => {
  it("reconstructs a canonically-empty forced order, which consumes no carriage", async () => {
    const nativeTxCbor = emptyTransactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      nativeTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });

    // Nine empty fields carry nothing, so the redeemer's vector is empty and the
    // walk opens the door at no slot at all (§7.1: an untouched field is never
    // authenticated).
    expect(material.carriage).toEqual([]);
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({ payload: payloadFor(nativeTxCbor) }),
      ),
    ).resolves.toEqual(nativeTxCbor);
  });

  it("reassembles a material-bearing forced order through the §8.8 door", async () => {
    // `transactionCbor()` puts two 5 kB-datum outputs in field 2. Under the
    // retired counted chain this arrived as four per-item openings walked back
    // through their receipts; under §8 it is one field preimage, authenticated
    // once against the flat commitment the payload's compact body carries.
    const nativeTxCbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      nativeTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    expect(material.carriage.map((field) => field.fieldIndex)).toEqual([2]);

    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: payloadFor(nativeTxCbor),
          material: { carriage: inlineCarriageFor(material) },
        }),
      ),
    ).resolves.toEqual(nativeTxCbor);
  });

  it("reassembles the same order from tier-2 predeployed carriage", async () => {
    // The tier is an encoding detail: the same bytes reached from a reference
    // input's nothing-but-bytes inline datum authenticate identically, which is
    // what makes the creator's publish-or-inline choice a budget decision and not
    // a protocol one (§8.11).
    const nativeTxCbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      nativeTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    const [field] = material.carriage;
    expect(field).toBeDefined();

    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: payloadFor(nativeTxCbor),
          material: {
            carriage: [{ carriage: "RawUtxo", refInputIndex: 1 }],
            referenceInputs: [
              { inlineDatumBytes: Buffer.from("deadbeef", "hex") },
              { inlineDatumBytes: field!.preimage },
            ],
          },
        }),
      ),
    ).resolves.toEqual(nativeTxCbor);
  });

  it("reassembles the same order from tier-3 certified carriage", async () => {
    // The third tier, and the one the other two rows cannot stand in for: it is
    // the only carriage whose bytes arrive split, whose length claim comes from a
    // separate authenticated datum, and whose §4 check the *lazy* on-chain door
    // would skip entirely. Four 5 kB-datum outputs put field 2 above §8.3's `K`,
    // so §8.4's partition leaves tier 3 as the only admissible carriage — which is
    // why the plan's own tier is asserted rather than assumed.
    //
    // Every piece of the reference-input set comes off the plan
    // `deriveTxOrderMaterial` produced: the certificate, its content-addressed
    // token name, and the chunk bytes in the order the digest vector is written.
    // Rebuilding them here from the preimage would be a second derivation that
    // could agree with the first while both disagreed with the publisher.
    const nativeTxCbor = transactionCbor({
      outputFills: [0x11, 0x22, 0x33, 0x44],
    });
    const material = SDK.deriveTxOrderMaterial({
      nativeTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    expect(material.carriage.map((field) => field.fieldIndex)).toEqual([2]);
    const [field] = material.carriage;
    expect(field!.plan.tier).toBe("Certified");
    expect(field!.plan.certificate).not.toBeNull();
    expect(field!.plan.publications.length).toBeGreaterThan(1);

    const referenceInputs = [
      {
        certificate: field!.plan.certificate!,
        certificateAssetName: field!.plan.certificateAssetName!,
      },
      ...field!.plan.publications.map((publication) => ({
        inlineDatumBytes: publication.bytes,
      })),
    ];
    const certifiedCarriage: readonly MidgardFieldCarriage[] = [
      {
        carriage: "Certified",
        certRefInputIndex: 0,
        chunkRefInputIndices: field!.plan.publications.map(
          (_publication, index) => index + 1,
        ),
      },
    ];
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: payloadFor(nativeTxCbor),
          material: { carriage: certifiedCarriage, referenceInputs },
        }),
      ),
    ).resolves.toEqual(nativeTxCbor);

    // Tier 3's wrong-bytes refusal, which is what makes the row above evidence of
    // authentication rather than of reassembly. The certificate is the real one and
    // its digest vector is over the real chunks; only the last chunk's content
    // differs, at the same length, so §8.4's split arithmetic and the certificate's
    // own identity checks all pass and the §4 commitment is the only thing left to
    // refuse it.
    const tampered = [...referenceInputs];
    const lastIndex = tampered.length - 1;
    const lastChunk = Buffer.from(
      (tampered[lastIndex] as { inlineDatumBytes: Buffer }).inlineDatumBytes,
    );
    lastChunk[lastChunk.length - 1] ^= 0xff;
    tampered[lastIndex] = { inlineDatumBytes: lastChunk };
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: payloadFor(nativeTxCbor),
          material: {
            carriage: certifiedCarriage,
            referenceInputs: tampered,
          },
        }),
      ),
    ).rejects.toMatchObject(RECONSTRUCTION_FAILURE);
  });

  it("fails closed on a material-bearing forced order with no carriage supplied", async () => {
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: payloadFor(transactionCbor()),
        }),
      ),
    ).rejects.toMatchObject(RECONSTRUCTION_FAILURE);
  });

  it("fails closed on carriage whose bytes are not the committed preimage", async () => {
    const nativeTxCbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      nativeTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });
    const [field] = material.carriage;
    expect(field).toBeDefined();
    // One byte of the §5.1 payload changed, the envelope and the length intact —
    // so only the flat §4 commitment refuses it.
    const corrupted = Buffer.from(field!.preimage);
    corrupted[corrupted.length - 1] ^= 0xff;

    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: payloadFor(nativeTxCbor),
          material: {
            carriage: [{ carriage: "Inline", preimage: corrupted }],
          },
        }),
      ),
    ).rejects.toMatchObject(RECONSTRUCTION_FAILURE);
  });

  it("fails closed on a carriage vector the nine commitments do not exhaust", async () => {
    // The mint's exhaustion rule, re-derived: a spare entry means the vector being
    // read is not the one the mint authenticated.
    const nativeTxCbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      nativeTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });

    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: payloadFor(nativeTxCbor),
          material: {
            carriage: [
              ...inlineCarriageFor(material),
              { carriage: "Inline", preimage: Buffer.from("80", "hex") },
            ],
          },
        }),
      ),
    ).rejects.toMatchObject(RECONSTRUCTION_FAILURE);
  });

  it("fails closed when the committed field lengths do not describe the source", async () => {
    const payload = payloadFor(emptyTransactionCbor());
    const lengths = Buffer.from(
      payload.source.field_preimage_lengths_cbor,
      "hex",
    );
    // Nine one-byte fields encode as nine `01`s behind a `89` header; claiming
    // two bytes for field 0 leaves every length still "empty enough" to pass the
    // header check and wrong against the source.
    const index = lengths.indexOf(0x01);
    expect(index).toBeGreaterThan(0);
    const mutated = Buffer.from(lengths);
    mutated[index] = 0x02;

    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: {
            ...payload,
            source: {
              ...payload.source,
              field_preimage_lengths_cbor: mutated.toString("hex"),
            },
          },
        }),
      ),
    ).rejects.toMatchObject(RECONSTRUCTION_FAILURE);
  });

  it("fails closed when the payload's transaction id is not the source's", async () => {
    const payload = payloadFor(emptyTransactionCbor());
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: { ...payload, tx_id: "11".repeat(32) },
        }),
      ),
    ).rejects.toMatchObject(RECONSTRUCTION_FAILURE);
  });

  it("fails closed when the payload's commitment is not the source's", async () => {
    const payload = payloadFor(emptyTransactionCbor());
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterial({
          payload: { ...payload, transaction_commitment: "22".repeat(32) },
        }),
      ),
    ).rejects.toMatchObject(RECONSTRUCTION_FAILURE);
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

  it("accepts one exact typed hash and rejects wrong roots, kinds, and encodings", () => {
    const preimage = encodeMidgardCekBlobChunk(Buffer.from("material"));
    const root = hashMidgardCekProgramMaterialPreimage("blobChunk", preimage);
    const datum: SDK.CekProgramMaterialDatum = {
      kind: 3n,
      root: Buffer.from(root).toString("hex"),
      preimage: preimage.toString("hex"),
    };
    const datumCbor = Data.to(datum, SDK.CekProgramMaterialDatum);

    const exact = publishedProgramMaterialEntries([materialUtxo(datumCbor)]);
    expect(exact.malformedCount).toBe(0);
    expect(exact.sourceStatus).toBe("clean");
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
    expect(hostile.malformedCount).toBe(4);
    expect(hostile.sourceStatus).toBe("malformed");
  });

  it("defers only a typed missing root from a clean publication snapshot", () => {
    const missing = new MidgardCekProgramMaterialMissingRootError(
      Buffer.alloc(32, 0x44),
    );
    const clean = {
      entries: [],
      malformedCount: 0,
      sourceStatus: "clean" as const,
    };
    const malformed = {
      entries: [],
      malformedCount: 1,
      sourceStatus: "malformed" as const,
    };

    expect(isDeferrablePublishedProgramMaterialError(clean, missing)).toBe(
      true,
    );
    expect(isDeferrablePublishedProgramMaterialError(malformed, missing)).toBe(
      false,
    );
    expect(
      isDeferrablePublishedProgramMaterialError(
        { ...clean, malformedCount: 1 },
        missing,
      ),
    ).toBe(false);
    expect(
      isDeferrablePublishedProgramMaterialError(clean, new Error("mismatch")),
    ).toBe(false);
    expect(
      publishedProgramMaterialSnapshotError(malformed, "addr_test1source"),
    ).toMatchObject({
      _tag: "LucidError",
      cause: {
        sourceAddress: "addr_test1source",
        sourceStatus: "malformed",
        malformedCount: 1,
      },
    });
    expect(
      publishedProgramMaterialSnapshotError(
        { ...clean, malformedCount: 1 },
        "addr_test1source",
      ),
    ).toBeInstanceOf(SDK.LucidError);
  });
});
