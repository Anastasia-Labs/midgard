import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardCekBlobChunkV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  hashMidgardCekProgramMaterialPreimageV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { MidgardCekProgramMaterialMissingRootError } from "@al-ft/midgard-core/cek-proof";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  isDeferrablePublishedProgramMaterialError,
  publishedProgramMaterialEntries,
  publishedProgramMaterialSnapshotError,
  reconstructTxOrderMaterialV1,
} from "@/fibers/fetch-and-insert-tx-order-utxos.js";

const transactionCbor = ({
  addrTxWitsPreimageCbor = EMPTY_CBOR_LIST,
  scriptTxWitsPreimageCbor = EMPTY_CBOR_LIST,
}: {
  readonly addrTxWitsPreimageCbor?: Buffer;
  readonly scriptTxWitsPreimageCbor?: Buffer;
} = {}): Buffer =>
  encodeMidgardNativeTxCanonicalV1(
    materializeMidgardNativeTxFromCanonicalV1({
      version: MIDGARD_NATIVE_TX_V1_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: encodeCbor(
          [0x11, 0x22].map((fill) =>
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
  encodeMidgardNativeTxCanonicalV1(
    materializeMidgardNativeTxFromCanonicalV1({
      version: MIDGARD_NATIVE_TX_V1_VERSION,
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

const payloadFor = (nativeTxCbor: Buffer): SDK.TxOrderPayloadV1 => {
  const material = SDK.deriveTxOrderMaterialV1({
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

describe("V1 tx-order §8 field carriage", () => {
  it("reconstructs a canonically-empty forced order, without the §8.8 door", async () => {
    // Not through `authenticatedMidgardFieldViewV1`: `reconstructTxOrderMaterialV1`
    // documents why the door cannot be the authenticator on this path while
    // `deriveNativeTxBodyCompact` is still counted (#585), and
    // `reconstructMidgardTransactionV1` is what binds the preimages to the payload
    // instead. This asserts the reconstruction that actually runs.
    const nativeTxCbor = emptyTransactionCbor();
    const material = SDK.deriveTxOrderMaterialV1({
      nativeTxCbor,
      owner: Buffer.alloc(28, 0x66),
    });

    // Nine empty fields carry nothing, so there is no §8 carriage to publish —
    // which is the only order state the tx-order mint admits today.
    expect(material.carriage).toEqual([]);
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterialV1({ payload: payloadFor(nativeTxCbor) }),
      ),
    ).resolves.toEqual(nativeTxCbor);
  });

  it("fails closed on a forced order that carries material", async () => {
    // `transactionCbor()` puts two 5 kB-datum outputs in field 2. Under the
    // retired counted chain this arrived as four per-item openings walked back
    // through their receipts; under §8 it is one field preimage with no
    // deployable carriage, so ingestion refuses it by name rather than
    // reassembling something no L1 order could have authenticated.
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterialV1({
          payload: payloadFor(transactionCbor()),
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
        reconstructTxOrderMaterialV1({
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
        reconstructTxOrderMaterialV1({
          payload: { ...payload, tx_id: "11".repeat(32) },
        }),
      ),
    ).rejects.toMatchObject(RECONSTRUCTION_FAILURE);
  });

  it("fails closed when the payload's commitment is not the source's", async () => {
    const payload = payloadFor(emptyTransactionCbor());
    await expect(
      Effect.runPromise(
        reconstructTxOrderMaterialV1({
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
    const preimage = encodeMidgardCekBlobChunkV1(Buffer.from("material"));
    const root = hashMidgardCekProgramMaterialPreimageV1("blobChunk", preimage);
    const datum: SDK.CekProgramMaterialDatumV1 = {
      kind: 3n,
      root: Buffer.from(root).toString("hex"),
      preimage: preimage.toString("hex"),
    };
    const datumCbor = Data.to(datum, SDK.CekProgramMaterialDatumV1);

    const exact = publishedProgramMaterialEntries([materialUtxo(datumCbor)]);
    expect(exact.malformedCount).toBe(0);
    expect(exact.sourceStatus).toBe("clean");
    expect(exact.entries).toEqual([{ kind: "blobChunk", root, preimage }]);

    const wrongRoot = Data.to(
      { ...datum, root: "00".repeat(32) },
      SDK.CekProgramMaterialDatumV1,
    );
    const wrongKind = Data.to(
      { ...datum, kind: 4n },
      SDK.CekProgramMaterialDatumV1,
    );
    const unknownKind = Data.to(
      { ...datum, kind: 8n },
      SDK.CekProgramMaterialDatumV1,
    );
    const noncanonicalKind = datumCbor.replace("d8799f03", "d8799f1803");
    expect(Data.from(noncanonicalKind, SDK.CekProgramMaterialDatumV1)).toEqual(
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
