import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardCekBlobChunkV1,
  encodeMidgardCekProgramEnvelopeV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardCekProgramMaterialPreimageV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import { deriveMidgardTxFieldReceiptAssetNameV1 } from "@al-ft/midgard-core/consensus-validation-v1";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  publishedProgramMaterialEntries,
  reconstructTxOrderMaterialV1,
} from "@/fibers/fetch-and-insert-tx-order-utxos.js";

const FIELD_PREIMAGE_ADDRESS = "addr_test1vfieldpreimage";
const FIELD_RECEIPT_ADDRESS = "addr_test1vfieldreceipt";
const FIELD_RECEIPT_POLICY_ID = "55".repeat(28);
const TX_ORDER_POLICY_ID = "44".repeat(28);
const TX_ORDER_ID: SDK.OutputReference = {
  transactionId: "33".repeat(32),
  outputIndex: 4n,
};

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

const outRefKey = ({
  transactionId,
  outputIndex,
}: SDK.OutputReference): string => `${transactionId}#${outputIndex.toString()}`;

const fragmentReference = (ordinal: number): SDK.OutputReference => ({
  transactionId: (0x80 + ordinal).toString(16).padStart(2, "0").repeat(32),
  outputIndex: BigInt(ordinal),
});

const receiptReference = (ordinal: number): SDK.OutputReference => ({
  transactionId: (0x90 + ordinal).toString(16).padStart(2, "0").repeat(32),
  outputIndex: BigInt(ordinal),
});

const materialFixture = (nativeTxCbor = transactionCbor()) => {
  const bundle = SDK.deriveTxOrderFragmentBundleV1({
    nativeTxCbor,
    fieldReceiptPolicyId: FIELD_RECEIPT_POLICY_ID,
    txOrderPolicyId: TX_ORDER_POLICY_ID,
    txOrderId: TX_ORDER_ID,
  });
  const utxos = new Map<string, UTxO>();
  let predecessorReceiptReference: SDK.OutputReference | null = null;

  for (const [ordinal, fragment] of bundle.fragments.entries()) {
    const fieldReference = fragmentReference(ordinal);
    utxos.set(outRefKey(fieldReference), {
      txHash: fieldReference.transactionId,
      outputIndex: Number(fieldReference.outputIndex),
      address: FIELD_PREIMAGE_ADDRESS,
      assets: { lovelace: 2_000_000n },
      datum: fragment.datumCbor,
    } as UTxO);

    const receiptOutRef = receiptReference(ordinal);
    const receiptAssetName = deriveMidgardTxFieldReceiptAssetNameV1({
      txOrderPolicyId: Buffer.from(TX_ORDER_POLICY_ID, "hex"),
      txOrderTransactionId: Buffer.from(TX_ORDER_ID.transactionId, "hex"),
      txOrderOutputIndex: TX_ORDER_ID.outputIndex,
      transactionCommitment: Buffer.from(bundle.transactionCommitment, "hex"),
      fieldIndex: fragment.fieldIndex,
      itemIndex: fragment.itemIndex,
      chunkIndex: fragment.chunkIndex,
    }).toString("hex");
    const receiptDatum: SDK.TxFieldReceiptV1 = {
      field_receipt_policy_id: FIELD_RECEIPT_POLICY_ID,
      tx_order_policy_id: TX_ORDER_POLICY_ID,
      tx_order_id: TX_ORDER_ID,
      transaction_commitment: bundle.transactionCommitment,
      collection_proof: fragment.datum.collection_proof,
      chunk_index: BigInt(fragment.chunkIndex),
      field_reference: fieldReference,
      predecessor_receipt_reference: predecessorReceiptReference,
      field_encoded_size: BigInt(fragment.fieldEncodedSize),
    };
    utxos.set(outRefKey(receiptOutRef), {
      txHash: receiptOutRef.transactionId,
      outputIndex: Number(receiptOutRef.outputIndex),
      address: FIELD_RECEIPT_ADDRESS,
      assets: {
        lovelace: 2_000_000n,
        [toUnit(FIELD_RECEIPT_POLICY_ID, receiptAssetName)]: 1n,
      },
      datum: Data.to(receiptDatum, SDK.TxFieldReceiptV1),
    } as UTxO);
    predecessorReceiptReference = receiptOutRef;
  }

  const lucid = {
    utxosByOutRef: async (
      references: readonly Pick<UTxO, "txHash" | "outputIndex">[],
    ): Promise<UTxO[]> =>
      references.flatMap((reference) => {
        const utxo = utxos.get(
          `${reference.txHash}#${reference.outputIndex.toString()}`,
        );
        return utxo === undefined ? [] : [utxo];
      }),
  } as unknown as LucidEvolution;
  return {
    nativeTxCbor,
    bundle,
    utxos,
    lucid,
    terminalReceiptReference: predecessorReceiptReference!,
  };
};

const mutateFragment = (
  fixture: ReturnType<typeof materialFixture>,
  ordinal: number,
  mutate: (datum: SDK.TxFieldPreimageV1) => SDK.TxFieldPreimageV1,
): void => {
  const reference = fragmentReference(ordinal);
  const utxo = fixture.utxos.get(outRefKey(reference))!;
  const datum = Data.from(
    utxo.datum!,
    SDK.TxFieldPreimageV1,
  ) as SDK.TxFieldPreimageV1;
  fixture.utxos.set(outRefKey(reference), {
    ...utxo,
    datum: Data.to(mutate(datum), SDK.TxFieldPreimageV1),
  });
};

const reconstruct = (
  fixture: ReturnType<typeof materialFixture>,
): Effect.Effect<Buffer, SDK.LucidError> =>
  reconstructTxOrderMaterialV1({
    lucid: fixture.lucid,
    txOrderId: TX_ORDER_ID,
    payload: {
      tx_id: fixture.bundle.transactionId,
      transaction_commitment: fixture.bundle.transactionCommitment,
      source: fixture.bundle.source,
      terminal_receipt_reference: fixture.terminalReceiptReference,
    },
    txOrderPolicyId: TX_ORDER_POLICY_ID,
    fieldPreimageAddress: FIELD_PREIMAGE_ADDRESS,
    fieldReceiptPolicyId: FIELD_RECEIPT_POLICY_ID,
    fieldReceiptAddress: FIELD_RECEIPT_ADDRESS,
  });

describe("V1 tx-order material receipt chain", () => {
  it("streams an authenticated multi-item, multi-chunk chain into the exact transaction", async () => {
    const fixture = materialFixture();

    expect(fixture.bundle.fragments.length).toBe(4);
    expect(fixture.nativeTxCbor.length).toBeGreaterThan(8 * 1_024);
    await expect(Effect.runPromise(reconstruct(fixture))).resolves.toEqual(
      fixture.nativeTxCbor,
    );
  });

  it("reconstructs field 6 scripts raw and field 7 address witnesses as byte-list items", async () => {
    const scriptTxWitsPreimageCbor = encodeMidgardVersionedScriptListPreimage([
      {
        language: "MidgardV1",
        scriptBytes: encodeMidgardCekProgramEnvelopeV1({
          uplcVersion: [1n, 1n, 0n],
          termRoot: Buffer.alloc(32, 0x33),
          nodeCount: 3n,
          materialByteLength: 144n,
        }),
      },
    ]);
    const addressWitnessCbor = encodeCbor([
      Buffer.alloc(32, 0x44),
      Buffer.alloc(64, 0x55),
    ]);
    const addrTxWitsPreimageCbor = encodeCbor([addressWitnessCbor]);
    const fixture = materialFixture(
      transactionCbor({
        addrTxWitsPreimageCbor,
        scriptTxWitsPreimageCbor,
      }),
    );
    const scriptFragments = fixture.bundle.fragments.filter(
      ({ fieldIndex }) => fieldIndex === 6,
    );
    const addressFragments = fixture.bundle.fragments.filter(
      ({ fieldIndex }) => fieldIndex === 7,
    );

    expect(scriptFragments).toHaveLength(1);
    expect(scriptFragments[0]).toMatchObject({
      fieldName: "script_witnesses",
      fieldEncodedSize: scriptTxWitsPreimageCbor.length,
    });
    expect(addressFragments).toHaveLength(1);
    expect(addressFragments[0]).toMatchObject({
      fieldName: "address_witnesses",
      fieldEncodedSize: addrTxWitsPreimageCbor.length,
    });
    await expect(Effect.runPromise(reconstruct(fixture))).resolves.toEqual(
      fixture.nativeTxCbor,
    );
  });

  it("fails closed when a predecessor lies about its encoded-size state", async () => {
    const fixture = materialFixture();
    const predecessorReference =
      fixture.bundle.fragments.length === 0
        ? fixture.terminalReceiptReference
        : receiptReference(fixture.bundle.fragments.length - 2);
    const predecessor = fixture.utxos.get(outRefKey(predecessorReference))!;
    const datum = Data.from(
      predecessor.datum!,
      SDK.TxFieldReceiptV1,
    ) as SDK.TxFieldReceiptV1;
    fixture.utxos.set(outRefKey(predecessorReference), {
      ...predecessor,
      datum: Data.to(
        {
          ...datum,
          field_encoded_size: datum.field_encoded_size + 1n,
        },
        SDK.TxFieldReceiptV1,
      ),
    });

    await expect(Effect.runPromise(reconstruct(fixture))).rejects.toMatchObject(
      {
        message:
          "Failed to walk and reconstruct the authenticated V1 tx-order material chain",
      },
    );
  });

  it.each([
    {
      name: "field kind",
      mutate: (datum: SDK.TxFieldPreimageV1): SDK.TxFieldPreimageV1 => ({
        ...datum,
        proof: {
          ...datum.proof,
          field_index: datum.proof.field_index + 1n,
        },
      }),
    },
    {
      name: "field hash",
      mutate: (datum: SDK.TxFieldPreimageV1): SDK.TxFieldPreimageV1 => ({
        ...datum,
        proof: {
          ...datum.proof,
          chunk: `${datum.proof.chunk.startsWith("00") ? "01" : "00"}${datum.proof.chunk.slice(2)}`,
        },
      }),
    },
    {
      name: "field length",
      mutate: (datum: SDK.TxFieldPreimageV1): SDK.TxFieldPreimageV1 => ({
        ...datum,
        proof: {
          ...datum.proof,
          total_length: datum.proof.total_length + 1n,
        },
      }),
    },
  ])("fails closed for a mutated $name", async ({ mutate }) => {
    const fixture = materialFixture();
    mutateFragment(fixture, 0, mutate);
    await expect(Effect.runPromise(reconstruct(fixture))).rejects.toMatchObject(
      {
        message:
          "Failed to walk and reconstruct the authenticated V1 tx-order material chain",
      },
    );
  });

  it("rejects semantically decodable but noncanonical fragment data", async () => {
    const fixture = materialFixture();
    const reference = fragmentReference(0);
    const utxo = fixture.utxos.get(outRefKey(reference))!;
    const noncanonical = utxo.datum!.replace("d8799f01", "d8799f1801");
    expect(noncanonical).not.toBe(utxo.datum);
    expect(Data.from(noncanonical, SDK.TxFieldPreimageV1)).toEqual(
      Data.from(utxo.datum!, SDK.TxFieldPreimageV1),
    );
    fixture.utxos.set(outRefKey(reference), {
      ...utxo,
      datum: noncanonical,
    });

    await expect(Effect.runPromise(reconstruct(fixture))).rejects.toMatchObject(
      {
        message:
          "Failed to walk and reconstruct the authenticated V1 tx-order material chain",
      },
    );
  });

  it("rejects semantically decodable but noncanonical receipt data", async () => {
    const fixture = materialFixture();
    const reference = fixture.terminalReceiptReference;
    const utxo = fixture.utxos.get(outRefKey(reference))!;
    const noncanonical = utxo.datum!.replace("d8799f01", "d8799f1801");
    expect(noncanonical).not.toBe(utxo.datum);
    expect(Data.from(noncanonical, SDK.TxFieldReceiptV1)).toEqual(
      Data.from(utxo.datum!, SDK.TxFieldReceiptV1),
    );
    fixture.utxos.set(outRefKey(reference), {
      ...utxo,
      datum: noncanonical,
    });

    await expect(Effect.runPromise(reconstruct(fixture))).rejects.toMatchObject(
      {
        message:
          "Failed to walk and reconstruct the authenticated V1 tx-order material chain",
      },
    );
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
  });
});
