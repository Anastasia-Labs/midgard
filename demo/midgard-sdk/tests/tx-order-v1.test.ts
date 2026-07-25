import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  encodeCbor,
  encodeMidgardCekBlobChunkV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  hashMidgardCekProgramMaterialPreimageV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import * as SDK from "../src/index.js";

const transactionCbor = (): Buffer =>
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
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    }),
  );

describe("V1 transaction-order fragments", () => {
  it("derives every canonical independently bounded field chunk", () => {
    const cbor = transactionCbor();
    const txOrderId: SDK.OutputReference = {
      transactionId: "33".repeat(32),
      outputIndex: 4n,
    };
    const bundle = SDK.deriveTxOrderFragmentBundleV1({
      nativeTxCbor: cbor,
      fieldReceiptPolicyId: "55".repeat(28),
      txOrderPolicyId: "44".repeat(28),
      txOrderId,
    });

    expect(cbor.length).toBeGreaterThan(8 * 1024);
    expect(cbor.length).toBeLessThanOrEqual(
      MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes,
    );
    expect(bundle.fragments.length).toBe(4);
    expect(bundle.fragments.map((fragment) => fragment.fieldIndex)).toEqual([
      2, 2, 2, 2,
    ]);
    expect(bundle.fragments.map((fragment) => fragment.itemIndex)).toEqual([
      0, 0, 1, 1,
    ]);
    expect(bundle.fragments.map((fragment) => fragment.chunkIndex)).toEqual([
      0, 1, 0, 1,
    ]);
    for (const fragment of bundle.fragments) {
      expect(Data.from(fragment.datumCbor, SDK.TxFieldPreimageV1)).toEqual(
        fragment.datum,
      );
      expect(fragment.datum.transaction_commitment).toBe(
        bundle.transactionCommitment,
      );
      expect(fragment.datum.field_receipt_policy_id).toBe("55".repeat(28));
      expect(fragment.datum.tx_order_id).toEqual(txOrderId);
      expect(Buffer.from(fragment.datum.proof.chunk, "hex").length).toBeLessThanOrEqual(
        4_095,
      );
    }
    expect(bundle.fragments.at(-1)!.fieldEncodedSize).toBe(
      decodeMidgardNativeTxFullV1FromCanonicalCbor(cbor).body
        .outputsPreimageCbor.length,
    );
  });

  it("fails closed for an unknown policy encoding", () => {
    expect(() =>
      SDK.deriveTxOrderFragmentBundleV1({
        nativeTxCbor: transactionCbor(),
        fieldReceiptPolicyId: "55".repeat(28),
        txOrderPolicyId: "AA".repeat(28),
        txOrderId: {
          transactionId: "33".repeat(32),
          outputIndex: 0n,
        },
      }),
    ).toThrow(/28-byte lowercase hex/u);
  });

  it("keeps a maximum field publication transaction below the L1 envelope", () => {
    const bundle = SDK.deriveTxOrderFragmentBundleV1({
      nativeTxCbor: transactionCbor(),
      fieldReceiptPolicyId: "11".repeat(28),
      txOrderPolicyId: "22".repeat(28),
      txOrderId: {
        transactionId: "33".repeat(32),
        outputIndex: 65_535n,
      },
    });
    const datum = bundle.fragments.find(
      (fragment) =>
        fragment.fieldIndex === 2 && fragment.datum.proof.chunk.length === 8_190,
    )!.datum;
    const datumCbor = Data.to(datum, SDK.TxFieldPreimageV1);
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_raw_bytes(Buffer.alloc(32, 1)),
        0n,
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_raw_bytes(
          Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, 2)]),
        ),
        CML.Value.from_coin(2_000_000n),
        CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datumCbor)),
        undefined,
      ),
    );
    const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
    const tx = CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
      undefined,
    );
    expect(datumCbor.length / 2).toBeLessThanOrEqual(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxFieldPublicationDatumBytes,
    );
    expect(tx.to_cbor_bytes().length).toBeLessThanOrEqual(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxFieldPublicationUnsignedTransactionBytes,
    );
    expect(tx.to_cbor_bytes().length).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes,
    );
  });

  it("encodes immutable content-addressed L1 program material below the independent field bound", () => {
    const preimage = encodeMidgardCekBlobChunkV1(Buffer.alloc(4_095, 0x5a));
    const root = hashMidgardCekProgramMaterialPreimageV1("blobChunk", preimage);
    const [publication] = SDK.deriveCekProgramMaterialPublicationsV1([
      { kind: "blobChunk", root, preimage },
    ]);

    expect(
      Data.from(publication!.datumCbor, SDK.CekProgramMaterialDatumV1),
    ).toEqual(publication!.datum);
    expect(publication!.datum.root).toBe(Buffer.from(root).toString("hex"));
    expect(Buffer.byteLength(publication!.datumCbor, "hex")).toBe(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxProgramMaterialPublicationDatumBytes,
    );
    expect(Buffer.byteLength(publication!.datumCbor, "hex")).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes,
    );
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_raw_bytes(Buffer.alloc(32, 1)),
        0n,
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_raw_bytes(
          Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, 2)]),
        ),
        CML.Value.from_coin(2_000_000n),
        CML.DatumOption.new_datum(
          CML.PlutusData.from_cbor_hex(publication!.datumCbor),
        ),
        undefined,
      ),
    );
    const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
    const tx = CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
      undefined,
    );
    expect(tx.to_cbor_bytes().length).toBe(
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxProgramMaterialPublicationUnsignedTransactionBytes,
    );
    expect(tx.to_cbor_bytes().length).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes,
    );
    expect(() =>
      SDK.deriveCekProgramMaterialPublicationsV1([
        { kind: "blobChunk", root, preimage },
        { kind: "blobChunk", root, preimage },
      ]),
    ).toThrow(/duplicate/u);
  });

  it("derives an exact receipt burn set", () => {
    const policyId = "66".repeat(28);
    const receipt = (assetName: string, outputIndex: number): UTxO =>
      ({
        txHash: "77".repeat(32),
        outputIndex,
        address: "addr_test1vreceipt",
        assets: {
          lovelace: 2_000_000n,
          [`${policyId}${assetName}`]: 1n,
        },
        datum: Data.to(0n),
      }) as UTxO;
    expect(
      SDK.txOrderFieldReceiptBurnAssetsV1(
        [receipt("01".repeat(32), 0), receipt("02".repeat(32), 1)],
        policyId,
      ),
    ).toEqual({
      [`${policyId}${"01".repeat(32)}`]: -1n,
      [`${policyId}${"02".repeat(32)}`]: -1n,
    });
    expect(() =>
      SDK.txOrderFieldReceiptBurnAssetsV1(
        [receipt("01".repeat(32), 0), receipt("01".repeat(32), 1)],
        policyId,
      ),
    ).toThrow(/duplicate/u);
  });
});
