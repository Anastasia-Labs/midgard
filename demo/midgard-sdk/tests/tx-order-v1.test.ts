import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
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
  it("pins the exact datum, spend, receipt-mint, and forced-key V1 vectors", () => {
    const txOrderId: SDK.OutputReference = {
      transactionId: "33".repeat(32),
      outputIndex: 4n,
    };
    const source: SDK.NativeTxProofSourceV1 = {
      compact_cbor: "01",
      witness_set_compact_cbor: "0203",
      field_preimage_lengths_cbor: "04",
    };
    const payload: SDK.TxOrderPayloadV1 = {
      tx_id: "44".repeat(32),
      transaction_commitment: "55".repeat(32),
      source,
      terminal_receipt_reference: null,
    };
    const event: SDK.TxOrderEventV1 = { id: txOrderId, tx: payload };
    const datum: SDK.TxOrderDatumV1 = {
      event,
      inclusion_time: 123n,
      witness: "66".repeat(28),
      refund_address: {
        paymentCredential: {
          PublicKeyCredential: ["77".repeat(28)],
        },
        stakeCredential: null,
      },
      refund_datum: "NoDatum",
    };
    const forced: SDK.ForcedInclusionTxV1 = {
      tx_id: payload.tx_id,
      transaction_commitment: payload.transaction_commitment,
      source,
      operator_validity: "FailedScript",
    };
    const spend: SDK.TxOrderSpendRedeemerV1 = {
      input_index: 0n,
      output_index: 1n,
      hub_ref_input_index: 2n,
      settlement_ref_input_index: 3n,
      burn_redeemer_index: 4n,
      membership_proof: {
        domain: "ForcedTransactionsV1RootDomain",
        root: "00".repeat(32),
        phas_root: "11".repeat(32),
        count: 1n,
        key: Data.to(txOrderId, SDK.OutputReference),
        value: Data.to(forced, SDK.ForcedInclusionTxV1),
        proof: [],
      },
      inclusion_proof_script_withdraw_redeemer_index: 5n,
      validity_override: "FailedScript",
    };
    const publish: SDK.TxFieldReceiptMintRedeemerV1 = {
      PublishField: {
        field_reference_input_index: 0n,
        predecessor_receipt_reference_input_index: -1n,
        receipt_output_index: 1n,
        transaction_id: payload.tx_id,
        source,
      },
    };
    const burn: SDK.TxFieldReceiptMintRedeemerV1 = {
      BurnReceipts: { receipt_input_indices: [0n, 2n] },
    };

    const datumCbor = Data.to(datum, SDK.TxOrderDatumV1);
    expect(Data.to(txOrderId, SDK.OutputReference)).toBe(
      `d8799f5820${"33".repeat(32)}04ff`,
    );
    expect(Data.to(payload, SDK.TxOrderPayloadV1)).toBe(
      `d8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffd87a80ff`,
    );
    expect(Data.to(event, SDK.TxOrderEventV1)).toBe(
      `d8799fd8799f5820${"33".repeat(32)}04ffd8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffd87a80ffff`,
    );
    expect(datumCbor).toBe(
      `d8799fd8799fd8799f5820${"33".repeat(32)}04ffd8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffd87a80ffff187b581c${"66".repeat(28)}d8799fd8799f581c${"77".repeat(28)}ffd87a80ffd87980ff`,
    );
    expect(Data.to(forced, SDK.ForcedInclusionTxV1)).toBe(
      `d8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffd87c80ff`,
    );
    expect(Data.to(spend, SDK.TxOrderSpendRedeemerV1)).toBe(
      "d8799f0001020304d8799fd87a805820000000000000000000000000000000000000000000000000000000000000000058201111111111111111111111111111111111111111111111111111111111111111015827d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff5f5840d8799f582044444444444444444444444444444444444444444444444444444444444444445820555555555555555555555555555555555555555555555555555655555555555555d8799f41014202034104ffd87c80ffff80ff05d87c80ff",
    );
    expect(Data.to(publish, SDK.TxFieldReceiptMintRedeemerV1)).toBe(
      `d8799f0020015820${"44".repeat(32)}d8799f41014202034104ffff`,
    );
    expect(Data.to(burn, SDK.TxFieldReceiptMintRedeemerV1)).toBe(
      "d87a9f9f0002ffff",
    );
    expect(
      SDK.decodeTxOrderDatumV1Cbor(SDK.encodeTxOrderDatumV1Cbor(datum)),
    ).toEqual(datum);

    const overlongInclusionTime = datumCbor.replace(
      "187b581c",
      "1a0000007b581c",
    );
    expect(() =>
      SDK.decodeTxOrderDatumV1Cbor(Buffer.from(overlongInclusionTime, "hex")),
    ).toThrow(/exact canonical encoding/u);
    expect(() =>
      Data.from(`d87a${datumCbor.slice(4)}`, SDK.TxOrderDatumV1),
    ).toThrow();
    expect(() =>
      Data.from(`${datumCbor.slice(0, -2)}00ff`, SDK.TxOrderDatumV1),
    ).toThrow();

    const pointerDatum: SDK.TxOrderDatumV1 = {
      ...datum,
      refund_address: {
        paymentCredential: datum.refund_address.paymentCredential,
        stakeCredential: {
          Pointer: {
            slotNumber: 6n,
            transactionIndex: 7n,
            certificateIndex: 8n,
          },
        },
      },
    };
    expect(
      Data.from(Data.to(pointerDatum, SDK.TxOrderDatumV1), SDK.TxOrderDatumV1),
    ).toEqual(pointerDatum);
    expect(() =>
      Data.to(
        {
          ...pointerDatum,
          refund_address: {
            ...pointerDatum.refund_address,
            stakeCredential: {
              Pointer: [
                {
                  slotNumber: 6n,
                  transactionIndex: 7n,
                  certificateIndex: 8n,
                },
              ],
            },
          },
        } as never,
        SDK.TxOrderDatumV1,
      ),
    ).toThrow();
  });

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
      expect(fragment.datum.collection_proof.field_index).toBe(
        fragment.datum.proof.field_index,
      );
      expect(fragment.datum.collection_proof.item_index).toBe(
        fragment.datum.proof.item_index,
      );
      expect(fragment.datum.collection_proof.item_length).toBe(
        fragment.datum.proof.total_length,
      );
      expect(fragment.datum.collection_proof.item_commitment).toHaveLength(64);
      expect(
        Buffer.from(fragment.datum.proof.chunk, "hex").length,
      ).toBeLessThanOrEqual(4_095);
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
        fragment.fieldIndex === 2 &&
        fragment.datum.proof.chunk.length === 8_190,
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
    expect(() =>
      SDK.deriveCekProgramMaterialPublicationsV1([
        {
          kind: "blobChunk",
          root: Buffer.alloc(32) as never,
          preimage,
        },
      ]),
    ).toThrow(/root does not match/u);
    expect(() =>
      SDK.deriveCekProgramMaterialPublicationsV1([
        { kind: "blobBranch", root, preimage },
      ]),
    ).toThrow(/root does not match/u);
    expect(() =>
      SDK.deriveCekProgramMaterialPublicationsV1([
        { kind: "unknown", root, preimage } as never,
      ]),
    ).toThrow();
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
