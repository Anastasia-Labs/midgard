import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardCekBlobChunkV1,
  encodeMidgardCekProgramEnvelopeV1,
  encodeMidgardCekProgramMaterialDaValueV1,
  encodeMidgardCekProgramMaterialSidecarV1,
  encodeMidgardCekTermNodeV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  hashMidgardCekProgramEnvelopeV1,
  hashMidgardCekProgramMaterialPreimageV1,
  hashMidgardCekTermNodeV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core";
import {
  midgardFieldCommitmentV1,
  selectMidgardFieldCarriageTierV1,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import { CML, Data, type LucidEvolution } from "@lucid-evolution/lucid";
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

const cekProgramMaterialAddress = CML.Address.from_raw_bytes(
  Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, 0x42)]),
).to_bech32();

const cekProgramMaterialContracts = {
  cekProgramMaterial: {
    spendingScriptAddress: cekProgramMaterialAddress,
  },
} as Pick<SDK.MidgardValidators, "cekProgramMaterial">;

const cekProgramMaterialPublication = (bytes: number) => {
  const preimage = encodeMidgardCekBlobChunkV1(Buffer.alloc(bytes, 0x5a));
  return SDK.deriveCekProgramMaterialPublicationsV1([
    {
      kind: "blobChunk",
      root: hashMidgardCekProgramMaterialPreimageV1("blobChunk", preimage),
      preimage,
    },
  ])[0]!;
};

const completeCekPublicationInput = () => {
  const term = { kind: "error" } as const;
  const preimage = encodeMidgardCekTermNodeV1(term);
  const root = hashMidgardCekTermNodeV1(term);
  const entry = { kind: "term" as const, root, preimage };
  const envelopeCbor = encodeMidgardCekProgramEnvelopeV1({
    uplcVersion: [1n, 1n, 0n],
    termRoot: root,
    nodeCount: 1n,
    materialByteLength: BigInt(preimage.length),
  });
  return {
    envelopeCbor,
    entry,
    sidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([entry]),
  };
};

const materialPublicationLucid = ({
  coinsPerUtxoByte,
  fundedLovelace,
  outputs,
}: {
  readonly coinsPerUtxoByte: bigint;
  readonly fundedLovelace: bigint[];
  readonly outputs?: {
    address: string;
    datum: unknown;
    lovelace: bigint;
  }[];
}): LucidEvolution => {
  const tx = {
    pay: {
      ToAddressWithData: (
        address: string,
        datum: unknown,
        assets: { readonly lovelace: bigint },
      ) => {
        fundedLovelace.push(assets.lovelace);
        outputs?.push({ address, datum, lovelace: assets.lovelace });
        return tx;
      },
    },
    complete: async () => ({}),
  };
  return {
    config: () => ({ protocolParameters: { coinsPerUtxoByte } }),
    newTx: () => tx,
  } as unknown as LucidEvolution;
};

describe("V1 transaction-order datum, §8 field carriage, and CEK program material", () => {
  it("pins the exact datum, spend, and forced-key V1 vectors", () => {
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

    const datumCbor = Data.to(datum, SDK.TxOrderDatumV1);
    expect(Data.to(txOrderId, SDK.OutputReference)).toBe(
      `d8799f5820${"33".repeat(32)}04ff`,
    );
    expect(Data.to(payload, SDK.TxOrderPayloadV1)).toBe(
      `d8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffff`,
    );
    expect(Data.to(event, SDK.TxOrderEventV1)).toBe(
      `d8799fd8799f5820${"33".repeat(32)}04ffd8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffffff`,
    );
    expect(datumCbor).toBe(
      `d8799fd8799fd8799f5820${"33".repeat(32)}04ffd8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffffff187b581c${"66".repeat(28)}d8799fd8799f581c${"77".repeat(28)}ffd87a80ffd87980ff`,
    );
    expect(Data.to(forced, SDK.ForcedInclusionTxV1)).toBe(
      `d8799f5820${"44".repeat(32)}d8799f41014202034104ffd87c80ff`,
    );
    expect(Data.to(spend, SDK.TxOrderSpendRedeemerV1)).toBe(
      "d8799f0001020304d8799fd87a805820000000000000000000000000000000000000000000000000000000000000000058201111111111111111111111111111111111111111111111111111111111111111015827d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff5834d8799f58204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffd87c80ff80ff05d87c80ff",
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

  it("derives the §8 carriage of every non-empty field and nothing for the empty ones", () => {
    const cbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterialV1({
      nativeTxCbor: cbor,
      owner: Buffer.alloc(28, 0x44),
    });

    expect(cbor.length).toBeGreaterThan(8 * 1024);
    expect(cbor.length).toBeLessThanOrEqual(
      MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes,
    );
    // Only field 2 (outputs) carries anything in this fixture, so the carriage
    // list is exactly one entry: the counted scheme published four per-item
    // chunks for the same bytes, which is the whole difference the §4 reversion
    // makes to a publisher.
    expect(material.carriage.map((field) => field.fieldIndex)).toEqual([2]);
    const outputs = material.carriage[0]!;
    expect(outputs.fieldName).toBe("outputs");
    expect(outputs.preimage).toEqual(
      decodeMidgardNativeTxFullV1FromCanonicalCbor(cbor).body
        .outputsPreimageCbor,
    );
    expect(outputs.commitment).toBe(
      midgardFieldCommitmentV1(outputs.preimage).toString("hex"),
    );
    // §8.4 is a partition, so the tier is a fact about the byte length and not a
    // choice this module makes.
    expect(outputs.plan.tier).toBe(
      selectMidgardFieldCarriageTierV1(outputs.preimage.length),
    );
    expect(outputs.plan.totalLength).toBe(outputs.preimage.length);
    expect(outputs.plan.commitment.toString("hex")).toBe(outputs.commitment);
    expect(outputs.plan.txId.toString("hex")).toBe(material.transactionId);
  });

  it("refuses material it cannot bind to a canonical transaction", () => {
    expect(() =>
      SDK.deriveTxOrderMaterialV1({
        nativeTxCbor: Buffer.from("00", "hex"),
        owner: Buffer.alloc(28, 0x44),
      }),
    ).toThrow();
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

  it("derives the exact stabilized min-Ada vector for CEK material at its actual script address", () => {
    const publication = cekProgramMaterialPublication(4_095);
    const minimumLovelace =
      SDK.minimumLovelaceForCekProgramMaterialPublicationV1({
        contracts: cekProgramMaterialContracts,
        publication,
        coinsPerUtxoByte: 4_310n,
      });

    expect(Buffer.byteLength(publication.datumCbor, "hex")).toBe(4_268);
    expect(minimumLovelace).toBe(19_287_250n);
  });

  it("raises adjacent underfunded CEK material publication funding to exact min-Ada", async () => {
    const publication = cekProgramMaterialPublication(64);
    const minimumLovelace =
      SDK.minimumLovelaceForCekProgramMaterialPublicationV1({
        contracts: cekProgramMaterialContracts,
        publication,
        coinsPerUtxoByte: 4_310n,
      });
    expect(Buffer.byteLength(publication.datumCbor, "hex")).toBe(110);
    expect(minimumLovelace).toBe(1_361_960n);
    const fundedLovelace: bigint[] = [];
    const lucid = materialPublicationLucid({
      coinsPerUtxoByte: 4_310n,
      fundedLovelace,
    });

    await SDK.unsignedCekProgramMaterialV1(
      lucid,
      cekProgramMaterialContracts as SDK.MidgardValidators,
      {
        entries: [publication.entry],
        lovelacePerEntry: minimumLovelace - 1n,
      },
    );
    await SDK.unsignedCekProgramMaterialV1(
      lucid,
      cekProgramMaterialContracts as SDK.MidgardValidators,
      {
        entries: [publication.entry],
        lovelacePerEntry: minimumLovelace + 1n,
      },
    );

    expect(fundedLovelace).toEqual([minimumLovelace, minimumLovelace + 1n]);
  });

  it("raises the CEK material min-Ada vector when a canonical datum grows", () => {
    const small = cekProgramMaterialPublication(0);
    const maximum = cekProgramMaterialPublication(4_095);
    const minimum = (publication: typeof small) =>
      SDK.minimumLovelaceForCekProgramMaterialPublicationV1({
        contracts: cekProgramMaterialContracts,
        publication,
        coinsPerUtxoByte: 4_310n,
      });

    expect(Buffer.byteLength(small.datumCbor, "hex")).toBe(41);
    expect(Buffer.byteLength(maximum.datumCbor, "hex")).toBe(4_268);
    expect(minimum(small)).toBe(1_064_570n);
    expect(minimum(maximum)).toBe(19_287_250n);
  });

  it("pins the immutable complete CEK publication datum ABI, hash, and caller copies", () => {
    const { envelopeCbor, entry, sidecarCbor } = completeCekPublicationInput();
    const publication = SDK.deriveCekSinglePublicationV1({
      envelopeCbor,
      sidecarCbor,
    });
    const expectedHash = Buffer.from(
      hashMidgardCekProgramEnvelopeV1({
        uplcVersion: [1n, 1n, 0n],
        termRoot: entry.root,
        nodeCount: 1n,
        materialByteLength: BigInt(entry.preimage.length),
      }),
    ).toString("hex");

    envelopeCbor[0] = envelopeCbor[0]! ^ 0x01;
    sidecarCbor[0] = sidecarCbor[0]! ^ 0x01;

    expect(publication.programEnvelopeHash).toBe(expectedHash);
    expect(publication.datum).toEqual({
      version: 1n,
      program_envelope_hash: expectedHash,
      sidecar_cbor: encodeMidgardCekProgramMaterialSidecarV1([entry]).toString(
        "hex",
      ),
    });
    expect(publication.datumCbor).toBe(
      "d8799f015820598a113063682ad2a899e44099a9e1e1b4440603eee17f2a860bab65c10cb0a9582d8201818258204c623a62d6dedf81bb74b1cf56f0b3e8ec85ed24ffb0b821b2d796c4f85a5d3d46830100428106ff",
    );
    expect(
      SDK.decodeCekSinglePublicationDatumV1Cbor(
        Buffer.from(publication.datumCbor, "hex"),
      ),
    ).toEqual(publication.datum);
    expect(
      SDK.encodeCekSinglePublicationDatumV1Cbor(publication.datum).toString(
        "hex",
      ),
    ).toBe(publication.datumCbor);
  });

  it("rejects noncanonical, malformed, and oversized complete-publication datum bytes", () => {
    const { envelopeCbor, sidecarCbor } = completeCekPublicationInput();
    const publication = SDK.deriveCekSinglePublicationV1({
      envelopeCbor,
      sidecarCbor,
    });
    const datumCbor = Buffer.from(publication.datumCbor, "hex");

    expect(() =>
      SDK.decodeCekSinglePublicationDatumV1Cbor(
        Buffer.concat([datumCbor, Buffer.from([0])]),
      ),
    ).toThrow(/canonical encoding/u);
    expect(() =>
      SDK.encodeCekSinglePublicationDatumV1Cbor({
        ...publication.datum,
        version: 2n,
      }),
    ).toThrow(/version 1/u);
    expect(() =>
      SDK.encodeCekSinglePublicationDatumV1Cbor({
        ...publication.datum,
        program_envelope_hash: "00".repeat(31),
      }),
    ).toThrow();
    expect(() =>
      SDK.encodeCekSinglePublicationDatumV1Cbor({
        ...publication.datum,
        sidecar_cbor: "00".repeat(
          MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableCompleteItemPublicationDatumBytes,
        ),
      }),
    ).toThrow(/datum envelope/u);
  });

  it("rejects incomplete, extra, substituted, unordered, count, byte-length, and trailing complete graph inputs", () => {
    const { envelopeCbor, entry, sidecarCbor } = completeCekPublicationInput();
    const extraPreimage = encodeMidgardCekBlobChunkV1(Buffer.from([0x99]));
    const extra = {
      kind: "blobChunk" as const,
      root: hashMidgardCekProgramMaterialPreimageV1("blobChunk", extraPreimage),
      preimage: extraPreimage,
    };
    const reordered = [entry, extra].sort((left, right) =>
      Buffer.compare(Buffer.from(right.root), Buffer.from(left.root)),
    );
    const unorderedSidecar = encodeCbor([
      1n,
      reordered.map((item) => [
        Buffer.from(item.root),
        encodeMidgardCekProgramMaterialDaValueV1(item),
      ]),
    ]);
    const decodedEnvelope = {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot: entry.root,
      nodeCount: 1n,
      materialByteLength: BigInt(entry.preimage.length),
    };

    expect(() =>
      SDK.deriveCekSinglePublicationV1({
        envelopeCbor,
        sidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([]),
      }),
    ).toThrow(/missing/i);
    expect(() =>
      SDK.deriveCekSinglePublicationV1({
        envelopeCbor,
        sidecarCbor: encodeMidgardCekProgramMaterialSidecarV1([entry, extra]),
      }),
    ).toThrow(/unreachable/i);
    expect(() =>
      SDK.deriveCekSinglePublicationV1({
        envelopeCbor,
        sidecarCbor: Buffer.concat([sidecarCbor, Buffer.from([0])]),
      }),
    ).toThrow(/trailing/i);
    expect(() =>
      SDK.deriveCekSinglePublicationV1({
        envelopeCbor,
        sidecarCbor: unorderedSidecar,
      }),
    ).toThrow(/sorted|canonical/i);
    expect(() =>
      SDK.deriveCekSinglePublicationV1({
        envelopeCbor: encodeMidgardCekProgramEnvelopeV1({
          ...decodedEnvelope,
          nodeCount: 2n,
        }),
        sidecarCbor,
      }),
    ).toThrow(/material nodes.*declares 2/i);
    expect(() =>
      SDK.deriveCekSinglePublicationV1({
        envelopeCbor: encodeMidgardCekProgramEnvelopeV1({
          ...decodedEnvelope,
          materialByteLength: decodedEnvelope.materialByteLength + 1n,
        }),
        sidecarCbor,
      }),
    ).toThrow(/material bytes.*declares/i);
    const substituted = Buffer.from(sidecarCbor);
    substituted[substituted.length - 1] = substituted.at(-1)! ^ 0x01;
    expect(() =>
      SDK.deriveCekSinglePublicationV1({
        envelopeCbor,
        sidecarCbor: substituted,
      }),
    ).toThrow();
    expect(() =>
      SDK.deriveCekSinglePublicationV1({
        envelopeCbor: Buffer.concat([envelopeCbor, Buffer.from([0])]),
        sidecarCbor,
      }),
    ).toThrow(/trailing/i);
  });

  it("publishes one immutable complete graph output at exact min-Ada", async () => {
    const { envelopeCbor, sidecarCbor } = completeCekPublicationInput();
    const publication = SDK.deriveCekSinglePublicationV1({
      envelopeCbor,
      sidecarCbor,
    });
    const minimumLovelace = SDK.minimumLovelaceForCekSinglePublicationV1({
      contracts: cekProgramMaterialContracts,
      publication,
      coinsPerUtxoByte: 4_310n,
    });
    expect(minimumLovelace).toBe(1_258_520n);
    const fundedLovelace: bigint[] = [];
    const outputs: { address: string; datum: unknown; lovelace: bigint }[] = [];
    const lucid = materialPublicationLucid({
      coinsPerUtxoByte: 4_310n,
      fundedLovelace,
      outputs,
    });

    await SDK.unsignedCekSinglePublicationV1(
      lucid,
      cekProgramMaterialContracts as SDK.MidgardValidators,
      {
        envelopeCbor,
        sidecarCbor,
        lovelace: minimumLovelace - 1n,
      },
    );

    expect(fundedLovelace).toEqual([minimumLovelace]);
    expect(outputs).toEqual([
      {
        address: cekProgramMaterialAddress,
        datum: { kind: "inline", value: publication.datumCbor },
        lovelace: minimumLovelace,
      },
    ]);
  });
});
