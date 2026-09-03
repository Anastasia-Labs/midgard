import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardCekBlobChunk,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialDaValue,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekTermNode,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
  hashMidgardCekProgramEnvelope,
  hashMidgardCekProgramMaterialPreimage,
  hashMidgardCekTermNode,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_ENVELOPE_MEASUREMENTS,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import {
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import { CML, Data, type LucidEvolution } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import * as SDK from "../src/index.js";

const transactionCbor = (): Buffer =>
  encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
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
  const preimage = encodeMidgardCekBlobChunk(Buffer.alloc(bytes, 0x5a));
  return SDK.deriveCekProgramMaterialPublications([
    {
      kind: "blobChunk",
      root: hashMidgardCekProgramMaterialPreimage("blobChunk", preimage),
      preimage,
    },
  ])[0]!;
};

const completeCekPublicationInput = () => {
  const term = { kind: "error" } as const;
  const preimage = encodeMidgardCekTermNode(term);
  const root = hashMidgardCekTermNode(term);
  const entry = { kind: "term" as const, root, preimage };
  const envelopeCbor = encodeMidgardCekProgramEnvelope({
    uplcVersion: [1n, 1n, 0n],
    termRoot: root,
    nodeCount: 1n,
    materialByteLength: BigInt(preimage.length),
  });
  return {
    envelopeCbor,
    entry,
    sidecarCbor: encodeMidgardCekProgramMaterialSidecar([entry]),
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
    const source: SDK.NativeTxProofSource = {
      compact_cbor: "01",
      witness_set_compact_cbor: "0203",
      field_preimage_lengths_cbor: "04",
    };
    const payload: SDK.TxOrderPayload = {
      tx_id: "44".repeat(32),
      transaction_commitment: "55".repeat(32),
      source,
    };
    const event: SDK.TxOrderEvent = { id: txOrderId, tx: payload };
    const datum: SDK.TxOrderDatum = {
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
    const forced: SDK.ForcedInclusionTx = {
      tx_id: payload.tx_id,
      source,
      verdict: {
        ForcedTxInvalid: {
          reason: { PlutusExecutionFailed: { execution_index: 0n } },
        },
      },
    };
    const spend: SDK.TxOrderSpendRedeemer = {
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
        value: Data.to(forced, SDK.ForcedInclusionTx),
        proof: [],
      },
      inclusion_proof_script_withdraw_redeemer_index: 5n,
      validity_override: {
        ForcedTxInvalid: {
          reason: { PlutusExecutionFailed: { execution_index: 0n } },
        },
      },
    };

    const datumCbor = Data.to(datum, SDK.TxOrderDatum);
    expect(Data.to(txOrderId, SDK.OutputReference)).toBe(
      `d8799f5820${"33".repeat(32)}04ff`,
    );
    expect(Data.to(payload, SDK.TxOrderPayload)).toBe(
      `d8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffff`,
    );
    expect(Data.to(event, SDK.TxOrderEvent)).toBe(
      `d8799fd8799f5820${"33".repeat(32)}04ffd8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffffff`,
    );
    expect(datumCbor).toBe(
      `d8799fd8799fd8799f5820${"33".repeat(32)}04ffd8799f5820${"44".repeat(32)}5820${"55".repeat(32)}d8799f41014202034104ffffff187b581c${"66".repeat(28)}d8799fd8799f581c${"77".repeat(28)}ffd87a80ffd87980ff`,
    );
    expect(Data.to(forced, SDK.ForcedInclusionTx)).toBe(
      `d8799f5820${"44".repeat(32)}d8799f41014202034104ffd87a9fd905229f00ffffff`,
    );
    expect(Data.to(spend, SDK.TxOrderSpendRedeemer)).toBe(
      "d8799f0001020304d8799fd87a805820000000000000000000000000000000000000000000000000000000000000000058201111111111111111111111111111111111111111111111111111111111111111015827d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff583bd8799f58204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffd87a9fd905229f00ffffff80ff05d87a9fd905229f00ffffff",
    );
    expect(
      SDK.decodeTxOrderDatumCbor(SDK.encodeTxOrderDatumCbor(datum)),
    ).toEqual(datum);

    const overlongInclusionTime = datumCbor.replace(
      "187b581c",
      "1a0000007b581c",
    );
    expect(() =>
      SDK.decodeTxOrderDatumCbor(Buffer.from(overlongInclusionTime, "hex")),
    ).toThrow(/exact canonical encoding/u);
    expect(() =>
      Data.from(`d87a${datumCbor.slice(4)}`, SDK.TxOrderDatum),
    ).toThrow();
    expect(() =>
      Data.from(`${datumCbor.slice(0, -2)}00ff`, SDK.TxOrderDatum),
    ).toThrow();

    const pointerDatum: SDK.TxOrderDatum = {
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
      Data.from(Data.to(pointerDatum, SDK.TxOrderDatum), SDK.TxOrderDatum),
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
        SDK.TxOrderDatum,
      ),
    ).toThrow();
  });

  it("derives the §8 carriage of every non-empty field and nothing for the empty ones", () => {
    const cbor = transactionCbor();
    const material = SDK.deriveTxOrderMaterial({
      nativeTxCbor: cbor,
      owner: Buffer.alloc(28, 0x44),
    });

    expect(cbor.length).toBeGreaterThan(8 * 1024);
    expect(cbor.length).toBeLessThanOrEqual(
      MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes,
    );
    // Only field 2 (outputs) carries anything in this fixture, so the carriage
    // list is exactly one entry: the counted scheme published four per-item
    // chunks for the same bytes, which is the whole difference the §4 reversion
    // makes to a publisher.
    expect(material.carriage.map((field) => field.fieldIndex)).toEqual([2]);
    const outputs = material.carriage[0]!;
    expect(outputs.fieldName).toBe("outputs");
    expect(outputs.preimage).toEqual(
      decodeMidgardNativeTxFullFromCanonicalCbor(cbor).body.outputsPreimageCbor,
    );
    expect(outputs.commitment).toBe(
      midgardFieldCommitment(outputs.preimage).toString("hex"),
    );
    // §8.4 is a partition, so the tier is a fact about the byte length and not a
    // choice this module makes.
    expect(outputs.plan.tier).toBe(
      selectMidgardFieldCarriageTier(outputs.preimage.length),
    );
    expect(outputs.plan.totalLength).toBe(outputs.preimage.length);
    expect(outputs.plan.commitment.toString("hex")).toBe(outputs.commitment);
    expect(outputs.plan.txId.toString("hex")).toBe(material.transactionId);
  });

  it("plans inline carriage under the order reserve and publishes what will not fit", () => {
    const cbor = transactionCbor();
    const owner = Buffer.alloc(28, 0x44);
    const material = SDK.deriveTxOrderMaterial({ nativeTxCbor: cbor, owner });
    const [outputs] = material.carriage;
    expect(outputs).toBeDefined();

    // Under the default reserve the one field fits the order transaction's own
    // redeemer, so nothing has to be published first.
    const inline = SDK.planTxOrderMaterialCarriage({ material, owner });
    expect(inline.carriage.map((field) => field.plan.tier)).toEqual(["Inline"]);
    expect(inline.referenced).toEqual([]);
    expect(inline.inlineBytes).toBe(outputs!.preimage.length);
    expect(inline.inlineReserveBytes).toBe(
      SDK.MIDGARD_TX_ORDER_INLINE_CARRIAGE_RESERVE_BYTES,
    );

    // One byte short of the field's own length is the whole decision: there is no
    // consensus threshold here, only this transaction's budget (§8.11), so the
    // same field is demoted to tier 2 and gets one publication at the creator's
    // own wallet address.
    const published = SDK.planTxOrderMaterialCarriage({
      material,
      owner,
      inlineReserveBytes: outputs!.preimage.length - 1,
    });
    expect(published.inline).toEqual([]);
    expect(published.inlineBytes).toBe(0);
    expect(published.referenced.map((field) => field.plan.tier)).toEqual([
      "RawUtxo",
    ]);
    const [demoted] = published.referenced;
    expect(demoted!.plan.inlinePreimage).toBeNull();
    expect(demoted!.plan.publications.map((entry) => entry.bytes)).toEqual([
      outputs!.preimage,
    ]);
    // Demotion changes whose budget pays and nothing about the bytes: the §4
    // commitment the door checks is the same one either way.
    expect(demoted!.plan.commitment).toEqual(outputs!.plan.commitment);
  });

  it("pins the tx-order mint redeemer wire form against its Aiken vector", () => {
    // The same three bytes-level vectors
    // `midgard/user_events/tx_order_v1.test`'s
    // `tx_order_mint_redeemer_wire_form_is_the_event_plus_the_carriage_vector`
    // pins. Both halves emit `Constr 0 [<user event redeemer>, <carriage list>]`,
    // and the carriage constructors keep §8.8's frozen tags 0/1/2.
    const event = {
      AuthenticateEvent: {
        nonce_input_index: 0n,
        event_output_index: 1n,
        hub_ref_input_index: 2n,
        witness_registration_redeemer_index: 3n,
      },
    } as const;

    expect(
      Data.to(
        { event, material_carriage: [] } satisfies SDK.TxOrderMintRedeemer,
        SDK.TxOrderMintRedeemer,
      ),
    ).toBe("d8799fd8799f00010203ff80ff");
    expect(
      Data.to(
        {
          event: {
            BurnEventNFT: {
              nonce_asset_name: "aabb",
              witness_unregistration_redeemer_index: 4n,
            },
          },
          material_carriage: [],
        } satisfies SDK.TxOrderMintRedeemer,
        SDK.TxOrderMintRedeemer,
      ),
    ).toBe("d8799fd87a9f42aabb04ff80ff");
    expect(
      Data.to(
        {
          event,
          material_carriage: [
            { Inline: { preimage: "80" } },
            { RawUtxo: { ref_input_index: 5n } },
            {
              Certified: {
                cert_ref_input_index: 6n,
                chunk_ref_input_indices: [7n, 8n],
              },
            },
          ],
        } satisfies SDK.TxOrderMintRedeemer,
        SDK.TxOrderMintRedeemer,
      ),
    ).toBe(
      "d8799fd8799f00010203ff9fd8799f4180ffd87a9f05ffd87b9f069f0708ffffffff",
    );
  });

  it("refuses material it cannot bind to a canonical transaction", () => {
    expect(() =>
      SDK.deriveTxOrderMaterial({
        nativeTxCbor: Buffer.from("00", "hex"),
        owner: Buffer.alloc(28, 0x44),
      }),
    ).toThrow();
  });

  it("encodes immutable content-addressed L1 program material below the independent field bound", () => {
    const preimage = encodeMidgardCekBlobChunk(Buffer.alloc(4_095, 0x5a));
    const root = hashMidgardCekProgramMaterialPreimage("blobChunk", preimage);
    const [publication] = SDK.deriveCekProgramMaterialPublications([
      { kind: "blobChunk", root, preimage },
    ]);

    expect(
      Data.from(publication!.datumCbor, SDK.CekProgramMaterialDatum),
    ).toEqual(publication!.datum);
    expect(publication!.datum.root).toBe(Buffer.from(root).toString("hex"));
    expect(Buffer.byteLength(publication!.datumCbor, "hex")).toBe(
      MIDGARD_ENVELOPE_MEASUREMENTS.maxProgramMaterialPublicationDatumBytes,
    );
    expect(Buffer.byteLength(publication!.datumCbor, "hex")).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes,
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
      MIDGARD_ENVELOPE_MEASUREMENTS.maxProgramMaterialPublicationUnsignedTransactionBytes,
    );
    expect(tx.to_cbor_bytes().length).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes,
    );
    expect(() =>
      SDK.deriveCekProgramMaterialPublications([
        { kind: "blobChunk", root, preimage },
        { kind: "blobChunk", root, preimage },
      ]),
    ).toThrow(/duplicate/u);
    expect(() =>
      SDK.deriveCekProgramMaterialPublications([
        {
          kind: "blobChunk",
          root: Buffer.alloc(32) as never,
          preimage,
        },
      ]),
    ).toThrow(/root does not match/u);
    expect(() =>
      SDK.deriveCekProgramMaterialPublications([
        { kind: "blobBranch", root, preimage },
      ]),
    ).toThrow(/root does not match/u);
    expect(() =>
      SDK.deriveCekProgramMaterialPublications([
        { kind: "unknown", root, preimage } as never,
      ]),
    ).toThrow();
  });

  it("derives the exact stabilized min-Ada vector for CEK material at its actual script address", () => {
    const publication = cekProgramMaterialPublication(4_095);
    const minimumLovelace = SDK.minimumLovelaceForCekProgramMaterialPublication(
      {
        contracts: cekProgramMaterialContracts,
        publication,
        coinsPerUtxoByte: 4_310n,
      },
    );

    expect(Buffer.byteLength(publication.datumCbor, "hex")).toBe(4_268);
    expect(minimumLovelace).toBe(19_287_250n);
  });

  it("raises adjacent underfunded CEK material publication funding to exact min-Ada", async () => {
    const publication = cekProgramMaterialPublication(64);
    const minimumLovelace = SDK.minimumLovelaceForCekProgramMaterialPublication(
      {
        contracts: cekProgramMaterialContracts,
        publication,
        coinsPerUtxoByte: 4_310n,
      },
    );
    expect(Buffer.byteLength(publication.datumCbor, "hex")).toBe(110);
    expect(minimumLovelace).toBe(1_361_960n);
    const fundedLovelace: bigint[] = [];
    const lucid = materialPublicationLucid({
      coinsPerUtxoByte: 4_310n,
      fundedLovelace,
    });

    await SDK.unsignedCekProgramMaterial(
      lucid,
      cekProgramMaterialContracts as SDK.MidgardValidators,
      {
        entries: [publication.entry],
        lovelacePerEntry: minimumLovelace - 1n,
      },
    );
    await SDK.unsignedCekProgramMaterial(
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
      SDK.minimumLovelaceForCekProgramMaterialPublication({
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
    const publication = SDK.deriveCekSinglePublication({
      envelopeCbor,
      sidecarCbor,
    });
    const expectedHash = Buffer.from(
      hashMidgardCekProgramEnvelope({
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
      sidecar_cbor: encodeMidgardCekProgramMaterialSidecar([entry]).toString(
        "hex",
      ),
    });
    expect(publication.datumCbor).toBe(
      "d8799f015820598a113063682ad2a899e44099a9e1e1b4440603eee17f2a860bab65c10cb0a9582d8201818258204c623a62d6dedf81bb74b1cf56f0b3e8ec85ed24ffb0b821b2d796c4f85a5d3d46830100428106ff",
    );
    expect(
      SDK.decodeCekSinglePublicationDatumCbor(
        Buffer.from(publication.datumCbor, "hex"),
      ),
    ).toEqual(publication.datum);
    expect(
      SDK.encodeCekSinglePublicationDatumCbor(publication.datum).toString(
        "hex",
      ),
    ).toBe(publication.datumCbor);
  });

  it("rejects noncanonical, malformed, and oversized complete-publication datum bytes", () => {
    const { envelopeCbor, sidecarCbor } = completeCekPublicationInput();
    const publication = SDK.deriveCekSinglePublication({
      envelopeCbor,
      sidecarCbor,
    });
    const datumCbor = Buffer.from(publication.datumCbor, "hex");

    expect(() =>
      SDK.decodeCekSinglePublicationDatumCbor(
        Buffer.concat([datumCbor, Buffer.from([0])]),
      ),
    ).toThrow(/canonical encoding/u);
    expect(() =>
      SDK.encodeCekSinglePublicationDatumCbor({
        ...publication.datum,
        version: 2n,
      }),
    ).toThrow(/version 1/u);
    expect(() =>
      SDK.encodeCekSinglePublicationDatumCbor({
        ...publication.datum,
        program_envelope_hash: "00".repeat(31),
      }),
    ).toThrow();
    expect(() =>
      SDK.encodeCekSinglePublicationDatumCbor({
        ...publication.datum,
        sidecar_cbor: "00".repeat(
          MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableCompleteItemPublicationDatumBytes,
        ),
      }),
    ).toThrow(/datum envelope/u);
  });

  it("rejects incomplete, extra, substituted, unordered, count, byte-length, and trailing complete graph inputs", () => {
    const { envelopeCbor, entry, sidecarCbor } = completeCekPublicationInput();
    const extraPreimage = encodeMidgardCekBlobChunk(Buffer.from([0x99]));
    const extra = {
      kind: "blobChunk" as const,
      root: hashMidgardCekProgramMaterialPreimage("blobChunk", extraPreimage),
      preimage: extraPreimage,
    };
    const reordered = [entry, extra].sort((left, right) =>
      Buffer.compare(Buffer.from(right.root), Buffer.from(left.root)),
    );
    const unorderedSidecar = encodeCbor([
      1n,
      reordered.map((item) => [
        Buffer.from(item.root),
        encodeMidgardCekProgramMaterialDaValue(item),
      ]),
    ]);
    const decodedEnvelope = {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot: entry.root,
      nodeCount: 1n,
      materialByteLength: BigInt(entry.preimage.length),
    };

    expect(() =>
      SDK.deriveCekSinglePublication({
        envelopeCbor,
        sidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
      }),
    ).toThrow(/missing/i);
    expect(() =>
      SDK.deriveCekSinglePublication({
        envelopeCbor,
        sidecarCbor: encodeMidgardCekProgramMaterialSidecar([entry, extra]),
      }),
    ).toThrow(/unreachable/i);
    expect(() =>
      SDK.deriveCekSinglePublication({
        envelopeCbor,
        sidecarCbor: Buffer.concat([sidecarCbor, Buffer.from([0])]),
      }),
    ).toThrow(/trailing/i);
    expect(() =>
      SDK.deriveCekSinglePublication({
        envelopeCbor,
        sidecarCbor: unorderedSidecar,
      }),
    ).toThrow(/sorted|canonical/i);
    expect(() =>
      SDK.deriveCekSinglePublication({
        envelopeCbor: encodeMidgardCekProgramEnvelope({
          ...decodedEnvelope,
          nodeCount: 2n,
        }),
        sidecarCbor,
      }),
    ).toThrow(/material nodes.*declares 2/i);
    expect(() =>
      SDK.deriveCekSinglePublication({
        envelopeCbor: encodeMidgardCekProgramEnvelope({
          ...decodedEnvelope,
          materialByteLength: decodedEnvelope.materialByteLength + 1n,
        }),
        sidecarCbor,
      }),
    ).toThrow(/material bytes.*declares/i);
    const substituted = Buffer.from(sidecarCbor);
    substituted[substituted.length - 1] = substituted.at(-1)! ^ 0x01;
    expect(() =>
      SDK.deriveCekSinglePublication({
        envelopeCbor,
        sidecarCbor: substituted,
      }),
    ).toThrow();
    expect(() =>
      SDK.deriveCekSinglePublication({
        envelopeCbor: Buffer.concat([envelopeCbor, Buffer.from([0])]),
        sidecarCbor,
      }),
    ).toThrow(/trailing/i);
  });

  it("publishes one immutable complete graph output at exact min-Ada", async () => {
    const { envelopeCbor, sidecarCbor } = completeCekPublicationInput();
    const publication = SDK.deriveCekSinglePublication({
      envelopeCbor,
      sidecarCbor,
    });
    const minimumLovelace = SDK.minimumLovelaceForCekSinglePublication({
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

    await SDK.unsignedCekSinglePublication(
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
