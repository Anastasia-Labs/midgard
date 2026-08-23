import {
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  buildMidgardLedgerOutputProofTraceV1,
  commitMidgardLedgerOutputReferenceScriptItemV1,
  commitMidgardValidationMerkleFrontierV1,
  decodeMidgardLedgerOutputCommitmentV1,
  digestMidgardLedgerOutputReferenceScriptV1,
  encodeMidgardLedgerOutputCommitmentV1,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
  summarizeMidgardLedgerOutputCardanoSpendDatumV1,
  summarizeMidgardLedgerOutputCardanoTxOutV1,
  summarizeMidgardLedgerOutputMidgardTxOutV1,
  verifyMidgardLedgerOutputChunkV1,
  verifyMidgardLedgerOutputDescriptorV1,
  verifyMidgardLedgerOutputReferenceScriptChunkV1,
} from "@al-ft/midgard-core";
import {
  decodeMidgardDatum,
  encodeMidgardSpendInputItemV1,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildCanonicalMidgardLedgerEntryOutputMaterialV1,
  buildCanonicalMidgardLedgerOutputMaterialV1,
} from "../src/ledger-output-descriptor.js";

const protectedScriptAddress = Buffer.concat([
  Buffer.from([0x78]),
  Buffer.alloc(28, 0x11),
]);

const outputFixture = (): {
  readonly output: MidgardTxOutput;
  readonly cbor: Buffer;
} => {
  const output: MidgardTxOutput = {
    address: protectedScriptAddress,
    value: {
      lovelace: 8_000_000n,
      assets: new Map([
        [
          "55".repeat(28),
          new Map([
            ["", 1n],
            ["0102", 42n],
          ]),
        ],
      ]),
    },
    datum: decodeMidgardDatum(Buffer.from(Data.to("ab".repeat(5_000)), "hex")),
    script_ref: {
      language: "PlutusV3",
      scriptBytes: Buffer.alloc(100, 0x6b),
    },
  };
  return { output, cbor: encodeMidgardTxOutput(output) };
};

describe("canonical ledger output descriptor V1", () => {
  it("derives the consensus value from an exact canonical ledger out-ref", () => {
    const fixture = outputFixture();
    // The ledger out-ref key is §5.3's field-0/1 item, so it is built with that
    // encoder rather than written out as a literal: a literal here could drift
    // from the encoder the ledger and the on-chain `ledger_outref_key` use.
    const outRef = encodeMidgardSpendInputItemV1({
      txId: Buffer.alloc(32, 0x42),
      outputIndex: 7,
    });
    const fromEntry = buildCanonicalMidgardLedgerEntryOutputMaterialV1({
      outRef,
      outputCbor: fixture.cbor,
    });
    const fromCreation = buildCanonicalMidgardLedgerOutputMaterialV1({
      outputIndex: 7,
      outputCbor: fixture.cbor,
    });

    expect(fromEntry.descriptorCbor).toStrictEqual(fromCreation.descriptorCbor);
    expect(fromEntry.descriptor.outputIndex).toBe(7);
  });

  it("fails closed for non-canonical or out-of-domain ledger out-refs", () => {
    const fixture = outputFixture();
    const txIdHex = "42".repeat(32);
    // §6.1: one valid byte form. Each of these is a different way of naming the
    // same out-ref, and every one of them must reject.
    const rejected = {
      // Indefinite-length array.
      indefinite: `9f5820${txIdHex}07ff`,
      // CML's minimal one-byte index — the spelling every producer emitted
      // before #586, and the one on-chain `decode_midgard_tx_input_cbor` rejects
      // when it asserts the `0x19` head. 36 bytes.
      minimalIndex: `825820${txIdHex}07`,
      // The two-byte minimal form for 24..255.
      shortIndex: `825820${txIdHex}1818`,
      // Index 65,536 as a minimal uint32 — outside §5.3's uint16 domain.
      oversizedIndex: `825820${txIdHex}1a00010000`,
      // A uint16 head with a trailing byte, so the width is wrong.
      trailingByte: `825820${txIdHex}19000700`,
    };

    for (const [label, hex] of Object.entries(rejected)) {
      expect(() =>
        buildCanonicalMidgardLedgerEntryOutputMaterialV1({
          outRef: Buffer.from(hex, "hex"),
          outputCbor: fixture.cbor,
        }),
      ).toThrow();
      expect(label).toBeTruthy();
    }
  });

  it("derives every compact fact from a multi-chunk canonical output", () => {
    const fixture = outputFixture();
    const material = buildCanonicalMidgardLedgerOutputMaterialV1({
      outputIndex: 7,
      outputCbor: fixture.cbor,
    });
    const repeated = buildCanonicalMidgardLedgerOutputMaterialV1({
      outputIndex: 7,
      outputCbor: fixture.cbor,
    });

    expect(material.item.bytes.length).toBeGreaterThan(4_095);
    expect(material.item.bytes.length).toBeLessThan(16_384);
    expect(material.descriptor.cardanoValueSize).toBeLessThanOrEqual(5_000);
    expect(material.descriptorCbor).toStrictEqual(repeated.descriptorCbor);
    expect(material.descriptor.assetCount).toBe(2);
    expect(material.descriptor.address).toStrictEqual(protectedScriptAddress);
    expect(material.descriptor.referenceScriptLanguage).toBe(3);
    expect(material.descriptor.referenceScriptHash).toHaveLength(28);
    expect(material.descriptor.referenceScriptItemCommitment).toHaveLength(32);
    expect(material.descriptor.cardanoTxOut.root).not.toStrictEqual(
      material.descriptor.midgardTxOut.root,
    );
    expect(
      encodeMidgardLedgerOutputCommitmentV1(
        decodeMidgardLedgerOutputCommitmentV1(material.descriptorCbor),
      ),
    ).toStrictEqual(material.descriptorCbor);

    for (
      let chunkIndex = 0;
      chunkIndex < material.item.frontier.count;
      chunkIndex += 1
    ) {
      expect(
        verifyMidgardLedgerOutputChunkV1({
          descriptor: material.descriptor,
          proof: buildMidgardBoundedItemChunkProofV1(material.item, chunkIndex),
        }),
      ).toBe(true);
    }

    const scriptCbor = encodeMidgardVersionedScript(fixture.output.script_ref!);
    const scriptItem = buildMidgardBoundedItemV1({
      fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
      itemIndex: 7,
      bytes: scriptCbor,
    });
    expect(scriptItem.commitment).toStrictEqual(
      material.descriptor.referenceScriptItemCommitment,
    );
    for (
      let chunkIndex = 0;
      chunkIndex < scriptItem.frontier.count;
      chunkIndex += 1
    ) {
      expect(
        verifyMidgardLedgerOutputReferenceScriptChunkV1({
          descriptor: material.descriptor,
          proof: buildMidgardBoundedItemChunkProofV1(scriptItem, chunkIndex),
        }),
      ).toBe(true);
    }
  });

  it("matches every descriptor fact currently authenticated by the L1 scan", () => {
    const fixture = outputFixture();
    const material = buildCanonicalMidgardLedgerOutputMaterialV1({
      outputIndex: 7,
      outputCbor: fixture.cbor,
    });
    const terminal = buildMidgardLedgerOutputProofTraceV1({
      outputIndex: 7,
      outputCbor: fixture.cbor,
    }).terminal;
    const scan = terminal.outputScan;

    expect(scan.address).toStrictEqual(material.descriptor.address);
    expect(scan.lovelace).toBe(material.descriptor.lovelace);
    expect(scan.assetFrontier.count).toBe(material.descriptor.assetCount);
    expect(
      commitMidgardValidationMerkleFrontierV1(scan.assetFrontier),
    ).toStrictEqual(material.descriptor.assetFrontierCommitment);
    expect(scan.cardanoValueSize).toBe(material.descriptor.cardanoValueSize);
    expect(scan.referenceScriptLanguage).toBe(
      material.descriptor.referenceScriptLanguage,
    );
    expect(
      summarizeMidgardLedgerOutputCardanoSpendDatumV1(terminal),
    ).toStrictEqual(material.descriptor.cardanoSpendDatum);
    expect(summarizeMidgardLedgerOutputCardanoTxOutV1(terminal)).toStrictEqual(
      material.descriptor.cardanoTxOut,
    );
    expect(summarizeMidgardLedgerOutputMidgardTxOutV1(terminal)).toStrictEqual(
      material.descriptor.midgardTxOut,
    );
    expect(terminal.totalLength - scan.referenceScriptItemOffset).toBe(
      material.descriptor.referenceScriptTotalLength,
    );
    expect(digestMidgardLedgerOutputReferenceScriptV1(terminal)).toStrictEqual(
      material.descriptor.referenceScriptHash,
    );
    expect(
      commitMidgardLedgerOutputReferenceScriptItemV1(terminal),
    ).toStrictEqual(material.descriptor.referenceScriptItemCommitment);
    expect(
      verifyMidgardLedgerOutputDescriptorV1({
        control: terminal,
        descriptor: material.descriptor,
      }),
    ).toBe(true);

    const changedBytes = (bytes: Uint8Array): Buffer => {
      const changed = Buffer.from(bytes);
      changed[0] = changed[0]! ^ 1;
      return changed;
    };
    const descriptor = material.descriptor;
    const substitutions = [
      { ...descriptor, outputIndex: descriptor.outputIndex + 1 },
      { ...descriptor, totalLength: descriptor.totalLength + 1 },
      {
        ...descriptor,
        itemCommitment: changedBytes(descriptor.itemCommitment),
      },
      { ...descriptor, address: changedBytes(descriptor.address) },
      { ...descriptor, lovelace: descriptor.lovelace + 1n },
      { ...descriptor, assetCount: descriptor.assetCount + 1 },
      {
        ...descriptor,
        assetFrontierCommitment: changedBytes(
          descriptor.assetFrontierCommitment,
        ),
      },
      {
        ...descriptor,
        cardanoValueSize: descriptor.cardanoValueSize + 1,
      },
      { ...descriptor, referenceScriptLanguage: 0 as const },
      {
        ...descriptor,
        referenceScriptHash: changedBytes(descriptor.referenceScriptHash),
      },
      {
        ...descriptor,
        referenceScriptTotalLength: descriptor.referenceScriptTotalLength + 1,
      },
      {
        ...descriptor,
        referenceScriptItemCommitment: changedBytes(
          descriptor.referenceScriptItemCommitment,
        ),
      },
      {
        ...descriptor,
        cardanoTxOut: {
          ...descriptor.cardanoTxOut,
          root: changedBytes(descriptor.cardanoTxOut.root),
        },
      },
      {
        ...descriptor,
        cardanoTxOut: {
          ...descriptor.cardanoTxOut,
          cborLength: descriptor.cardanoTxOut.cborLength + 1n,
        },
      },
      {
        ...descriptor,
        cardanoTxOut: {
          ...descriptor.cardanoTxOut,
          memory: descriptor.cardanoTxOut.memory + 1n,
        },
      },
      {
        ...descriptor,
        midgardTxOut: {
          ...descriptor.midgardTxOut,
          root: changedBytes(descriptor.midgardTxOut.root),
        },
      },
      {
        ...descriptor,
        midgardTxOut: {
          ...descriptor.midgardTxOut,
          cborLength: descriptor.midgardTxOut.cborLength + 1n,
        },
      },
      {
        ...descriptor,
        midgardTxOut: {
          ...descriptor.midgardTxOut,
          memory: descriptor.midgardTxOut.memory + 1n,
        },
      },
      {
        ...descriptor,
        cardanoSpendDatum: {
          ...descriptor.cardanoSpendDatum,
          root: changedBytes(descriptor.cardanoSpendDatum.root),
        },
      },
      {
        ...descriptor,
        cardanoSpendDatum: {
          ...descriptor.cardanoSpendDatum,
          cborLength: descriptor.cardanoSpendDatum.cborLength + 1n,
        },
      },
      {
        ...descriptor,
        cardanoSpendDatum: {
          ...descriptor.cardanoSpendDatum,
          memory: descriptor.cardanoSpendDatum.memory + 1n,
        },
      },
    ];
    for (const substituted of substitutions) {
      expect(
        verifyMidgardLedgerOutputDescriptorV1({
          control: terminal,
          descriptor: substituted,
        }),
      ).toBe(false);
    }

    const {
      datum: _datum,
      script_ref: _scriptRef,
      ...withoutDatum
    } = fixture.output;
    const withoutDatumCbor = encodeMidgardTxOutput(withoutDatum);
    const withoutDatumMaterial = buildCanonicalMidgardLedgerOutputMaterialV1({
      outputIndex: 7,
      outputCbor: withoutDatumCbor,
    });
    const withoutDatumTerminal = buildMidgardLedgerOutputProofTraceV1({
      outputIndex: 7,
      outputCbor: withoutDatumCbor,
    }).terminal;
    expect(
      summarizeMidgardLedgerOutputCardanoSpendDatumV1(withoutDatumTerminal),
    ).toStrictEqual(withoutDatumMaterial.descriptor.cardanoSpendDatum);
    expect(
      summarizeMidgardLedgerOutputCardanoTxOutV1(withoutDatumTerminal),
    ).toStrictEqual(withoutDatumMaterial.descriptor.cardanoTxOut);
    expect(
      summarizeMidgardLedgerOutputMidgardTxOutV1(withoutDatumTerminal),
    ).toStrictEqual(withoutDatumMaterial.descriptor.midgardTxOut);
    expect(
      verifyMidgardLedgerOutputDescriptorV1({
        control: withoutDatumTerminal,
        descriptor: withoutDatumMaterial.descriptor,
      }),
    ).toBe(true);
  });
});

// The cross-language descriptor vectors. Each triple below is pinned
// byte-for-byte in `onchain/aiken/lib/midgard/ledger-output-descriptor-v1.test.ak`
// as well, so the on-chain one-shot builder and this encoder cannot drift
// apart silently: whichever side moves, one of the two pinned sets goes red.
// This is the channel that matters for the transition-trace fraud proofs,
// which now derive the `utxos_root` value on chain and must derive exactly the
// value the node committed.
describe("cross-language ledger output descriptor V1 vectors", () => {
  it.each([
    {
      label: "minimal output, no datum and no reference script",
      outputIndex: 0,
      outputCbor:
        "a200581d7811111111111111111111111111111111111111111111111111111111018200a0",
      descriptorCbor:
        "90010018255820855089c279a2084237bfc980ad11c3cb72bd80055b3d89fdf9105faff6b9d3ec581d781111111111111111111111111111111111111111111111111111111100005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd012040004083582068221e2117402083ee82606dd1c3296b6b06d69d71bd1417555a0f1848ab7f731834183c83582023656d4d955fd1968ba6d1d923f25fda39fe3bbc3c9a8e474b16c9340ab32a081834183c8358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304",
    },
    {
      label: "inline datum only",
      outputIndex: 0,
      outputCbor:
        "a300581d7811111111111111111111111111111111111111111111111111111111018200a00243d87980",
      descriptorCbor:
        "900100182a5820831569d49cdf1d62af52a7ba84294373584f80bafa4b90aafb97dd9070ddbe41581d781111111111111111111111111111111111111111111111111111111100005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd01204000408358200a2e786d1965b870e307b60f251e29f6653560d7dbf72fa5ece0c5c44bbfd03c18381840835820ad9e3605aefc6d2070c186504f4e16ec0afef0f6c1342f7f423fa47f569e93421838184083582029f8b517a22889be5795b8b91050155dbc935ce541d077fd7124de5af8dd664c0708",
    },
    {
      label: "PlutusV3 reference script",
      outputIndex: 0,
      outputCbor:
        "a300581d7811111111111111111111111111111111111111111111111111111111018200a0038203436b6b6b",
      descriptorCbor:
        "900100182c582022477a156a6db4a27cd5c7af0cda47026474f4a97dbd31f3baabeb17b5064b78581d781111111111111111111111111111111111111111111111111111111100005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd0103581c556f006134510d3f7f607d251bd0e49aae300988b3fcfc0756c568e2065820e03a1cdbe9503904744a34d1163c1b22985c89586a997faa8c152a440043c34283582038d29cb45b5ac7901e51036d0fac974140394ebbd34ca5f70320e724380187521853185c835820bc293befa4e0e81021c3b002707d83cc1350338be43fc26303aa9acd600a55791853185c8358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304",
    },
    {
      label: "native reference script at the top of the index domain",
      outputIndex: 65535,
      outputCbor:
        "a300581d7811111111111111111111111111111111111111111111111111111111018200a003820058208200581c33333333333333333333333333333333333333333333333333333333",
      descriptorCbor:
        "900119ffff184a58209cc398e4f08855f03f791f3e43d1d00fa55b196c2aa8432c8558e3c8df3dc9cf581d781111111111111111111111111111111111111111111111111111111100005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd0100581cc78b7b4b696fffb06ba43034b2ddb692c43a88ea824ddfdf455b93721824582057e1c7765325cfa3e8676ca5c28b3477b878a1637cc3348d1027245a30a414978358204ccd01eb52febe88afa3b3be5af0a74c8936116264b85db97508b5cb88605d0c1853185c835820a06f11972ff408d3ca430a373f274af65377a84ed13bb21bd75008505ea71ec11853185c8358209525e1ea4350de9f831fc817b64355d7c3e26427effb7f4ca9bd29541d5eda390304",
    },
    {
      label:
        "multi-chunk output: two policies, base address, 2 KiB datum, reference script",
      outputIndex: 7,
      outputCbor:
        "a400583900111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111101821a007a1200a2581c55555555555555555555555555555555555555555555555555555555a24001420102182a581c66666666666666666666666666666666666666666666666666666666a15820abababababababababababababababababababababababababababababababab1b000000044b82fa09025908115f5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab50ababababababababababababababababff03820358646b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b",
      descriptorCbor:
        "90010719093358201d44e4026471138e8ee55364b7f5edfb548bbcb418ef6158fb2234dc604aebf758390011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111a007a120003582008059de11e25e694627ded37fc07251446b431d0b0f01ad7988b50fae7b37cd2187703581c22c0e1c50c8393c705226fd7578a859b9049b24e0c9eb6c1a5f7b2bb18685820fa4940294dd31f0806fb59218d53fe245a6717649bf8785e8eba6e7fddf755fc8358205f721c66e18ad314fbda580939ed42add1cd7476d78918ff074a579a441407341909041908f78358205f721c66e18ad314fbda580939ed42add1cd7476d78918ff074a579a441407341909041908f7835820b0396a03f113d1587164a43e74652ff1f1054dd2e54d8c6ab7628031346a65a21908151907d8",
    },
  ])(
    "encodes the pinned $label vector exactly",
    ({ outputIndex, outputCbor, descriptorCbor }) => {
      const material = buildCanonicalMidgardLedgerOutputMaterialV1({
        outputIndex,
        outputCbor: Buffer.from(outputCbor, "hex"),
      });
      expect(Buffer.from(material.descriptorCbor).toString("hex")).toBe(
        descriptorCbor,
      );
      expect(
        decodeMidgardLedgerOutputCommitmentV1(
          Buffer.from(descriptorCbor, "hex"),
        ),
      ).toEqual(material.descriptor);
    },
  );
});
