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
    datum: decodeMidgardDatum(
      Buffer.from(Data.to("ab".repeat(5_000)), "hex"),
    ),
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
    const outRef = Buffer.from(
      "825820" + "42".repeat(32) + "07",
      "hex",
    );
    const fromEntry =
      buildCanonicalMidgardLedgerEntryOutputMaterialV1({
        outRef,
        outputCbor: fixture.cbor,
      });
    const fromCreation = buildCanonicalMidgardLedgerOutputMaterialV1({
      outputIndex: 7,
      outputCbor: fixture.cbor,
    });

    expect(fromEntry.descriptorCbor).toStrictEqual(
      fromCreation.descriptorCbor,
    );
    expect(fromEntry.descriptor.outputIndex).toBe(7);
  });

  it("fails closed for non-canonical or out-of-domain ledger out-refs", () => {
    const fixture = outputFixture();
    const indefiniteOutRef = Buffer.from(
      "9f5820" + "42".repeat(32) + "07ff",
      "hex",
    );
    const oversizedIndexOutRef = Buffer.from(
      "825820" + "42".repeat(32) + "1a00010000",
      "hex",
    );

    expect(() =>
      buildCanonicalMidgardLedgerEntryOutputMaterialV1({
        outRef: indefiniteOutRef,
        outputCbor: fixture.cbor,
      }),
    ).toThrow();
    expect(() =>
      buildCanonicalMidgardLedgerEntryOutputMaterialV1({
        outRef: oversizedIndexOutRef,
        outputCbor: fixture.cbor,
      }),
    ).toThrow("ledger output index exceeds the V1 descriptor domain");
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
    expect(material.descriptor.address).toStrictEqual(
      protectedScriptAddress,
    );
    expect(material.descriptor.referenceScriptLanguage).toBe(3);
    expect(material.descriptor.referenceScriptHash).toHaveLength(28);
    expect(material.descriptor.referenceScriptItemCommitment).toHaveLength(
      32,
    );
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
          proof: buildMidgardBoundedItemChunkProofV1(
            material.item,
            chunkIndex,
          ),
        }),
      ).toBe(true);
    }

    const scriptCbor = encodeMidgardVersionedScript(
      fixture.output.script_ref!,
    );
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
          proof: buildMidgardBoundedItemChunkProofV1(
            scriptItem,
            chunkIndex,
          ),
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
    expect(scan.cardanoValueSize).toBe(
      material.descriptor.cardanoValueSize,
    );
    expect(scan.referenceScriptLanguage).toBe(
      material.descriptor.referenceScriptLanguage,
    );
    expect(
      summarizeMidgardLedgerOutputCardanoSpendDatumV1(
        terminal,
      ),
    ).toStrictEqual(
      material.descriptor.cardanoSpendDatum,
    );
    expect(
      summarizeMidgardLedgerOutputCardanoTxOutV1(terminal),
    ).toStrictEqual(material.descriptor.cardanoTxOut);
    expect(
      summarizeMidgardLedgerOutputMidgardTxOutV1(terminal),
    ).toStrictEqual(material.descriptor.midgardTxOut);
    expect(
      terminal.totalLength - scan.referenceScriptItemOffset,
    ).toBe(material.descriptor.referenceScriptTotalLength);
    expect(
      digestMidgardLedgerOutputReferenceScriptV1(terminal),
    ).toStrictEqual(material.descriptor.referenceScriptHash);
    expect(
      commitMidgardLedgerOutputReferenceScriptItemV1(terminal),
    ).toStrictEqual(
      material.descriptor.referenceScriptItemCommitment,
    );
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
        referenceScriptHash: changedBytes(
          descriptor.referenceScriptHash,
        ),
      },
      {
        ...descriptor,
        referenceScriptTotalLength:
          descriptor.referenceScriptTotalLength + 1,
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
          cborLength:
            descriptor.cardanoSpendDatum.cborLength + 1n,
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
    const withoutDatumMaterial =
      buildCanonicalMidgardLedgerOutputMaterialV1({
        outputIndex: 7,
        outputCbor: withoutDatumCbor,
      });
    const withoutDatumTerminal =
      buildMidgardLedgerOutputProofTraceV1({
        outputIndex: 7,
        outputCbor: withoutDatumCbor,
      }).terminal;
    expect(
      summarizeMidgardLedgerOutputCardanoSpendDatumV1(
        withoutDatumTerminal,
      ),
    ).toStrictEqual(
      withoutDatumMaterial.descriptor.cardanoSpendDatum,
    );
    expect(
      summarizeMidgardLedgerOutputCardanoTxOutV1(
        withoutDatumTerminal,
      ),
    ).toStrictEqual(withoutDatumMaterial.descriptor.cardanoTxOut);
    expect(
      summarizeMidgardLedgerOutputMidgardTxOutV1(
        withoutDatumTerminal,
      ),
    ).toStrictEqual(withoutDatumMaterial.descriptor.midgardTxOut);
    expect(
      verifyMidgardLedgerOutputDescriptorV1({
        control: withoutDatumTerminal,
        descriptor: withoutDatumMaterial.descriptor,
      }),
    ).toBe(true);
  });
});
