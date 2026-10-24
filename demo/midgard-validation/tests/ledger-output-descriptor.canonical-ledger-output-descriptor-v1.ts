import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  buildMidgardLedgerOutputProofTrace,
  commitMidgardLedgerOutputReferenceScriptItem,
  commitMidgardValidationMerkleFrontier,
  decodeMidgardLedgerOutputCommitment,
  digestMidgardLedgerOutputReferenceScript,
  encodeMidgardLedgerOutputCommitment,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
  summarizeMidgardLedgerOutputCardanoSpendDatum,
  summarizeMidgardLedgerOutputCardanoTxOut,
  summarizeMidgardLedgerOutputMidgardTxOut,
  verifyMidgardLedgerOutputChunk,
  verifyMidgardLedgerOutputDescriptor,
  verifyMidgardLedgerOutputReferenceScriptChunk,
} from "@al-ft/midgard-core";
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildCanonicalMidgardLedgerOutputMaterial,
} from "../src/ledger-output-descriptor.js";
import {
  outputFixture,
  protectedScriptAddress,
} from "./ledger-output-descriptor.output-fixture.js";

describe("canonical ledger output descriptor V1", () => {
  it("derives the consensus value from an exact canonical ledger out-ref", () => {
    const fixture = outputFixture();
    // The ledger out-ref key is §5.3's field-0/1 item, so it is built with that
    // encoder rather than written out as a literal: a literal here could drift
    // from the encoder the ledger and the on-chain `ledger_outref_key` use.
    const outRef = encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x42),
      outputIndex: 7,
    });
    const fromEntry = buildCanonicalMidgardLedgerEntryOutputMaterial({
      outRef,
      outputCbor: fixture.cbor,
    });
    const fromCreation = buildCanonicalMidgardLedgerOutputMaterial({
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
        buildCanonicalMidgardLedgerEntryOutputMaterial({
          outRef: Buffer.from(hex, "hex"),
          outputCbor: fixture.cbor,
        }),
      ).toThrow();
      expect(label).toBeTruthy();
    }
  });

  it("derives every compact fact from a multi-chunk canonical output", () => {
    const fixture = outputFixture();
    const material = buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: 7,
      outputCbor: fixture.cbor,
    });
    const repeated = buildCanonicalMidgardLedgerOutputMaterial({
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
      encodeMidgardLedgerOutputCommitment(
        decodeMidgardLedgerOutputCommitment(material.descriptorCbor),
      ),
    ).toStrictEqual(material.descriptorCbor);

    for (
      let chunkIndex = 0;
      chunkIndex < material.item.frontier.count;
      chunkIndex += 1
    ) {
      expect(
        verifyMidgardLedgerOutputChunk({
          descriptor: material.descriptor,
          proof: buildMidgardBoundedItemChunkProof(material.item, chunkIndex),
        }),
      ).toBe(true);
    }

    const scriptCbor = encodeMidgardVersionedScript(fixture.output.script_ref!);
    const scriptItem = buildMidgardBoundedItem({
      fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
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
        verifyMidgardLedgerOutputReferenceScriptChunk({
          descriptor: material.descriptor,
          proof: buildMidgardBoundedItemChunkProof(scriptItem, chunkIndex),
        }),
      ).toBe(true);
    }
  });

  it("matches every descriptor fact currently authenticated by the L1 scan", () => {
    const fixture = outputFixture();
    const material = buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: 7,
      outputCbor: fixture.cbor,
    });
    const terminal = buildMidgardLedgerOutputProofTrace({
      outputIndex: 7,
      outputCbor: fixture.cbor,
    }).terminal;
    const scan = terminal.outputScan;

    expect(scan.address).toStrictEqual(material.descriptor.address);
    expect(scan.lovelace).toBe(material.descriptor.lovelace);
    expect(scan.assetFrontier.count).toBe(material.descriptor.assetCount);
    expect(
      commitMidgardValidationMerkleFrontier(scan.assetFrontier),
    ).toStrictEqual(material.descriptor.assetFrontierCommitment);
    expect(scan.cardanoValueSize).toBe(material.descriptor.cardanoValueSize);
    expect(scan.referenceScriptLanguage).toBe(
      material.descriptor.referenceScriptLanguage,
    );
    expect(
      summarizeMidgardLedgerOutputCardanoSpendDatum(terminal),
    ).toStrictEqual(material.descriptor.cardanoSpendDatum);
    expect(summarizeMidgardLedgerOutputCardanoTxOut(terminal)).toStrictEqual(
      material.descriptor.cardanoTxOut,
    );
    expect(summarizeMidgardLedgerOutputMidgardTxOut(terminal)).toStrictEqual(
      material.descriptor.midgardTxOut,
    );
    expect(terminal.totalLength - scan.referenceScriptItemOffset).toBe(
      material.descriptor.referenceScriptTotalLength,
    );
    expect(digestMidgardLedgerOutputReferenceScript(terminal)).toStrictEqual(
      material.descriptor.referenceScriptHash,
    );
    expect(
      commitMidgardLedgerOutputReferenceScriptItem(terminal),
    ).toStrictEqual(material.descriptor.referenceScriptItemCommitment);
    expect(
      verifyMidgardLedgerOutputDescriptor({
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
        verifyMidgardLedgerOutputDescriptor({
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
    const withoutDatumMaterial = buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: 7,
      outputCbor: withoutDatumCbor,
    });
    const withoutDatumTerminal = buildMidgardLedgerOutputProofTrace({
      outputIndex: 7,
      outputCbor: withoutDatumCbor,
    }).terminal;
    expect(
      summarizeMidgardLedgerOutputCardanoSpendDatum(withoutDatumTerminal),
    ).toStrictEqual(withoutDatumMaterial.descriptor.cardanoSpendDatum);
    expect(
      summarizeMidgardLedgerOutputCardanoTxOut(withoutDatumTerminal),
    ).toStrictEqual(withoutDatumMaterial.descriptor.cardanoTxOut);
    expect(
      summarizeMidgardLedgerOutputMidgardTxOut(withoutDatumTerminal),
    ).toStrictEqual(withoutDatumMaterial.descriptor.midgardTxOut);
    expect(
      verifyMidgardLedgerOutputDescriptor({
        control: withoutDatumTerminal,
        descriptor: withoutDatumMaterial.descriptor,
      }),
    ).toBe(true);
  });
});
