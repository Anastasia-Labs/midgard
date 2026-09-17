import { describe, expect, it } from "vitest";

import { buildMidgardBoundedItem } from "../src/bounded-item.js";
import { encodeMidgardCekProgramEnvelope } from "../src/cek-proof.js";
import { encodeCbor } from "../src/codec/cbor.js";
import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardAddressFromText,
  type MidgardNativeTxCanonical,
} from "../src/codec/index.js";
import {
  decodeMidgardVersionedScript,
  encodeMidgardVersionedScript,
  encodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
  type MidgardVersionedScript,
} from "../src/codec/versioned-script.js";
import {
  collectMidgardAttachedProgramEnvelopes,
  collectMidgardReferencedProgramEnvelopes,
  decodeMidgardScriptProgramEnvelope,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardMintAssetLeaf,
  hashMidgardOutputDescriptorLeaf,
  hashMidgardOutputLeaf,
  hashMidgardRedeemerLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptContextItemLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
  hashMidgardScriptSourceLeaf,
  hashMidgardSignerLeaf,
  hashMidgardV1VersionedScript,
} from "../src/script-proof.js";
import {
  buildMidgardValidationMerkleFrontier,
  commitMidgardValidationMerkleFrontier,
} from "../src/validation-merkle.js";

describe("script proof primitives", () => {
  it("matches the Aiken source, redeemer, and purpose vectors", () => {
    const script = {
      language: "PlutusV3",
      scriptBytes: Buffer.from("010203", "hex"),
    } satisfies MidgardVersionedScript;
    const scriptHash = Buffer.from(hashMidgardVersionedScript(script), "hex");
    const redeemerCbor = encodeCbor([
      0n,
      2n,
      Buffer.from("d87980", "hex"),
      [3n, 4n],
    ]);

    expect(scriptHash.toString("hex")).toBe(
      "8b8c11dcad0af38c40d742ed155b4c938acc5507a0ecbcfcea36496a",
    );
    const midgardScript = {
      language: "MidgardV1",
      scriptBytes: Buffer.from("010203", "hex"),
    } satisfies MidgardVersionedScript;
    expect(hashMidgardVersionedScript(midgardScript)).toBe(
      "760b621a49505853e1f4562d126e185f78483932825e0fb077a1ed80",
    );
    const sourceLeaf = hashMidgardScriptSourceLeaf({
      originKind: "inline",
      sourceKey: Buffer.from("00", "hex"),
      script,
    });
    expect(sourceLeaf.toString("hex")).toBe(
      "6b4984caceb70e0446b5d02a9b01068b7f409c76a54cf4e3b2b78f965b9eecc6",
    );
    const redeemerLeaf = hashMidgardRedeemerLeaf({
      redeemerIndex: 0,
      canonicalRedeemerWitnessCbor: redeemerCbor,
    });
    expect(redeemerLeaf.toString("hex")).toBe(
      "e42aed2342a26c9334ac80aea22c66b8f649cf6be5a5a4a70c6f33cbd8bda8ab",
    );
    const purposeLeaf = hashMidgardScriptPurposeLeaf({
      purposeKind: 0,
      purposeIndex: 2n,
      scriptHash,
      subject: Buffer.from("0102", "hex"),
    });
    expect(purposeLeaf.toString("hex")).toBe(
      "24c90c22834ab9ec656bee1db27d5421515010ec4c6b8928a63f65aecb5e367b",
    );
    expect(
      hashMidgardScriptExecutionLeaf({
        languageTag: 3,
        purposeLeaf,
        sourceLeaf,
        redeemerLeaf,
      }).toString("hex"),
    ).toBe("b5f8846e6888a5ebab1619bd6738ce3591323a63eb41fc06954729c5f4418d21");
    expect(
      hashMidgardOutputLeaf({
        outputIndex: 2,
        outputCbor: Buffer.from("0102", "hex"),
      }).toString("hex"),
    ).toBe("f9a3fa502da0ee4fe7048a78088ca4b89a72fe3aa0d313e12fbae7305e171727");
    expect(
      hashMidgardOutputDescriptorLeaf({
        outputIndex: 2,
        descriptorCbor: Buffer.from("820102", "hex"),
      }).toString("hex"),
    ).toBe("7d1979d9a1af11ab5b13ceeb3ae750046f31ea6ed7776f99cb2d41b253e87fd4");
    expect(
      hashMidgardMintAssetLeaf({
        policyId: Buffer.alloc(28, 0x11),
        assetName: Buffer.from("abcd", "hex"),
        quantity: -7n,
      }).toString("hex"),
    ).toBe("4813bd9aad26eea82fa41280aefd50c848041a2a6cf27be416e1873c2876a479");
    expect(
      hashMidgardScriptContextItemLeaf({
        collectionKind: 0,
        itemIndex: 2,
        semanticRoot: Buffer.from(
          "66b39a3f329165f6ab15f249df38d5e8bc99230853ce5c16976b4780af1ed029",
          "hex",
        ),
        cborLength: 176n,
        memory: 218n,
      }).toString("hex"),
    ).toBe("2758fe3ec7c263c1630eab8cb4bb7f431cd403838a25b46bfe3848b0481daef3");

    expect(
      hashMidgardScriptPurposeLeaf({
        purposeKind: 1,
        purposeIndex: 2n,
        scriptHash,
        subject: Buffer.from("0102", "hex"),
      }),
    ).not.toEqual(purposeLeaf);
    expect(
      hashMidgardRedeemerLeaf({
        redeemerIndex: 0,
        canonicalRedeemerWitnessCbor: Buffer.from(redeemerCbor).subarray(0, -1),
      }),
    ).not.toEqual(redeemerLeaf);
  });

  it("binds inline script sources to raw-item field 6, not address field 7", () => {
    const script = {
      language: "PlutusV3",
      scriptBytes: Buffer.from("010203", "hex"),
    } satisfies MidgardVersionedScript;
    const scriptCbor = encodeMidgardVersionedScript(script);
    const scriptHash = Buffer.from(hashMidgardVersionedScript(script), "hex");
    const sourceLeaf = hashMidgardScriptSourceLeaf({
      originKind: "inline",
      sourceKey: Buffer.from("00", "hex"),
      script,
    });
    const sourceLeafForField = (fieldIndex: 6 | 7) =>
      hashMidgardInlineScriptSourceLeaf({
        sourceIndex: 0n,
        scriptLanguageTag: 3,
        scriptHash,
        scriptTotalLength: scriptCbor.length,
        itemCommitment: buildMidgardBoundedItem({
          fieldIndex,
          itemIndex: 0,
          bytes: scriptCbor,
        }).commitment,
      });

    expect(sourceLeaf).toEqual(sourceLeafForField(6));
    expect(sourceLeaf).not.toEqual(sourceLeafForField(7));
  });

  it("matches the Aiken signer leaf and seven-leaf frontier root vector", () => {
    const signerLeaf = hashMidgardSignerLeaf(Buffer.alloc(28, 0x11));
    expect(signerLeaf.toString("hex")).toBe(
      "9e4bab3a1b4ca49640fe5c54486aac6a1183fb7da45eec6b30d46382d8f3418b",
    );

    const frontier = buildMidgardValidationMerkleFrontier([
      Buffer.from(
        "6b4984caceb70e0446b5d02a9b01068b7f409c76a54cf4e3b2b78f965b9eecc6",
        "hex",
      ),
      Buffer.from(
        "e42aed2342a26c9334ac80aea22c66b8f649cf6be5a5a4a70c6f33cbd8bda8ab",
        "hex",
      ),
      Buffer.from(
        "24c90c22834ab9ec656bee1db27d5421515010ec4c6b8928a63f65aecb5e367b",
        "hex",
      ),
      Buffer.from(
        "b5f8846e6888a5ebab1619bd6738ce3591323a63eb41fc06954729c5f4418d21",
        "hex",
      ),
      signerLeaf,
      Buffer.from(
        "f9a3fa502da0ee4fe7048a78088ca4b89a72fe3aa0d313e12fbae7305e171727",
        "hex",
      ),
      Buffer.from(
        "2758fe3ec7c263c1630eab8cb4bb7f431cd403838a25b46bfe3848b0481daef3",
        "hex",
      ),
    ]);
    expect(frontier.count).toBe(7);
    expect(
      frontier.peaks.map(({ height, hash }) => [height, hash.toString("hex")]),
    ).toEqual([
      [0, "2758fe3ec7c263c1630eab8cb4bb7f431cd403838a25b46bfe3848b0481daef3"],
      [1, "9bb71e0b9929d3033fa1a74d906acb5e040f5ffc05ca359f27602878fb40d2d3"],
      [2, "6782c5608a8972e206c68c1693c7ce08730b0140df296974013184b4874aa813"],
    ]);
    expect(
      commitMidgardValidationMerkleFrontier(frontier).toString("hex"),
    ).toBe("3f81171ae98f8745f125cbe28461e23204fe4f39e609e9ac4be7537b9dac126f");
  });

  it("keeps raw Cardano-style hashing isolated from V1 envelope admission", () => {
    const rawScript = {
      language: "PlutusV3",
      scriptBytes: Buffer.from("010203", "hex"),
    } satisfies MidgardVersionedScript;
    expect(hashMidgardVersionedScript(rawScript)).toMatch(/^[0-9a-f]{56}$/u);
    expect(() => hashMidgardV1VersionedScript(rawScript)).toThrow(
      /program[_ -]envelope/u,
    );

    const envelopeBackedScript = {
      language: "PlutusV3",
      scriptBytes: encodeMidgardCekProgramEnvelope({
        uplcVersion: [1n, 1n, 0n],
        termRoot: Buffer.alloc(32, 0x22),
        nodeCount: 3n,
        materialByteLength: 144n,
      }),
    } satisfies MidgardVersionedScript;
    expect(hashMidgardV1VersionedScript(envelopeBackedScript)).toBe(
      hashMidgardVersionedScript(envelopeBackedScript),
    );

    const rawNativeScript = {
      language: "NativeCardano",
      scriptBytes: Buffer.alloc(0),
      nativeScript: {
        type: "sig",
        keyHash: Buffer.alloc(28, 0x33),
      },
    } satisfies MidgardVersionedScript;
    expect(decodeMidgardScriptProgramEnvelope(rawNativeScript)).toBeNull();
    expect(() => hashMidgardV1VersionedScript(rawNativeScript)).toThrow(
      /not canonical Midgard V1 program envelopes/u,
    );
    expect(() => decodeMidgardScriptProgramEnvelope(rawScript)).toThrow(
      /program[_ -]envelope/u,
    );
    expect(() =>
      decodeMidgardVersionedScript(Buffer.from("8218814101", "hex")),
    ).toThrow(/Unsupported Midgard versioned script tag/u);
    expect(() =>
      decodeMidgardVersionedScript(Buffer.from("8203410100", "hex")),
    ).toThrow(/Trailing bytes/u);
  });

  it("resolves historical reference programs from exact ledger outrefs", () => {
    // The ledger out-ref, in its one Midgard spelling: §5.3's fixed-index
    // field-0/1 item, so index 2 is `19 0002` and the key is 38 bytes.
    const outRef = encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x44),
      outputIndex: 2,
    });
    const envelope = encodeMidgardCekProgramEnvelope({
      uplcVersion: [1n, 1n, 0n],
      termRoot: Buffer.alloc(32, 0x55),
      nodeCount: 3n,
      materialByteLength: 144n,
    });
    const tx: MidgardNativeTxCanonical = {
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: encodeCbor([outRef]),
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
    };
    const output = encodeMidgardTxOutput({
      address: midgardAddressFromText(
        "addr1q9ynxme7c0tcmmvgk2tjuv63aw7zk9tk6yqkaqd48ulhkyl5f6v47dp5rc7286z5f57339d0c79khw4y3lwxzm8ywkzs02spk6",
      ),
      value: { lovelace: 2_000_000n, assets: new Map() },
      script_ref: {
        language: "PlutusV3",
        scriptBytes: envelope,
      },
    });

    const attachedTx: MidgardNativeTxCanonical = {
      ...tx,
      body: {
        ...tx.body,
        outputsPreimageCbor: encodeCbor([output]),
      },
      witnessSet: {
        ...tx.witnessSet,
        scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
          {
            language: "PlutusV3",
            scriptBytes: envelope,
          },
        ]),
      },
    };
    const expectedEnvelope = {
      uplcVersion: [1n, 1n, 0n],
      termRoot: Buffer.alloc(32, 0x55),
      nodeCount: 3n,
      materialByteLength: 144n,
    };
    const referenceLeafInput = (sourceKey: Uint8Array) =>
      hashMidgardReferenceScriptSourceLeaf({
        sourceKey,
        scriptLanguageTag: 3,
        scriptHash: Buffer.from(
          hashMidgardVersionedScript({
            language: "PlutusV3",
            scriptBytes: envelope,
          }),
          "hex",
        ),
        scriptTotalLength: encodeMidgardVersionedScript({
          language: "PlutusV3",
          scriptBytes: envelope,
        }).length,
        itemCommitment: buildMidgardBoundedItem({
          fieldIndex: 2,
          itemIndex: 2,
          bytes: encodeMidgardVersionedScript({
            language: "PlutusV3",
            scriptBytes: envelope,
          }),
        }).commitment,
      });
    expect(collectMidgardAttachedProgramEnvelopes(attachedTx)).toEqual([
      expectedEnvelope,
      expectedEnvelope,
    ]);
    expect(referenceLeafInput(outRef)).toEqual(
      hashMidgardScriptSourceLeaf({
        originKind: "reference",
        sourceKey: outRef,
        script: { language: "PlutusV3", scriptBytes: envelope },
      }),
    );

    expect(
      collectMidgardReferencedProgramEnvelopes(
        tx,
        new Map([[outRef.toString("hex"), output]]),
      ),
    ).toEqual([expectedEnvelope]);
    expect(() =>
      collectMidgardReferencedProgramEnvelopes(tx, new Map()),
    ).toThrow(/no resolved ledger output/u);
    // A 31-byte tx_id: the wrapper lies about a 38-byte key's contents, so the
    // §5.3 decoder rejects it.
    expect(() =>
      referenceLeafInput(
        Buffer.concat([
          Buffer.from("82581f", "hex"),
          Buffer.alloc(31, 0x44),
          Buffer.from("190002", "hex"),
        ]),
      ),
    ).toThrow(/source key is not an output reference/u);
    // The retired minimal-index spelling — CML's `TransactionInput` CBOR, 36
    // bytes. §5.3 does not admit it, in either language.
    expect(() =>
      referenceLeafInput(encodeCbor([Buffer.alloc(32, 0x44), 2n])),
    ).toThrow(/source key is not an output reference/u);
    // `18 XX` one-byte-argument index.
    expect(() =>
      referenceLeafInput(
        Buffer.concat([
          Buffer.from("825820", "hex"),
          Buffer.alloc(32, 0x44),
          Buffer.from("1802", "hex"),
        ]),
      ),
    ).toThrow(/source key is not an output reference/u);
    // Right length, wrong canon: a `1a` index head in the 38-byte envelope.
    expect(() =>
      referenceLeafInput(
        Buffer.concat([
          Buffer.from("825820", "hex"),
          Buffer.alloc(32, 0x44),
          Buffer.from("1a0002", "hex"),
        ]),
      ),
    ).toThrow(/source key is not an output reference/u);
    // A non-minimal `59 0020` byte-string header for the 32-byte tx_id: 39
    // bytes, and the one shape a positional on-chain reader will happily walk
    // (`decode_definite_bytes_at` accepts the wide header), so the width guard
    // is what makes the two twins agree that this is not an out-ref.
    expect(() =>
      referenceLeafInput(
        Buffer.concat([
          Buffer.from("82590020", "hex"),
          Buffer.alloc(32, 0x44),
          Buffer.from("190002", "hex"),
        ]),
      ),
    ).toThrow(/source key is not an output reference/u);
    // Index 65,536 is outside the ledger's uint16 index domain, so it has no
    // §5.3 encoding at all — the encoder cannot even build the bytes.
    expect(() =>
      encodeMidgardSpendInputItem({
        txId: Buffer.alloc(32, 0x44),
        outputIndex: 65_536,
      }),
    ).toThrow(/output index must be 0\.\.65,535/u);
    // So the leaf path's own rejection of an out-of-domain index has to be
    // reached with hand-built bytes: index 65,536 spelled as a minimal uint32,
    // a 41-byte key. Without this the reference-leaf path was only ever shown
    // rejecting *shapes*, never an index outside the domain.
    expect(() =>
      referenceLeafInput(
        Buffer.concat([
          Buffer.from("825820", "hex"),
          Buffer.alloc(32, 0x44),
          Buffer.from("1a00010000", "hex"),
        ]),
      ),
    ).toThrow(/source key is not an output reference/u);
  });
});
