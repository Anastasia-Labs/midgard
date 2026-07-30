import { describe, expect, it } from "vitest";

import { encodeMidgardCekProgramEnvelopeV1 } from "../src/cek-proof.js";
import { encodeCbor } from "../src/codec/cbor.js";
import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardTxOutput,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardAddressFromText,
  type MidgardNativeTxCanonicalV1,
} from "../src/codec/index.js";
import {
  hashMidgardVersionedScript,
  type MidgardVersionedScript,
} from "../src/codec/versioned-script.js";
import {
  collectMidgardV1ReferencedProgramEnvelopes,
  hashMidgardMintAssetLeafV1,
  hashMidgardOutputDescriptorLeafV1,
  hashMidgardOutputLeafV1,
  hashMidgardRedeemerLeafV1,
  hashMidgardScriptContextItemLeafV1,
  hashMidgardScriptExecutionLeafV1,
  hashMidgardScriptPurposeLeafV1,
  hashMidgardScriptSourceLeafV1,
  hashMidgardSignerLeafV1,
  hashMidgardV1VersionedScript,
} from "../src/script-proof.js";
import {
  buildMidgardValidationMerkleFrontierV1,
  commitMidgardValidationMerkleFrontierV1,
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
    const sourceLeaf = hashMidgardScriptSourceLeafV1({
      originKind: "inline",
      sourceKey: Buffer.from("00", "hex"),
      script,
    });
    expect(sourceLeaf.toString("hex")).toBe(
      "6b4984caceb70e0446b5d02a9b01068b7f409c76a54cf4e3b2b78f965b9eecc6",
    );
    const redeemerLeaf = hashMidgardRedeemerLeafV1({
      redeemerIndex: 0,
      canonicalRedeemerWitnessCbor: redeemerCbor,
    });
    expect(redeemerLeaf.toString("hex")).toBe(
      "e42aed2342a26c9334ac80aea22c66b8f649cf6be5a5a4a70c6f33cbd8bda8ab",
    );
    const purposeLeaf = hashMidgardScriptPurposeLeafV1({
      purposeKind: 0,
      purposeIndex: 2n,
      scriptHash,
      subject: Buffer.from("0102", "hex"),
    });
    expect(purposeLeaf.toString("hex")).toBe(
      "24c90c22834ab9ec656bee1db27d5421515010ec4c6b8928a63f65aecb5e367b",
    );
    expect(
      hashMidgardScriptExecutionLeafV1({
        languageTag: 3,
        purposeLeaf,
        sourceLeaf,
        redeemerLeaf,
      }).toString("hex"),
    ).toBe("b5f8846e6888a5ebab1619bd6738ce3591323a63eb41fc06954729c5f4418d21");
    expect(
      hashMidgardOutputLeafV1({
        outputIndex: 2,
        outputCbor: Buffer.from("0102", "hex"),
      }).toString("hex"),
    ).toBe("f9a3fa502da0ee4fe7048a78088ca4b89a72fe3aa0d313e12fbae7305e171727");
    expect(
      hashMidgardOutputDescriptorLeafV1({
        outputIndex: 2,
        descriptorCbor: Buffer.from("820102", "hex"),
      }).toString("hex"),
    ).toBe("7d1979d9a1af11ab5b13ceeb3ae750046f31ea6ed7776f99cb2d41b253e87fd4");
    expect(
      hashMidgardMintAssetLeafV1({
        policyId: Buffer.alloc(28, 0x11),
        assetName: Buffer.from("abcd", "hex"),
        quantity: -7n,
      }).toString("hex"),
    ).toBe("4813bd9aad26eea82fa41280aefd50c848041a2a6cf27be416e1873c2876a479");
    expect(
      hashMidgardScriptContextItemLeafV1({
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
  });

  it("matches the Aiken signer leaf and seven-leaf frontier root vector", () => {
    const signerLeaf = hashMidgardSignerLeafV1(Buffer.alloc(28, 0x11));
    expect(signerLeaf.toString("hex")).toBe(
      "9e4bab3a1b4ca49640fe5c54486aac6a1183fb7da45eec6b30d46382d8f3418b",
    );

    const frontier = buildMidgardValidationMerkleFrontierV1([
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
      commitMidgardValidationMerkleFrontierV1(frontier).toString("hex"),
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
      scriptBytes: encodeMidgardCekProgramEnvelopeV1({
        uplcVersion: [1n, 1n, 0n],
        termRoot: Buffer.alloc(32, 0x22),
        nodeCount: 3n,
        materialByteLength: 144n,
      }),
    } satisfies MidgardVersionedScript;
    expect(hashMidgardV1VersionedScript(envelopeBackedScript)).toBe(
      hashMidgardVersionedScript(envelopeBackedScript),
    );
  });

  it("resolves historical reference programs from exact ledger outrefs", () => {
    const outRef = encodeCbor([Buffer.alloc(32, 0x44), 2n]);
    const envelope = encodeMidgardCekProgramEnvelopeV1({
      uplcVersion: [1n, 1n, 0n],
      termRoot: Buffer.alloc(32, 0x55),
      nodeCount: 3n,
      materialByteLength: 144n,
    });
    const tx: MidgardNativeTxCanonicalV1 = {
      version: MIDGARD_NATIVE_TX_V1_VERSION,
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

    expect(
      collectMidgardV1ReferencedProgramEnvelopes(
        tx,
        new Map([[outRef.toString("hex"), output]]),
      ),
    ).toEqual([
      {
        uplcVersion: [1n, 1n, 0n],
        termRoot: Buffer.alloc(32, 0x55),
        nodeCount: 3n,
        materialByteLength: 144n,
      },
    ]);
    expect(() =>
      collectMidgardV1ReferencedProgramEnvelopes(tx, new Map()),
    ).toThrow(/no resolved ledger output/u);
  });
});
