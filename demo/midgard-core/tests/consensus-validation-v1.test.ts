import { describe, expect, it } from "vitest";

import {
  computeMidgardNativeTxCanonicalSizeFromProofSourceV1,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  deriveMidgardNativeTxProofSourceV1,
  deriveMidgardTxFieldReceiptAssetNameV1,
  deriveMidgardV1TxFieldChunks,
  deriveMidgardV1TxFieldEvidence,
  deriveMidgardV1TxFieldPreimages,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardCekProgramEnvelopeV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardAddressFromText,
  type MidgardNativeScript,
  type MidgardNativeTxCanonicalV1,
  type MidgardTxOutput,
  protectMidgardAddress,
  reconstructMidgardTransactionV1,
  reconstructMidgardTransactionV1FromChunks,
  validateMidgardConsensusV1Tx,
  validateMidgardConsensusV1TxCbor,
  verifyMidgardNativeTxProofSourceV1,
  verifyMidgardV1TxFieldChunk,
  verifyMidgardV1TxFieldItem,
  verifyMidgardV1TxFieldPreimage,
} from "../src/index.js";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "../src/plutus-data-cbor.js";

const canonicalDataBytes = (payload: Buffer): Buffer =>
  Buffer.from(
    aikenSerialisedPlutusDataCborPreservingMapOrder(
      encodeCbor(payload).toString("hex"),
    ),
    "hex",
  );

const address = midgardAddressFromText(
  "addr1q9ynxme7c0tcmmvgk2tjuv63aw7zk9tk6yqkaqd48ulhkyl5f6v47dp5rc7286z5f57339d0c79khw4y3lwxzm8ywkzs02spk6",
);

const cekProgramEnvelope = (
  nodeCount = 3n,
  materialByteLength = 144n,
): Buffer =>
  encodeMidgardCekProgramEnvelopeV1({
    uplcVersion: [1n, 1n, 0n],
    termRoot: Buffer.alloc(32, 0x33),
    nodeCount,
    materialByteLength,
  });

const output = (overrides: Partial<MidgardTxOutput> = {}): MidgardTxOutput => ({
  address: protectMidgardAddress(address),
  value: { lovelace: 2_000_000n, assets: new Map() },
  script_ref: {
    language: "MidgardV1",
    scriptBytes: cekProgramEnvelope(),
  },
  ...overrides,
});

const canonical = (
  version = MIDGARD_NATIVE_TX_V1_VERSION,
): MidgardNativeTxCanonicalV1 => ({
  version,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: EMPTY_CBOR_LIST,
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
    outputsPreimageCbor: encodeCbor([encodeMidgardTxOutput(output())]),
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimageCbor: encodeCbor([Buffer.alloc(28, 7)]),
    requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
    mintPreimageCbor: encodeCbor(
      new Map([
        [Buffer.alloc(28, 8), new Map([[Buffer.from("asset", "ascii"), 1n]])],
      ]),
    ),
    scriptIntegrityHash: Buffer.alloc(32, 9),
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
      { language: "MidgardV1", scriptBytes: cekProgramEnvelope() },
    ]),
    redeemerTxWitsPreimageCbor: encodeCbor([Buffer.from([0x80])]),
  },
});

const nestedNativeScript = (depth: number): MidgardNativeScript => {
  let script: MidgardNativeScript = {
    type: "sig",
    keyHash: Buffer.alloc(28, 0x44),
  };
  for (let index = 1; index < depth; index += 1) {
    script = { type: "all", scripts: [script] };
  }
  return script;
};

describe("canonical V1 consensus transaction bounds", () => {
  it("derives the exact L1 field-receipt asset-name vector", () => {
    expect(
      deriveMidgardTxFieldReceiptAssetNameV1({
        txOrderPolicyId: Buffer.from("11".repeat(28), "hex"),
        txOrderTransactionId: Buffer.from("44".repeat(32), "hex"),
        txOrderOutputIndex: 7n,
        transactionCommitment: Buffer.alloc(32),
        fieldIndex: 0,
        itemIndex: 0,
        chunkIndex: 0,
      }).toString("hex"),
    ).toBe("7bc0ae911756007cc44a1e956fedc08d67af38ce19778405ab040faa77b60123");
    expect(() =>
      deriveMidgardTxFieldReceiptAssetNameV1({
        txOrderPolicyId: Buffer.alloc(27),
        txOrderTransactionId: Buffer.alloc(32),
        txOrderOutputIndex: 0n,
        transactionCommitment: Buffer.alloc(32),
        fieldIndex: 0,
        itemIndex: 0,
        chunkIndex: 0,
      }),
    ).toThrow(/exactly 28 bytes/u);
  });

  it("admits the requested V1 feature surface instead of feature-gating it", () => {
    const tx = materializeMidgardNativeTxFromCanonicalV1(canonical());
    const txCbor = encodeMidgardNativeTxCanonicalV1(tx);
    expect(validateMidgardConsensusV1Tx(tx, txCbor.length)).toBeNull();
    expect(validateMidgardConsensusV1TxCbor(txCbor)).toBeNull();
  });

  it("binds the full nine-field surface through a compact V1 source", () => {
    const tx = materializeMidgardNativeTxFromCanonicalV1(canonical());
    const source = deriveMidgardNativeTxProofSourceV1(tx);
    const transactionId = computeMidgardNativeTxIdV1(tx);
    expect(source.compactCbor.length).toBeLessThan(1024);
    expect(source.witnessSetCompactCbor.length).toBeLessThan(256);
    expect(source.fieldPreimageLengthsCbor.length).toBeLessThan(64);
    expect(computeMidgardNativeTxCanonicalSizeFromProofSourceV1(source)).toBe(
      encodeMidgardNativeTxCanonicalV1(tx).length,
    );
    expect(
      verifyMidgardNativeTxProofSourceV1({ transactionId, source }),
    ).toEqual(tx.compact);
    expect(computeMidgardNativeTxProofCommitmentV1(source)).toHaveLength(32);

    const tampered = Buffer.from(source.witnessSetCompactCbor);
    tampered[tampered.length - 1] ^= 1;
    expect(() =>
      verifyMidgardNativeTxProofSourceV1({
        transactionId,
        source: { ...source, witnessSetCompactCbor: tampered },
      }),
    ).toThrow(/does not match/u);
  });

  it("reveals and verifies exactly one bounded field preimage per L1 instruction", () => {
    const tx = materializeMidgardNativeTxFromCanonicalV1(canonical());
    const txCbor = encodeMidgardNativeTxCanonicalV1(tx);
    const source = deriveMidgardNativeTxProofSourceV1(tx);
    const transactionId = computeMidgardNativeTxIdV1(tx);
    const transactionCommitment =
      computeMidgardNativeTxProofCommitmentV1(source);
    const fields = deriveMidgardV1TxFieldPreimages(txCbor);

    expect(fields.map((field) => field.fieldIndex)).toEqual([
      0, 1, 2, 3, 4, 5, 6, 7, 8,
    ]);
    for (const field of fields) {
      expect(
        verifyMidgardV1TxFieldPreimage({
          transactionId,
          transactionCommitment,
          source,
          fieldIndex: field.fieldIndex,
          preimageCbor: field.preimageCbor,
        }),
      ).toEqual(field);
    }

    const tampered = Buffer.from(fields[2]!.preimageCbor);
    tampered[tampered.length - 1] ^= 1;
    expect(() =>
      verifyMidgardV1TxFieldPreimage({
        transactionId,
        transactionCommitment,
        source,
        fieldIndex: 2,
        preimageCbor: tampered,
      }),
    ).toThrow(/hash mismatch/u);
    expect(() =>
      verifyMidgardV1TxFieldPreimage({
        transactionId,
        transactionCommitment,
        source,
        fieldIndex: 9,
        preimageCbor: EMPTY_CBOR_LIST,
      }),
    ).toThrow(/unknown V1 transaction field index/u);
  });

  it("authenticates a transaction field one bounded chunk at a time", () => {
    const tx = materializeMidgardNativeTxFromCanonicalV1(canonical());
    const source = deriveMidgardNativeTxProofSourceV1(tx);
    const transactionId = computeMidgardNativeTxIdV1(tx);
    const transactionCommitment =
      computeMidgardNativeTxProofCommitmentV1(source);
    const entry = deriveMidgardV1TxFieldChunks(
      encodeMidgardNativeTxCanonicalV1(tx),
    ).find((field) => field.proof.fieldIndex === 2)!;
    const { collectionProof, proof } = entry;

    expect(
      verifyMidgardV1TxFieldChunk({
        transactionId,
        transactionCommitment,
        source,
        collectionProof,
        proof,
      }),
    ).toEqual(proof);
    expect(() =>
      verifyMidgardV1TxFieldChunk({
        transactionId,
        transactionCommitment,
        source,
        collectionProof,
        proof: { ...proof, fieldIndex: 1 },
      }),
    ).toThrow(/item descriptor|chunk proof is invalid/u);
    expect(() =>
      verifyMidgardV1TxFieldChunk({
        transactionId,
        transactionCommitment: Buffer.alloc(32),
        source,
        collectionProof,
        proof,
      }),
    ).toThrow(/does not match transaction commitment/u);
  });

  it("keeps fitting canonical proof items complete and commitment-bound", () => {
    const tx = materializeMidgardNativeTxFromCanonicalV1(canonical());
    const canonicalCbor = encodeMidgardNativeTxCanonicalV1(tx);
    const source = deriveMidgardNativeTxProofSourceV1(tx);
    const transactionId = computeMidgardNativeTxIdV1(tx);
    const transactionCommitment =
      computeMidgardNativeTxProofCommitmentV1(source);
    const evidence = deriveMidgardV1TxFieldEvidence(canonicalCbor);
    const item = evidence.find(
      (entry) =>
        entry.kind === "completeItem" && entry.collectionProof.fieldIndex === 2,
    );
    expect(item?.kind).toBe("completeItem");
    if (item?.kind !== "completeItem") {
      throw new Error("expected a complete output item");
    }
    expect(
      verifyMidgardV1TxFieldItem({
        transactionId,
        transactionCommitment,
        source,
        collectionProof: item.collectionProof,
        itemCbor: item.itemCbor,
      }),
    ).toEqual(item.itemCbor);
    expect(() =>
      verifyMidgardV1TxFieldItem({
        transactionId,
        transactionCommitment,
        source,
        collectionProof: item.collectionProof,
        itemCbor: Buffer.concat([item.itemCbor, Buffer.from([0])]),
      }),
    ).toThrow(/length|commitment/u);
    expect(
      evidence
        .filter((entry) => entry.kind === "completeItem")
        .every(
          (entry) =>
            entry.itemCbor.length <=
            MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes,
        ),
    ).toBe(true);
  });

  it("reconstructs the exact canonical forced transaction from nine authenticated fragments", () => {
    const tx = materializeMidgardNativeTxFromCanonicalV1(canonical());
    const expected = encodeMidgardNativeTxCanonicalV1(tx);
    const source = deriveMidgardNativeTxProofSourceV1(tx);
    const transactionId = computeMidgardNativeTxIdV1(tx);
    const transactionCommitment =
      computeMidgardNativeTxProofCommitmentV1(source);
    const fields = deriveMidgardV1TxFieldPreimages(expected);

    expect(
      reconstructMidgardTransactionV1({
        transactionId,
        transactionCommitment,
        source,
        fieldPreimages: fields.map((field) => field.preimageCbor),
      }),
    ).toEqual(expected);
    expect(() =>
      reconstructMidgardTransactionV1({
        transactionId,
        transactionCommitment,
        source,
        fieldPreimages: fields.slice(0, 8).map((field) => field.preimageCbor),
      }),
    ).toThrow(/exactly 9 field preimages/u);
  });

  it("reconstructs exactly from the canonical complete chunk sequence", () => {
    const tx = materializeMidgardNativeTxFromCanonicalV1(canonical());
    const expected = encodeMidgardNativeTxCanonicalV1(tx);
    const source = deriveMidgardNativeTxProofSourceV1(tx);
    const transactionId = computeMidgardNativeTxIdV1(tx);
    const transactionCommitment =
      computeMidgardNativeTxProofCommitmentV1(source);
    const proofs = deriveMidgardV1TxFieldChunks(expected);

    expect(
      reconstructMidgardTransactionV1FromChunks({
        transactionId,
        transactionCommitment,
        source,
        chunkProofs: proofs,
      }),
    ).toEqual(expected);
    expect(() =>
      reconstructMidgardTransactionV1FromChunks({
        transactionId,
        transactionCommitment,
        source,
        chunkProofs: proofs.slice(1),
      }),
    ).toThrow(/sequence diverges|reconstructed length/u);
    expect(() =>
      reconstructMidgardTransactionV1FromChunks({
        transactionId,
        transactionCommitment,
        source,
        chunkProofs: [proofs[1]!, proofs[0]!, ...proofs.slice(2)],
      }),
    ).toThrow(/sequence diverges|reconstructed length/u);
  });

  it("rejects every unsupported transaction version", () => {
    const current = materializeMidgardNativeTxFromCanonicalV1(canonical());
    const unsupported = {
      ...current,
      version: 23n,
      compact: { ...current.compact, version: 23n },
    };
    expect(validateMidgardConsensusV1Tx(unsupported, 1)).toMatchObject({
      code: "E_TX_VERSION",
    });
    expect(() =>
      encodeMidgardNativeTxCanonicalV1({
        ...canonical(),
        version: 23n,
      }),
    ).toThrow(/transaction\.version must equal 1/u);
  });

  it("supports large output content while retaining the Cardano Value rule", () => {
    const largeValueAndDatum = materializeMidgardNativeTxFromCanonicalV1({
      ...canonical(),
      body: {
        ...canonical().body,
        outputsPreimageCbor: encodeCbor([
          encodeMidgardTxOutput(
            output({
              datum: {
                kind: "inline",
                cbor: canonicalDataBytes(Buffer.alloc(1500)),
              },
              script_ref: undefined,
            }),
          ),
        ]),
      },
    });
    expect(validateMidgardConsensusV1Tx(largeValueAndDatum, 1)).toBeNull();

    const oversizedValueAssets = new Map<string, bigint>(
      Array.from({ length: 160 }, (_, index) => [
        index.toString(16).padStart(64, "0"),
        1n,
      ]),
    );
    const valueTooLarge = materializeMidgardNativeTxFromCanonicalV1({
      ...canonical(),
      body: {
        ...canonical().body,
        outputsPreimageCbor: encodeCbor([
          encodeMidgardTxOutput(
            output({
              value: {
                lovelace: 2_000_000n,
                assets: new Map([["ab".repeat(28), oversizedValueAssets]]),
              },
              script_ref: undefined,
            }),
          ),
        ]),
      },
    });
    expect(validateMidgardConsensusV1Tx(valueTooLarge, 1)).toMatchObject({
      code: "E_VALUE_SIZE",
    });

    const outputTooLarge = materializeMidgardNativeTxFromCanonicalV1({
      ...canonical(),
      body: {
        ...canonical().body,
        outputsPreimageCbor: encodeCbor([
          encodeMidgardTxOutput(
            output({
              datum: {
                kind: "inline",
                cbor: canonicalDataBytes(
                  Buffer.alloc(
                    MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOutputPreimageBytes,
                  ),
                ),
              },
              script_ref: undefined,
            }),
          ),
        ]),
      },
    });
    expect(validateMidgardConsensusV1Tx(outputTooLarge, 1)).toMatchObject({
      code: "E_LEDGER_OUTPUT_SIZE",
    });
  });

  it("rejects malformed inline and reference program envelopes fail closed", () => {
    const malformedInline = materializeMidgardNativeTxFromCanonicalV1({
      ...canonical(),
      witnessSet: {
        ...canonical().witnessSet,
        scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
          {
            language: "PlutusV3",
            scriptBytes: Buffer.from("010203", "hex"),
          },
        ]),
      },
    });
    expect(validateMidgardConsensusV1Tx(malformedInline, 1)).toMatchObject({
      code: "E_SCRIPT_PROGRAM_ENCODING",
      featureId: "script_witnesses",
    });

    const malformedReference = materializeMidgardNativeTxFromCanonicalV1({
      ...canonical(),
      body: {
        ...canonical().body,
        outputsPreimageCbor: encodeCbor([
          encodeMidgardTxOutput(
            output({
              script_ref: {
                language: "MidgardV1",
                scriptBytes: Buffer.from("010203", "hex"),
              },
            }),
          ),
        ]),
      },
    });
    expect(validateMidgardConsensusV1Tx(malformedReference, 1)).toMatchObject({
      code: "E_SCRIPT_PROGRAM_ENCODING",
      featureId: "reference_scripts",
    });
  });

  it("uses the sum of bounded fields as the effective transaction cap", () => {
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes).toBeGreaterThan(
      8 * 1024,
    );
    const base = materializeMidgardNativeTxFromCanonicalV1(canonical());
    const oversizedField = {
      ...base,
      body: {
        ...base.body,
        mintPreimageCbor: Buffer.alloc(
          MIDGARD_CONSENSUS_LIMITS_V1.maxMintPreimageBytes + 1,
          0x80,
        ),
      },
    };
    expect(validateMidgardConsensusV1Tx(oversizedField, 1)).toMatchObject({
      code: "E_FIELD_PREIMAGE_SIZE",
      featureId: "mint_preimage",
    });
  });

  it("admits canonical V1 transactions larger than the launch 8 KiB cap", () => {
    const largeOutputs = Array.from({ length: 4 }, (_, index) =>
      encodeMidgardTxOutput(
        output({
          datum: {
            kind: "inline",
            cbor: canonicalDataBytes(Buffer.alloc(2_100, index + 1)),
          },
          script_ref:
            index === 0
              ? {
                  language: "MidgardV1",
                  scriptBytes: cekProgramEnvelope(),
                }
              : undefined,
        }),
      ),
    );
    for (const encodedOutput of largeOutputs) {
      expect(encodedOutput.length).toBeLessThanOrEqual(
        MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOutputPreimageBytes,
      );
    }
    const tx = materializeMidgardNativeTxFromCanonicalV1({
      ...canonical(),
      body: {
        ...canonical().body,
        outputsPreimageCbor: encodeCbor(largeOutputs),
      },
    });
    const txCbor = encodeMidgardNativeTxCanonicalV1(tx);
    expect(txCbor.length).toBeGreaterThan(8 * 1024);
    expect(validateMidgardConsensusV1TxCbor(txCbor)).toBeNull();
  });

  it("accepts the exact output envelope and rejects the next byte", () => {
    const encodedOutputForDatumPayload = (payloadBytes: number): Buffer =>
      encodeMidgardTxOutput(
        output({
          datum: {
            kind: "inline",
            cbor: canonicalDataBytes(Buffer.alloc(payloadBytes, 0x66)),
          },
          script_ref: undefined,
        }),
      );
    const maximum = MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOutputPreimageBytes;
    let lower = 0;
    let upper = maximum;
    while (lower < upper) {
      const midpoint = Math.ceil((lower + upper) / 2);
      if (encodedOutputForDatumPayload(midpoint).length <= maximum) {
        lower = midpoint;
      } else {
        upper = midpoint - 1;
      }
    }
    const payloadBytes = lower;
    const atMaximum = encodedOutputForDatumPayload(payloadBytes);
    const aboveMaximum = encodedOutputForDatumPayload(payloadBytes + 1);
    expect(atMaximum.length).toBe(maximum);
    expect(aboveMaximum.length).toBe(maximum + 1);

    const transactionWithOutput = (encodedOutput: Buffer) =>
      materializeMidgardNativeTxFromCanonicalV1({
        ...canonical(),
        body: {
          ...canonical().body,
          outputsPreimageCbor: encodeCbor([encodedOutput]),
        },
      });
    expect(
      validateMidgardConsensusV1Tx(transactionWithOutput(atMaximum), 1),
    ).toBeNull();
    expect(
      validateMidgardConsensusV1Tx(transactionWithOutput(aboveMaximum), 1),
    ).toMatchObject({ code: "E_LEDGER_OUTPUT_SIZE" });
  });

  it("admits a valid field above the old 9,215-byte cap and rejects malformed aggregate bytes", () => {
    const aboveOldLimit = encodeMidgardVersionedScriptListPreimage([
      ...Array.from({ length: 220 }, () => ({
        language: "PlutusV3" as const,
        scriptBytes: cekProgramEnvelope(1_597_819n, 67_108_418n),
      })),
    ]);
    const malformed = Buffer.alloc(40_000, 0x80);
    expect(aboveOldLimit.length).toBeGreaterThan(9_215);

    const transactionWithScripts = (scriptTxWitsPreimageCbor: Buffer) =>
      materializeMidgardNativeTxFromCanonicalV1({
        ...canonical(),
        witnessSet: {
          ...canonical().witnessSet,
          scriptTxWitsPreimageCbor,
        },
      });
    expect(
      validateMidgardConsensusV1Tx(transactionWithScripts(aboveOldLimit), 1),
    ).toBeNull();
    expect(() => transactionWithScripts(malformed)).toThrow(/trailing bytes/u);
  });

  it("does not impose the old arbitrary 128-asset transaction cap", () => {
    const policy = "ab".repeat(28);
    const outputs = Array.from({ length: 43 }, (_, outputIndex) => {
      const assets = new Map<string, bigint>();
      for (let assetIndex = 0; assetIndex < 3; assetIndex += 1) {
        const ordinal = outputIndex * 3 + assetIndex;
        assets.set(ordinal.toString(16).padStart(4, "0"), 1n);
      }
      return encodeMidgardTxOutput(
        output({
          value: {
            lovelace: 2_000_000n,
            assets: new Map([[policy, assets]]),
          },
          script_ref: undefined,
        }),
      );
    });
    const manyAssets = materializeMidgardNativeTxFromCanonicalV1({
      ...canonical(),
      body: {
        ...canonical().body,
        outputsPreimageCbor: encodeCbor(outputs),
        mintPreimageCbor: EMPTY_CBOR_LIST,
      },
    });

    expect(validateMidgardConsensusV1Tx(manyAssets, 1)).toBeNull();
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxDistinctAssetCount).toBeGreaterThan(
      128,
    );
  });

  it("removes the old depth-16 and node-32 native-script caps", () => {
    const withNativeScript = (
      nativeScript: MidgardNativeScript,
    ): ReturnType<typeof materializeMidgardNativeTxFromCanonicalV1> =>
      materializeMidgardNativeTxFromCanonicalV1({
        ...canonical(),
        witnessSet: {
          ...canonical().witnessSet,
          scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
            {
              language: "NativeCardano",
              scriptBytes: Buffer.alloc(0),
              nativeScript,
            },
          ]),
        },
      });

    expect(
      validateMidgardConsensusV1Tx(withNativeScript(nestedNativeScript(16)), 1),
    ).toBeNull();
    expect(
      validateMidgardConsensusV1Tx(withNativeScript(nestedNativeScript(17)), 1),
    ).toBeNull();
    expect(
      validateMidgardConsensusV1Tx(
        withNativeScript({
          type: "any",
          scripts: Array.from({ length: 33 }, () => ({
            type: "after" as const,
            slot: 0n,
          })),
        }),
        1,
      ),
    ).toBeNull();
    expect(MIDGARD_CONSENSUS_LIMITS_V1.maxNativeScriptDepth).toBe(16_384);
    expect(
      MIDGARD_CONSENSUS_LIMITS_V1.maxNativeScriptNodeCount,
    ).toBeGreaterThan(32);
  });
});
