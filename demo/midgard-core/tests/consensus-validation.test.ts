import { describe, expect, it } from "vitest";

import {
  computeMidgardNativeTxCanonicalSizeFromProofSource,
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxProofSource,
  deriveMidgardTxFieldPreimages,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardFieldPreimageForField,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardAddressFromText,
  midgardExpectedChunkCount,
  midgardFieldCommitment,
  midgardFieldCommitmentFromItems,
  type MidgardNativeScript,
  type MidgardNativeTxCanonical,
  type MidgardTxOutput,
  protectMidgardAddress,
  reconstructMidgardTransaction,
  splitMidgardFieldPreimageIntoChunks,
  validateMidgardConsensusTx,
  validateMidgardConsensusTxCbor,
  verifyMidgardNativeTxProofSource,
  verifyMidgardTxFieldPreimage,
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
  encodeMidgardCekProgramEnvelope({
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
  version = MIDGARD_NATIVE_TX_VERSION,
): MidgardNativeTxCanonical => ({
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
    // §5.6: the enveloped per-policy item list, not the retired raw map.
    mintPreimageCbor: encodeMidgardFieldPreimageForField({
      fieldIndex: 5,
      items: [
        {
          policyId: Buffer.alloc(28, 8),
          assets: [{ assetName: Buffer.from("asset", "ascii"), quantity: 1n }],
        },
      ],
    }),
    scriptIntegrityHash: Buffer.alloc(32, 9),
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
      { language: "MidgardV1", scriptBytes: cekProgramEnvelope() },
    ]),
    redeemerTxWitsPreimageCbor: encodeMidgardFieldPreimageForField({
      fieldIndex: 8,
      items: [
        {
          purpose: "Spend",
          index: 0n,
          redeemerCbor: Buffer.from([0x80]),
          executionUnits: { memory: 0n, steps: 0n },
        },
      ],
    }),
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
  it("admits the requested V1 feature surface instead of feature-gating it", () => {
    const tx = materializeMidgardNativeTxFromCanonical(canonical());
    const txCbor = encodeMidgardNativeTxCanonical(tx);
    expect(validateMidgardConsensusTx(tx, txCbor.length)).toBeNull();
    expect(validateMidgardConsensusTxCbor(txCbor)).toBeNull();
  });

  it("binds the full nine-field surface through a compact V1 source", () => {
    const tx = materializeMidgardNativeTxFromCanonical(canonical());
    const source = deriveMidgardNativeTxProofSource(tx);
    const transactionId = computeMidgardNativeTxId(tx);
    expect(source.compactCbor.length).toBeLessThan(1024);
    expect(source.witnessSetCompactCbor.length).toBeLessThan(256);
    expect(source.fieldPreimageLengthsCbor.length).toBeLessThan(64);
    expect(computeMidgardNativeTxCanonicalSizeFromProofSource(source)).toBe(
      encodeMidgardNativeTxCanonical(tx).length,
    );
    expect(verifyMidgardNativeTxProofSource({ transactionId, source })).toEqual(
      tx.compact,
    );
    expect(computeMidgardNativeTxProofCommitment(source)).toHaveLength(32);

    const tampered = Buffer.from(source.witnessSetCompactCbor);
    tampered[tampered.length - 1] ^= 1;
    expect(() =>
      verifyMidgardNativeTxProofSource({
        transactionId,
        source: { ...source, witnessSetCompactCbor: tampered },
      }),
    ).toThrow(/does not match/u);
  });

  it("reveals and verifies exactly one bounded field preimage per L1 instruction", () => {
    const tx = materializeMidgardNativeTxFromCanonical(canonical());
    const txCbor = encodeMidgardNativeTxCanonical(tx);
    const source = deriveMidgardNativeTxProofSource(tx);
    const transactionId = computeMidgardNativeTxId(tx);
    const transactionCommitment = computeMidgardNativeTxProofCommitment(source);
    const fields = deriveMidgardTxFieldPreimages(txCbor);

    expect(fields.map((field) => field.fieldIndex)).toEqual([
      0, 1, 2, 3, 4, 5, 6, 7, 8,
    ]);
    for (const field of fields) {
      expect(
        verifyMidgardTxFieldPreimage({
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
      verifyMidgardTxFieldPreimage({
        transactionId,
        transactionCommitment,
        source,
        fieldIndex: 2,
        preimageCbor: tampered,
      }),
    ).toThrow(/hash mismatch/u);
    expect(() =>
      verifyMidgardTxFieldPreimage({
        transactionId,
        transactionCommitment,
        source,
        fieldIndex: 9,
        preimageCbor: EMPTY_CBOR_LIST,
      }),
    ).toThrow(/unknown V1 transaction field index/u);
  });

  it("carries §4 flat commitments in the compact structure and nothing else", () => {
    // What the retired counted publication chain's tests were for: proving the
    // compact's nine hashes are reachable from the revealed bytes. Under §4 that
    // is one `blake2b_256` per field with no domain tag, version prefix or field
    // index in the input, so the property is stated directly instead of through
    // per-item openings — which §4 leaves nothing to check against.
    const tx = materializeMidgardNativeTxFromCanonical(canonical());
    const txCbor = encodeMidgardNativeTxCanonical(tx);

    for (const field of deriveMidgardTxFieldPreimages(txCbor)) {
      expect(field.expectedHash).toEqual(
        midgardFieldCommitment(field.preimageCbor),
      );
      // The commitment is over the bytes alone: re-hashing the items through the
      // §5.1 envelope reproduces it, and no per-field salt exists to make the
      // two disagree.
      expect(field.expectedHash).toEqual(
        midgardFieldCommitmentFromItems(
          decodeMidgardFieldPreimage(field.preimageCbor),
        ),
      );
    }
  });

  it("reveals a field through §8 carriage rather than per-item openings", () => {
    // The replacement for chunked per-item publication: a preimage too large for
    // tier 1 is split deterministically, the chunks are re-joined, and the field
    // authenticates once against the same compact hash. Nothing per-item is
    // published, and nothing per-item is verified.
    const tx = materializeMidgardNativeTxFromCanonical(canonical());
    const txCbor = encodeMidgardNativeTxCanonical(tx);
    const source = deriveMidgardNativeTxProofSource(tx);
    const transactionId = computeMidgardNativeTxId(tx);
    const transactionCommitment = computeMidgardNativeTxProofCommitment(source);
    const field = deriveMidgardTxFieldPreimages(txCbor).find(
      (candidate) => candidate.fieldIndex === 2,
    )!;

    const chunks = splitMidgardFieldPreimageIntoChunks(field.preimageCbor);
    expect(chunks).toHaveLength(
      midgardExpectedChunkCount(field.preimageCbor.length),
    );
    expect(Buffer.concat(chunks)).toEqual(field.preimageCbor);
    expect(
      verifyMidgardTxFieldPreimage({
        transactionId,
        transactionCommitment,
        source,
        fieldIndex: 2,
        preimageCbor: Buffer.concat(chunks),
      }),
    ).toEqual(field);

    // A chunk boundary is not a trust boundary: dropping one changes the bytes,
    // and the single §4 check is what catches it.
    expect(() =>
      verifyMidgardTxFieldPreimage({
        transactionId,
        transactionCommitment,
        source,
        fieldIndex: 2,
        preimageCbor: Buffer.concat(chunks.slice(0, -1)),
      }),
    ).toThrow(
      /preimage length does not match its compact source|hash mismatch/u,
    );
  });

  it("reconstructs the exact canonical forced transaction from nine authenticated fragments", () => {
    const tx = materializeMidgardNativeTxFromCanonical(canonical());
    const expected = encodeMidgardNativeTxCanonical(tx);
    const source = deriveMidgardNativeTxProofSource(tx);
    const transactionId = computeMidgardNativeTxId(tx);
    const transactionCommitment = computeMidgardNativeTxProofCommitment(source);
    const fields = deriveMidgardTxFieldPreimages(expected);

    expect(
      reconstructMidgardTransaction({
        transactionId,
        transactionCommitment,
        source,
        fieldPreimages: fields.map((field) => field.preimageCbor),
      }),
    ).toEqual(expected);
    expect(() =>
      reconstructMidgardTransaction({
        transactionId,
        transactionCommitment,
        source,
        fieldPreimages: fields.slice(0, 8).map((field) => field.preimageCbor),
      }),
    ).toThrow(/exactly 9 field preimages/u);
  });

  it("rejects every unsupported transaction version", () => {
    const current = materializeMidgardNativeTxFromCanonical(canonical());
    const unsupported = {
      ...current,
      version: 23n,
      compact: { ...current.compact, version: 23n },
    };
    expect(validateMidgardConsensusTx(unsupported, 1)).toMatchObject({
      code: "E_TX_VERSION",
    });
    expect(() =>
      encodeMidgardNativeTxCanonical({
        ...canonical(),
        version: 23n,
      }),
    ).toThrow(/transaction\.version must equal 1/u);
  });

  it("supports large output content while retaining the Cardano Value rule", () => {
    const largeValueAndDatum = materializeMidgardNativeTxFromCanonical({
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
    expect(validateMidgardConsensusTx(largeValueAndDatum, 1)).toBeNull();

    const oversizedValueAssets = new Map<string, bigint>(
      Array.from({ length: 160 }, (_, index) => [
        index.toString(16).padStart(64, "0"),
        1n,
      ]),
    );
    const valueTooLarge = materializeMidgardNativeTxFromCanonical({
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
    expect(validateMidgardConsensusTx(valueTooLarge, 1)).toMatchObject({
      code: "E_VALUE_SIZE",
    });

    const outputTooLarge = materializeMidgardNativeTxFromCanonical({
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
                    MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
                  ),
                ),
              },
              script_ref: undefined,
            }),
          ),
        ]),
      },
    });
    expect(validateMidgardConsensusTx(outputTooLarge, 1)).toMatchObject({
      code: "E_LEDGER_OUTPUT_SIZE",
    });
  });

  it("rejects malformed inline and reference program envelopes fail closed", () => {
    const malformedInline = materializeMidgardNativeTxFromCanonical({
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
    expect(validateMidgardConsensusTx(malformedInline, 1)).toMatchObject({
      code: "E_SCRIPT_PROGRAM_ENCODING",
      featureId: "script_witnesses",
    });

    const malformedReference = materializeMidgardNativeTxFromCanonical({
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
    expect(validateMidgardConsensusTx(malformedReference, 1)).toMatchObject({
      code: "E_SCRIPT_PROGRAM_ENCODING",
      featureId: "reference_scripts",
    });
  });

  it("uses the sum of bounded fields as the effective transaction cap", () => {
    expect(MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes).toBeGreaterThan(
      8 * 1024,
    );
    const base = materializeMidgardNativeTxFromCanonical(canonical());
    const oversizedField = {
      ...base,
      body: {
        ...base.body,
        mintPreimageCbor: Buffer.alloc(
          MIDGARD_CONSENSUS_LIMITS.maxMintPreimageBytes + 1,
          0x80,
        ),
      },
    };
    expect(validateMidgardConsensusTx(oversizedField, 1)).toMatchObject({
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
        MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
      );
    }
    const tx = materializeMidgardNativeTxFromCanonical({
      ...canonical(),
      body: {
        ...canonical().body,
        outputsPreimageCbor: encodeCbor(largeOutputs),
      },
    });
    const txCbor = encodeMidgardNativeTxCanonical(tx);
    expect(txCbor.length).toBeGreaterThan(8 * 1024);
    expect(validateMidgardConsensusTxCbor(txCbor)).toBeNull();
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
    const maximum = MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes;
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
      materializeMidgardNativeTxFromCanonical({
        ...canonical(),
        body: {
          ...canonical().body,
          outputsPreimageCbor: encodeCbor([encodedOutput]),
        },
      });
    expect(
      validateMidgardConsensusTx(transactionWithOutput(atMaximum), 1),
    ).toBeNull();
    expect(
      validateMidgardConsensusTx(transactionWithOutput(aboveMaximum), 1),
    ).toMatchObject({ code: "E_LEDGER_OUTPUT_SIZE" });
  });

  it("admits a valid field above the old 9,215-byte cap and rejects malformed aggregate bytes", () => {
    const aboveOldLimit = encodeMidgardVersionedScriptListPreimage([
      ...Array.from({ length: 220 }, () => ({
        language: "PlutusV3" as const,
        scriptBytes: cekProgramEnvelope(
          BigInt(MIDGARD_CONSENSUS_LIMITS.maxCekProgramNodeCount),
          BigInt(MIDGARD_CONSENSUS_LIMITS.maxCekProgramMaterialBytes),
        ),
      })),
    ]);
    const malformed = Buffer.alloc(40_000, 0x80);
    expect(aboveOldLimit.length).toBeGreaterThan(9_215);

    const transactionWithScripts = (scriptTxWitsPreimageCbor: Buffer) =>
      materializeMidgardNativeTxFromCanonical({
        ...canonical(),
        witnessSet: {
          ...canonical().witnessSet,
          scriptTxWitsPreimageCbor,
        },
      });
    expect(
      validateMidgardConsensusTx(transactionWithScripts(aboveOldLimit), 1),
    ).toBeNull();
    // 40,000 bytes of `0x80` is `80` — a §5.1 empty-field header — followed by
    // 39,999 trailing bytes, so materialisation refuses it at the grammar check
    // rather than committing to bytes no decoder accepts.
    expect(() => transactionWithScripts(malformed)).toThrow(
      /script_tx_wits is not a canonical §5\.1 field preimage/u,
    );
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
    const manyAssets = materializeMidgardNativeTxFromCanonical({
      ...canonical(),
      body: {
        ...canonical().body,
        outputsPreimageCbor: encodeCbor(outputs),
        mintPreimageCbor: EMPTY_CBOR_LIST,
      },
    });

    expect(validateMidgardConsensusTx(manyAssets, 1)).toBeNull();
    expect(MIDGARD_CONSENSUS_LIMITS.maxDistinctAssetCount).toBeGreaterThan(128);
  });

  it("removes the old depth-16 and node-32 native-script caps", () => {
    const withNativeScript = (
      nativeScript: MidgardNativeScript,
    ): ReturnType<typeof materializeMidgardNativeTxFromCanonical> =>
      materializeMidgardNativeTxFromCanonical({
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
      validateMidgardConsensusTx(withNativeScript(nestedNativeScript(16)), 1),
    ).toBeNull();
    expect(
      validateMidgardConsensusTx(withNativeScript(nestedNativeScript(17)), 1),
    ).toBeNull();
    expect(
      validateMidgardConsensusTx(
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
    expect(MIDGARD_CONSENSUS_LIMITS.maxNativeScriptDepth).toBe(16_384);
    expect(MIDGARD_CONSENSUS_LIMITS.maxNativeScriptNodeCount).toBeGreaterThan(
      32,
    );
  });
});
