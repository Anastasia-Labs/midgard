import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import * as codec from "../src/index.js";
import {
  assertNativePosixTimeOrNone,
  computeHash32,
  computeMidgardNativeTxFullHashFromCanonicalCborV1,
  computeMidgardNativeTxFullHashV1,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardFieldPreimageV1,
  decodeMidgardNativeScript,
  decodeMidgardNativeTxBodyCanonicalV1,
  decodeMidgardNativeTxBodyCompactV1,
  decodeMidgardNativeTxCanonicalEnvelopeForFaultEvidenceV1,
  decodeMidgardNativeTxCanonicalV1,
  decodeMidgardNativeTxCompactV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardNativeTxProofFieldLengthsV1,
  decodeMidgardNativeTxWitnessPreimagesV1,
  decodeMidgardNativeTxWitnessSetCompactV1,
  decodeSingleCbor,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  deriveMidgardNativeTxProofSourceV1,
  deriveMidgardNativeTxWitnessSetCompactV1,
  EMPTY_CBOR_LIST,
  EMPTY_CBOR_NULL,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeCborArrayRaw,
  encodeMidgardFieldPreimageV1,
  encodeMidgardNativeScript,
  encodeMidgardNativeTxBodyCanonicalV1,
  encodeMidgardNativeTxBodyCompactV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardNativeTxProofFieldLengthsV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_EMPTY_FIELD_COMMITMENT_V1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  midgardFieldCommitmentV1,
  type MidgardNativeScript,
  type MidgardNativeTxCanonicalV1,
  verifyMidgardNativeScript,
  verifyMidgardNativeTxProofSourceV1,
} from "../src/index.js";
import {
  deriveNativeTxVectorV1,
  nativeTxVectorCanonicalV1,
} from "./fixtures/native-tx-vector-v1.vectors.mjs";

const makeCanonical = (): MidgardNativeTxCanonicalV1 => ({
  version: MIDGARD_NATIVE_TX_V1_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputsPreimageCbor: EMPTY_CBOR_LIST,
    referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
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
});

/**
 * The `n01`–`n09` golden, defined once in
 * `tests/fixtures/native-tx-vector-v1.vectors.mjs` and driven from here through
 * `src/` while `scripts/generate-native-tx-vector-v1-goldens.mjs` drives the same
 * definition through `dist/`. That is what lets the Aiken `n0x` constants be
 * generated rather than hand-mirrored (#588).
 */
const makeGoldenCanonical = (): MidgardNativeTxCanonicalV1 =>
  nativeTxVectorCanonicalV1(codec);

/**
 * The checked-in vector fixture. Recomputing it here rather than pinning the
 * bytes inline is the TypeScript half of the golden channel: a codec that drifts
 * fails here, not only under the generator's `--check` or on the Aiken side.
 */
const generatedNativeTxVector: unknown = (
  JSON.parse(
    readFileSync(
      new URL("./fixtures/native-tx-vector-v1.generated.json", import.meta.url),
      "utf8",
    ),
  ) as { readonly vector: unknown }
).vector;

describe("Midgard native v1 codec", () => {
  it("round trips canonical transaction CBOR into a materialized full transaction", () => {
    const tx = materializeMidgardNativeTxFromCanonicalV1(makeCanonical());
    const encoded = encodeMidgardNativeTxCanonicalV1(tx);
    const decoded = decodeMidgardNativeTxFullV1FromCanonicalCbor(encoded);

    expect(decoded.version).toBe(MIDGARD_NATIVE_TX_V1_VERSION);
    expect(decoded.compact.transactionBody).toEqual(tx.compact.transactionBody);
    expect(decoded.compact.transactionWitnessSetHash).toEqual(
      tx.compact.transactionWitnessSetHash,
    );
    // §5.6: an empty mint is `80` like every other field, so its §4 commitment
    // is the one every empty field shares.
    expect(decoded.compact.transactionBody.mintHash).toEqual(
      MIDGARD_EMPTY_FIELD_COMMITMENT_V1,
    );
  });

  it("decodes canonical transaction CBOR without deriving compact fields", () => {
    const canonical = makeCanonical();
    const decoded = decodeMidgardNativeTxCanonicalV1(
      encodeMidgardNativeTxCanonicalV1(canonical),
    );

    expect(decoded).toEqual(canonical);
    expect("compact" in decoded).toBe(false);
  });

  it("derives fault evidence from an outer-canonical transaction with an opaque malformed field", () => {
    const valid = encodeMidgardNativeTxCanonicalV1(makeCanonical());
    const decoded = decodeSingleCbor(valid);
    expect(Array.isArray(decoded)).toBe(true);
    const outer = decoded as unknown[];
    const body = outer[1];
    expect(Array.isArray(body)).toBe(true);
    const malformed = encodeCbor([
      outer[0],
      [
        ...(body as unknown[]).slice(0, 2),
        Buffer.from([0]),
        ...(body as unknown[]).slice(3),
      ],
      outer[2],
      outer[3],
    ]);

    expect(() => decodeMidgardNativeTxCanonicalV1(malformed)).toThrow(
      /outputs is not a canonical §5\.1 field preimage/u,
    );
    const envelope =
      decodeMidgardNativeTxCanonicalEnvelopeForFaultEvidenceV1(malformed);
    expect(envelope.body.outputsPreimageCbor).toEqual(Buffer.from([0]));

    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(malformed);
    expect(material.compact.transactionBody.outputsHash).toEqual(
      midgardFieldCommitmentV1(Buffer.from([0])),
    );
    expect(material.fieldPreimages[2]).toEqual(Buffer.from([0]));
    expect(
      verifyMidgardNativeTxProofSourceV1({
        transactionId: material.transactionId,
        source: material.proofSource,
      }),
    ).toEqual(material.compact);

    expect(() =>
      decodeMidgardNativeTxCanonicalEnvelopeForFaultEvidenceV1(
        malformed.subarray(0, malformed.length - 1),
      ),
    ).toThrow();
    expect(() =>
      decodeMidgardNativeTxCanonicalEnvelopeForFaultEvidenceV1(
        encodeCbor([...outer, 0n]),
      ),
    ).toThrow(/exactly 4 elements/u);
  });

  it("uses the canonical V1 compact-body domain for the transaction id", () => {
    const tx = materializeMidgardNativeTxFromCanonicalV1(makeCanonical());
    const bodyCbor = encodeMidgardNativeTxBodyCompactV1(
      tx.compact.transactionBody,
    );

    expect(computeMidgardNativeTxIdV1(tx)).toEqual(
      computeHash32(
        Buffer.concat([
          Buffer.from("MidgardNativeTxBodyV1", "ascii"),
          Buffer.from([1]),
          bodyCbor,
        ]),
      ),
    );
  });

  it("wraps field 6 and field 7 items in the one §5.1 envelope", () => {
    // §5.1: the per-item byte-string envelope applies to all nine fields. Under
    // the retired counted scheme field 6 concatenated raw item CBOR; that form is
    // prohibited, so the same item now appears as `bytes(enc_6)`.
    const scriptItem = encodeCbor([3n, Buffer.from("010203", "hex")]);
    const scriptPreimage = encodeMidgardFieldPreimageV1([scriptItem]);
    const address = Buffer.from("aabbcc", "hex");
    const addressPreimage = encodeMidgardFieldPreimageV1([address]);

    expect(decodeMidgardFieldPreimageV1(scriptPreimage)).toEqual([scriptItem]);
    expect(decodeMidgardFieldPreimageV1(addressPreimage)).toEqual([address]);
    // The retired raw-concatenation form for field 6 no longer decodes: `82` is
    // not a §5.1 definite byte-string header.
    expect(() =>
      decodeMidgardFieldPreimageV1(encodeCborArrayRaw([scriptItem])),
    ).toThrow(/not a §5.1 definite byte-string header/u);

    const witnessSet = {
      addrTxWitsPreimageCbor: addressPreimage,
      scriptTxWitsPreimageCbor: scriptPreimage,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    };
    const compact = deriveMidgardNativeTxWitnessSetCompactV1(witnessSet);
    expect(compact.scriptTxWitsHash).toEqual(
      midgardFieldCommitmentV1(scriptPreimage),
    );
    expect(compact.addrTxWitsHash).toEqual(
      midgardFieldCommitmentV1(addressPreimage),
    );
    // §4 is plain hashing with no field index in the input, so fields 6 and 7
    // alias on identical content — and the empty redeemer field shares the one
    // empty commitment with all eight of its siblings.
    expect(compact.redeemerTxWitsHash).toEqual(
      MIDGARD_EMPTY_FIELD_COMMITMENT_V1,
    );
  });

  it("pins every native-V1 transaction schema and commitment language", () => {
    const tx = materializeMidgardNativeTxFromCanonicalV1(makeGoldenCanonical());
    const source = deriveMidgardNativeTxProofSourceV1(tx);
    const exact = deriveNativeTxVectorV1(codec);

    expect(exact).toEqual(generatedNativeTxVector);

    expect(
      decodeMidgardNativeTxBodyCanonicalV1(
        Buffer.from(exact.bodyCanonical, "hex"),
      ),
    ).toEqual(tx.body);
    expect(
      decodeMidgardNativeTxBodyCompactV1(Buffer.from(exact.bodyCompact, "hex")),
    ).toEqual(tx.compact.transactionBody);
    expect(
      decodeMidgardNativeTxWitnessPreimagesV1(
        Buffer.from(exact.witnessPreimages, "hex"),
      ),
    ).toEqual(tx.witnessSet);
    expect(
      decodeMidgardNativeTxWitnessSetCompactV1(
        Buffer.from(exact.witnessCompact, "hex"),
      ),
    ).toEqual(
      decodeMidgardNativeTxWitnessSetCompactV1(source.witnessSetCompactCbor),
    );
    expect(
      decodeMidgardNativeTxCompactV1(Buffer.from(exact.compact, "hex")),
    ).toEqual(tx.compact);
    expect(
      decodeMidgardNativeTxCanonicalV1(Buffer.from(exact.canonical, "hex")),
    ).toEqual(makeGoldenCanonical());
    expect(
      computeMidgardNativeTxFullHashFromCanonicalCborV1(
        Buffer.from(exact.canonical, "hex"),
      ).toString("hex"),
    ).toBe(exact.fullHash);
    expect(
      computeMidgardNativeTxFullHashFromCanonicalCborV1(
        Buffer.from("80", "hex"),
      ).toString("hex"),
    ).toBe("00039b53f81dc1d7d1e55c12c2369f2cbe7aa4c2943f990ddf7faee2377b3178");
    expect(
      decodeMidgardNativeTxProofFieldLengthsV1(
        Buffer.from(exact.orderedLengthTuple, "hex"),
      ),
    ).toEqual([1, 2, 3, 4, 5, 6, 7, 8, 9]);
    expect(
      verifyMidgardNativeTxProofSourceV1({
        transactionId: Buffer.from(exact.transactionId, "hex"),
        source,
      }),
    ).toEqual(tx.compact);

    expect(() =>
      verifyMidgardNativeTxProofSourceV1({
        transactionId: Buffer.alloc(32, 0xff),
        source,
      }),
    ).toThrow(/does not match the transaction id/u);
    expect(() =>
      verifyMidgardNativeTxProofSourceV1({
        transactionId: computeMidgardNativeTxIdV1(tx),
        source: {
          ...source,
          witnessSetCompactCbor: Buffer.from(
            source.witnessSetCompactCbor.map((byte, index) =>
              index === source.witnessSetCompactCbor.length - 1
                ? byte ^ 1
                : byte,
            ),
          ),
        },
      }),
    ).toThrow(/compact witness set does not match/u);
    expect(() =>
      encodeMidgardNativeTxProofFieldLengthsV1([1, 2, 3, 4, 5, 6, 7, 8]),
    ).toThrow(/exactly nine/u);

    const withoutLast = (hex: string): Buffer => {
      const decoded = decodeSingleCbor(Buffer.from(hex, "hex"));
      expect(Array.isArray(decoded)).toBe(true);
      return encodeCbor((decoded as unknown[]).slice(0, -1));
    };
    const withSmuggledField = (hex: string): Buffer => {
      const decoded = decodeSingleCbor(Buffer.from(hex, "hex"));
      expect(Array.isArray(decoded)).toBe(true);
      return encodeCbor([...(decoded as unknown[]), 0n]);
    };
    expect(() =>
      decodeMidgardNativeTxBodyCanonicalV1(withoutLast(exact.bodyCanonical)),
    ).toThrow(/exactly 12 elements/u);
    expect(() =>
      decodeMidgardNativeTxBodyCompactV1(withoutLast(exact.bodyCompact)),
    ).toThrow(/exactly 12 elements/u);
    expect(() =>
      decodeMidgardNativeTxWitnessPreimagesV1(
        withoutLast(exact.witnessPreimages),
      ),
    ).toThrow(/exactly 3 elements/u);
    expect(() =>
      decodeMidgardNativeTxWitnessSetCompactV1(
        withoutLast(exact.witnessCompact),
      ),
    ).toThrow(/exactly 3 elements/u);
    expect(() =>
      decodeMidgardNativeTxCompactV1(withoutLast(exact.compact)),
    ).toThrow(/exactly 4 elements/u);
    expect(() =>
      decodeMidgardNativeTxCanonicalV1(withoutLast(exact.canonical)),
    ).toThrow(/exactly 4 elements/u);

    // W-T2 soundness: exact arity is a closed schema, not merely a minimum.
    // These are canonical CBOR values, so rejection proves that an otherwise
    // well-formed trailing field cannot be smuggled into any native-V1 record.
    expect(() =>
      decodeMidgardNativeTxBodyCanonicalV1(
        withSmuggledField(exact.bodyCanonical),
      ),
    ).toThrow(/exactly 12 elements/u);
    expect(() =>
      decodeMidgardNativeTxBodyCompactV1(withSmuggledField(exact.bodyCompact)),
    ).toThrow(/exactly 12 elements/u);
    expect(() =>
      decodeMidgardNativeTxWitnessPreimagesV1(
        withSmuggledField(exact.witnessPreimages),
      ),
    ).toThrow(/exactly 3 elements/u);
    expect(() =>
      decodeMidgardNativeTxWitnessSetCompactV1(
        withSmuggledField(exact.witnessCompact),
      ),
    ).toThrow(/exactly 3 elements/u);
    expect(() =>
      decodeMidgardNativeTxCompactV1(withSmuggledField(exact.compact)),
    ).toThrow(/exactly 4 elements/u);
    expect(() =>
      decodeMidgardNativeTxCanonicalV1(withSmuggledField(exact.canonical)),
    ).toThrow(/exactly 4 elements/u);

    // `81 00` is a §5.1 array header followed by an integer where an item's
    // byte-string wrapper must be.
    expect(() =>
      encodeMidgardNativeTxCanonicalV1({
        ...makeGoldenCanonical(),
        body: {
          ...makeGoldenCanonical().body,
          spendInputsPreimageCbor: encodeCbor([0n]),
        },
      }),
    ).toThrow(/spend_inputs is not a canonical §5\.1 field preimage/u);
    const malformedNestedPreimage = Buffer.from(exact.canonical, "hex");
    malformedNestedPreimage[4] = 0x9f;
    // Byte 4 is the `80` payload of the empty spend-inputs preimage; `9f` is
    // CBOR's indefinite-length array head, which §5.1's minimal-width grammar
    // refuses as an array header at all.
    expect(() =>
      decodeMidgardNativeTxCanonicalV1(malformedNestedPreimage),
    ).toThrow(/spend_inputs is not a canonical §5\.1 field preimage/u);

    const witnessMutated = materializeMidgardNativeTxFromCanonicalV1({
      ...makeGoldenCanonical(),
      witnessSet: {
        ...makeGoldenCanonical().witnessSet,
        addrTxWitsPreimageCbor: encodeCbor([Buffer.from([0])]),
      },
    });
    expect(computeMidgardNativeTxIdV1(witnessMutated)).toEqual(
      computeMidgardNativeTxIdV1(tx),
    );
    expect(computeMidgardNativeTxFullHashV1(witnessMutated)).not.toEqual(
      computeMidgardNativeTxFullHashV1(tx),
    );
    expect(
      computeMidgardNativeTxProofCommitmentV1(
        deriveMidgardNativeTxProofSourceV1(witnessMutated),
      ),
    ).not.toEqual(computeMidgardNativeTxProofCommitmentV1(source));
  });

  it("pins the native network-id language to absence, testnet, or mainnet", () => {
    const canonical = makeCanonical().body;
    const compact =
      materializeMidgardNativeTxFromCanonicalV1(makeCanonical()).compact
        .transactionBody;

    for (const networkId of [MIDGARD_NATIVE_NETWORK_ID_NONE, 0n, 1n]) {
      expect(
        decodeMidgardNativeTxBodyCanonicalV1(
          encodeMidgardNativeTxBodyCanonicalV1({
            ...canonical,
            networkId,
          }),
        ).networkId,
      ).toBe(networkId);
      expect(
        decodeMidgardNativeTxBodyCompactV1(
          encodeMidgardNativeTxBodyCompactV1({
            ...compact,
            networkId,
          }),
        ).networkId,
      ).toBe(networkId);
    }

    for (const networkId of [2n, 254n, 256n]) {
      expect(() =>
        encodeMidgardNativeTxBodyCanonicalV1({
          ...canonical,
          networkId,
        }),
      ).toThrow(/network_id must be 0, 1, or 255/u);
      expect(() =>
        encodeMidgardNativeTxBodyCompactV1({
          ...compact,
          networkId,
        }),
      ).toThrow(/network_id must be 0, 1, or 255/u);
    }

    const canonicalNone = encodeMidgardNativeTxBodyCanonicalV1(canonical);
    const compactNone = encodeMidgardNativeTxBodyCompactV1(compact);
    expect(() =>
      decodeMidgardNativeTxBodyCanonicalV1(
        Buffer.concat([canonicalNone.subarray(0, -2), Buffer.from([2])]),
      ),
    ).toThrow(/transaction_body\[11\] must be 0, 1, or 255/u);
    expect(() =>
      decodeMidgardNativeTxBodyCompactV1(
        Buffer.concat([compactNone.subarray(0, -2), Buffer.from([2])]),
      ),
    ).toThrow(/transaction_body\[11\] must be 0, 1, or 255/u);
  });

  it("pins the optional POSIX-time sentinel and adjacent invalid value", () => {
    expect(
      assertNativePosixTimeOrNone(
        MIDGARD_POSIX_TIME_NONE,
        "validity_interval_start",
      ),
    ).toBe(-1n);
    expect(assertNativePosixTimeOrNone(0n, "validity_interval_start")).toBe(0n);
    expect(assertNativePosixTimeOrNone(1n, "validity_interval_end")).toBe(1n);
    expect(() =>
      assertNativePosixTimeOrNone(-2n, "validity_interval_start"),
    ).toThrow(/must be -1 or a nonnegative POSIX time/u);
  });

  it("rejects every unsupported native transaction version", () => {
    const encoded = encodeMidgardNativeTxCanonicalV1(makeCanonical());
    const unsupported = Buffer.from(encoded);
    unsupported[1] = 2;
    expect(() => decodeMidgardNativeTxCanonicalV1(unsupported)).toThrow(
      /transaction\[0\] must equal 1/u,
    );
  });

  it("rejects the same non-minimal transaction encodings as the Aiken decoder", () => {
    const encoded = encodeMidgardNativeTxCanonicalV1(
      materializeMidgardNativeTxFromCanonicalV1(makeCanonical()),
    );
    expect(encoded[0]).toBe(0x84);
    expect(encoded[1]).toBe(0x01);

    // These are the TypeScript twins of
    // `v1_n01_noncanonical_array_head_rejects` and
    // `v1_n01_noncanonical_version_encoding_rejects` in
    // `fraud-proofs/native-tx-v1.test.ak`.
    const nonMinimalArrayHead = Buffer.concat([
      Buffer.from([0x98, 0x04]),
      encoded.subarray(1),
    ]);
    const nonMinimalVersion = Buffer.concat([
      encoded.subarray(0, 1),
      Buffer.from([0x18, 0x01]),
      encoded.subarray(2),
    ]);

    expect(() =>
      decodeMidgardNativeTxFullV1FromCanonicalCbor(nonMinimalArrayHead),
    ).toThrow(/Non-minimal CBOR integer or length encoding/u);
    expect(() =>
      decodeMidgardNativeTxFullV1FromCanonicalCbor(nonMinimalVersion),
    ).toThrow(/Non-minimal CBOR integer or length encoding/u);
  });

  it("rejects derived compact body drift", () => {
    const tx = materializeMidgardNativeTxFromCanonicalV1(makeCanonical());
    const tampered: typeof tx = {
      ...tx,
      compact: {
        ...tx.compact,
        transactionBody: {
          ...tx.compact.transactionBody,
          outputsHash: Buffer.alloc(32, 1),
        },
      },
    };

    expect(() => encodeMidgardNativeTxCanonicalV1(tampered)).toThrow(
      /transaction_compact\.transaction_body must match the derived compact body/,
    );
  });

  it("rejects trailing CBOR bytes", () => {
    const encoded = encodeMidgardNativeTxCanonicalV1(
      materializeMidgardNativeTxFromCanonicalV1(makeCanonical()),
    );

    expect(() =>
      decodeMidgardNativeTxFullV1FromCanonicalCbor(
        Buffer.concat([encoded, EMPTY_CBOR_NULL]),
      ),
    ).toThrow(/cbor has trailing bytes/);
  });

  it("pins every Cardano native-script tag and arity", () => {
    const keyHash = Buffer.alloc(28, 0xaa);
    const vectors: readonly {
      readonly script: MidgardNativeScript;
      readonly cborHex: string;
    }[] = [
      {
        script: { type: "sig", keyHash },
        cborHex: `8200581c${"aa".repeat(28)}`,
      },
      { script: { type: "all", scripts: [] }, cborHex: "820180" },
      { script: { type: "any", scripts: [] }, cborHex: "820280" },
      {
        script: { type: "atLeast", required: 0n, scripts: [] },
        cborHex: "83030080",
      },
      { script: { type: "after", slot: 0n }, cborHex: "820400" },
      { script: { type: "before", slot: 0n }, cborHex: "820500" },
    ];

    for (const vector of vectors) {
      const encoded = encodeMidgardNativeScript(vector.script);
      expect(encoded.toString("hex")).toBe(vector.cborHex);
      expect(
        decodeMidgardNativeScript(Buffer.from(vector.cborHex, "hex")).script,
      ).toEqual(vector.script);
    }

    expect(() =>
      decodeMidgardNativeScript(Buffer.from("820600", "hex")),
    ).toThrow(/Unsupported native script tag/);
  });

  it("accepts the V1 native-script depth boundary and rejects its canonical adjacent", () => {
    const keyHash = Buffer.alloc(28, 0xaa);
    const leaf = Buffer.from(`8200581c${keyHash.toString("hex")}`, "hex");
    const canonicalAtDepth = (depth: number): Buffer =>
      Buffer.concat([Buffer.from("820181".repeat(depth - 1), "hex"), leaf]);
    const maximum = MIDGARD_CONSENSUS_LIMITS_V1.maxNativeScriptDepth;
    const acceptedBytes = canonicalAtDepth(maximum);
    const accepted = decodeMidgardNativeScript(acceptedBytes);

    expect(accepted.cbor).toEqual(acceptedBytes);
    expect(encodeMidgardNativeScript(accepted.script)).toEqual(acceptedBytes);
    expect(
      verifyMidgardNativeScript(accepted.script, {
        witnessSigners: new Set([keyHash.toString("hex")]),
      }),
    ).toBe(true);
    const adjacentScript: MidgardNativeScript = {
      type: "all",
      scripts: [accepted.script],
    };
    expect(() => encodeMidgardNativeScript(adjacentScript)).toThrow(
      /Native script nesting exceeds the V1 maximum/u,
    );
    expect(
      verifyMidgardNativeScript(adjacentScript, {
        witnessSigners: new Set([keyHash.toString("hex")]),
      }),
    ).toBe(false);
    expect(() =>
      decodeMidgardNativeScript(canonicalAtDepth(maximum + 1)),
    ).toThrow(/Native script nesting exceeds the V1 maximum/u);
  });

  it("accepts the V1 native-script node boundary and rejects its wide adjacent", () => {
    const maximum = MIDGARD_CONSENSUS_LIMITS_V1.maxNativeScriptNodeCount;
    const leaf = (): MidgardNativeScript => ({
      type: "sig",
      keyHash: Buffer.alloc(28, 0xbb),
    });
    const accepted: MidgardNativeScript = {
      type: "all",
      scripts: Array.from({ length: maximum - 1 }, leaf),
    };
    const acceptedBytes = encodeMidgardNativeScript(accepted);
    const decoded = decodeMidgardNativeScript(acceptedBytes);

    expect(encodeMidgardNativeScript(decoded.script)).toEqual(acceptedBytes);
    expect(
      verifyMidgardNativeScript(decoded.script, {
        witnessSigners: new Set([Buffer.alloc(28, 0xbb).toString("hex")]),
      }),
    ).toBe(true);

    const adjacent: MidgardNativeScript = {
      type: "all",
      scripts: [...accepted.scripts, leaf()],
    };
    const leafBytes = encodeMidgardNativeScript(leaf());
    const adjacentBytes = Buffer.concat([
      Buffer.from([0x82, 0x01, 0x99, 0x40, 0x00]),
      acceptedBytes.subarray(5),
      leafBytes,
    ]);
    expect(() => encodeMidgardNativeScript(adjacent)).toThrow(
      /Native script node count exceeds the V1 maximum/u,
    );
    expect(() => decodeMidgardNativeScript(adjacentBytes)).toThrow(
      /Native script node count exceeds the V1 maximum/u,
    );
    expect(
      verifyMidgardNativeScript(adjacent, {
        witnessSigners: new Set([Buffer.alloc(28, 0xbb).toString("hex")]),
      }),
    ).toBe(false);
  });

  it("matches Cardano before timelock boundary semantics", () => {
    const script = { type: "before", slot: 100n } as const;
    const witnessSigners = new Set<string>();

    expect(verifyMidgardNativeScript(script, { witnessSigners })).toBe(false);
    expect(
      verifyMidgardNativeScript(script, {
        validityIntervalEnd: 99n,
        witnessSigners,
      }),
    ).toBe(true);
    expect(
      verifyMidgardNativeScript(script, {
        validityIntervalEnd: 100n,
        witnessSigners,
      }),
    ).toBe(true);
    expect(
      verifyMidgardNativeScript(script, {
        validityIntervalEnd: 101n,
        witnessSigners,
      }),
    ).toBe(false);
  });

  it("matches Cardano after timelock boundary semantics", () => {
    const script = { type: "after", slot: 100n } as const;
    const witnessSigners = new Set<string>();

    expect(verifyMidgardNativeScript(script, { witnessSigners })).toBe(false);
    expect(
      verifyMidgardNativeScript(script, {
        validityIntervalStart: 99n,
        witnessSigners,
      }),
    ).toBe(false);
    expect(
      verifyMidgardNativeScript(script, {
        validityIntervalStart: 100n,
        witnessSigners,
      }),
    ).toBe(true);
  });
});
