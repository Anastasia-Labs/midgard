import { describe, expect, it } from "vitest";

import {
  assertNativePosixTimeOrNone,
  computeHash32,
  computeMidgardNativeTxCanonicalSizeFromProofSourceV1,
  computeMidgardNativeTxFullHashFromCanonicalCborV1,
  computeMidgardNativeTxFullHashV1,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeScript,
  decodeMidgardNativeTxBodyCanonicalV1,
  decodeMidgardNativeTxBodyCompactV1,
  decodeMidgardNativeTxCanonicalV1,
  decodeMidgardNativeTxCompactV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardNativeTxProofFieldLengthsV1,
  decodeMidgardNativeTxWitnessPreimagesV1,
  decodeMidgardNativeTxWitnessSetCompactV1,
  decodeSingleCbor,
  deriveMidgardNativeFieldCollectionV1,
  deriveMidgardNativeFieldItemBytesV1,
  deriveMidgardNativeTxProofSourceV1,
  deriveMidgardNativeTxWitnessSetCompactV1,
  EMPTY_CBOR_LIST,
  EMPTY_CBOR_NULL,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeScript,
  encodeMidgardNativeTxBodyCanonicalV1,
  encodeMidgardNativeTxBodyCompactV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardNativeTxCompactV1,
  encodeMidgardNativeTxProofFieldLengthsV1,
  encodeMidgardNativeTxProofSourceV1,
  encodeMidgardNativeTxWitnessPreimagesV1,
  encodeMidgardNativeTxWitnessSetCompactV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeScript,
  type MidgardNativeTxCanonicalV1,
  verifyMidgardNativeScript,
  verifyMidgardNativeTxProofSourceV1,
} from "../src/index.js";

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

const makeGoldenCanonical = (): MidgardNativeTxCanonicalV1 => ({
  ...makeCanonical(),
  validity: "FailedScript",
  body: {
    ...makeCanonical().body,
    fee: 23n,
    validityIntervalEnd: 24n,
    scriptIntegrityHash: Buffer.alloc(32, 0x11),
    auxiliaryDataHash: Buffer.alloc(32, 0x22),
    networkId: 1n,
  },
});

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
    expect(decoded.compact.transactionBody.mintHash).toEqual(
      deriveMidgardNativeFieldCollectionV1({
        fieldIndex: 5,
        preimageCbor: EMPTY_CBOR_LIST,
      }).commitment,
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

  it("binds field 6 to raw script items and field 7 to address bytes", () => {
    const scriptItem = encodeCbor([3n, Buffer.from("010203", "hex")]);
    const scriptPreimage = encodeCbor([[3n, Buffer.from("010203", "hex")]]);
    const address = Buffer.from("aabbcc", "hex");
    const addressPreimage = encodeCbor([address]);

    expect(
      deriveMidgardNativeFieldItemBytesV1({
        fieldIndex: 6,
        preimageCbor: scriptPreimage,
      }),
    ).toEqual([scriptItem]);
    expect(
      deriveMidgardNativeFieldItemBytesV1({
        fieldIndex: 7,
        preimageCbor: addressPreimage,
      }),
    ).toEqual([address]);
    expect(() =>
      deriveMidgardNativeFieldItemBytesV1({
        fieldIndex: 7,
        preimageCbor: scriptPreimage,
      }),
    ).toThrow(/must be a CBOR byte string/u);

    const witnessSet = {
      addrTxWitsPreimageCbor: addressPreimage,
      scriptTxWitsPreimageCbor: scriptPreimage,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    };
    const compact = deriveMidgardNativeTxWitnessSetCompactV1(witnessSet);
    expect(compact.scriptTxWitsHash).toEqual(
      deriveMidgardNativeFieldCollectionV1({
        fieldIndex: 6,
        preimageCbor: scriptPreimage,
      }).commitment,
    );
    expect(compact.addrTxWitsHash).toEqual(
      deriveMidgardNativeFieldCollectionV1({
        fieldIndex: 7,
        preimageCbor: addressPreimage,
      }).commitment,
    );
  });

  it("pins every native-V1 transaction schema and commitment language", () => {
    const tx = materializeMidgardNativeTxFromCanonicalV1(makeGoldenCanonical());
    const source = deriveMidgardNativeTxProofSourceV1(tx);
    const orderedLengthTx = materializeMidgardNativeTxFromCanonicalV1({
      ...makeGoldenCanonical(),
      body: {
        ...makeGoldenCanonical().body,
        spendInputsPreimageCbor: encodeCbor([]),
        referenceInputsPreimageCbor: encodeCbor([Buffer.alloc(0)]),
        outputsPreimageCbor: encodeCbor([Buffer.alloc(1)]),
        requiredObserversPreimageCbor: encodeCbor([Buffer.alloc(2)]),
        requiredSignersPreimageCbor: encodeCbor([Buffer.alloc(3)]),
        mintPreimageCbor: encodeCbor(
          new Map([[Buffer.alloc(28), new Map([[Buffer.alloc(0), 1n]])]]),
        ),
      },
      witnessSet: {
        addrTxWitsPreimageCbor: encodeCbor([Buffer.alloc(4)]),
        scriptTxWitsPreimageCbor: encodeCbor([0n, 1n, 2n, 3n, 4n, 5n]),
        redeemerTxWitsPreimageCbor: encodeCbor([0n, 1n, 2n, 3n, 4n, 5n, 6n]),
      },
    });
    const exact = {
      bodyCanonical: encodeMidgardNativeTxBodyCanonicalV1(tx.body).toString(
        "hex",
      ),
      bodyCompact: encodeMidgardNativeTxBodyCompactV1(
        tx.compact.transactionBody,
      ).toString("hex"),
      witnessPreimages: encodeMidgardNativeTxWitnessPreimagesV1(
        tx.witnessSet,
      ).toString("hex"),
      witnessCompact: encodeMidgardNativeTxWitnessSetCompactV1(
        deriveMidgardNativeTxWitnessSetCompactV1(tx.witnessSet),
      ).toString("hex"),
      compact: encodeMidgardNativeTxCompactV1(tx.compact).toString("hex"),
      canonical: encodeMidgardNativeTxCanonicalV1(tx).toString("hex"),
      transactionId: computeMidgardNativeTxIdV1(tx).toString("hex"),
      fullHash: computeMidgardNativeTxFullHashV1(tx).toString("hex"),
      proofCompact: source.compactCbor.toString("hex"),
      proofWitnessCompact: source.witnessSetCompactCbor.toString("hex"),
      proofLengths: source.fieldPreimageLengthsCbor.toString("hex"),
      proofSource: encodeMidgardNativeTxProofSourceV1(source).toString("hex"),
      proofCommitment:
        computeMidgardNativeTxProofCommitmentV1(source).toString("hex"),
      canonicalSize:
        computeMidgardNativeTxCanonicalSizeFromProofSourceV1(source),
      orderedLengthTuple: encodeMidgardNativeTxProofFieldLengthsV1([
        1, 2, 3, 4, 5, 6, 7, 8, 9,
      ]).toString("hex"),
      derivedOrderedLengthTuple:
        deriveMidgardNativeTxProofSourceV1(
          orderedLengthTx,
        ).fieldPreimageLengthsCbor.toString("hex"),
    };

    expect(exact).toEqual({
      bodyCanonical:
        "8c41804180418017201818418041804180582011111111111111111111111111111111111111111111111111111111111111115820222222222222222222222222222222222222222222222222222222222222222201",
      bodyCompact:
        "8c5820eb25ed4ae02426602eee44b29d93e9dcd0be514b2087eda02f398b16fbb0ec765820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f58203e8a3792d0d05e816cc08e97bdbd1f0e4375a9ef83f196c8c51705ec6f463e20172018185820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582011111111111111111111111111111111111111111111111111111111111111115820222222222222222222222222222222222222222222222222222222222222222201",
      witnessPreimages: "83418041804180",
      witnessCompact:
        "835820a8c162c7cb77ea61f1195a083ecb8a64b97410652666d380b0e52f90eb74bb9e5820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3",
      compact:
        "84018c5820eb25ed4ae02426602eee44b29d93e9dcd0be514b2087eda02f398b16fbb0ec765820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f58203e8a3792d0d05e816cc08e97bdbd1f0e4375a9ef83f196c8c51705ec6f463e20172018185820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d58201111111111111111111111111111111111111111111111111111111111111111582022222222222222222222222222222222222222222222222222222222222222220158206ab07ab512fb3a2860ddc7f57ca940073e9300c01e62c9b51cf2de2a0d7d964303",
      canonical:
        "84018c418041804180172018184180418041805820111111111111111111111111111111111111111111111111111111111111111158202222222222222222222222222222222222222222222222222222222222222222018341804180418003",
      transactionId:
        "7d098ce4ee22ae378974ca4560877d3ae850c90ff3b36b5ddcc4675dbb77dedc",
      fullHash:
        "aee3bfa02625d824be4c2b786d5e6f4d159a1903ef282ab3434634e4da2735e1",
      proofCompact:
        "84018c5820eb25ed4ae02426602eee44b29d93e9dcd0be514b2087eda02f398b16fbb0ec765820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f58203e8a3792d0d05e816cc08e97bdbd1f0e4375a9ef83f196c8c51705ec6f463e20172018185820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d58201111111111111111111111111111111111111111111111111111111111111111582022222222222222222222222222222222222222222222222222222222222222220158206ab07ab512fb3a2860ddc7f57ca940073e9300c01e62c9b51cf2de2a0d7d964303",
      proofWitnessCompact:
        "835820a8c162c7cb77ea61f1195a083ecb8a64b97410652666d380b0e52f90eb74bb9e5820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3",
      proofLengths: "89010101010101010101",
      proofSource:
        "8359013b84018c5820eb25ed4ae02426602eee44b29d93e9dcd0be514b2087eda02f398b16fbb0ec765820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f58203e8a3792d0d05e816cc08e97bdbd1f0e4375a9ef83f196c8c51705ec6f463e20172018185820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d58201111111111111111111111111111111111111111111111111111111111111111582022222222222222222222222222222222222222222222222222222222222222220158206ab07ab512fb3a2860ddc7f57ca940073e9300c01e62c9b51cf2de2a0d7d9643035867835820a8c162c7cb77ea61f1195a083ecb8a64b97410652666d380b0e52f90eb74bb9e5820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef34a89010101010101010101",
      proofCommitment:
        "b5abe2283df143ce1d2da68a8c439ebf826af7d43324216cec3e1068f824a8a2",
      canonicalSize: 96,
      orderedLengthTuple: "89010203040506070809",
      derivedOrderedLengthTuple: "8901020304051822070608",
    });

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

    expect(() =>
      encodeMidgardNativeTxCanonicalV1({
        ...makeGoldenCanonical(),
        body: {
          ...makeGoldenCanonical().body,
          spendInputsPreimageCbor: encodeCbor([0n]),
        },
      }),
    ).toThrow(/must be a CBOR byte string/u);
    const malformedNestedPreimage = Buffer.from(exact.canonical, "hex");
    malformedNestedPreimage[4] = 0x9f;
    expect(() =>
      decodeMidgardNativeTxCanonicalV1(malformedNestedPreimage),
    ).toThrow(/Indefinite-length CBOR is not valid/u);

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
