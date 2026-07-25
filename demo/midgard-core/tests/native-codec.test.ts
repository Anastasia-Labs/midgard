import { describe, expect, it } from "vitest";

import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxCanonicalV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_CBOR_NULL,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxBodyCompactV1,
  encodeMidgardNativeTxCanonicalV1,
  deriveMidgardNativeFieldCollectionV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonicalV1,
  verifyMidgardNativeScript,
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
