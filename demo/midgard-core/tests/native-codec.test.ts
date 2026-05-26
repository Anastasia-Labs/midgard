import { describe, expect, it } from "vitest";
import {
  decodeMidgardNativeTxFull,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxFull,
  materializeMidgardNativeTxFromCanonical,
  computeHash32,
  computeMidgardNativeTxId,
  EMPTY_PREIMAGE_LIST,
  EMPTY_CBOR_NULL,
  EMPTY_NULL_ROOT,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  verifyMidgardNativeScript,
  type MidgardNativeTxCanonical,
  type MidgardNativeTxFull,
} from "../src/index.js";

const makeCanonical = (): MidgardNativeTxCanonical => ({
  version: MIDGARD_NATIVE_TX_VERSION,
  validity: "TxIsValid",
  body: {
    spendInputsPreimage: EMPTY_PREIMAGE_LIST,
    referenceInputsPreimage: EMPTY_PREIMAGE_LIST,
    outputsPreimage: EMPTY_PREIMAGE_LIST,
    fee: 0n,
    validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
    validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
    requiredObserversPreimage: EMPTY_PREIMAGE_LIST,
    requiredSignersPreimage: EMPTY_PREIMAGE_LIST,
    mintPreimage: EMPTY_PREIMAGE_LIST,
    scriptIntegrityHash: EMPTY_NULL_ROOT,
    auxiliaryDataHash: EMPTY_NULL_ROOT,
    networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
  },
  witnessSet: {
    addrTxWitsPreimage: EMPTY_PREIMAGE_LIST,
    scriptTxWitsPreimage: EMPTY_PREIMAGE_LIST,
    redeemerTxWitsPreimage: EMPTY_PREIMAGE_LIST,
  },
});

describe("Midgard native v1 codec", () => {
  it("round trips a canonical full transaction", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());
    const encoded = encodeMidgardNativeTxFull(tx);
    const decoded = decodeMidgardNativeTxFull(encoded);

    expect(decoded.version).toBe(MIDGARD_NATIVE_TX_VERSION);
    expect(decoded.compact.transactionBody).toEqual(tx.compact.transactionBody);
    expect(decoded.compact.transactionWitnessSetHash).toEqual(
      tx.compact.transactionWitnessSetHash,
    );
    expect(decoded.compact.transactionBody.mintHash).toEqual(
      computeHash32(EMPTY_PREIMAGE_LIST),
    );
  });

  it("uses the compact body hash as the transaction id", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());

    expect(computeMidgardNativeTxId(tx)).toEqual(
      computeHash32(
        encodeMidgardNativeTxBodyCompact(tx.compact.transactionBody),
      ),
    );
  });

  it("rejects a truncated transaction envelope", () => {
    const encoded = encodeMidgardNativeTxFull(
      materializeMidgardNativeTxFromCanonical(makeCanonical()),
    );

    expect(() =>
      decodeMidgardNativeTxFull(encoded.subarray(0, encoded.length - 8)),
    ).toThrow(/Unexpected end of binary input/);
  });

  it("rejects derived compact body drift", () => {
    const tx = materializeMidgardNativeTxFromCanonical(makeCanonical());
    const tampered: MidgardNativeTxFull = {
      ...tx,
      compact: {
        ...tx.compact,
        transactionBody: {
          ...tx.compact.transactionBody,
          outputsHash: Buffer.alloc(32, 1),
        },
      },
    };

    expect(() => encodeMidgardNativeTxFull(tampered)).toThrow(
      /transaction_compact\.transaction_body must match the derived compact body/,
    );
  });

  it("rejects trailing bytes after the transaction envelope", () => {
    const encoded = encodeMidgardNativeTxFull(
      materializeMidgardNativeTxFromCanonical(makeCanonical()),
    );

    expect(() =>
      decodeMidgardNativeTxFull(Buffer.concat([encoded, EMPTY_CBOR_NULL])),
    ).toThrow(/transaction has trailing bytes/);
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
