/**
 * Validity bounds, `docs/spec/midgard-tx.md` §2.1: a bound is `-1` (absent) or
 * a nonnegative slot, and anything below `-1` is not a decodable body. The
 * TypeScript twin of the validity-bound tests in
 * `onchain/aiken/lib/midgard/fraud-proofs/native-tx-v1.test.ak`.
 */
import { describe, expect, it } from "vitest";

import {
  decodeMidgardNativeTxBodyCanonical,
  decodeMidgardNativeTxBodyCompact,
  decodeMidgardNativeTxCanonical,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxBodyCanonical,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
} from "../src/index.js";

const makeCanonical = (): MidgardNativeTxCanonical => ({
  version: MIDGARD_NATIVE_TX_VERSION,
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

describe("Midgard native v1 validity bounds", () => {
  it("decodes and encodes validity bounds only as -1 or a nonnegative slot", () => {
    const canonical = makeCanonical().body;
    const compact =
      materializeMidgardNativeTxFromCanonical(makeCanonical()).compact
        .transactionBody;
    const largeSlot = 18_446_744_073_709_551_615n;

    // Both bodies spell the honest `-1, -1` bounds as `20 20`, right after the
    // three leading fields and the fee.
    const withRawBounds = (
      honest: Buffer,
      offset: number,
      start: bigint,
      end: bigint,
    ): Buffer =>
      Buffer.concat([
        honest.subarray(0, offset),
        encodeCbor(start),
        encodeCbor(end),
        honest.subarray(offset + 2),
      ]);
    const compactHonest = encodeMidgardNativeTxBodyCompact(compact);
    const compactOffset = 1 + 3 * 34 + encodeCbor(compact.fee).length;
    const canonicalHonest = encodeMidgardNativeTxBodyCanonical(canonical);
    const canonicalOffset =
      1 +
      canonical.spendInputsPreimageCbor.length +
      1 +
      canonical.referenceInputsPreimageCbor.length +
      1 +
      canonical.outputsPreimageCbor.length +
      1 +
      encodeCbor(canonical.fee).length;
    const txHonest = encodeMidgardNativeTxCanonical(makeCanonical());
    expect(compactHonest.subarray(compactOffset, compactOffset + 2)).toEqual(
      Buffer.from("2020", "hex"),
    );
    expect(
      canonicalHonest.subarray(canonicalOffset, canonicalOffset + 2),
    ).toEqual(Buffer.from("2020", "hex"));
    expect(txHonest.subarray(2 + canonicalOffset, 4 + canonicalOffset)).toEqual(
      Buffer.from("2020", "hex"),
    );

    for (const [start, end] of [
      [MIDGARD_POSIX_TIME_NONE, MIDGARD_POSIX_TIME_NONE],
      [0n, MIDGARD_POSIX_TIME_NONE],
      [MIDGARD_POSIX_TIME_NONE, 0n],
      [0n, largeSlot],
      [largeSlot, MIDGARD_POSIX_TIME_NONE],
    ] as const) {
      const compactBytes = encodeMidgardNativeTxBodyCompact({
        ...compact,
        validityIntervalStart: start,
        validityIntervalEnd: end,
      });
      expect(compactBytes).toEqual(
        withRawBounds(compactHonest, compactOffset, start, end),
      );
      const decodedCompact = decodeMidgardNativeTxBodyCompact(compactBytes);
      expect(decodedCompact.validityIntervalStart).toBe(start);
      expect(decodedCompact.validityIntervalEnd).toBe(end);

      const canonicalBytes = encodeMidgardNativeTxBodyCanonical({
        ...canonical,
        validityIntervalStart: start,
        validityIntervalEnd: end,
      });
      expect(canonicalBytes).toEqual(
        withRawBounds(canonicalHonest, canonicalOffset, start, end),
      );
      const decodedCanonical =
        decodeMidgardNativeTxBodyCanonical(canonicalBytes);
      expect(decodedCanonical.validityIntervalStart).toBe(start);
      expect(decodedCanonical.validityIntervalEnd).toBe(end);
    }

    for (const [start, end] of [
      [-2n, MIDGARD_POSIX_TIME_NONE],
      [-5n, MIDGARD_POSIX_TIME_NONE],
      [MIDGARD_POSIX_TIME_NONE, -2n],
      [0n, -5n],
    ] as const) {
      const badField = start < MIDGARD_POSIX_TIME_NONE ? 4 : 5;
      const badName =
        badField === 4 ? "validity_interval_start" : "validity_interval_end";
      const refused = /must be -1 or a nonnegative slot/u;

      expect(() =>
        encodeMidgardNativeTxBodyCompact({
          ...compact,
          validityIntervalStart: start,
          validityIntervalEnd: end,
        }),
      ).toThrow(new RegExp(`transaction_body_compact\\.${badName}`, "u"));
      expect(() =>
        encodeMidgardNativeTxBodyCompact({
          ...compact,
          validityIntervalStart: start,
          validityIntervalEnd: end,
        }),
      ).toThrow(refused);
      expect(() =>
        encodeMidgardNativeTxBodyCanonical({
          ...canonical,
          validityIntervalStart: start,
          validityIntervalEnd: end,
        }),
      ).toThrow(refused);
      expect(() =>
        decodeMidgardNativeTxBodyCompact(
          withRawBounds(compactHonest, compactOffset, start, end),
        ),
      ).toThrow(new RegExp(`\\[${badField.toString()}\\] must be -1`, "u"));
      expect(() =>
        decodeMidgardNativeTxBodyCanonical(
          withRawBounds(canonicalHonest, canonicalOffset, start, end),
        ),
      ).toThrow(refused);
      expect(() =>
        decodeMidgardNativeTxCanonical(
          withRawBounds(txHonest, 2 + canonicalOffset, start, end),
        ),
      ).toThrow(refused);
    }
  });
});
