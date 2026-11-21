import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import { encodeCbor } from "../src/codec/cbor.js";
import {
  computeMidgardForcedTxCanonicalSizeFromProofSource,
  computeMidgardForcedTxProofCommitment,
  decodeMidgardForcedTxCanonical,
  decodeMidgardForcedTxCompact,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxFaultEvidenceMaterial,
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  encodeMidgardForcedTxCompact,
  encodeMidgardForcedTxProofSource,
  submittedForcedTransactionFromNative,
  verifyMidgardForcedTxProofSource,
} from "../src/codec/forced.js";
import {
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "../src/codec/native.js";
const forced = JSON.parse(
  readFileSync(
    new URL("./fixtures/forced-submission-vector.json", import.meta.url),
    "utf8",
  ),
) as Record<string, string>;
const native = JSON.parse(
  readFileSync(
    new URL("./fixtures/native-tx-vector-v1.generated.json", import.meta.url),
    "utf8",
  ),
) as { vector: Record<string, string> };

const bytes = (hex: string) => Buffer.from(hex, "hex");
const tx = () =>
  decodeMidgardForcedTxFullFromCanonicalCbor(bytes(forced.canonical));
const source = () => deriveMidgardForcedTxProofSource(tx());

describe("immutable forced submission codec", () => {
  it("matches the independent full and compact bytes without a validity member", () => {
    expect(encodeMidgardForcedTxCanonical(tx()).toString("hex")).toBe(
      forced.canonical,
    );
    expect(encodeMidgardForcedTxCompact(tx().compact).toString("hex")).toBe(
      forced.compact,
    );
    expect(tx()).not.toHaveProperty("validity");
    expect(tx().compact).not.toHaveProperty("validity");
  });
  it("preserves the native body ID", () => {
    expect(computeMidgardNativeTxId(tx().compact).toString("hex")).toBe(
      forced.transactionId,
    );
  });
  it("matches the independently calculated commitment and source size", () => {
    expect(
      computeMidgardForcedTxProofCommitment(source()).toString("hex"),
    ).toBe(forced.commitment);
    expect(encodeMidgardForcedTxProofSource(source())).toHaveLength(434);
  });
  it("domain separates the same source bytes from a normal commitment", () => {
    expect(computeMidgardForcedTxProofCommitment(source())).not.toEqual(
      computeMidgardNativeTxProofCommitment(source()),
    );
  });
  it("refuses obsolete full and compact envelopes", () => {
    expect(() =>
      decodeMidgardForcedTxCanonical(bytes(native.vector.canonical)),
    ).toThrow();
    expect(() =>
      decodeMidgardForcedTxCompact(bytes(native.vector.compact)),
    ).toThrow();
    expect(() => decodeMidgardNativeTxCompact(bytes(forced.compact))).toThrow();
  });
  it("refuses unsupported and nonminimal versions and trailing bytes", () => {
    for (const hex of [
      "8302" + forced.canonical.slice(4),
      "831801" + forced.canonical.slice(4),
      forced.canonical + "00",
    ]) {
      expect(() => decodeMidgardForcedTxCanonical(bytes(hex))).toThrow();
    }
  });
  it("authenticates the witness commitment and body ID", () => {
    expect(
      verifyMidgardForcedTxProofSource({
        transactionId: bytes(forced.transactionId),
        source: source(),
      }),
    ).toEqual(tx().compact);
    expect(() =>
      verifyMidgardForcedTxProofSource({
        transactionId: Buffer.alloc(32),
        source: source(),
      }),
    ).toThrow();
    const witness = bytes(forced.witness);
    witness[witness.length - 1] ^= 1;
    expect(() =>
      verifyMidgardForcedTxProofSource({
        transactionId: bytes(forced.transactionId),
        source: { ...source(), witnessSetCompactCbor: witness },
      }),
    ).toThrow();
  });
  it("commits length declarations and rejects noncanonical declarations", () => {
    expect(
      computeMidgardForcedTxProofCommitment({
        ...source(),
        fieldPreimageLengthsCbor: encodeCbor([2, 1, 1, 1, 1, 1, 1, 1, 1]),
      }),
    ).not.toEqual(computeMidgardForcedTxProofCommitment(source()));
    expect(() =>
      verifyMidgardForcedTxProofSource({
        transactionId: bytes(forced.transactionId),
        source: {
          ...source(),
          fieldPreimageLengthsCbor: bytes("8918010101010101010101"),
        },
      }),
    ).toThrow();
  });
  it("keeps the ledger size charge while reducing transport by one byte", () => {
    expect(bytes(forced.canonical)).toHaveLength(95);
    expect(computeMidgardForcedTxCanonicalSizeFromProofSource(source())).toBe(
      96,
    );
  });
  it("preserves size at fee integer boundaries", () => {
    for (const fee of [23n, 24n, 255n, 256n, 65535n, 65536n]) {
      const canonical = { ...tx(), body: { ...tx().body, fee } };
      const { compact: _, ...material } = canonical;
      const encoded = encodeMidgardForcedTxCanonical(material);
      const derived = deriveMidgardForcedTxProofSource(
        decodeMidgardForcedTxFullFromCanonicalCbor(encoded),
      );
      expect(computeMidgardForcedTxCanonicalSizeFromProofSource(derived)).toBe(
        encoded.length + 1,
      );
    }
  });
  it("only projects a native admission-valid input at the client boundary", () => {
    const invalid = decodeMidgardNativeTxFullFromCanonicalCbor(
      bytes(native.vector.canonical),
    );
    expect(() => submittedForcedTransactionFromNative(invalid)).toThrow();
    const valid = decodeMidgardNativeTxFullFromCanonicalCbor(
      bytes(native.vector.canonical.slice(0, -2) + "00"),
    );
    expect(
      encodeMidgardForcedTxCanonical(
        submittedForcedTransactionFromNative(valid),
      ).toString("hex"),
    ).toBe(forced.canonical);
  });
  it("retains malformed inner fields as authenticated fault evidence", () => {
    const malformed = bytes(forced.canonical.replace("4180", "4181"));
    expect(() => decodeMidgardForcedTxCanonical(malformed)).toThrow();
    const evidence = deriveMidgardForcedTxFaultEvidenceMaterial(malformed);
    expect(evidence.fieldPreimages[0]).toEqual(bytes("81"));
    expect(
      verifyMidgardForcedTxProofSource({
        transactionId: evidence.transactionId,
        source: evidence.proofSource,
      }),
    ).toEqual(evidence.compact);
  });
  it("rejects a derived compact that no longer describes the material", () => {
    expect(() =>
      encodeMidgardForcedTxCanonical({
        ...tx(),
        body: { ...tx().body, fee: 100n },
      }),
    ).toThrow();
  });
});
