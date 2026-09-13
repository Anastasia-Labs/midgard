import { readFileSync } from "node:fs";

import {
  computeHash32,
  computeMidgardNativeTxId,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core";
import {
  computeMidgardForcedTxProofCommitment,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
  submittedForcedTransactionFromNative,
  verifyMidgardForcedTxProofSource,
} from "@al-ft/midgard-core/codec/forced";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import * as SDK from "../src/index.js";

const vector = JSON.parse(
  readFileSync(
    new URL(
      "../../midgard-core/tests/fixtures/forced-submission-vector.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as {
  canonical: string;
  compact: string;
  witness: string;
  lengths: string;
  transactionId: string;
  commitment: string;
};
const submitted = decodeMidgardForcedTxFullFromCanonicalCbor(
  Buffer.from(vector.canonical, "hex"),
);
const source = deriveMidgardForcedTxProofSource(submitted);
const wireSource: SDK.ForcedTxProofSource = {
  compact_cbor: vector.compact,
  witness_set_compact_cbor: vector.witness,
  field_preimage_lengths_cbor: vector.lengths,
};
const payload: SDK.TxOrderPayload = {
  tx_id: vector.transactionId,
  transaction_commitment: vector.commitment,
  submitted_source: wireSource,
};
const native = materializeMidgardNativeTxFromCanonical({
  ...submitted,
  validity: "TxIsValid",
});

describe("immutable forced submission SDK boundary", () => {
  it("constructs a submission explicitly and preserves its body identity", () => {
    const projected = submittedForcedTransactionFromNative(native);
    expect(encodeMidgardForcedTxCanonical(projected).toString("hex")).toBe(
      vector.canonical,
    );
    expect(projected).not.toHaveProperty("validity");
    expect(projected.compact).not.toHaveProperty("validity");
    expect(computeMidgardNativeTxId(projected)).toEqual(
      computeMidgardNativeTxId(native),
    );
  });

  it("refuses a native invalid admission claim at client construction", () => {
    const invalid = materializeMidgardNativeTxFromCanonical({
      ...native,
      validity: "TxIsInvalid",
    });
    expect(() => submittedForcedTransactionFromNative(invalid)).toThrow(
      /admission-valid/,
    );
  });

  it("derives order material matching the independent cross-language vector", () => {
    const admissible = materializeMidgardForcedTxFromCanonical({
      ...submitted,
      body: {
        ...submitted.body,
        auxiliaryDataHash: EMPTY_NULL_ROOT,
        scriptIntegrityHash: EMPTY_NULL_ROOT,
      },
    });
    const material = SDK.deriveTxOrderMaterial({
      submittedTxCbor: encodeMidgardForcedTxCanonical(admissible),
      owner: Buffer.alloc(28, 0x44),
    });
    expect(computeMidgardNativeTxId(submitted).toString("hex")).toBe(
      vector.transactionId,
    );
    expect(material.transactionId).toBe(
      computeMidgardNativeTxId(admissible).toString("hex"),
    );
    expect(computeMidgardForcedTxProofCommitment(source).toString("hex")).toBe(
      vector.commitment,
    );
    expect(material.transactionCommitment).toBe(
      computeMidgardForcedTxProofCommitment(
        deriveMidgardForcedTxProofSource(admissible),
      ).toString("hex"),
    );
    expect(material.submitted_source.compact_cbor).toBe(
      deriveMidgardForcedTxProofSource(admissible).compactCbor.toString("hex"),
    );
    expect(
      Data.from(Data.to(payload, SDK.TxOrderPayload), SDK.TxOrderPayload),
    ).toEqual(payload);
  });

  it("refuses the obsolete four-element submitted encoding and unknown outer constructors", () => {
    expect(() =>
      SDK.deriveTxOrderMaterial({
        submittedTxCbor: encodeMidgardNativeTxCanonical(native),
        owner: Buffer.alloc(28, 0x44),
      }),
    ).toThrow();
    const encoded = Data.from(Data.to(payload, SDK.TxOrderPayload));
    if (!(encoded instanceof Constr))
      throw new Error("expected payload constructor");
    expect(() =>
      Data.from(Data.to(new Constr(1, encoded.fields)), SDK.TxOrderPayload),
    ).toThrow();
  });

  it("commits each verdict and reason while preserving the exact submission", () => {
    const verdicts: SDK.OperatorVerdict[] = [
      "ForcedTxValid",
      { ForcedTxInvalid: { reason: "EmptyInputs" } },
      { ForcedTxInvalid: { reason: "FeeBelowMinimum" } },
    ];
    const hashes = verdicts.map((verdict) => {
      const leaf: SDK.ForcedInclusionTxV1 = {
        tx_id: payload.tx_id,
        submitted_source: payload.submitted_source,
        verdict,
      };
      const decoded = Data.from(
        Data.to(leaf, SDK.ForcedInclusionTxV1),
        SDK.ForcedInclusionTxV1,
      );
      expect(decoded.submitted_source).toEqual(payload.submitted_source);
      expect(decoded.tx_id).toBe(vector.transactionId);
      expect(
        computeMidgardForcedTxProofCommitment(source).toString("hex"),
      ).toBe(payload.transaction_commitment);
      return computeHash32(
        Buffer.from(Data.to(leaf, SDK.ForcedInclusionTxV1), "hex"),
      ).toString("hex");
    });
    expect(new Set(hashes).size).toBe(verdicts.length);
  });

  it("refuses witness substitution against the original submitted compact", () => {
    const changed = Buffer.from(source.witnessSetCompactCbor);
    changed[changed.length - 1] = changed[changed.length - 1]! ^ 1;
    expect(() =>
      verifyMidgardForcedTxProofSource({
        transactionId: Buffer.from(vector.transactionId, "hex"),
        source: { ...source, witnessSetCompactCbor: changed },
      }),
    ).toThrow();
    expect(
      computeMidgardForcedTxProofCommitment({
        ...source,
        witnessSetCompactCbor: changed,
      }),
    ).not.toEqual(computeMidgardForcedTxProofCommitment(source));
  });
});
