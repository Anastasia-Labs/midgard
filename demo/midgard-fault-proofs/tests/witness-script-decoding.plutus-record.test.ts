import {
  encodeMidgardFieldPreimage,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyWitnessScriptDecodingFinding,
  prepareWitnessScriptDecodingEvidence,
  witnessScriptDecodingEvidenceCloses,
  WitnessScriptDecodingResultClasses,
  witnessScriptDecodingViolationId,
} from "../src/witness-script-decoding/index.js";

const txId = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";
const witnessSetHash = "11".repeat(32);
const accepted = acceptedVerdictSubject(txId);

// PlutusV3 `[3, bytes(script)]` items. The first script is the flat program
// 1.1.0 `(con unit ())`, which decodes; the second is not a CBOR byte string,
// so its decode record refuses it with class 1.
const plutusItem = Buffer.from("820346450101004981", "hex");
const undecodablePlutusItem = Buffer.from("82034401020304", "hex");
const signatureItem = Buffer.concat([
  Buffer.from("82005820", "hex"),
  Buffer.from("8200581c", "hex"),
  Buffer.alloc(28, 0x99),
]);

const undecodable = (itemKind: bigint, itemIndex: bigint, cls: bigint) =>
  forcedVerdictSubject({
    transactionId: txId,
    sourceKey: { transactionId: "33".repeat(32), outputIndex: 0n },
    rejectionReason: {
      PlutusScriptUndecodable: {
        item_kind: itemKind,
        item_index: itemIndex,
        class: cls,
      },
    } as never,
  });

const evidence = (subject: typeof accepted, item: Buffer) => {
  const preimage = encodeMidgardFieldPreimage([item]);
  return prepareWitnessScriptDecodingEvidence({
    finding: { subject, witnessSetHash, scriptIndex: 0 },
    fieldPreimage: preimage,
    committedFieldHashHex: midgardFieldCommitment(preimage).toString("hex"),
  });
};

describe("witnessScriptDecoding Plutus items", () => {
  it("closes a Plutus item by the class its decode record ends in", () => {
    const decodable = evidence(accepted, plutusItem);
    expect(decodable.resultClass).toBe(
      WitnessScriptDecodingResultClasses.NoFault,
    );
    expect(decodable.initialResultClass).toBe(
      WitnessScriptDecodingResultClasses.NoFault,
    );
    expect(witnessScriptDecodingEvidenceCloses(decodable)).toBe(false);
    const refused = evidence(accepted, undecodablePlutusItem);
    expect(refused.resultClass).toBe(101);
    expect(refused.initialResultClass).toBe(101);
    expect(witnessScriptDecodingEvidenceCloses(refused)).toBe(true);
    expect(witnessScriptDecodingViolationId(101)).toBe(
      "witness-plutus-script-undecodable",
    );
  });

  it("binds the Plutus undecodable reason to its item, kind, and class", () => {
    const honest = evidence(undecodable(0n, 0n, 1n), undecodablePlutusItem);
    expect(honest.finding.accusedClass).toBe(101);
    expect(witnessScriptDecodingEvidenceCloses(honest)).toBe(false);
    for (const [subject, item] of [
      [undecodable(0n, 0n, 3n), undecodablePlutusItem],
      [undecodable(0n, 0n, 1n), plutusItem],
      [undecodable(0n, 0n, 1n), signatureItem],
    ] as const) {
      expect(witnessScriptDecodingEvidenceCloses(evidence(subject, item))).toBe(
        true,
      );
    }
    for (const subject of [
      undecodable(1n, 0n, 1n),
      undecodable(0n, 1n, 1n),
      undecodable(0n, 0n, 0n),
    ]) {
      expect(() =>
        classifyWitnessScriptDecodingFinding({
          subject,
          witnessSetHash,
          scriptIndex: 0,
        }),
      ).toThrow(/coordinate differs/u);
    }
  });
});
