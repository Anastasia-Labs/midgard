import {
  computeMidgardNativeTxIdV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyOutputReferenceScriptDecodingFindingV1,
  OUTPUT_REFERENCE_SCRIPT_DECODING_ID_V1,
  OUTPUT_REFERENCE_SCRIPT_DEPTH_LIMIT_VIOLATION_ID_V1,
  OUTPUT_REFERENCE_SCRIPT_MALFORMED_VIOLATION_ID_V1,
  OUTPUT_REFERENCE_SCRIPT_NODE_LIMIT_VIOLATION_ID_V1,
  outputReferenceScriptDecodingViolationIdV1,
  OutputReferenceScriptResultClassesV1,
  prepareOutputReferenceScriptDecodingEvidenceV1,
} from "../src/output-reference-script-decoding/index.js";
import { makeNativeTx } from "./support/submit-init-emulator-shared.js";

const outputWithScript = (kind: "valid" | "malformed") => {
  const encoded = encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 1)]),
    value: { lovelace: 2_000_000n, assets: new Map() },
    script_ref:
      kind === "valid"
        ? {
            language: "NativeCardano",
            scriptBytes: Buffer.alloc(0),
            nativeScript: { type: "sig", keyHash: Buffer.alloc(28, 2) },
          }
        : { language: "PlutusV3", scriptBytes: Buffer.from("820700", "hex") },
  });
  if (kind === "valid") return encoded;
  const marker = Buffer.from("820343820700", "hex");
  const offset = encoded.indexOf(marker);
  if (offset < 0) throw new Error("reference-script fixture marker absent");
  const malformed = Buffer.from(encoded);
  malformed[offset + 1] = 0;
  return malformed;
};

const transaction = (kind: "valid" | "malformed") =>
  makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    outputCbor: outputWithScript(kind),
  });

describe("outputReferenceScriptDecoding V1", () => {
  it("freezes ID and binds all exact forced reasons", () => {
    expect(OUTPUT_REFERENCE_SCRIPT_DECODING_ID_V1).toBe("0000002a");
    for (const constructor of [
      "OutputReferenceScriptMalformed",
      "OutputReferenceScriptNodeLimit",
      "OutputReferenceScriptDepthLimit",
    ] as const) {
      const subject = forcedVerdictSubjectV1({
        transactionId: "11".repeat(32),
        sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
        rejectionReason: { [constructor]: { output_index: 3n } } as never,
      });
      expect(() =>
        classifyOutputReferenceScriptDecodingFindingV1({
          subject,
          outputIndex: 3,
        }),
      ).not.toThrow();
      expect(() =>
        classifyOutputReferenceScriptDecodingFindingV1({
          subject,
          outputIndex: 2,
        }),
      ).toThrow(/coordinate differs/u);
    }
  });

  it("routes each terminal arm to its exact detector-owned identity", () => {
    expect(
      outputReferenceScriptDecodingViolationIdV1(
        OutputReferenceScriptResultClassesV1.Malformed,
      ),
    ).toBe(OUTPUT_REFERENCE_SCRIPT_MALFORMED_VIOLATION_ID_V1);
    expect(
      outputReferenceScriptDecodingViolationIdV1(
        OutputReferenceScriptResultClassesV1.NodeLimit,
      ),
    ).toBe(OUTPUT_REFERENCE_SCRIPT_NODE_LIMIT_VIOLATION_ID_V1);
    expect(
      outputReferenceScriptDecodingViolationIdV1(
        OutputReferenceScriptResultClassesV1.DepthLimit,
      ),
    ).toBe(OUTPUT_REFERENCE_SCRIPT_DEPTH_LIMIT_VIOLATION_ID_V1);
    expect(() =>
      outputReferenceScriptDecodingViolationIdV1(
        OutputReferenceScriptResultClassesV1.NoFault,
      ),
    ).toThrow(/no output-reference violation id/u);
  });

  it("detects malformed accepted reference script and refuses subject substitution", () => {
    const tx = transaction("malformed");
    const bytes = encodeMidgardNativeTxCanonicalV1(tx);
    const id = computeMidgardNativeTxIdV1(tx).toString("hex");
    const evidence = prepareOutputReferenceScriptDecodingEvidenceV1({
      subject: acceptedVerdictSubjectV1(id),
      outputIndex: 0,
      canonicalTransactionCbor: bytes,
    });
    expect(evidence.resultClass).toBe(
      OutputReferenceScriptResultClassesV1.Malformed,
    );
    expect(evidence.referenceScriptItemCommitmentHex).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidenceV1({
        subject: acceptedVerdictSubjectV1("ff".repeat(32)),
        outputIndex: 0,
        canonicalTransactionCbor: bytes,
      }),
    ).toThrow(/identity was substituted/u);
  });

  it("proves decodable wrongful rejection and refuses its honest polarity", () => {
    const tx = transaction("valid");
    const bytes = encodeMidgardNativeTxCanonicalV1(tx);
    const id = computeMidgardNativeTxIdV1(tx).toString("hex");
    const forced = (
      reason:
        | "OutputReferenceScriptMalformed"
        | "OutputReferenceScriptNodeLimit",
    ) =>
      forcedVerdictSubjectV1({
        transactionId: id,
        sourceKey: { transactionId: "33".repeat(32), outputIndex: 0n },
        rejectionReason: { [reason]: { output_index: 0n } } as never,
      });
    const evidence = prepareOutputReferenceScriptDecodingEvidenceV1({
      subject: forced("OutputReferenceScriptMalformed"),
      outputIndex: 0,
      canonicalTransactionCbor: bytes,
    });
    expect(evidence.resultClass).toBe(
      OutputReferenceScriptResultClassesV1.NoFault,
    );
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidenceV1({
        subject: acceptedVerdictSubjectV1(id),
        outputIndex: 0,
        canonicalTransactionCbor: bytes,
      }),
    ).toThrow(/agrees with operator verdict/u);
  });

  it("refuses output coordinate and non-family reason substitutions", () => {
    const tx = transaction("malformed");
    const bytes = encodeMidgardNativeTxCanonicalV1(tx);
    const id = computeMidgardNativeTxIdV1(tx).toString("hex");
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidenceV1({
        subject: acceptedVerdictSubjectV1(id),
        outputIndex: 1,
        canonicalTransactionCbor: bytes,
      }),
    ).toThrow(/out of range/u);
    expect(() =>
      classifyOutputReferenceScriptDecodingFindingV1({
        subject: forcedVerdictSubjectV1({
          transactionId: id,
          sourceKey: { transactionId: "44".repeat(32), outputIndex: 0n },
          rejectionReason: {
            ProtectedOutputSignerMissing: { output_index: 0n },
          },
        }),
        outputIndex: 0,
      }),
    ).toThrow(/wrong typed reason/u);
  });
});
