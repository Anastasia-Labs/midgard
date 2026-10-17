import {
  computeMidgardNativeTxId,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  buildOutputReferenceScriptDecodingFaultProofContracts,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  applyOutputReferenceScriptDecodingScripts,
  OUTPUT_REFERENCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
} from "../src/output-reference-script-decoding/contracts.js";
import {
  classifyOutputReferenceScriptDecodingFinding,
  OUTPUT_REFERENCE_SCRIPT_DECODING_ID,
  OUTPUT_REFERENCE_SCRIPT_DEPTH_LIMIT_VIOLATION_ID,
  OUTPUT_REFERENCE_SCRIPT_MALFORMED_VIOLATION_ID,
  OUTPUT_REFERENCE_SCRIPT_NODE_LIMIT_VIOLATION_ID,
  outputReferenceScriptDecodingViolationId,
  outputReferenceScriptEvidenceCloses,
  OutputReferenceScriptResultClasses,
  prepareOutputReferenceScriptDecodingEvidence,
} from "../src/output-reference-script-decoding/index.js";
import { buildRegisteredChainFixture } from "./support/emulator/registered-chain.js";
import {
  OUTPUT_REFERENCE_MAX_OUTPUT_BYTES,
  outputWithRawNativePayload,
  rawNativeOutputOfLength,
  subjectTransaction,
} from "./support/output-reference-script-decoding-emulator.js";
import { makeNativeTx } from "./support/submit-init-emulator-shared.js";

const subjectTransactionWith = (output: Buffer) =>
  subjectTransaction([output], 7n);

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
    expect(OUTPUT_REFERENCE_SCRIPT_DECODING_ID).toBe("0000002a");
    for (const constructor of [
      "OutputReferenceScriptMalformed",
      "OutputReferenceScriptNodeLimit",
      "OutputReferenceScriptDepthLimit",
    ] as const) {
      const subject = forcedVerdictSubject({
        transactionId: "11".repeat(32),
        sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
        rejectionReason: { [constructor]: { output_index: 3n } } as never,
      });
      expect(() =>
        classifyOutputReferenceScriptDecodingFinding({
          subject,
          outputIndex: 3,
        }),
      ).not.toThrow();
      expect(() =>
        classifyOutputReferenceScriptDecodingFinding({
          subject,
          outputIndex: 2,
        }),
      ).toThrow(/coordinate differs/u);
    }
  });

  it("routes each terminal arm to its exact detector-owned identity", () => {
    expect(
      outputReferenceScriptDecodingViolationId(
        OutputReferenceScriptResultClasses.Malformed,
      ),
    ).toBe(OUTPUT_REFERENCE_SCRIPT_MALFORMED_VIOLATION_ID);
    expect(
      outputReferenceScriptDecodingViolationId(
        OutputReferenceScriptResultClasses.NodeLimit,
      ),
    ).toBe(OUTPUT_REFERENCE_SCRIPT_NODE_LIMIT_VIOLATION_ID);
    expect(
      outputReferenceScriptDecodingViolationId(
        OutputReferenceScriptResultClasses.DepthLimit,
      ),
    ).toBe(OUTPUT_REFERENCE_SCRIPT_DEPTH_LIMIT_VIOLATION_ID);
    expect(() =>
      outputReferenceScriptDecodingViolationId(
        OutputReferenceScriptResultClasses.NoFault,
      ),
    ).toThrow(/no output-reference violation id/u);
  });

  it("detects malformed accepted reference script and refuses subject substitution", () => {
    const tx = transaction("malformed");
    const bytes = encodeMidgardNativeTxCanonical(tx);
    const id = computeMidgardNativeTxId(tx).toString("hex");
    const evidence = prepareOutputReferenceScriptDecodingEvidence({
      subject: acceptedVerdictSubject(id),
      outputIndex: 0,
      canonicalTransactionCbor: bytes,
    });
    expect(evidence.resultClass).toBe(
      OutputReferenceScriptResultClasses.Malformed,
    );
    expect(evidence.referenceScriptItemCommitmentHex).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidence({
        subject: acceptedVerdictSubject("ff".repeat(32)),
        outputIndex: 0,
        canonicalTransactionCbor: bytes,
      }),
    ).toThrow(/identity was substituted/u);
  });

  it("proves decodable wrongful rejection and refuses its honest polarity", () => {
    const tx = transaction("valid");
    const bytes = encodeMidgardNativeTxCanonical(tx);
    const id = computeMidgardNativeTxId(tx).toString("hex");
    const forced = (
      reason:
        | "OutputReferenceScriptMalformed"
        | "OutputReferenceScriptNodeLimit",
    ) =>
      forcedVerdictSubject({
        transactionId: id,
        sourceKey: { transactionId: "33".repeat(32), outputIndex: 0n },
        rejectionReason: { [reason]: { output_index: 0n } } as never,
      });
    const evidence = prepareOutputReferenceScriptDecodingEvidence({
      subject: forced("OutputReferenceScriptMalformed"),
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardForcedTxCanonical(tx),
    });
    expect(evidence.resultClass).toBe(
      OutputReferenceScriptResultClasses.NoFault,
    );
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidence({
        subject: acceptedVerdictSubject(id),
        outputIndex: 0,
        canonicalTransactionCbor: bytes,
      }),
    ).toThrow(/agrees with operator verdict/u);
  });

  it("refuses output coordinate and non-family reason substitutions", () => {
    const tx = transaction("malformed");
    const bytes = encodeMidgardNativeTxCanonical(tx);
    const id = computeMidgardNativeTxId(tx).toString("hex");
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidence({
        subject: acceptedVerdictSubject(id),
        outputIndex: 1,
        canonicalTransactionCbor: bytes,
      }),
    ).toThrow(/out of range/u);
    expect(() =>
      classifyOutputReferenceScriptDecodingFinding({
        subject: forcedVerdictSubject({
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

  it("classes a tag-0 empty payload malformed at the bind, as canonical validation does", () => {
    // `[0, h'']` is `LedgerOutputProofInvalidReferenceScript` in the
    // canonical output proof; the family closes it at the bind with no scan
    // control, in both directions.
    const tx = subjectTransactionWith(
      outputWithRawNativePayload(Buffer.alloc(0)),
    );
    const bytes = encodeMidgardNativeTxCanonical(tx);
    const id = computeMidgardNativeTxId(tx).toString("hex");
    const accepted = prepareOutputReferenceScriptDecodingEvidence({
      subject: acceptedVerdictSubject(id),
      outputIndex: 0,
      canonicalTransactionCbor: bytes,
    });
    expect(accepted.referenceScriptItemHex).toBe("820040");
    expect(accepted.resultClass).toBe(
      OutputReferenceScriptResultClasses.Malformed,
    );
    expect(accepted.initialControlCbor).toBe("");
    const wrongArm = prepareOutputReferenceScriptDecodingEvidence({
      subject: forcedVerdictSubject({
        transactionId: id,
        sourceKey: { transactionId: "55".repeat(32), outputIndex: 0n },
        rejectionReason: {
          OutputReferenceScriptNodeLimit: { output_index: 0n },
        },
      }),
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardForcedTxCanonical(tx),
    });
    expect(wrongArm.resultClass).toBe(
      OutputReferenceScriptResultClasses.Malformed,
    );
    expect(wrongArm.accusedClass).toBe(
      OutputReferenceScriptResultClasses.NodeLimit,
    );
  });

  it("admits the exact 16,384-byte output bound and refuses the adjacent byte", () => {
    const exact = subjectTransactionWith(
      rawNativeOutputOfLength(OUTPUT_REFERENCE_MAX_OUTPUT_BYTES),
    );
    const evidence = prepareOutputReferenceScriptDecodingEvidence({
      subject: acceptedVerdictSubject(
        computeMidgardNativeTxId(exact).toString("hex"),
      ),
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(exact),
    });
    expect(evidence.outputLength).toBe(OUTPUT_REFERENCE_MAX_OUTPUT_BYTES);
    expect(evidence.carriage).toBe("Certified");
    const over = subjectTransactionWith(
      rawNativeOutputOfLength(OUTPUT_REFERENCE_MAX_OUTPUT_BYTES + 1),
    );
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidence({
        subject: acceptedVerdictSubject(
          computeMidgardNativeTxId(over).toString("hex"),
        ),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(over),
      }),
    ).toThrow(/exceeds canonical size bound/u);
  });

  it("keeps an honest verdict's evidence only under the diagnostic flag, and the builders still refuse to close on it", () => {
    const tx = transaction("valid");
    const bytes = encodeMidgardNativeTxCanonical(tx);
    const id = computeMidgardNativeTxId(tx).toString("hex");
    const honest = prepareOutputReferenceScriptDecodingEvidence({
      subject: acceptedVerdictSubject(id),
      outputIndex: 0,
      canonicalTransactionCbor: bytes,
      admitHonestVerdict: true,
    });
    expect(honest.resultClass).toBe(OutputReferenceScriptResultClasses.NoFault);
    expect(outputReferenceScriptEvidenceCloses(honest)).toBe(false);
  });
});

describe("outputReferenceScriptDecoding registered-chain parity", () => {
  it("applies the same chain the SDK registers for the same shared policies", async () => {
    const fixture = await buildRegisteredChainFixture(
      buildOutputReferenceScriptDecodingFaultProofContracts,
    );
    const registered = fixture.contracts.outputReferenceScriptDecoding;
    const applied = applyOutputReferenceScriptDecodingScripts(
      fixture.applyParams,
    );
    expect(applied.map((step) => step.spendingScriptHash)).toStrictEqual(
      registered.steps.map((step) => step.spendingScriptHash),
    );
    expect(registered.firstStep.spendingScriptHash).toBe(
      applied[0].spendingScriptHash,
    );
    expect(applied.map((step) => step.blueprintTitle)).toStrictEqual([
      ...OUTPUT_REFERENCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
    ]);
  });
});
