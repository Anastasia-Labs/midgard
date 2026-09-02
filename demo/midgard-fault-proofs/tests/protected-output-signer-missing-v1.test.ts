import { createPrivateKey, createPublicKey, sign } from "node:crypto";

import {
  computeMidgardNativeTxIdV1,
  encodeCbor,
  encodeMidgardAddressWitnessItemV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
  missingSignatureVkeyHashV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyProtectedOutputSignerMissingFindingV1,
  detectProtectedOutputSignerMissingCompleteReplayV1,
  prepareProtectedOutputSignerMissingEvidenceV1,
  PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES_V1,
  PROTECTED_OUTPUT_SIGNER_MISSING_ID_V1,
} from "../src/protected-output-signer-missing/index.js";
import { makeNativeTx } from "./support/submit-init-emulator-shared.js";

const txId = "11".repeat(32);
const seed = Buffer.alloc(32, 11);
const privateKey = createPrivateKey({
  key: Buffer.concat([
    Buffer.from("302e020100300506032b657004220420", "hex"),
    seed,
  ]),
  format: "der",
  type: "pkcs8",
});
const verificationKey = createPublicKey(privateKey)
  .export({ format: "der", type: "spki" })
  .subarray(-32);
const paymentCredential = missingSignatureVkeyHashV1(
  verificationKey.toString("hex"),
);
const protectedOutput = encodeMidgardTxOutput({
  address: Buffer.concat([
    Buffer.from([0x68]),
    Buffer.from(paymentCredential, "hex"),
  ]),
  value: { lovelace: 2_000_000n, assets: new Map() },
});

const transactionWithWitness = (kind: "empty" | "valid" | "invalid") => {
  const empty = makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    outputCbor: protectedOutput,
  });
  const id = computeMidgardNativeTxIdV1(empty);
  const signature =
    kind === "valid" ? sign(null, id, privateKey) : Buffer.alloc(64, 0xff);
  return makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    outputCbor: protectedOutput,
    addrTxWitsPreimageCbor:
      kind === "empty"
        ? encodeCbor([])
        : encodeCbor([
            encodeMidgardAddressWitnessItemV1({
              verificationKey,
              signature,
            }),
          ]),
  });
};

const transactionWithWitnessCount = (count: number) =>
  makeNativeTx({
    spendInputCbors: [],
    fee: 7n,
    outputCbor: protectedOutput,
    addrTxWitsPreimageCbor: encodeCbor(
      Array.from({ length: count }, (_unused, index) => {
        const key = Buffer.alloc(32);
        key.writeUInt32BE(index + 1, 28);
        return encodeMidgardAddressWitnessItemV1({
          verificationKey: key,
          signature: Buffer.alloc(64, 0xff),
        });
      }),
    ),
  });

describe("protectedOutputSignerMissing V1 binding", () => {
  it("freezes the plan identity and accepts the accepted coordinate", () => {
    expect(PROTECTED_OUTPUT_SIGNER_MISSING_ID_V1).toBe("0000002b");
    expect(() =>
      classifyProtectedOutputSignerMissingFindingV1({
        subject: acceptedVerdictSubjectV1(txId),
        outputIndex: 3,
      }),
    ).not.toThrow();
  });

  it("binds the exact forced reason and output coordinate", () => {
    const subject = forcedVerdictSubjectV1({
      transactionId: txId,
      sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
      rejectionReason: {
        ProtectedOutputSignerMissing: { output_index: 3n },
      },
    });
    expect(() =>
      classifyProtectedOutputSignerMissingFindingV1({
        subject,
        outputIndex: 3,
      }),
    ).not.toThrow();
    expect(() =>
      classifyProtectedOutputSignerMissingFindingV1({
        subject,
        outputIndex: 2,
      }),
    ).toThrow(/coordinate was substituted/u);
  });

  it("refuses another missing-witness reason", () => {
    const subject = forcedVerdictSubjectV1({
      transactionId: txId,
      sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
      rejectionReason: { RequiredSignerUnsigned: { signer_index: 3n } },
    });
    expect(() =>
      classifyProtectedOutputSignerMissingFindingV1({
        subject,
        outputIndex: 3,
      }),
    ).toThrow(/wrong typed rejection reason/u);
  });

  it("admits only Ed25519-valid witnesses to the signer frontier in both directions", () => {
    for (const kind of ["empty", "invalid"] as const) {
      const transaction = transactionWithWitness(kind);
      const id = computeMidgardNativeTxIdV1(transaction).toString("hex");
      const evidence = prepareProtectedOutputSignerMissingEvidenceV1({
        subject: acceptedVerdictSubjectV1(id),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(transaction),
      });
      expect(evidence.signerPresent).toBe(false);
      expect(evidence.validSignerHashes).toEqual([]);
    }

    const valid = transactionWithWitness("valid");
    const validId = computeMidgardNativeTxIdV1(valid).toString("hex");
    expect(() =>
      prepareProtectedOutputSignerMissingEvidenceV1({
        subject: acceptedVerdictSubjectV1(validId),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(valid),
      }),
    ).toThrow(/agrees with the operator verdict/u);
    const forced = prepareProtectedOutputSignerMissingEvidenceV1({
      subject: forcedVerdictSubjectV1({
        transactionId: validId,
        sourceKey: { transactionId: "55".repeat(32), outputIndex: 0n },
        rejectionReason: {
          ProtectedOutputSignerMissing: { output_index: 0n },
        },
      }),
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(valid),
    });
    expect(forced.signerPresent).toBe(true);
    expect(forced.validSignerHashes).toEqual([paymentCredential]);

    const missing = transactionWithWitness("empty");
    const missingId = computeMidgardNativeTxIdV1(missing).toString("hex");
    expect(() =>
      prepareProtectedOutputSignerMissingEvidenceV1({
        subject: forcedVerdictSubjectV1({
          transactionId: missingId,
          sourceKey: { transactionId: "66".repeat(32), outputIndex: 0n },
          rejectionReason: {
            ProtectedOutputSignerMissing: { output_index: 0n },
          },
        }),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(missing),
      }),
    ).toThrow(/agrees with the operator verdict/u);
  });

  it("accepts the maximum witness frontier and refuses its adjacent overflow", () => {
    expect(PROTECTED_OUTPUT_SIGNER_MAX_WITNESSES_V1).toBe(318);
    const maximum = transactionWithWitnessCount(318);
    const maximumId = computeMidgardNativeTxIdV1(maximum).toString("hex");
    const evidence = prepareProtectedOutputSignerMissingEvidenceV1({
      subject: acceptedVerdictSubjectV1(maximumId),
      outputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(maximum),
    });
    expect(evidence.checkpoints.at(-1)).toEqual({
      cursor: 318,
      signerPresent: false,
    });
    expect(evidence.witnessCarriage).toBe("Certified");

    const overflow = transactionWithWitnessCount(319);
    const overflowId = computeMidgardNativeTxIdV1(overflow).toString("hex");
    expect(() =>
      prepareProtectedOutputSignerMissingEvidenceV1({
        subject: acceptedVerdictSubjectV1(overflowId),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(overflow),
      }),
    ).toThrow(/frontier exceeds the canonical maximum/u);
  });

  it("complete replay scans accepted output coordinates and omits honest transactions", () => {
    const missing = transactionWithWitness("empty");
    const valid = transactionWithWitness("valid");
    const block = (transactions: readonly unknown[]) =>
      ({
        transactions,
        reconstruction: { forcedTransactions: [] },
      }) as never;
    expect(
      detectProtectedOutputSignerMissingCompleteReplayV1(
        block([
          {
            txCbor: encodeMidgardNativeTxCanonicalV1(missing).toString("hex"),
          },
        ]),
      ),
    ).toHaveLength(1);
    expect(
      detectProtectedOutputSignerMissingCompleteReplayV1(
        block([
          {
            txCbor: encodeMidgardNativeTxCanonicalV1(valid).toString("hex"),
          },
        ]),
      ),
    ).toEqual([]);
  });
});
