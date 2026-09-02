import { createPrivateKey, createPublicKey, sign } from "node:crypto";

import {
  computeMidgardNativeTxIdV1,
  encodeCbor,
  encodeMidgardAddressWitnessItemV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
  missingSignatureVkeyHashV1,
} from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterialV1 } from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  classifySpendInputSignerMissingFindingV1,
  prepareSpendInputSignerMissingEvidenceV1,
  SPEND_INPUT_SIGNER_MAX_WITNESSES_V1,
  SPEND_INPUT_SIGNER_MISSING_ID_V1,
} from "../src/spend-input-signer-missing/index.js";
import { makeNativeTx } from "./support/submit-init-emulator-shared.js";

const seed = Buffer.alloc(32, 7);
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
const priorTxId = "33".repeat(32);
const priorOutput = encodeMidgardTxOutput({
  address: Buffer.concat([
    Buffer.from([0x60]),
    Buffer.from(paymentCredential, "hex"),
  ]),
  value: { lovelace: 2_000_000n, assets: new Map() },
});
const descriptor = buildCanonicalMidgardLedgerOutputMaterialV1({
  outputIndex: 0,
  outputCbor: priorOutput,
});
const priorRoot = "44".repeat(32);
const resolved = Object.freeze({
  priorRoot,
  transactionId: priorTxId,
  outputIndex: 0,
  descriptorCborHex: descriptor.descriptorCbor.toString("hex"),
  outputCborHex: priorOutput.toString("hex"),
  membershipProofCborHex: "80",
  membershipProof: [],
});
const spendItem = encodeMidgardSpendInputItemV1({
  txId: Buffer.from(priorTxId, "hex"),
  outputIndex: 0,
});

const transactionWithSignature = (kind: "empty" | "valid" | "invalid") => {
  const empty = makeNativeTx({ spendInputCbors: [spendItem], fee: 7n });
  const txId = computeMidgardNativeTxIdV1(empty);
  const signature =
    kind === "valid" ? sign(null, txId, privateKey) : Buffer.alloc(64, 0xff);
  return makeNativeTx({
    spendInputCbors: [spendItem],
    fee: 7n,
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
    spendInputCbors: [spendItem],
    fee: 7n,
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

describe("spendInputSignerMissing V1", () => {
  it("freezes ID 00000027 and binds the exact forced coordinate/reason", () => {
    expect(SPEND_INPUT_SIGNER_MISSING_ID_V1).toBe("00000027");
    expect(SPEND_INPUT_SIGNER_MAX_WITNESSES_V1).toBe(318);
    const subject = forcedVerdictSubjectV1({
      transactionId: "11".repeat(32),
      sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
      rejectionReason: { SpendInputSignerMissing: { input_index: 3n } },
    });
    expect(() =>
      classifySpendInputSignerMissingFindingV1({ subject, inputIndex: 3 }),
    ).not.toThrow();
    expect(() =>
      classifySpendInputSignerMissingFindingV1({ subject, inputIndex: 2 }),
    ).toThrow(/coordinate was substituted/u);
    expect(() =>
      classifySpendInputSignerMissingFindingV1({
        subject: forcedVerdictSubjectV1({
          transactionId: "11".repeat(32),
          sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
          rejectionReason: {
            InputNotFound: { source_kind: 0n, input_index: 3n },
          },
        }),
        inputIndex: 3,
      }),
    ).toThrow(/wrong typed rejection reason/u);
  });

  it("admits only cryptographically valid matching witnesses", () => {
    const empty = transactionWithSignature("empty");
    const emptyTxId = computeMidgardNativeTxIdV1(empty).toString("hex");
    const missing = prepareSpendInputSignerMissingEvidenceV1({
      subject: acceptedVerdictSubjectV1(emptyTxId),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(empty),
      resolved,
    });
    expect(missing.signerMissing).toBe(true);
    expect(missing.validSignerHashes).toEqual([]);

    const invalid = transactionWithSignature("invalid");
    const invalidTxId = computeMidgardNativeTxIdV1(invalid).toString("hex");
    const invalidEvidence = prepareSpendInputSignerMissingEvidenceV1({
      subject: acceptedVerdictSubjectV1(invalidTxId),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(invalid),
      resolved,
    });
    expect(invalidEvidence.signerMissing).toBe(true);
    expect(invalidEvidence.validSignerHashes).toEqual([]);

    const valid = transactionWithSignature("valid");
    const validTxId = computeMidgardNativeTxIdV1(valid).toString("hex");
    expect(() =>
      prepareSpendInputSignerMissingEvidenceV1({
        subject: acceptedVerdictSubjectV1(validTxId),
        inputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(valid),
        resolved,
      }),
    ).toThrow(/agrees with the operator verdict/u);

    const forced = prepareSpendInputSignerMissingEvidenceV1({
      subject: forcedVerdictSubjectV1({
        transactionId: validTxId,
        sourceKey: { transactionId: "55".repeat(32), outputIndex: 0n },
        rejectionReason: { SpendInputSignerMissing: { input_index: 0n } },
      }),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(valid),
      resolved,
    });
    expect(forced.signerMissing).toBe(false);
    expect(forced.validSignerHashes).toEqual([paymentCredential]);
  });

  it("refuses out-ref, descriptor, and transaction substitutions", () => {
    const transaction = transactionWithSignature("empty");
    const txId = computeMidgardNativeTxIdV1(transaction).toString("hex");
    const args = {
      subject: acceptedVerdictSubjectV1(txId),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(transaction),
    };
    expect(() =>
      prepareSpendInputSignerMissingEvidenceV1({
        ...args,
        resolved: { ...resolved, transactionId: "66".repeat(32) },
      }),
    ).toThrow(/resolved out-ref differs/u);
    expect(() =>
      prepareSpendInputSignerMissingEvidenceV1({
        ...args,
        resolved: { ...resolved, descriptorCborHex: "80" },
      }),
    ).toThrow();
    expect(() =>
      prepareSpendInputSignerMissingEvidenceV1({
        ...args,
        subject: acceptedVerdictSubjectV1("77".repeat(32)),
        resolved,
      }),
    ).toThrow(/transaction identity was substituted/u);
  });

  it("admits the exact 318-witness frontier and refuses adjacent 319", () => {
    const maximum = transactionWithWitnessCount(318);
    const maximumTxId = computeMidgardNativeTxIdV1(maximum).toString("hex");
    const evidence = prepareSpendInputSignerMissingEvidenceV1({
      subject: acceptedVerdictSubjectV1(maximumTxId),
      inputIndex: 0,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(maximum),
      resolved,
    });
    expect(evidence.checkpoints).toHaveLength(20);
    expect(evidence.checkpoints.at(-1)).toEqual({
      cursor: 318,
      signerPresent: false,
    });
    expect(evidence.witnessCarriage).toBe("Certified");

    const over = transactionWithWitnessCount(319);
    const overTxId = computeMidgardNativeTxIdV1(over).toString("hex");
    expect(() =>
      prepareSpendInputSignerMissingEvidenceV1({
        subject: acceptedVerdictSubjectV1(overTxId),
        inputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(over),
        resolved,
      }),
    ).toThrow(/frontier exceeds the canonical maximum/u);
  });
});
