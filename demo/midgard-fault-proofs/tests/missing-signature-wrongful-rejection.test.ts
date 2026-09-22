import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  detectMissingSignatureWrongfulRejections,
  missingSignatureWrongfulRejectionCloses,
} from "../src/missing-signature/wrongful-rejection.js";
import { buildMissingSignatureForcedTransaction } from "./support/missing-signature-forced-shapes.js";

const forcedBlock = (forged = false) => {
  const tx = buildMissingSignatureForcedTransaction({ forged });
  return {
    headerHash: "04".repeat(28),
    reconstruction: {
      forcedTransactions: [
        {
          key: { transactionId: "05".repeat(32), outputIndex: 0n },
          value: {
            tx_id: tx.transactionId,
            submitted_source: tx.source,
            verdict: {
              ForcedTxInvalid: {
                reason: { RequiredSignerUnsigned: { signer_index: 0n } },
              },
            },
          },
          fullTransactionCbor: encodeMidgardForcedTxCanonical(tx.transaction),
        },
      ],
    },
  };
};
const detect = (block: ReturnType<typeof forcedBlock>) =>
  detectMissingSignatureWrongfulRejections({
    block: block as unknown as CanonicalBlockEvidence,
  });
describe("missingSignature authenticated wrongful rejection evidence", () => {
  it("derives the exact source, reason and signed required coordinate", () => {
    const [found] = detect(forcedBlock());
    expect(found).toBeDefined();
    expect(found!.evidence.signerIndex).toBe(0n);
    expect(found!.witnessIndex).toBe(0n);
    expect(missingSignatureWrongfulRejectionCloses(found!.evidence)).toBe(true);
  });
  it("does not classify a matching key with a forged signature", () =>
    expect(detect(forcedBlock(true))).toHaveLength(0));
  it.each([
    "tx_id",
    "compact_cbor",
    "witness_set_compact_cbor",
    "field_preimage_lengths_cbor",
  ] as const)("rejects substituted %s", (field) => {
    const block = forcedBlock();
    const leaf = block.reconstruction.forcedTransactions[0]!;
    if (field === "tx_id") leaf.value.tx_id = "ff".repeat(32);
    else leaf.value.submitted_source[field] = "80";
    expect(() => detect(block)).toThrow(/differs from/u);
  });
  it("refuses a subject whose reason coordinate differs from its evidence", () => {
    const evidence = detect(forcedBlock())[0]!.evidence;
    expect(
      missingSignatureWrongfulRejectionCloses({ ...evidence, signerIndex: 1n }),
    ).toBe(false);
    expect(
      missingSignatureWrongfulRejectionCloses({
        ...evidence,
        subject: { ...evidence.subject, direction: 0n },
      }),
    ).toBe(false);
  });
});
