import {
  adjudicateMidgardNativeTxFullValidity,
  deriveMidgardNativeTxProofSource,
  encodeMidgardNativeTxCanonical,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  invalidSignatureTerminalContradiction,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  invalidSignatureEvidenceFromForcedSource,
  invalidSignatureWrongfulRejectionCloses,
} from "../src/invalid-signature/wrongful-rejection.js";
import { INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  buildInvalidSignatureSubject,
  honestAddressWitness,
  invalidAddressWitness,
} from "./support/invalid-signature-emulator.js";
const txId = "11".repeat(32);
const subject = (index: bigint) =>
  forcedVerdictSubject({
    transactionId: txId,
    sourceKey: { transactionId: "22".repeat(32), outputIndex: 0n },
    rejectionReason: {
      AddressWitnessSignatureInvalid: { witness_index: index },
    },
  });
const honest = honestAddressWitness({ index: 0, txId });
const bad = invalidAddressWitness(0);
describe("invalidSignature exact wrongful rejection", () => {
  it("convicts a genuinely valid signature", () =>
    expect(
      invalidSignatureTerminalContradiction({
        subject: subject(0n),
        witnessIndex: 0n,
        addressWitnesses: [honest],
      }),
    ).toBe(true));
  it("refuses honest rejection", () =>
    expect(
      invalidSignatureTerminalContradiction({
        subject: subject(0n),
        witnessIndex: 0n,
        addressWitnesses: [bad],
      }),
    ).toBe(false));
  it.each([-1n, 1n, 65535n])(
    "convicts impossible authenticated index %s",
    (witnessIndex) =>
      expect(
        invalidSignatureTerminalContradiction({
          subject: subject(witnessIndex),
          witnessIndex,
          addressWitnesses: [honest],
        }),
      ).toBe(true),
  );
  it("refuses substituted coordinate even with a valid signature", () =>
    expect(() =>
      invalidSignatureTerminalContradiction({
        subject: subject(1n),
        witnessIndex: 0n,
        addressWitnesses: [honest, bad],
      }),
    ).toThrow(/reason\/index/));
  it("refuses unrelated reason", () =>
    expect(() =>
      invalidSignatureTerminalContradiction({
        subject: { ...subject(0n), rejection_reason: "EmptyInputs" },
        witnessIndex: 0n,
        addressWitnesses: [honest],
      }),
    ).toThrow(/reason\/index/));
  it("refuses substituted transaction", () =>
    expect(
      invalidSignatureTerminalContradiction({
        subject: { ...subject(0n), transaction_id: "33".repeat(32) },
        witnessIndex: 0n,
        addressWitnesses: [honest],
      }),
    ).toBe(false));
  it("preserves accepted polarity", () => {
    for (const [addressWitnesses, result] of [
      [[bad], true],
      [[honest], false],
      [[], false],
    ] as const)
      expect(
        invalidSignatureTerminalContradiction({
          subject: acceptedVerdictSubject(txId),
          witnessIndex: 0n,
          addressWitnesses,
        }),
      ).toBe(result);
  });
  it.each(["TxIsValid", "TxIsInvalid"] as const)(
    "derives evidence from retained %s source and rejects source mutations",
    async (submittedValidity) => {
      const signed = await buildInvalidSignatureSubject({ accused: "honest" });
      const invalid = adjudicateMidgardNativeTxFullValidity(
        signed.nativeTx,
        "TxIsInvalid",
      );
      const source = deriveMidgardNativeTxProofSource(invalid);
      const forced = {
        key: { transactionId: "22".repeat(32), outputIndex: 0n },
        value: {
          tx_id: signed.nativeTxId,
          source: {
            compact_cbor: source.compactCbor.toString("hex"),
            witness_set_compact_cbor:
              source.witnessSetCompactCbor.toString("hex"),
            field_preimage_lengths_cbor:
              source.fieldPreimageLengthsCbor.toString("hex"),
          },
          verdict: {
            ForcedTxInvalid: {
              reason: { AddressWitnessSignatureInvalid: { witness_index: 0n } },
            },
          },
        },
        fullTransactionCbor: encodeMidgardNativeTxCanonical(
          adjudicateMidgardNativeTxFullValidity(
            signed.nativeTx,
            submittedValidity,
          ),
        ),
      } as const;
      await expect(
        INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY.replay({
          headerHash: "33".repeat(28),
          transactions: [],
          reconstruction: { forcedTransactions: [forced] },
        } as never),
      ).resolves.toMatchObject({
        detections: [
          { violationId: "invalid-signature-wrongful-rejection", position: 0n },
        ],
      });
      const evidence = invalidSignatureEvidenceFromForcedSource(forced);
      expect(evidence).not.toBeNull();
      expect(invalidSignatureWrongfulRejectionCloses(evidence!)).toBe(true);
      for (const field of [
        "compact_cbor",
        "witness_set_compact_cbor",
        "field_preimage_lengths_cbor",
      ] as const)
        expect(() =>
          invalidSignatureEvidenceFromForcedSource({
            ...forced,
            value: {
              ...forced.value,
              source: { ...forced.value.source, [field]: "00" },
            },
          }),
        ).toThrow(/authenticated leaf/);
    },
  );
});
