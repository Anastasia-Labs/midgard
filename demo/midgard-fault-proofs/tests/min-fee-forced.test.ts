import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  encodeMidgardNativeTxCanonical,
} from "@al-ft/midgard-core";
import {
  forcedVerdictSubject,
  minFeeTerminalContradiction,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  detectMinFeeForcedReplay,
  prepareMinFeeForcedPlan,
} from "../src/min-fee-forced.js";
import { MIN_FEE_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";

const fixture = (minimum = 1_000n, reason = "FeeBelowMinimum") => {
  const submitted = makeNativeTx({ spendInputCbors: [], fee: 1_000n });
  const adjudicated = adjudicateMidgardNativeTxFullValidity(
    submitted,
    "TxIsInvalid",
  );
  const source = deriveMidgardNativeTxProofSource(adjudicated);
  const block = {
    transactions: [],
    headerHash: "04".repeat(28),
    header: { minFeeA: 0n, minFeeB: minimum },
    reconstruction: {
      forcedTransactions: [
        {
          key: { transactionId: "05".repeat(32), outputIndex: 0n },
          value: {
            tx_id: computeMidgardNativeTxId(submitted).toString("hex"),
            source: {
              compact_cbor: source.compactCbor.toString("hex"),
              witness_set_compact_cbor:
                source.witnessSetCompactCbor.toString("hex"),
              field_preimage_lengths_cbor:
                source.fieldPreimageLengthsCbor.toString("hex"),
            },
            verdict: { ForcedTxInvalid: { reason } },
          },
          // Real retained DA carries the submitted scalar, unlike the leaf.
          fullTransactionCbor: encodeMidgardNativeTxCanonical(submitted),
        },
      ],
    },
  };
  return { block, canonical: block as unknown as CanonicalBlockEvidence };
};

describe("minFee authenticated forced detection", () => {
  it.each([999n, 1_000n])(
    "derives exact rejection contradiction at %s from submitted DA bytes",
    (minimum) => {
      const { canonical } = fixture(minimum);
      const detected = detectMinFeeForcedReplay(canonical);
      expect(detected).toHaveLength(1);
      expect(detected[0]!.evidence.state.bad_tx.validity_code).toBe(1n);
      expect(detected[0]!.evidence.minimumFee).toBe(minimum);
      expect(detected[0]!.evidence.fieldItemCbors).toHaveLength(9);
      expect(detected[0]!.evidence.subject.rejection_reason).toBe(
        "FeeBelowMinimum",
      );
    },
  );
  it("includes forced rejection in complete canonical min-fee replay", async () => {
    const result = await MIN_FEE_COMPLETE_CANONICAL_REPLAY.replay(
      fixture().canonical,
    );
    expect(result.detections).toHaveLength(1);
    expect(result.detections[0]!.detectionId).toMatch(/^min-fee:forced:/u);
  });
  it("refuses an honest rejection and foreign reason", () => {
    expect(detectMinFeeForcedReplay(fixture(1_001n).canonical)).toHaveLength(0);
    expect(
      detectMinFeeForcedReplay(fixture(1_000n, "EmptyInputs").canonical),
    ).toHaveLength(0);
  });
  it.each([
    "tx_id",
    "compact_cbor",
    "witness_set_compact_cbor",
    "field_preimage_lengths_cbor",
  ])("rejects substituted %s", (field) => {
    const { block, canonical } = fixture();
    const leaf = block.reconstruction.forcedTransactions[0]!.value;
    if (field === "tx_id") leaf.tx_id = "ff".repeat(32);
    else leaf.source[field as keyof typeof leaf.source] += "00";
    expect(() => detectMinFeeForcedReplay(canonical)).toThrow(
      /preimage differs/,
    );
  });
  it("refuses an exact detection selector that is absent", async () => {
    await expect(
      prepareMinFeeForcedPlan({
        block: fixture().canonical,
        detectionId: "other",
      }),
    ).rejects.toThrow(/no authenticated/);
  });
  it("refuses reason substitution at the terminal twin", () => {
    const subject = forcedVerdictSubject({
      transactionId: "01".repeat(32),
      sourceKey: { transactionId: "02".repeat(32), outputIndex: 0n },
      rejectionReason: "EmptyInputs",
    });
    expect(() => minFeeTerminalContradiction(subject, false)).toThrow(
      /wrong forced rejection reason/,
    );
  });
});
