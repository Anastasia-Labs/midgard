import { readFileSync } from "node:fs";

import {
  decodeMidgardFieldPreimage,
  deriveMidgardTxFieldPreimages,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  MIDGARD_CONSENSUS_LIMITS,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { exerciseMidgardRetainedDaCanonicalBoundary } from "./helpers/retained-da-boundary.js";

const sizeBalancedFixture = JSON.parse(
  readFileSync(
    new URL(
      "../../lucid-midgard/tests/fixtures/native-size-balanced-15_5k.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as {
  readonly txIdHex: string;
  readonly fullTxCborHex: string;
  readonly sizes: {
    readonly fullTxCborBytes: number;
  };
};

/**
 * A reference model for the number of bounded reveal steps a canonical
 * transaction owes, written from §5.1's rule rather than from the machine that
 * produces them: every item of every field preimage travels in
 * `MIDGARD_BOUNDED_ITEM_CHUNK_BYTES`-sized chunks, and an empty item still
 * costs one step. It is deliberately arithmetic over the decoded preimages, so
 * a machine that stopped a field short, folded two items into one step, or
 * widened its chunk disagrees with it.
 */
const expectedRevealStepCount = (canonicalTransactionCbor: Buffer): number =>
  deriveMidgardTxFieldPreimages(canonicalTransactionCbor)
    .flatMap((field) => decodeMidgardFieldPreimage(field.preimageCbor))
    .reduce(
      (total, item) =>
        total +
        Math.max(1, Math.ceil(item.length / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES)),
      0,
    );

describe("canonical V1 retained-DA boundary harness", () => {
  it("reconstructs the same large canonical transaction from normal and forced retention", async () => {
    const canonicalTransactionCbor = Buffer.from(
      sizeBalancedFixture.fullTxCborHex,
      "hex",
    );
    const measurement = await exerciseMidgardRetainedDaCanonicalBoundary({
      canonicalTransactionCbor,
      corpusLabel: "mixed-size-balanced",
      productionAdmission: "diagnostic-synthetic-script-witnesses",
    });

    // The transaction the harness carried is the fixture's, identified by the
    // id its independent producer recorded (`demo/lucid-midgard`'s
    // `fixtures:native-size-balanced` construction, whose check mode runs in
    // that package's CI test lane).
    expect(measurement.transactionIdHex).toBe(sizeBalancedFixture.txIdHex);
    expect(measurement.normal.sourceKind).toBe("normal");
    expect(measurement.forced.sourceKind).toBe("forced");

    // Both classifications retain the whole transaction, and the fold gives
    // back the *same bytes* — length agreement is not the claim, so the
    // digests are compared rather than the sizes.
    for (const classification of [measurement.normal, measurement.forced]) {
      expect(classification.retainedPreimageBytes).toBe(
        sizeBalancedFixture.sizes.fullTxCborBytes -
          (classification.sourceKind === "forced" ? 1 : 0),
      );
      expect(classification.reconstructedCanonicalDigestHex).toBe(
        classification.retainedPreimageDigestHex,
      );
      expect(classification.reconstructedCanonicalBytes).toBe(
        sizeBalancedFixture.sizes.fullTxCborBytes -
          (classification.sourceKind === "forced" ? 1 : 0),
      );
      expect(classification.transactionIdHex).toBe(sizeBalancedFixture.txIdHex);
      expect(classification.transactionCommitmentHex).toBe(
        classification.sourceKind === "forced"
          ? measurement.forcedTransactionCommitmentHex
          : measurement.transactionCommitmentHex,
      );
      // The reveal count is decided against the reference model above, not
      // against the other classification or a recorded number.
      expect(classification.revealStepCount).toBe(
        expectedRevealStepCount(canonicalTransactionCbor),
      );
    }
    // Distinct committed encodings retain one body ID. Their wire digests
    // differ because the forced source contains no validity scalar.
    expect(measurement.forced.reconstructedCanonicalDigestHex).not.toBe(
      measurement.normal.reconstructedCanonicalDigestHex,
    );

    expect(measurement.innerPayloadBytes).toBeGreaterThan(
      sizeBalancedFixture.sizes.fullTxCborBytes * 2,
    );
    expect(measurement.storedPayloadBytes).toBeGreaterThan(
      measurement.innerPayloadBytes,
    );
    expect(measurement.storedPayloadBytes).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
    );
  });
});
