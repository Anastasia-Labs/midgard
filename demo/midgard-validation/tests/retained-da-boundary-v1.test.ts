import { readFileSync } from "node:fs";

import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { exerciseMidgardRetainedDaCanonicalBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";

const sizeBalancedFixture = JSON.parse(
  readFileSync(
    new URL(
      "../../lucid-midgard/tests/fixtures/native-size-balanced-15_5k.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as {
  readonly fullTxCborHex: string;
  readonly sizes: {
    readonly fullTxCborBytes: number;
  };
};

describe("canonical V1 retained-DA boundary harness", () => {
  it("reconstructs the same large canonical transaction from normal and forced retention", async () => {
    const measurement =
      await exerciseMidgardRetainedDaCanonicalBoundaryV1({
        canonicalTransactionCbor: Buffer.from(
          sizeBalancedFixture.fullTxCborHex,
          "hex",
        ),
      });

    expect(measurement.normal.sourceKind).toBe("normal");
    expect(measurement.forced.sourceKind).toBe("forced");
    expect(measurement.normal.retainedPreimageBytes).toBe(
      sizeBalancedFixture.sizes.fullTxCborBytes,
    );
    expect(measurement.forced.retainedPreimageBytes).toBe(
      sizeBalancedFixture.sizes.fullTxCborBytes,
    );
    expect(
      measurement.normal.reconstructedCanonicalBytes,
    ).toBe(measurement.normal.retainedPreimageBytes);
    expect(
      measurement.forced.reconstructedCanonicalBytes,
    ).toBe(measurement.forced.retainedPreimageBytes);
    expect(measurement.normal.revealStepCount).toBeGreaterThan(0);
    expect(measurement.forced.revealStepCount).toBe(
      measurement.normal.revealStepCount,
    );
    expect(measurement.innerPayloadBytes).toBeGreaterThan(
      sizeBalancedFixture.sizes.fullTxCborBytes * 2,
    );
    expect(measurement.storedPayloadBytes).toBeGreaterThan(
      measurement.innerPayloadBytes,
    );
    expect(measurement.storedPayloadBytes).toBeLessThan(
      MIDGARD_CONSENSUS_LIMITS_V1.maxDaPayloadBytes,
    );
  });
});
