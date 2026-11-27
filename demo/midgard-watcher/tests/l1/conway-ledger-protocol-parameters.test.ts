/**
 * The watcher reads the live Conway protocol parameters through the L1
 * follower's ledger decoder; these pin the fields and encodings it decodes.
 */
import { CborTag, encodeCbor } from "@al-ft/l1-node-transport";
import { deriveDeploymentManifestCardanoProtocolParametersFromLedger } from "@al-ft/midgard-l1-follower/protocol-parameters";
import { describe, expect, it } from "vitest";

import { WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS } from "../support/deployment-authority-fixture.build-watcher-authority-contracts.js";
import { ledgerProtocolParameters } from "../support/ledger-protocol-parameters.js";

describe("Conway ledger protocol parameters", () => {
  it("reads the funding fields at their Conway positions with exact reduced rationals", () => {
    expect(
      deriveDeploymentManifestCardanoProtocolParametersFromLedger(
        ledgerProtocolParameters({
          minFeeA: 47n,
          minFeeB: 155_382n,
          maxTxSize: 16_385n,
          coinsPerUtxoByte: 4_311n,
          priceMemory: [1154n, 20_000n],
          priceSteps: [722n, 10_000_000n],
          maxTxExUnits: [16_500_001n, 10_000_000_001n],
          maxValueSize: 5_001n,
          collateralPercentage: 151n,
          maxCollateralInputs: 4n,
          minFeeRefScriptCostPerByte: [30n, 2n],
        }),
      ),
    ).toEqual({
      minFeeA: "47",
      minFeeB: "155382",
      priceMemory: { numerator: "577", denominator: "10000" },
      priceSteps: { numerator: "361", denominator: "5000000" },
      coinsPerUtxoByte: "4311",
      collateralPercentage: "151",
      maxCollateralInputs: "4",
      maxTxSize: "16385",
      maxValueSize: "5001",
      maxTxExUnits: { memory: "16500001", steps: "10000000001" },
      referenceScriptFee: {
        base: { numerator: "15", denominator: "1" },
        range: "25600",
        multiplier: { numerator: "6", denominator: "5" },
        maximumSizeBytes: "204800",
      },
    });
  });

  it("reads a bignum-tagged integer and an untagged rational pair", () => {
    const custom = encodeCbor([
      new CborTag(2n, Uint8Array.of(0x2c)),
      155_381n,
      0n,
      16_384n,
      ...Array.from({ length: 10 }, () => 0n),
      4_310n,
      0n,
      [
        [577n, 10_000n],
        [721n, 10_000_000n],
      ],
      [16_500_000n, 10_000_000_000n],
      0n,
      5_000n,
      150n,
      3n,
      ...Array.from({ length: 8 }, () => 0n),
      new CborTag(30n, [15n, 1n]),
    ]);
    expect(
      deriveDeploymentManifestCardanoProtocolParametersFromLedger(custom),
    ).toEqual(WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS);
  });

  it("refuses a short array, a negative natural and a zero denominator", () => {
    expect(() =>
      deriveDeploymentManifestCardanoProtocolParametersFromLedger(
        encodeCbor(Array.from({ length: 30 }, () => 0n)),
      ),
    ).toThrow("PParams is not an array of at least 31 items");
    expect(() =>
      deriveDeploymentManifestCardanoProtocolParametersFromLedger(
        ledgerProtocolParameters({ minFeeA: -1n }),
      ),
    ).toThrow("minFeeA is negative");
    expect(() =>
      deriveDeploymentManifestCardanoProtocolParametersFromLedger(
        ledgerProtocolParameters({ priceMemory: [577n, 0n] }),
      ),
    ).toThrow("priceMemory is not a nonnegative rational");
  });
});
