import type { MidgardValue } from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import { RejectCodes, validatePhaseASingle } from "../src/index.js";
import {
  isZeroValueDelta,
  MIN_ADA_OUTPUT_OVERHEAD_BYTES_V1,
  minAdaLovelaceV1,
  mintToValueDelta,
  outputMeetsMinAdaV1,
  valuePreservationDelta,
} from "../src/value-accounting.js";
import { makeNativeTx, makeOutput, makeQueued } from "./validation-fixtures.js";

const policyId = "11".repeat(28);
const assetName = "abcd";

const value = (lovelace: bigint, quantity: bigint): MidgardValue => ({
  lovelace,
  assets:
    quantity === 0n
      ? new Map()
      : new Map([[policyId, new Map([[assetName, quantity]])]]),
});

describe("mint-aware value accounting", () => {
  it("preserves positive mint quantities in the dormant profile-independent equation", () => {
    const mintDelta = mintToValueDelta({
      assets: [
        {
          policyId: Buffer.from(policyId, "hex"),
          assetName: Buffer.from(assetName, "hex"),
          quantity: 3n,
        },
      ],
    });

    expect(
      isZeroValueDelta(
        valuePreservationDelta(value(5n, 2n), 1n, mintDelta, value(4n, 5n)),
      ),
    ).toBe(true);
  });

  it("preserves negative burn quantities in the dormant profile-independent equation", () => {
    const mintDelta = mintToValueDelta({
      assets: [
        {
          policyId: Buffer.from(policyId, "hex"),
          assetName: Buffer.from(assetName, "hex"),
          quantity: -2n,
        },
      ],
    });

    expect(
      isZeroValueDelta(
        valuePreservationDelta(value(5n, 5n), 1n, mintDelta, value(4n, 3n)),
      ),
    ).toBe(true);
  });

  // C49 (#540). Mirrors the two negative legs of the Aiken
  // `parameterized_ada_and_multi_asset_conservation_matches_typescript`
  // selector, over the SAME mint vector as the positive case above (inputs
  // {lovelace 5, asset 2}, fee 1, mint +3, outputs {lovelace 4, asset 5}):
  // each vector below perturbs exactly one number away from that settled
  // vector, so exactly one half of `inputs + mint - outputs - fee == 0` is
  // left nonzero.
  it("rejects value-preservation vectors that are one lovelace or one asset unit short of the conservation equation", () => {
    const mintDeltaFor = (quantity: bigint) =>
      mintToValueDelta({
        assets: [
          {
            policyId: Buffer.from(policyId, "hex"),
            assetName: Buffer.from(assetName, "hex"),
            quantity,
          },
        ],
      });

    // Outputs carry 5 lovelace instead of 4: Ada is short by exactly one
    // lovelace, matching the Aiken
    // `conservation_final_step_is_provable(0, 0, False)` negative leg.
    expect(
      isZeroValueDelta(
        valuePreservationDelta(
          value(5n, 2n),
          1n,
          mintDeltaFor(3n),
          value(5n, 5n),
        ),
      ),
    ).toBe(false);

    // The mint quantity is +2 instead of +3: the asset delta settles at -1
    // instead of 0, matching the Aiken
    // `conservation_final_step_is_provable(1, -1, False)` negative leg.
    expect(
      isZeroValueDelta(
        valuePreservationDelta(
          value(5n, 2n),
          1n,
          mintDeltaFor(2n),
          value(4n, 5n),
        ),
      ),
    ).toBe(false);
  });
});

describe("minimum-fee boundary (target-snapshot parameterized)", () => {
  it("accepts a fee exactly at, and rejects one lovelace under, the parameterized min-fee boundary (a=44, b=155381)", () => {
    const minFeeA = 44n;
    const minFeeB = 155_381n;
    const config = {
      expectedNetworkId: 0n,
      minFeeA,
      minFeeB,
      concurrency: 1,
      strictnessProfile: "value-accounting-min-fee-boundary",
    };

    // Probe at a fee already in the same CBOR-width bucket (>= 65_536, a
    // 4-byte uint32) the eventual boundary fee will land in, since
    // `minFeeB` alone already clears that threshold.
    const probe = makeNativeTx({ fee: minFeeB });
    const probeSize = BigInt(probe.txCbor.length);
    const boundaryFee = minFeeA * probeSize + minFeeB;

    const accepted = makeNativeTx({ fee: boundaryFee });
    const rejected = makeNativeTx({ fee: boundaryFee - 1n });
    // The fixed point: the fee's CBOR width -- and therefore the canonical
    // size the min-fee formula is evaluated over -- is identical for the
    // probe, the boundary fee and the adjacent transaction.
    expect(BigInt(accepted.txCbor.length)).toBe(probeSize);
    expect(BigInt(rejected.txCbor.length)).toBe(probeSize);

    const acceptedResult = validatePhaseASingle(
      makeQueued(accepted.txId, accepted.txCbor),
      config,
    );
    expect("ledgerTx" in acceptedResult).toBe(true);

    const rejectedResult = validatePhaseASingle(
      makeQueued(rejected.txId, rejected.txCbor),
      config,
    );
    expect("ledgerTx" in rejectedResult).toBe(false);
    if (!("ledgerTx" in rejectedResult)) {
      expect(rejectedResult.code).toBe(RejectCodes.MinFee);
    }
  });
});

describe("minimum-Ada boundary (target-snapshot parameterized)", () => {
  it("pins the parameterized min-Ada floor at coinsPerUtxoByte=4_310, its +/-1 legs, and the 689_600 zero-byte floor", () => {
    const coinsPerUtxoByte = 4_310n;
    const outputBytes = BigInt(makeOutput(2_000_000n).length);
    const floor = minAdaLovelaceV1(coinsPerUtxoByte, outputBytes);

    expect(floor).toBe(
      coinsPerUtxoByte * (MIN_ADA_OUTPUT_OVERHEAD_BYTES_V1 + outputBytes),
    );
    expect(outputMeetsMinAdaV1(coinsPerUtxoByte, outputBytes, floor)).toBe(
      true,
    );
    expect(outputMeetsMinAdaV1(coinsPerUtxoByte, outputBytes, floor - 1n)).toBe(
      false,
    );
    expect(outputMeetsMinAdaV1(coinsPerUtxoByte, outputBytes, floor + 1n)).toBe(
      true,
    );
    // +1 output byte moves the floor by exactly `coinsPerUtxoByte`.
    expect(minAdaLovelaceV1(coinsPerUtxoByte, outputBytes + 1n) - floor).toBe(
      coinsPerUtxoByte,
    );
    expect(minAdaLovelaceV1(coinsPerUtxoByte, 0n)).toBe(689_600n);
  });
});
