import {
  calculateMinLovelaceFromUTxO,
  Data,
  Emulator,
  Lucid,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  calculateCorrectionLockMinLovelace,
  CORRECTION_LOCK_ASSET_NAME,
  type CorrectionIdentity,
  CorrectionLockDatum,
  CorrectionLockRedeemer,
  correctionLockUnit,
  utxosToCorrectionLockUTxOs,
} from "../src/correction-lock.js";

const POLICY_ID = "11".repeat(28);
const ADDRESS =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

const lockUtxo = (
  datum: string,
  assetName = CORRECTION_LOCK_ASSET_NAME,
): UTxO => ({
  txHash: "22".repeat(32),
  outputIndex: 0,
  address: ADDRESS,
  assets: { lovelace: 2_000_000n, [`${POLICY_ID}${assetName}`]: 1n },
  datum,
  datumHash: undefined,
  scriptRef: undefined,
});

describe("correction-lock SDK wire boundary", () => {
  it.each([4310n, 8620n])(
    "reserves the largest supported Locked datum at UTxO cost %s",
    async (coinsPerUtxoByte) => {
      const defaults = await new Emulator([]).getProtocolParameters();
      const lucid = await Lucid(
        new Emulator([], { ...defaults, coinsPerUtxoByte }),
        "Custom",
      );
      const reserved = calculateCorrectionLockMinLovelace(lucid, {
        correctionLockAddress: ADDRESS,
        hubOraclePolicyId: POLICY_ID,
      });
      const identities: CorrectionIdentity[] = [
        { FraudProof: { fraud_proof_asset_name: "33".repeat(32) } },
        "AttestationTimeout",
        { AvailabilityChallenge: { challenge_asset_name: "44".repeat(32) } },
      ];
      const minimums = identities.map((correction_identity) =>
        calculateMinLovelaceFromUTxO(
          coinsPerUtxoByte,
          lockUtxo(
            Data.to(
              {
                Locked: {
                  target_header_hash: "55".repeat(28),
                  correction_identity,
                },
              },
              CorrectionLockDatum,
            ),
          ),
        ),
      );
      expect(minimums[0]).toBe(minimums[2]);
      expect(minimums[1]).toBeLessThan(minimums[0]!);
      expect(reserved).toBe(minimums[0]);
    },
  );

  it("round-trips Idle, every correction identity, and both redeemers", () => {
    const identities = [
      {
        FraudProof: { fraud_proof_asset_name: "33".repeat(32) },
      },
      "AttestationTimeout",
      {
        AvailabilityChallenge: { challenge_asset_name: "44".repeat(32) },
      },
    ] as const;

    expect(
      Data.from(Data.to("Idle", CorrectionLockDatum), CorrectionLockDatum),
    ).toBe("Idle");
    for (const correctionIdentity of identities) {
      const datum = {
        Locked: {
          target_header_hash: "55".repeat(28),
          correction_identity: correctionIdentity,
        },
      } as const;
      expect(
        Data.from(Data.to(datum, CorrectionLockDatum), CorrectionLockDatum),
      ).toEqual(datum);
    }
    for (const redeemer of [
      { Correct: { hub_oracle_ref_input_index: 2n } },
      { Deinit: { hub_oracle_input_index: 3n } },
    ] as const) {
      expect(
        Data.from(
          Data.to(redeemer, CorrectionLockRedeemer),
          CorrectionLockRedeemer,
        ),
      ).toEqual(redeemer);
    }
  });

  it("authenticates only the exact deployment-bound lock asset name", async () => {
    const idle = Data.to("Idle", CorrectionLockDatum);
    const authentic = await Effect.runPromise(
      utxosToCorrectionLockUTxOs(
        [lockUtxo(idle), lockUtxo(idle, "00")],
        POLICY_ID,
      ),
    );

    expect(correctionLockUnit(POLICY_ID)).toBe(
      `${POLICY_ID}${CORRECTION_LOCK_ASSET_NAME}`,
    );
    expect(authentic).toHaveLength(1);
    expect(authentic[0]?.datum).toBe("Idle");
    expect(authentic[0]?.assetName).toBe(CORRECTION_LOCK_ASSET_NAME);
  });

  it("rejects non-canonical availability challenge asset-name widths", () => {
    for (const challenge_asset_name of ["44".repeat(31), "44".repeat(33)]) {
      expect(() =>
        Data.to(
          {
            Locked: {
              target_header_hash: "55".repeat(28),
              correction_identity: {
                AvailabilityChallenge: { challenge_asset_name },
              },
            },
          },
          CorrectionLockDatum,
        ),
      ).toThrow();
    }
  });
});
