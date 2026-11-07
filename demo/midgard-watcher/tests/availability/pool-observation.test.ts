import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  authenticWatcherDaBondPool,
  deriveWatcherDaBondPoolObservation,
} from "../../src/availability/pool-observation.js";
import {
  DA_BOND_POOL_ADDRESS as POOL_ADDRESS,
  DA_BOND_POOL_POLICY_ID as POLICY,
  daBondPoolUtxo as poolUtxo,
  parametersFixture,
  utxo,
} from "../support/availability-challenge-fixture.js";

const PARAMETERS = parametersFixture();
const UNIT = SDK.daBondPoolUnit(POLICY);
const FLOOR = PARAMETERS.da_bond_pool_floor_lovelace;
const BOND = PARAMETERS.da_bond_lovelace;
/** A pool backing exactly one full DA bond. */
const FULL = FLOOR + BOND;
const UNLOCK_AT = 1_900_000_000_000n;

const withdrawing: SDK.DaBondPoolDatum = {
  Withdrawing: { unlock_at: UNLOCK_AT },
};
const derive = (pool: UTxO | undefined, nowMs = UNLOCK_AT - 1n) =>
  deriveWatcherDaBondPoolObservation({
    pool,
    policyId: POLICY,
    parameters: PARAMETERS,
    nowMs,
  });

describe("watcher DA bond pool readout (spec #685 E5, #691)", () => {
  it("reports a Bonded pool backing a full bond with no alert", () => {
    expect(derive(poolUtxo(FULL))).toEqual({
      state: "bonded",
      lovelace: FULL.toString(),
      backing: BOND.toString(),
      requiredBacking: BOND.toString(),
      belowBond: false,
      alerts: { underBacked: false, withdrawing: false },
    });
  });

  it("fires under-backed on a pool drained by a slash and clears once a top-up restores exactly da_bond", () => {
    // A pool holding one and a half bonds, slashed once, is left short.
    const before = FLOOR + BOND + BOND / 2n;
    const slash = SDK.planDaBondPoolSlash({
      poolLovelace: before,
      parameters: PARAMETERS,
    });
    expect(slash.taken).toBe(BOND);
    const drained = derive(poolUtxo(slash.poolOutputLovelace));
    expect(drained).toMatchObject({
      state: "bonded",
      lovelace: slash.poolOutputLovelace.toString(),
      backing: (BOND / 2n).toString(),
      belowBond: true,
      alerts: { underBacked: true, withdrawing: false },
    });
    // One lovelace short of the bond is still short.
    expect(derive(poolUtxo(FULL - 1n)).alerts.underBacked).toBe(true);
    // Topped up to exactly da_bond of backing: the alert clears.
    const toppedUp = derive(poolUtxo(slash.poolOutputLovelace + BOND / 2n));
    expect(toppedUp).toMatchObject({
      backing: BOND.toString(),
      belowBond: false,
      alerts: { underBacked: false, withdrawing: false },
    });
  });

  it("reports a pool at or below its floor as backing nothing", () => {
    expect(derive(poolUtxo(FLOOR - 1n))).toMatchObject({
      backing: "0",
      belowBond: true,
      alerts: { underBacked: true },
    });
  });

  it("fires withdrawing on BeginWithdraw, reports unlock_at, and clears after a cancel", () => {
    const begun = derive(poolUtxo(FULL, withdrawing));
    expect(begun).toEqual({
      state: "withdrawing",
      lovelace: FULL.toString(),
      backing: BOND.toString(),
      requiredBacking: BOND.toString(),
      belowBond: false,
      unlockAt: UNLOCK_AT.toString(),
      unlockable: false,
      alerts: { underBacked: false, withdrawing: true },
    });
    // CompleteWithdraw is admissible from unlock_at on.
    expect(derive(poolUtxo(FULL, withdrawing), UNLOCK_AT).unlockable).toBe(
      true,
    );
    const cancelled = derive(poolUtxo(FULL, "Bonded"));
    expect(cancelled.alerts).toEqual({
      underBacked: false,
      withdrawing: false,
    });
    expect(cancelled).not.toHaveProperty("unlockAt");
    expect(cancelled).not.toHaveProperty("unlockable");
  });

  it("reports a withdrawing pool that is also short with both alerts", () => {
    expect(derive(poolUtxo(FULL - 1n, withdrawing))).toMatchObject({
      state: "withdrawing",
      belowBond: true,
      alerts: { underBacked: true, withdrawing: true },
    });
  });

  it("reports a missing pool as under-backed", () => {
    expect(derive(undefined)).toEqual({
      state: "missing",
      requiredBacking: BOND.toString(),
      belowBond: true,
      alerts: { underBacked: true, withdrawing: false },
    });
  });

  it("serializes as JSON without loss", () => {
    const readout = derive(poolUtxo(FULL - 1n, withdrawing));
    expect(JSON.parse(JSON.stringify(readout))).toEqual(readout);
  });

  it("refuses a readout for an output without the pool NFT", () => {
    expect(() =>
      derive({ ...poolUtxo(FULL), assets: { lovelace: FULL } }),
    ).toThrow("requires an authenticated pool");
  });
});

describe("watcher DA bond pool authentication", () => {
  const authentic = (utxos: readonly UTxO[]) =>
    authenticWatcherDaBondPool({
      utxos,
      policyId: POLICY,
      address: POOL_ADDRESS,
    });
  const stray = { ...utxo(3, 5_000_000n), address: POOL_ADDRESS };

  it("selects the one NFT holder and ignores outputs without the NFT", () => {
    const pool = poolUtxo(FULL);
    expect(authentic([stray, pool])).toBe(pool);
  });

  it.each([
    // TopUp pins the continuing datum by Data value, so anyone may re-store
    // it under another encoding; the local source's canonical re-encoding
    // turns lucid's indefinite-length Withdrawing datum definite-length.
    ["an indefinite-length Bonded datum", "d8799fff", "bonded"],
    [
      "a definite-length Withdrawing datum",
      "d87a811b000001ba60d33800",
      "withdrawing",
    ],
    ["a tag-102 Bonded datum", "d866820080", "bonded"],
    [
      "a tag-102 Withdrawing datum",
      "d86682019f1b000001ba60d33800ff",
      "withdrawing",
    ],
  ] as const)(
    "accepts %s, as the pool validator does",
    (_label, datum, state) => {
      const pool = { ...poolUtxo(FULL), datum };
      expect(authentic([pool])).toBe(pool);
      expect(derive(pool)).toMatchObject({
        state,
        backing: BOND.toString(),
        ...(state === "withdrawing" ? { unlockAt: UNLOCK_AT.toString() } : {}),
      });
    },
  );

  it("reads no NFT holder as a missing pool", () => {
    expect(authentic([])).toBeUndefined();
    expect(authentic([stray])).toBeUndefined();
  });

  it.each([
    [
      "the NFT in two outputs",
      [poolUtxo(FULL), { ...poolUtxo(FULL), outputIndex: 1 }],
      "more than one output",
    ],
    [
      "the NFT twice",
      [{ ...poolUtxo(FULL), assets: { lovelace: FULL, [UNIT]: 2n } }],
      "Unauthentic",
    ],
    [
      "a foreign token beside the NFT",
      [
        {
          ...poolUtxo(FULL),
          assets: { lovelace: FULL, [UNIT]: 1n, [`${"ab".repeat(28)}01`]: 1n },
        },
      ],
      "Unauthentic",
    ],
    [
      "a datum hash",
      [{ ...poolUtxo(FULL), datum: undefined, datumHash: "cd".repeat(32) }],
      "Unauthentic",
    ],
    [
      "a reference script",
      [
        {
          ...poolUtxo(FULL),
          scriptRef: {
            type: "PlutusV3" as const,
            script: "4e4d01000033222220051",
          },
        },
      ],
      "Unauthentic",
    ],
    [
      "another address",
      [
        {
          ...poolUtxo(FULL),
          address: credentialToAddress("Preprod", {
            type: "Script",
            hash: "ef".repeat(28),
          }),
        },
      ],
      "Unauthentic",
    ],
    [
      "a datum that is not a pool datum",
      [{ ...poolUtxo(FULL), datum: "00" }],
      "DA bond pool datum",
    ],
  ] as const)("fails closed on %s", (_label, utxos, message) => {
    expect(() => authentic(utxos)).toThrow(message);
  });

  it("fails closed on the pool credential at another address", () => {
    // Same payment credential, but a stake part the deployment did not bind.
    const staked = credentialToAddress(
      "Preprod",
      { type: "Script", hash: POLICY },
      { type: "Key", hash: "12".repeat(28) },
    );
    expect(() => authentic([{ ...poolUtxo(FULL), address: staked }])).toThrow(
      "Unauthentic",
    );
  });

  it("fails closed when the pool address is not locked by the pool policy", () => {
    const address = credentialToAddress("Preprod", {
      type: "Script",
      hash: "ef".repeat(28),
    });
    expect(() =>
      authenticWatcherDaBondPool({
        utxos: [{ ...poolUtxo(FULL), address }],
        policyId: POLICY,
        address,
      }),
    ).toThrow("Unauthentic");
  });
});
