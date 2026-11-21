import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  Constr,
  Data,
  fromText,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  availabilityResponseGeometry,
  DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  daAvailabilityParameters,
} from "../src/availability-challenge.js";
import {
  assertCanonicalDaBondPoolDatum,
  DA_BOND_POOL_ASSET_NAME,
  daBondPoolBacking,
  DaBondPoolDatum,
  DaBondPoolMintRedeemer,
  DaBondPoolSpendRedeemer,
  daBondPoolUnit,
  daBondPoolUnlockAt,
  decodeDaBondPoolDatum,
  encodeDaBondPoolDatum,
  fetchDaBondPool,
  planDaBondPoolSlash,
} from "../src/da-bond-pool.js";
import * as Sdk from "../src/index.js";

const ADA = 1_000_000n;
const POLICY_ID = "ab".repeat(28);
const POOL_ADDRESS =
  "addr_test1wz4t42hgj7vs9lqa9c2gn5xpfj3c3mr8huukhj2d9ctsx6g4w8hzk";

const parameters = daAvailabilityParameters({
  responseGeometry: availabilityResponseGeometry(
    DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
  ),
  ...DA_AVAILABILITY_PROFILE_BOND_AMOUNTS,
  challengerBondLovelace:
    DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE_MEASUREMENT_CANDIDATE,
  maxOpenFeeLovelace: 500_000n,
  maxPublicationFeeLovelace: 500_000n,
  maxSettlementFeeLovelace: 500_000n,
  maxCloseFeeLovelace: 1_000_000n,
  maxTimeoutFeeLovelace: 1_200_000n,
});

describe("DA bond pool codecs", () => {
  it("is exported from the package entry point", () => {
    expect(Sdk.planDaBondPoolSlash).toBe(planDaBondPoolSlash);
    expect(Sdk.DA_BOND_POOL_ASSET_NAME).toBe(DA_BOND_POOL_ASSET_NAME);
  });

  it("names the pool NFT as Aiken da_bond_pool_asset_name", () => {
    expect(DA_BOND_POOL_ASSET_NAME).toBe(
      Buffer.from("MIDGARD_DA_BOND_POOL", "ascii").toString("hex"),
    );
    expect(DA_BOND_POOL_ASSET_NAME).toBe(fromText("MIDGARD_DA_BOND_POOL"));
    expect(daBondPoolUnit(POLICY_ID)).toBe(
      `${POLICY_ID}${DA_BOND_POOL_ASSET_NAME}`,
    );
  });

  it("round-trips both pool datum states at their Aiken constructors", () => {
    const bonded: DaBondPoolDatum = "Bonded";
    const withdrawing: DaBondPoolDatum = {
      Withdrawing: { unlock_at: 1_790_000_000_000n },
    };
    const bondedCbor = encodeDaBondPoolDatum(bonded);
    const withdrawingCbor = encodeDaBondPoolDatum(withdrawing);
    expect(bondedCbor).toBe(Data.to(new Constr(0, [])));
    expect(withdrawingCbor).toBe(Data.to(new Constr(1, [1_790_000_000_000n])));
    expect(decodeDaBondPoolDatum(bondedCbor)).toEqual(bonded);
    expect(decodeDaBondPoolDatum(withdrawingCbor)).toEqual(withdrawing);
    expect(Data.from(withdrawingCbor, DaBondPoolDatum)).toEqual(withdrawing);
  });

  it("refuses malformed pool datums and non-canonical values", () => {
    const withdrawingCbor = Data.to(new Constr(1, [5n]));
    expect(decodeDaBondPoolDatum(withdrawingCbor)).toEqual({
      Withdrawing: { unlock_at: 5n },
    });
    expect(() => decodeDaBondPoolDatum(withdrawingCbor.toUpperCase())).toThrow(
      "lowercase CBOR hex",
    );
    expect(() => decodeDaBondPoolDatum("")).toThrow("lowercase CBOR hex");
    expect(() => decodeDaBondPoolDatum(`${withdrawingCbor}00`)).toThrow(
      "not valid Plutus Data",
    );
    // Truncated: the field list promises one item and carries none.
    expect(() => decodeDaBondPoolDatum("d87a81")).toThrow(
      "not valid Plutus Data",
    );
    expect(() => decodeDaBondPoolDatum(Data.to(new Constr(2, [])))).toThrow(
      "not valid Plutus Data",
    );
    expect(() => decodeDaBondPoolDatum(Data.to(new Constr(1, [-1n])))).toThrow(
      "non-negative unlock_at",
    );
    expect(() =>
      assertCanonicalDaBondPoolDatum({ Withdrawing: { unlock_at: -1n } }),
    ).toThrow("non-negative unlock_at");
    expect(() =>
      encodeDaBondPoolDatum({ Withdrawing: { unlock_at: -1n } }),
    ).toThrow("non-negative unlock_at");
  });

  it("reads a chain pool datum by value, in any Plutus Data encoding", () => {
    const withdrawing: DaBondPoolDatum = {
      Withdrawing: { unlock_at: 1_900_000_000_000n },
    };
    // The validator compares datums as Data values, so these reach the chain.
    expect(decodeDaBondPoolDatum("d87980")).toBe("Bonded");
    expect(decodeDaBondPoolDatum("d8799fff")).toBe("Bonded");
    expect(decodeDaBondPoolDatum(encodeDaBondPoolDatum(withdrawing))).toEqual(
      withdrawing,
    );
    expect(decodeDaBondPoolDatum("d87a811b000001ba60d33800")).toEqual(
      withdrawing,
    );
    // The general constructor form (tag 102), and non-minimal integers.
    expect(decodeDaBondPoolDatum("d866820080")).toBe("Bonded");
    expect(decodeDaBondPoolDatum("d86682009fff")).toBe("Bonded");
    expect(decodeDaBondPoolDatum("d86682018105")).toEqual({
      Withdrawing: { unlock_at: 5n },
    });
    expect(decodeDaBondPoolDatum("d87a811b0000000000000005")).toEqual({
      Withdrawing: { unlock_at: 5n },
    });
    expect(decodeDaBondPoolDatum("d87a81c24105")).toEqual({
      Withdrawing: { unlock_at: 5n },
    });
    // Still one well-formed canonical datum, nothing after it.
    expect(() => decodeDaBondPoolDatum("d8798000")).toThrow(
      "not valid Plutus Data",
    );
    expect(() => decodeDaBondPoolDatum("D87980")).toThrow("lowercase CBOR hex");
    expect(() => decodeDaBondPoolDatum(Data.to(new Constr(2, [])))).toThrow(
      "not valid Plutus Data",
    );
    expect(() => decodeDaBondPoolDatum("d87a8120")).toThrow(
      "non-negative unlock_at",
    );
  });

  it("keeps the Aiken MintRedeemer and SpendRedeemer constructor layout", () => {
    // Aiken InitPool { output_index } is the type's one constructor.
    expect(Data.to({ output_index: 3n }, DaBondPoolMintRedeemer)).toBe(
      Data.to(new Constr(0, [3n])),
    );
    expect(
      Data.from(Data.to(new Constr(0, [3n])), DaBondPoolMintRedeemer),
    ).toEqual({ output_index: 3n });
    expect(() =>
      Data.from(Data.to(new Constr(1, [3n])), DaBondPoolMintRedeemer),
    ).toThrow();

    const arms = [
      [{ TopUp: { output_index: 1n } }, new Constr(0, [1n])],
      [
        {
          Slash: {
            hub_oracle_ref_input_index: 1n,
            state_queue_mint_redeemer_index: 2n,
            correction_lock_input_index: 3n,
            output_index: 4n,
          },
        },
        new Constr(1, [1n, 2n, 3n, 4n]),
      ],
      [
        { BeginWithdraw: { da_params_ref_input_index: 1n, output_index: 2n } },
        new Constr(2, [1n, 2n]),
      ],
      [
        {
          CancelWithdraw: { da_params_ref_input_index: 1n, output_index: 2n },
        },
        new Constr(3, [1n, 2n]),
      ],
      [
        {
          CompleteWithdraw: {
            amount: 7n,
            da_params_ref_input_index: 1n,
            output_index: 2n,
          },
        },
        new Constr(4, [7n, 1n, 2n]),
      ],
    ] as const;
    for (const [redeemer, constr] of arms) {
      const encoded = Data.to(constr);
      expect(Data.to(redeemer, DaBondPoolSpendRedeemer)).toBe(encoded);
      expect(Data.from(encoded, DaBondPoolSpendRedeemer)).toEqual(redeemer);
    }
    expect(() =>
      Data.from(Data.to(new Constr(5, [1n])), DaBondPoolSpendRedeemer),
    ).toThrow();
  });
});

describe("DA bond pool arithmetic", () => {
  // The selected profile (preprod-testing) pins these; every case below is
  // written against them.
  it("runs on the selected profile's pool amounts", () => {
    expect(SELECTED_DEPLOYMENT_PROFILE.name).toBe("preprod-testing");
    expect(parameters.da_bond_lovelace).toBe(500n * ADA);
    expect(parameters.da_slash_penalty_lovelace).toBe(100n * ADA);
    expect(parameters.da_bond_pool_floor_lovelace).toBe(5n * ADA);
  });

  it("counts only lovelace above the floor as backing", () => {
    expect(daBondPoolBacking({ lovelace: 505n * ADA, parameters })).toBe(
      500n * ADA,
    );
    expect(daBondPoolBacking({ lovelace: 5n * ADA + 1n, parameters })).toBe(1n);
    expect(daBondPoolBacking({ lovelace: 5n * ADA, parameters })).toBe(0n);
    expect(daBondPoolBacking({ lovelace: 2n * ADA, parameters })).toBe(0n);
    expect(daBondPoolBacking({ lovelace: 0n, parameters })).toBe(0n);
  });

  it.each([
    [
      "full backing: takes exactly da_bond",
      1_005n * ADA,
      {
        backing: 1_000n * ADA,
        taken: 500n * ADA,
        feePart: 100n * ADA,
        payout: 400n * ADA,
        poolOutputLovelace: 505n * ADA,
      },
    ],
    [
      "backing exactly da_bond: drains to the floor",
      505n * ADA,
      {
        backing: 500n * ADA,
        taken: 500n * ADA,
        feePart: 100n * ADA,
        payout: 400n * ADA,
        poolOutputLovelace: 5n * ADA,
      },
    ],
    [
      "partial backing above the penalty: penalty first, rest paid out",
      305n * ADA,
      {
        backing: 300n * ADA,
        taken: 300n * ADA,
        feePart: 100n * ADA,
        payout: 200n * ADA,
        poolOutputLovelace: 5n * ADA,
      },
    ],
    [
      "partial backing below the penalty: all of it is fee, no payout",
      65n * ADA,
      {
        backing: 60n * ADA,
        taken: 60n * ADA,
        feePart: 60n * ADA,
        payout: 0n,
        poolOutputLovelace: 5n * ADA,
      },
    ],
    [
      "empty pool at the floor: nothing moves",
      5n * ADA,
      {
        backing: 0n,
        taken: 0n,
        feePart: 0n,
        payout: 0n,
        poolOutputLovelace: 5n * ADA,
      },
    ],
    [
      "pool below the floor: nothing moves, the pool keeps its lovelace",
      3n * ADA,
      {
        backing: 0n,
        taken: 0n,
        feePart: 0n,
        payout: 0n,
        poolOutputLovelace: 3n * ADA,
      },
    ],
  ] as const)("slash plan, %s", (_label, poolLovelace, expected) => {
    const plan = planDaBondPoolSlash({ poolLovelace, parameters });
    expect(plan).toEqual(expected);
    expect(plan.feePart + plan.payout).toBe(plan.taken);
    expect(plan.poolOutputLovelace + plan.taken).toBe(poolLovelace);
  });

  it("refuses a negative pool value and non-profile parameters", () => {
    expect(() =>
      planDaBondPoolSlash({ poolLovelace: -1n, parameters }),
    ).toThrow("non-negative");
    expect(() =>
      planDaBondPoolSlash({
        poolLovelace: 1_005n * ADA,
        parameters: { ...parameters, da_bond_lovelace: 600n * ADA },
      }),
    ).toThrow("selected deployment profile");
  });

  it("anchors unlock_at at the inclusive upper bound validTo - 1", () => {
    const withdrawDelayMs = BigInt(
      SELECTED_DEPLOYMENT_PROFILE.timing.da_bond_withdraw_delay_ms,
    );
    expect(withdrawDelayMs).toBe(2_380_000n);
    expect(
      daBondPoolUnlockAt({ validToMs: 1_790_000_000_000n, withdrawDelayMs }),
    ).toBe(1_790_002_379_999n);
    expect(daBondPoolUnlockAt({ validToMs: 1n, withdrawDelayMs: 0n })).toBe(0n);
  });
});

describe("fetchDaBondPool", () => {
  const unit = daBondPoolUnit(POLICY_ID);
  const poolUtxo = (overrides: Partial<UTxO> = {}): UTxO => ({
    txHash: "cd".repeat(32),
    outputIndex: 0,
    address: POOL_ADDRESS,
    assets: { lovelace: 305n * ADA, [unit]: 1n },
    datum: encodeDaBondPoolDatum("Bonded"),
    datumHash: undefined,
    scriptRef: undefined,
    ...overrides,
  });
  const lucidWith = (utxos: readonly UTxO[]) => {
    const calls: [string, string][] = [];
    const lucid = {
      utxosAtWithUnit: (address: string, requested: string) => {
        calls.push([address, requested]);
        return Promise.resolve([...utxos]);
      },
    } as unknown as LucidEvolution;
    return { lucid, calls };
  };

  it("reads the one pool UTxO by its NFT and parses its datum", async () => {
    const utxo = poolUtxo();
    const { lucid, calls } = lucidWith([utxo]);
    const pool = await fetchDaBondPool(lucid, {
      policyId: POLICY_ID,
      address: POOL_ADDRESS,
    });
    expect(calls).toEqual([[POOL_ADDRESS, unit]]);
    expect(pool).toEqual({ utxo, datum: "Bonded" });
    const withBacking = await fetchDaBondPool(lucid, {
      policyId: POLICY_ID,
      address: POOL_ADDRESS,
      parameters,
    });
    expect(withBacking.backing).toBe(300n * ADA);
  });

  it("parses a withdrawing pool", async () => {
    const utxo = poolUtxo({
      datum: encodeDaBondPoolDatum({ Withdrawing: { unlock_at: 42n } }),
    });
    const pool = await fetchDaBondPool(lucidWith([utxo]).lucid, {
      policyId: POLICY_ID,
      address: POOL_ADDRESS,
    });
    expect(pool.datum).toEqual({ Withdrawing: { unlock_at: 42n } });
  });

  it.each([
    // The pool validator compares datums as Data values (TopUp keeps the
    // input datum by value), so anyone may store the same datum this way.
    ["an indefinite-length Bonded datum", "d8799fff", "Bonded"],
    ["a tag-102 Bonded datum", "d866820080", "Bonded"],
    [
      "a definite-length Withdrawing datum",
      "d87a811b000001ba60d33800",
      { Withdrawing: { unlock_at: 1_900_000_000_000n } },
    ],
    [
      "a tag-102 Withdrawing datum",
      "d86682018105",
      { Withdrawing: { unlock_at: 5n } },
    ],
    [
      "a non-minimal unlock_at",
      "d87a9f1b0000000000000005ff",
      { Withdrawing: { unlock_at: 5n } },
    ],
  ] as const)("reads a pool storing %s", async (_label, datum, expected) => {
    const utxo = poolUtxo({ datum });
    const pool = await fetchDaBondPool(lucidWith([utxo]).lucid, {
      policyId: POLICY_ID,
      address: POOL_ADDRESS,
    });
    expect(pool).toEqual({ utxo, datum: expected });
  });

  it.each([
    ["no pool", [], "exactly one DA bond pool UTxO"],
    [
      "an output without the pool NFT",
      [poolUtxo({ assets: { lovelace: 305n * ADA } })],
      "hold the pool NFT exactly once",
    ],
    [
      "trailing bytes after the datum",
      [poolUtxo({ datum: `${encodeDaBondPoolDatum("Bonded")}00` })],
      "not valid Plutus Data",
    ],
    [
      "malformed CBOR",
      [poolUtxo({ datum: "d87a81" })],
      "not valid Plutus Data",
    ],
    [
      "a negative unlock_at",
      [poolUtxo({ datum: Data.to(new Constr(1, [-1n])) })],
      "non-negative unlock_at",
    ],
    ["two pools", [poolUtxo(), poolUtxo({ outputIndex: 1 })], "exactly one"],
    [
      "a doubled NFT",
      [poolUtxo({ assets: { lovelace: 305n * ADA, [unit]: 2n } })],
      "hold the pool NFT exactly once",
    ],
    [
      "another address",
      [poolUtxo({ address: "addr_test1vz" })],
      "sit at the pool address",
    ],
    [
      "a datum hash",
      [poolUtxo({ datum: undefined, datumHash: "ef".repeat(32) })],
      "inline datum",
    ],
    [
      // A provider that resolved a datum-hash output's preimage fills both.
      "a resolved datum hash",
      [poolUtxo({ datumHash: "ef".repeat(32) })],
      "inline datum",
    ],
    [
      "a foreign datum",
      [poolUtxo({ datum: Data.to(new Constr(7, [])) })],
      "not valid Plutus Data",
    ],
  ] as const)("fails closed on %s", async (_label, utxos, reason) => {
    await expect(
      fetchDaBondPool(lucidWith(utxos).lucid, {
        policyId: POLICY_ID,
        address: POOL_ADDRESS,
      }),
    ).rejects.toThrow(reason);
  });
});
