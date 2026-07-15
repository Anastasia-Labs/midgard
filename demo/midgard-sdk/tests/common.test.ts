import {
  Emulator,
  Kupmios,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { utxosAtByNFTPolicyId } from "../src/common.js";
import { LucidError } from "../src/errors.js";

const address = "addr_test1_policy_query";
const policyId = "aa".repeat(28);
const foreignPolicyId = "bb".repeat(28);

const utxo = (
  txByte: string,
  policy: string,
  assetName: string,
  outputIndex = 0,
): UTxO => ({
  txHash: txByte.repeat(32),
  outputIndex,
  address,
  assets: {
    lovelace: 2_000_000n,
    [`${policy}${assetName}`]: 1n,
  },
});

const lucidFixture = ({
  provider,
  addressResult,
  policyResult,
}: {
  readonly provider: unknown;
  readonly addressResult: unknown;
  readonly policyResult: unknown;
}) => {
  const utxosAt = vi.fn(async () => addressResult);
  const utxosAtWithUnit = vi.fn(async () => policyResult);
  return {
    lucid: {
      config: () => ({ provider }),
      utxosAt,
      utxosAtWithUnit,
    } as unknown as LucidEvolution,
    utxosAt,
    utxosAtWithUnit,
  };
};

describe("utxosAtByNFTPolicyId", () => {
  it("uses address-wide filtering for Emulator providers", async () => {
    const matching = utxo("11", policyId, "01");
    const foreign = utxo("22", foreignPolicyId, "02", 1);
    const fixture = lucidFixture({
      provider: new Emulator([]),
      addressResult: [foreign, matching],
      policyResult: [],
    });

    const result = await Effect.runPromise(
      utxosAtByNFTPolicyId(fixture.lucid, address, policyId),
    );

    expect(fixture.utxosAt).toHaveBeenCalledWith(address);
    expect(fixture.utxosAtWithUnit).not.toHaveBeenCalled();
    expect(result).toEqual([{ utxo: matching, policyId, assetName: "01" }]);
  });

  it("retains the Kupmios policy-scoped fast path", async () => {
    const matching = utxo("33", policyId, "03");
    const fixture = lucidFixture({
      provider: new Kupmios("http://kupo.test", "ws://ogmios.test"),
      addressResult: new Error("address-wide query must not run"),
      policyResult: [matching],
    });

    const result = await Effect.runPromise(
      utxosAtByNFTPolicyId(fixture.lucid, address, policyId),
    );

    expect(fixture.utxosAtWithUnit).toHaveBeenCalledWith(address, policyId);
    expect(fixture.utxosAt).not.toHaveBeenCalled();
    expect(result).toEqual([{ utxo: matching, policyId, assetName: "03" }]);
  });

  it("does not infer policy-query support from exact-unit method presence", async () => {
    const matching = utxo("44", policyId, "04");
    const fixture = lucidFixture({
      provider: { kind: "non-kupmios-exact-unit-provider" },
      addressResult: [matching],
      // An exact-unit provider interprets the policy id as a complete unit and
      // would return a false empty result if this method were selected.
      policyResult: [],
    });

    const result = await Effect.runPromise(
      utxosAtByNFTPolicyId(fixture.lucid, address, policyId),
    );

    expect(fixture.utxosAt).toHaveBeenCalledWith(address);
    expect(fixture.utxosAtWithUnit).not.toHaveBeenCalled();
    expect(result).toEqual([{ utxo: matching, policyId, assetName: "04" }]);
  });

  it("falls back when Kupmios is configured without the Lucid policy-query method", async () => {
    const matching = utxo("66", policyId, "06");
    const utxosAt = vi.fn(async () => [matching]);
    const lucid = {
      config: () => ({
        provider: new Kupmios("http://kupo.test", "ws://ogmios.test"),
      }),
      utxosAt,
    } as unknown as LucidEvolution;

    const result = await Effect.runPromise(
      utxosAtByNFTPolicyId(lucid, address, policyId),
    );

    expect(utxosAt).toHaveBeenCalledWith(address);
    expect(result).toEqual([{ utxo: matching, policyId, assetName: "06" }]);
  });

  it("fails with LucidError when a provider returns malformed UTxOs", async () => {
    const malformed = {
      ...utxo("55", policyId, "05"),
      assets: { [`${policyId}05`]: "1" },
    };
    const fixture = lucidFixture({
      provider: new Emulator([]),
      addressResult: [malformed],
      policyResult: [],
    });

    const result = await Effect.runPromise(
      Effect.either(utxosAtByNFTPolicyId(fixture.lucid, address, policyId)),
    );

    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left).toBeInstanceOf(LucidError);
      expect(result.left.message).toBe(`Failed to fetch UTxOs at: ${address}`);
    }
  });
});
