import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
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

const lucidFixture = (policyResult: unknown) => {
  const utxosAtWithPolicy = vi.fn(async () => policyResult);
  return {
    lucid: { utxosAtWithPolicy } as unknown as LucidEvolution,
    utxosAtWithPolicy,
  };
};

describe("utxosAtByNFTPolicyId", () => {
  it("uses Lucid's provider-neutral policy query and reauthenticates results", async () => {
    const matching = utxo("11", policyId, "01");
    const foreign = utxo("22", foreignPolicyId, "02", 1);
    const fixture = lucidFixture([foreign, matching]);

    const result = await Effect.runPromise(
      utxosAtByNFTPolicyId(fixture.lucid, address, policyId),
    );

    expect(fixture.utxosAtWithPolicy).toHaveBeenCalledWith(address, policyId);
    expect(result).toEqual([{ utxo: matching, policyId, assetName: "01" }]);
  });

  it("wraps provider failures as LucidError", async () => {
    const utxosAtWithPolicy = vi.fn(() =>
      Promise.reject(new Error("provider unavailable")),
    );
    const lucid = { utxosAtWithPolicy } as unknown as LucidEvolution;

    await expect(
      Effect.runPromise(utxosAtByNFTPolicyId(lucid, address, policyId)),
    ).rejects.toMatchObject({
      message: `Failed to fetch UTxOs at: ${address}`,
    });
  });

  it("fails with LucidError when a provider returns malformed UTxOs", async () => {
    const malformed = {
      ...utxo("55", policyId, "05"),
      assets: { [`${policyId}05`]: "1" },
    };
    const fixture = lucidFixture([malformed]);

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
