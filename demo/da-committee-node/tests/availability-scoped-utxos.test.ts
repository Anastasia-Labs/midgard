import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  committeeScopedOutRefs,
  committeeScopedUtxos,
} from "../src/l1/availability-scoped-utxos.js";

const limits = {
  requestRefusalMs: 1000,
  httpResponseBytes: 10000,
  webSocketMessageBytes: 10000,
  rawUtxos: 2,
};
const row = {
  transaction_id: "ab".repeat(32),
  output_index: 0,
  address: "fixture-address",
  spent_at: null,
  value: { coins: "2000000", assets: { [`${"cd".repeat(28)}.01`]: 2 } },
  datum_type: "inline",
  datum: "d87980",
  datum_hash: "ef".repeat(32),
  script: null,
};
const read = async (data: unknown) => {
  const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
  try {
    return await committeeScopedUtxos({
      kupoUrl: "http://fixture",
      limits,
      fetchImpl: async (url, init) => {
        expect(String(url)).toBe(
          "http://fixture/matches/fixture-address?unspent&resolve_hashes",
        );
        expect(init?.signal).toBeInstanceOf(AbortSignal);
        return new Response(JSON.stringify(data));
      },
    })(row.address, scope);
  } finally {
    scope.close();
  }
};
describe("scoped resolved Kupo outputs", () => {
  it("preserves exact string quantities, inline bytes and asset identity", async () => {
    expect(await read([row])).toEqual([
      {
        txHash: row.transaction_id,
        outputIndex: 0,
        address: row.address,
        assets: { lovelace: 2000000n, [`${"cd".repeat(28)}01`]: 2n },
        datum: "d87980",
        datumHash: undefined,
        scriptRef: undefined,
      },
    ]);
  });
  it("refuses excess raw rows before trying to convert their malformed contents", async () => {
    await expect(read([null, null, null])).rejects.toThrow("raw UTxO count");
  });
  it.each([
    { ...row, address: "foreign" },
    { ...row, spent_at: { slot_no: 1 } },
    { ...row, value: { coins: 9007199254740992, assets: {} } },
    { ...row, value: { coins: "18446744073709551616", assets: {} } },
    { ...row, datum: "not-cbor" },
  ])("refuses foreign, spent, inexact or malformed outputs", async (bad) => {
    await expect(read([bad])).rejects.toThrow();
  });
  it("refuses duplicate outrefs", async () => {
    await expect(read([row, row])).rejects.toThrow("duplicate");
  });
  it("reads exact requested unspent refs with the same bounded transport", async () => {
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    try {
      const read = committeeScopedOutRefs({
        kupoUrl: "http://fixture",
        limits,
        fetchImpl: async (url, init) => {
          expect(String(url)).toBe(
            `http://fixture/matches/*@${row.transaction_id}?unspent&resolve_hashes`,
          );
          expect(init?.signal).toBeInstanceOf(AbortSignal);
          return new Response(
            JSON.stringify([row, { ...row, output_index: 1 }]),
          );
        },
      });
      const outputs = await read(
        [{ txHash: row.transaction_id, outputIndex: 0 }],
        scope,
      );
      expect(outputs).toHaveLength(1);
      expect(outputs[0]?.txHash).toBe(row.transaction_id);
      expect(outputs[0]?.outputIndex).toBe(0);
    } finally {
      scope.close();
    }
  });
  it("holds a foreign transaction or an over-cap exact input lookup before using it", async () => {
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    let calls = 0;
    const read = committeeScopedOutRefs({
      kupoUrl: "http://fixture",
      limits,
      fetchImpl: async () => {
        calls++;
        return new Response(
          JSON.stringify([{ ...row, transaction_id: "cd".repeat(32) }]),
        );
      },
    });
    try {
      await expect(
        read([{ txHash: row.transaction_id, outputIndex: 0 }], scope),
      ).rejects.toThrow("foreign transaction");
      await expect(
        read(
          [0, 1, 2].map((outputIndex) => ({
            txHash: row.transaction_id,
            outputIndex,
          })),
          scope,
        ),
      ).rejects.toThrow("lookup count");
      expect(calls).toBe(1);
    } finally {
      scope.close();
    }
  });
});
