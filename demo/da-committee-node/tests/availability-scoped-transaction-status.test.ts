import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { committeeScopedTransactionStatus } from "../src/l1/availability-scoped-transaction-status.js";

const txHash = "ab".repeat(32),
  blockHash = "cd".repeat(32);
const limits = {
  requestRefusalMs: 1000,
  httpResponseBytes: 4096,
  webSocketMessageBytes: 4096,
  rawUtxos: 2,
};
const output = {
  transaction_id: txHash,
  output_index: 0,
  created_at: { slot_no: 20, header_hash: blockHash },
};
const read = async (raw: unknown) => {
  const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
  try {
    return await committeeScopedTransactionStatus({
      kupoUrl: "http://fixture",
      limits,
      fetchImpl: async (url, init) => {
        expect(String(url)).toBe(`http://fixture/matches/*@${txHash}`);
        expect(init?.signal).toBeInstanceOf(AbortSignal);
        return new Response(JSON.stringify(raw));
      },
    })(txHash, scope);
  } finally {
    scope.close();
  }
};

describe("bounded Kupo status observation", () => {
  it("preserves pinned provider provenance without inventing a height or depth", async () => {
    expect(await read([output, { ...output, output_index: 1 }])).toEqual({
      status: "confirmed",
      txHash,
      confirmation: { txHash, slot: 20, blockHash },
    });
    expect(await read([])).toEqual({ status: "not_found", txHash });
  });
  it.each([
    [output, output],
    [{ ...output, transaction_id: "ef".repeat(32) }],
    [
      output,
      {
        ...output,
        output_index: 1,
        created_at: { slot_no: 21, header_hash: blockHash },
      },
    ],
    [null, null, null],
  ])(
    "holds duplicate, foreign, disagreeing or over-cap provider evidence",
    async (...rows) => {
      await expect(read(rows)).rejects.toThrow();
    },
  );
  it("cancels the actual status request on aggregate scope expiry", async () => {
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 50 });
    let signal: AbortSignal | null | undefined;
    const reader = committeeScopedTransactionStatus({
      kupoUrl: "http://fixture",
      limits,
      fetchImpl: async (_url, init) => {
        signal = init?.signal;
        return new Promise<Response>((_resolve, reject) => {
          signal?.addEventListener("abort", () => reject(signal?.reason), {
            once: true,
          });
        });
      },
    });
    try {
      await expect(reader(txHash, scope)).rejects.toThrow("expired");
      expect(signal?.aborted).toBe(true);
    } finally {
      scope.close();
    }
  });
});
