import * as SDK from "@al-ft/midgard-sdk";
import {
  kupoExchange,
  type L1Recording,
  loadL1Recording,
  ogmiosExchanges,
  recordedFetch,
  recordedOgmiosWebSocket,
  recordedTransaction,
} from "@al-ft/midgard-test-support/l1-recordings";
import { describe, expect, it } from "vitest";

import { availabilityForeignSpendReaders } from "../src/availability/factory.js";
import type { StateQueueReplayWebSocket } from "../src/l1/state-queue-replay-provider.js";

/**
 * The committee's Kupo and Ogmios readers behind a rival-spend check, driven
 * by a recorded preprod state-queue timeout removal: Kupo's `spent_at` names
 * the removal, Ogmios serves its block, and only the removal's own bytes can
 * prove it consumed the input.
 */

const REMOVAL = "preprod-state-queue-removal-a2a47d2e";
const SPEND_BLOCK_NO = 5_220_548;

const removal = loadL1Recording(REMOVAL);
const removalBlock = removal.transaction!.block;
const removalInputs = (
  recordedTransaction(removal).inputs as {
    transaction: { id: string };
    index: number;
  }[]
).map((input) => `${input.transaction.id}#${input.index.toString()}`);
const spentInput = removalInputs[0]!;

const matchPath = (outRef: string): string => {
  const [txHash, index] = outRef.split("#");
  return `/matches/${index!}@${txHash!}?resolve_hashes`;
};

const matchesOf = (
  recording: L1Recording,
  outRef: string,
): Record<string, unknown>[] =>
  kupoExchange(recording, matchPath(outRef)).response.body as Record<
    string,
    unknown
  >[];

const resolve = (recording: L1Recording, outRef = spentInput) => {
  const ogmios = recordedOgmiosWebSocket(recording);
  return SDK.resolveDaAvailabilityForeignSpend({
    ...availabilityForeignSpendReaders({
      kupoUrl: "http://kupo.recorded",
      ogmiosUrl: "ws://ogmios.recorded",
      fetchImpl: recordedFetch(recording),
      webSocketFactory: (url) =>
        new ogmios.WebSocket(url) as unknown as StateQueueReplayWebSocket,
    }),
    readBoundary: async () => ({
      pointId: "tip",
      blockNo: SPEND_BLOCK_NO + 40,
    }),
    outRef,
  });
};

describe("committee readers for an availability rival spend", () => {
  it("proves the recorded removal consumed its input", async () => {
    expect(removalInputs).toContain(spentInput);
    await expect(resolve(removal)).resolves.toStrictEqual({
      outRef: spentInput,
      spendingTxHash: removal.transaction!.id,
      spendPoint: `${removalBlock.slot.toString()}:${removalBlock.id}`,
      confirmationDepth: 40,
      spendingTransactionCbor: (
        recordedTransaction(removal) as { cbor: string }
      ).cbor,
    });
  });

  it("reads an output Kupo does not know as no spend", async () => {
    const recording = loadL1Recording(REMOVAL);
    matchesOf(recording, spentInput).length = 0;
    await expect(resolve(recording)).resolves.toBeUndefined();
  });

  it("propagates an output Kupo knows more than once", async () => {
    const recording = loadL1Recording(REMOVAL);
    const matches = matchesOf(recording, spentInput);
    matches.push({ ...matches[0]! });
    await expect(resolve(recording)).rejects.toThrow(/exactly once/);
  });

  it("refuses a spent_at whose transaction the removal does not list", async () => {
    // Kupo's answer for an output the removal never spent, pointing at it.
    const recording = loadL1Recording(REMOVAL);
    const unlisted = `${"12".repeat(32)}#0`;
    const exchange = kupoExchange(recording, matchPath(spentInput));
    recording.exchanges.push({
      ...exchange,
      request: { ...exchange.request, path: matchPath(unlisted) },
      response: {
        status: exchange.response.status,
        headers: exchange.response.headers,
        body: [
          {
            ...matchesOf(recording, spentInput)[0]!,
            transaction_id: "12".repeat(32),
            output_index: 0,
          },
        ],
      },
    });
    await expect(resolve(recording, unlisted)).resolves.toBeUndefined();
  });

  it("refuses a spent_at naming a transaction its block does not carry", async () => {
    const recording = loadL1Recording(REMOVAL);
    const [match] = matchesOf(recording, spentInput);
    match!.spent_at = {
      ...(match!.spent_at as Record<string, unknown>),
      transaction_id: "ee".repeat(32),
    };
    await expect(resolve(recording)).resolves.toBeUndefined();
  });

  it("names the Ogmios flag when the block is served without raw transactions", async () => {
    const recording = loadL1Recording(REMOVAL);
    for (const exchange of ogmiosExchanges(recording, "nextBlock")) {
      const body = exchange.response.body as {
        result?: { block?: { transactions?: Record<string, unknown>[] } };
      };
      for (const transaction of body.result?.block?.transactions ?? [])
        delete transaction.cbor;
    }
    await expect(resolve(recording)).rejects.toThrow(
      "Ogmios must run with --include-transaction-cbor to verify a rival spend",
    );
  });
});
