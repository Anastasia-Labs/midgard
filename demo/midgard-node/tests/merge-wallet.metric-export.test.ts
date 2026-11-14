/**
 * A merge whose wallet cannot fund it counts on the merge failure counter
 * `MidgardMergeFailures` takes the rate of, read through the node's real
 * Prometheus exporter; a merge it can fund counts nothing.
 */
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { Effect, Layer } from "effect";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  IntentJournal,
  IntentJournalWithoutFollower,
} from "../src/services/intent-journal.js";
import { WalletViewUnavailable } from "../src/services/intent-journal.wallet-view.js";
import {
  readMergeWalletView,
  requireMergeWalletInputs,
} from "../src/transactions/state-queue/merge-to-confirmed-state.merge-wallet.js";
import {
  sampleValue,
  startPrometheusExporter,
} from "./prometheus-scrape.fixture.js";

const MERGE_WALLET = "addr_test1mergewallet";

// Only the selected wallet's address, which the merge reads its view by.
const lucid = {
  wallet: () => ({ address: () => Promise.resolve(MERGE_WALLET) }),
} as unknown as LucidEvolution;

const funding: UTxO = {
  txHash: "ff".repeat(32),
  outputIndex: 1,
  address: MERGE_WALLET,
  assets: { lovelace: 20_000_000n },
  datum: null,
  datumHash: null,
  scriptRef: null,
};

/** A journal whose merge wallet view holds `utxos`, or cannot be read. */
const journal = (utxos: readonly UTxO[] | "unavailable") =>
  Layer.succeed(IntentJournal, {
    ...Effect.runSync(
      Effect.provide(IntentJournal, IntentJournalWithoutFollower),
    ),
    walletView: (_lucid, address) =>
      utxos === "unavailable"
        ? Effect.fail(
            new WalletViewUnavailable({
              address,
              message: "the follower store is unreachable",
              cause: "test",
            }),
          )
        : Effect.succeed({
            address,
            source: "follower" as const,
            utxos,
            predictedBy: new Map<string, string>(),
            held: new Set<string>(),
          }),
  });

/** The merge's wallet reads, as the merge builder runs them. */
const fundMerge = (utxos: readonly UTxO[] | "unavailable") =>
  Effect.runPromise(
    readMergeWalletView(lucid).pipe(
      Effect.flatMap((view) => requireMergeWalletInputs(view.utxos)),
      Effect.provide(journal(utxos)),
    ),
  );

let exporter: Awaited<ReturnType<typeof startPrometheusExporter>>;
beforeAll(async () => {
  exporter = await startPrometheusExporter();
});
afterAll(async () => {
  await exporter.stop();
});

const mergeFailures = async () =>
  sampleValue(await exporter.scrape(), "merge_failure_count_total") ?? 0;

describe("merge wallet funding", () => {
  it("counts an empty or unreadable merge wallet as a merge failure, and a funded one as none", async () => {
    const before = await mergeFailures();

    expect(await fundMerge([funding])).toEqual([funding]);
    expect(await mergeFailures()).toBe(before);

    await expect(fundMerge([])).rejects.toThrow(
      "No operator wallet inputs available to fund state_queue merge tx",
    );
    expect(await mergeFailures()).toBe(before + 1);

    await expect(fundMerge("unavailable")).rejects.toThrow(
      "Failed to read the merge wallet view",
    );
    expect(await mergeFailures()).toBe(before + 2);
  });
});
