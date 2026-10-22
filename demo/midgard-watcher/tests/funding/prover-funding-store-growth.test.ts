import { rm } from "node:fs/promises";

import { afterEach, describe, expect, it } from "vitest";

import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationStore,
} from "../../src/funding/prover-funding-reservation.js";
import {
  openStore,
  plan,
  prepareTransition,
  signedTransition,
  temporaryDirectories,
} from "./sqlite-prover-funding-reservation-store.signed-transition.js";

afterEach(async () => {
  await Promise.all(
    temporaryDirectories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

const currentPlan = plan("aa", "66");
const reservationId = currentPlan.reservationId;
// Further inputs the reservation does not own give each signed attempt the
// bulk of a real proof step, so a store that re-verifies them shows it.
const protocolInputHashes = Array.from({ length: 24 }, (_, index) =>
  (index + 0x30).toString(16).padStart(2, "0").repeat(32),
);

type Chain = {
  readonly store: WatcherProverFundingReservationStore;
  readonly close: () => void;
  record: WatcherProverFundingReservationRecord;
  inputHash: string;
  retained: number;
};

/** A reservation whose record actions each sign a chained attempt. */
const openChain = async (): Promise<Chain> => {
  const opened = await openStore();
  const store = opened.runtime.store;
  await store.reserve(currentPlan);
  return {
    store,
    close: opened.runtime.close,
    record: parseWatcherProverFundingReservationRecord(
      (await store.readAll())[0],
    ),
    inputHash: "11".repeat(32),
    retained: 0,
  };
};

/** One record action: prepare and confirm the next signed attempt. */
const advance = async (chain: Chain) => {
  chain.retained += 1;
  const signed = signedTransition({
    inputHash: chain.inputHash,
    outputLovelace: 99_000_000n - BigInt(chain.retained),
    protocolInputHashes,
  });
  const started = performance.now();
  const prepared = await prepareTransition(chain.store, {
    plan: currentPlan,
    expectedRevision: chain.record.revision,
    actionKind: "proof.step",
    ...signed,
    consumedOutRefs: [`${chain.inputHash}#0`],
  });
  chain.record = await chain.store.confirmTransition({
    journal: [],
    plan: currentPlan,
    expectedRevision: prepared.revision,
    transactionHash: signed.transactionHash,
    transitionDigest: prepared.pendingTransition!.transitionDigest,
  });
  chain.inputHash = signed.transactionHash;
  return performance.now() - started;
};

const cheapestAction = async (chain: Chain, samples: number) => {
  let best = Number.POSITIVE_INFINITY;
  for (let sample = 0; sample < samples; sample += 1)
    best = Math.min(best, await advance(chain));
  return best;
};

/** The reads a funded workflow makes around each of its actions. */
const readRound = async ({ store, inputHash }: Chain) => {
  await store.readAll();
  await store.readPendingTransition({ reservationId });
  await store.readLegacyAbandonedTransactions!({ reservationId });
  await store.isReconciliationOnly!({ reservationId });
  await store.assertSubmissionAuthority!({ reservationId });
  await store.readReservedOutRefs({ excludingReservationId: reservationId });
  await store.readSupersededAttemptFundingOutRefs!({ reservationId });
  await store.hasSignedHistory!({ reservationId });
  await store.readConfirmedInput({ reservationId, outRef: `${inputHash}#0` });
};

const readBatch = async (chain: Chain) => {
  const started = performance.now();
  for (let round = 0; round < 10; round += 1) await readRound(chain);
  return (performance.now() - started) / 10;
};

describe("prover funding store growth", () => {
  it("keeps per-read and per-action cost flat as a record retains submissions", async () => {
    const small = await openChain();
    const large = await openChain();
    try {
      while (small.retained < 12) await advance(small);
      const baseline = await cheapestAction(small, 6);
      // A store that re-verifies every retained submission is caught on the
      // way to sixteen times as many, rather than after it.
      while (large.retained < 192)
        expect((await cheapestAction(large, 6)) / baseline).toBeLessThan(6);
      // Alternating samples between the two records, so machine load falls
      // on both alike; the large one keeps about eight times as many.
      const cost = {
        small: { read: Infinity, action: Infinity },
        large: { read: Infinity, action: Infinity },
      };
      for (let round = 0; round < 8; round += 1)
        for (const [chain, key] of [
          [small, "small"],
          [large, "large"],
        ] as const) {
          cost[key].read = Math.min(cost[key].read, await readBatch(chain));
          cost[key].action = Math.min(cost[key].action, await advance(chain));
        }
      expect(cost.large.read / cost.small.read).toBeLessThan(2);
      expect(cost.large.action / cost.small.action).toBeLessThan(3);
    } finally {
      small.close();
      large.close();
    }
  }, 600_000);
});
