import { memoizeForRun } from "@al-ft/midgard-test-support/run-shared-fixtures";
import { CML } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import { signAtomicInitialization } from "../initialization-emulator.init-emulator-lucid.js";

/**
 * The protocol initialization a watcher replays: the accepted atomic
 * initialization transaction, the one-shot outpoint it spends, and every
 * transaction that created an output it references.
 */
export type WatcherInitializationFrame = {
  readonly canonicalOneShotOutRef: string;
  readonly transactionId: string;
  readonly transactionCbor: string;
  readonly creatingTransactions: readonly {
    readonly transactionId: string;
    readonly transactionCbor: string;
  }[];
};

/**
 * The wall clock the initialization runs at. The emulator's start time, the
 * reference-script auth policy's deadline (and so its policy id) and every
 * validity interval follow from it, so holding it fixed makes the frame the
 * same bytes on every run of one blueprint.
 */
const INITIALIZATION_CLOCK_UNIX_TIME_MS = 1_788_739_200_000;

const initializeAtFixedClock = async () => {
  const { lucid, nonceUtxo, signed, acceptedFrames } =
    await signAtomicInitialization();
  const transactionId = await signed.submit();
  await lucid.awaitTx(transactionId);
  return { nonceUtxo, signed, transactionId, acceptedFrames };
};

const produceWatcherInitializationFrame =
  async (): Promise<WatcherInitializationFrame> => {
    // Only `Date` is faked: timers stay real, and the clock does not move
    // while the initialization runs.
    vi.useFakeTimers({
      toFake: ["Date"],
      now: INITIALIZATION_CLOCK_UNIX_TIME_MS,
    });
    let initialized: Awaited<ReturnType<typeof initializeAtFixedClock>>;
    try {
      initialized = await initializeAtFixedClock();
    } finally {
      vi.useRealTimers();
    }
    const { nonceUtxo, signed, transactionId, acceptedFrames } = initialized;
    const transactionCbor = signed.toCBOR();
    const references = CML.Transaction.from_cbor_hex(transactionCbor)
      .body()
      .reference_inputs();
    const creatingIds = new Set<string>();
    for (let index = 0; index < (references?.len() ?? 0); index += 1)
      creatingIds.add(references!.get(index).transaction_id().to_hex());
    const creatingTransactions = [...creatingIds].map((id) => {
      const cbor = acceptedFrames.get(id);
      if (cbor === undefined)
        throw new Error(
          `Initialization reference ${id} was not submitted in this emulator`,
        );
      return { transactionId: id, transactionCbor: cbor };
    });
    return {
      canonicalOneShotOutRef: `${nonceUtxo.txHash}#${String(nonceUtxo.outputIndex)}`,
      transactionId,
      transactionCbor,
      creatingTransactions,
    };
  };

/**
 * The initialization frame derived from this run's blueprint by the real
 * node initialization on a deterministic emulator (fixed wallets, clock and
 * protocol parameters), produced once per run. Each call returns its own
 * copy.
 */
export const deriveWatcherInitializationFrame: () => Promise<WatcherInitializationFrame> =
  memoizeForRun(
    "watcher-initialization-frame",
    produceWatcherInitializationFrame,
  );
