import { setTimeout as pause } from "node:timers/promises";

import type { recordCrossBlockRawEmulator } from "@al-ft/midgard-fault-proofs/test-support/cross-block-raw-emulator";
import type { Emulator } from "@lucid-evolution/lucid";

import type {
  SyntheticNativeTip,
  SyntheticUserEventOriginFixture,
} from "./user-event-origin-fixture.js";

/** Actual emulator confirmations transported into the native chain fixture.
 * One queue owns submission, inclusion and clock growth. This changes only the
 * local chain transport; watcher decisions and finality are untouched. */
export const createEmulatorChainTransport = async ({
  emulator,
  native,
  recorder,
  initializationTxHash,
  confirmationBlocksPerSubmission = 0,
}: Readonly<{
  emulator: Emulator;
  native: SyntheticUserEventOriginFixture;
  recorder: Pick<ReturnType<typeof recordCrossBlockRawEmulator>, "signedCbors">;
  initializationTxHash: string;
  /** Produce real empty native blocks after each new accepted transaction. */
  confirmationBlocksPerSubmission?: number;
}>) => {
  if (
    !Number.isSafeInteger(confirmationBlocksPerSubmission) ||
    confirmationBlocksPerSubmission < 0 ||
    confirmationBlocksPerSubmission > 100
  )
    throw new Error(
      "Emulator confirmation blocks per submission must be an integer from 0 to 100",
    );
  const confirmed = async (txHash: string) => {
    const status = await emulator.getTransactionStatus(txHash);
    if (
      status.status !== "confirmed" ||
      status.confirmation.slot === undefined ||
      status.confirmation.blockHeight === undefined
    )
      throw new Error(
        `Emulator transaction ${txHash} has no actual inclusion coordinates`,
      );
    return {
      txHash,
      slot: status.confirmation.slot,
      blockHeight: status.confirmation.blockHeight,
    };
  };
  const initialization = await confirmed(initializationTxHash);
  if (BigInt(native.activationBlock.point.slot) !== BigInt(initialization.slot))
    throw new Error(
      "Native activation slot differs from the emulator confirmation; pass published.inclusionSlot",
    );
  let tip: SyntheticNativeTip = native.emptySuccessorBlock.point;
  await native.setNativeTip(tip);
  const hashes = [...recorder.signedCbors.keys()];
  const position = hashes.indexOf(initializationTxHash);
  if (position < 0)
    throw new Error("Initialization transaction is absent from the recorder");
  const groups: {
    slot: number;
    blockHeight: number;
    transactions: string[];
  }[] = [];
  for (const hash of hashes.slice(position + 1)) {
    const inclusion = await confirmed(hash);
    const previous = groups.at(-1);
    const cbor = recorder.signedCbors.get(hash)!;
    if (
      previous?.slot === inclusion.slot &&
      previous.blockHeight === inclusion.blockHeight
    )
      previous.transactions.push(cbor);
    else groups.push({ ...inclusion, transactions: [cbor] });
  }
  for (const group of groups) {
    const block = await native.appendNativeBlock(group);
    tip = block.point;
  }
  let queue: Promise<void> = Promise.resolve();
  let closed = false;
  let failure: unknown;
  const serialize = <T>(action: () => Promise<T>): Promise<T> => {
    const result = queue.then(async () => {
      if (closed) throw new Error("Emulator chain transport is closed");
      if (failure !== undefined) throw failure;
      return await action();
    });
    queue = result.then(
      () => undefined,
      () => undefined,
    );
    return result;
  };
  const advanceEmulator = (slot: number) => {
    if (slot > emulator.slot) emulator.awaitSlot(slot - emulator.slot);
  };
  const previousSubmit = Object.getOwnPropertyDescriptor(emulator, "submitTx");
  const submit = emulator.submitTx.bind(emulator);
  Object.defineProperty(emulator, "submitTx", {
    configurable: true,
    writable: true,
    value: (cbor: string) =>
      serialize(async () => {
        advanceEmulator(Number(tip.slot));
        const txHash = await submit(cbor);
        // awaitTx produces the emulator's real ledger inclusion, rather than
        // inventing a slot from the transaction's lower validity bound.
        await emulator.awaitTx(txHash);
        const inclusion = await confirmed(txHash);
        try {
          const block = await native.appendNativeBlock({
            transactions: [cbor],
            slot: inclusion.slot,
          });
          tip = block.point;
          if (confirmationBlocksPerSubmission > 0) {
            tip = await native.growNativeTip(
              confirmationBlocksPerSubmission,
              emulator.slot,
            );
            advanceEmulator(Number(tip.slot));
          }
        } catch (cause) {
          failure = cause;
          throw cause;
        }
        return txHash;
      }),
  });
  const growTip = async (count: number) => {
    tip = await native.growNativeTip(count, emulator.slot);
    advanceEmulator(Number(tip.slot));
    return tip;
  };
  const grow = (count = 1) => serialize(() => growTip(count));
  let pausedBackgroundGrowth = 0;
  /** Let a fixture producer construct against a stable clock while its actual
   * submissions still produce normal inclusion and confirmation blocks. */
  const withPausedBackgroundGrowth = async <T>(
    action: () => Promise<T>,
  ): Promise<T> => {
    pausedBackgroundGrowth += 1;
    try {
      // Drain an already-running tick before the producer reads the clock.
      // Do not hold the queue while it builds: its submissions use that queue.
      await serialize(async () => undefined);
      return await action();
    } finally {
      pausedBackgroundGrowth -= 1;
    }
  };
  const abort = new AbortController();
  let growth: Promise<void> | undefined;
  const start = ({
    intervalMs = 1_000,
    blocksPerTick = 1,
  }: Readonly<{ intervalMs?: number; blocksPerTick?: number }> = {}) => {
    if (growth !== undefined || closed)
      throw new Error("Emulator chain growth is already started or closed");
    if (
      !Number.isSafeInteger(intervalMs) ||
      intervalMs < 10 ||
      intervalMs > 60_000 ||
      !Number.isSafeInteger(blocksPerTick) ||
      blocksPerTick < 1 ||
      blocksPerTick > 100
    )
      throw new Error(
        "Emulator chain growth settings are outside fixture bounds",
      );
    growth = (async () => {
      while (!abort.signal.aborted) {
        await pause(intervalMs, undefined, { signal: abort.signal });
        await serialize(async () => {
          if (pausedBackgroundGrowth === 0) await growTip(blocksPerTick);
        });
      }
    })().catch((cause: unknown) => {
      if (!abort.signal.aborted) failure = cause;
    });
  };
  return {
    grow,
    start,
    withPausedBackgroundGrowth,
    tip: () => tip,
    assertHealthy: () => {
      if (failure !== undefined) throw failure;
    },
    close: async () => {
      abort.abort();
      await growth;
      await queue;
      closed = true;
      if (previousSubmit === undefined)
        delete (emulator as Partial<Emulator>).submitTx;
      else Object.defineProperty(emulator, "submitTx", previousSubmit);
      if (failure !== undefined) throw failure;
    },
  };
};
