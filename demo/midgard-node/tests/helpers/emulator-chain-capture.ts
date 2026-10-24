/**
 * Every transaction a lucid `Emulator` accepts, kept as its exact bytes, and
 * the emulator's genesis outputs as they stood before its first submission
 * (`Emulator.prototype.submitTx` is wrapped when this module loads). A
 * follower replay (`helpers/intent-journal-replay.ts`) rebuilds the chain's
 * blocks from them.
 *
 * The capture is plain data in an own property of the emulator, so every
 * snapshot of an emulator's state (`emulatorState`, a fixture shared across
 * the run's processes) carries it, and a restored emulator's chain is
 * complete from its genesis.
 */
import { Emulator, type UTxO } from "@lucid-evolution/lucid";

/** The emulator's genesis transaction hash. */
export const GENESIS_HASH = "00".repeat(32);

const CAPTURE = "capturedChain";

type Capture = { genesis: UTxO[] | null; txs: Map<string, string> };
type Capturing = Emulator & { [CAPTURE]?: Capture };

export type CapturedChain = Readonly<{
  genesis: readonly UTxO[] | null;
  /** Each accepted transaction's exact bytes, by hash. */
  tx: (hash: string) => Buffer | undefined;
  has: (hash: string) => boolean;
}>;

const captureOf = (emulator: Capturing): Capture =>
  (emulator[CAPTURE] ??= { genesis: null, txs: new Map() });

/** `emulator`'s capture so far (empty for one that has accepted nothing). */
export const capturedChainOf = (emulator: Emulator): CapturedChain => {
  const capture = captureOf(emulator);
  return {
    genesis: capture.genesis,
    tx: (hash) => {
      const hex = capture.txs.get(hash);
      return hex === undefined ? undefined : Buffer.from(hex, "hex");
    },
    has: (hash) => capture.txs.has(hash),
  };
};

const submitTx = Emulator.prototype.submitTx;
Emulator.prototype.submitTx = function (
  this: Capturing,
  tx: string,
): Promise<string> {
  const capture = captureOf(this);
  capture.genesis ??= Object.values(this.ledger)
    .map(({ utxo }) => structuredClone(utxo))
    .filter((utxo) => utxo.txHash === GENESIS_HASH);
  return submitTx.call(this, tx).then((hash) => {
    capture.txs.set(hash, tx);
    return hash;
  });
};

/** The capture as plain data, for a snapshot that copies fields one by one. */
export type CaptureSnapshot = Readonly<{ capturedChain?: Capture }>;

export const snapshotCapture = (emulator: Emulator): CaptureSnapshot => ({
  capturedChain: structuredClone((emulator as Capturing)[CAPTURE]),
});

/** Gives an emulator rebuilt from a snapshot the capture it was taken with. */
export const restoreCapture = (
  emulator: Emulator,
  snapshot: CaptureSnapshot,
): void => {
  if (snapshot.capturedChain !== undefined)
    (emulator as Capturing)[CAPTURE] = structuredClone(snapshot.capturedChain);
};
