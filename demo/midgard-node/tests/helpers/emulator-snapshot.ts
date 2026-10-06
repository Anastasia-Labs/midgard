/**
 * Capturing a deployed emulator as plain data and restoring independent
 * copies of it, for fixtures that deploy once and hand every caller its own
 * chain (`run-shared-fixture-directory.ts`).
 */
import {
  type Emulator,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

/** Plain data of an emulator ledger: every own non-function field. Wrapped
 * provider methods installed on the instance are the only own functions. */
export type EmulatorState = Record<string, unknown>;

export const emulatorState = (emulator: Emulator): EmulatorState =>
  structuredClone(
    Object.fromEntries(
      Object.entries(emulator).filter(
        ([, value]) => typeof value !== "function",
      ),
    ),
  );

/** The UTxO view a wallet has pinned with `overrideUTxOs`, or `undefined`
 * when it reads the provider. The wallet object keeps the pin private, so the
 * provider read is answered with a sentinel for this one call. */
export const pinnedWalletUtxos = async (
  lucid: LucidEvolution,
  emulator: Emulator,
): Promise<UTxO[] | undefined> => {
  const sentinel: UTxO[] = [];
  const own = Object.getOwnPropertyDescriptor(emulator, "getUtxos");
  emulator.getUtxos = () => Promise.resolve(sentinel);
  try {
    const utxos = await lucid.wallet().getUtxos();
    return utxos === sentinel ? undefined : structuredClone(utxos);
  } finally {
    if (own === undefined)
      delete (emulator as Partial<Pick<Emulator, "getUtxos">>).getUtxos;
    else Object.defineProperty(emulator, "getUtxos", own);
  }
};

/** A lucid instance created exactly as the deployment created its own: an
 * emulator lucid reads its slot config from the emulator at creation, so the
 * emulator is put back at that instant while `create` runs. */
export const recreateLucid = async (
  emulator: Emulator,
  at: { readonly time: number; readonly slot: number },
  seedPhrase: string,
  pinned: readonly UTxO[] | undefined,
  create: (emulator: Emulator) => Promise<LucidEvolution>,
) => {
  const { time, slot } = emulator;
  emulator.time = at.time;
  emulator.slot = at.slot;
  let lucid: LucidEvolution;
  try {
    lucid = await create(emulator);
  } finally {
    emulator.time = time;
    emulator.slot = slot;
  }
  lucid.selectWallet.fromSeed(seedPhrase);
  if (pinned !== undefined) lucid.overrideUTxOs(structuredClone([...pinned]));
  return lucid;
};
