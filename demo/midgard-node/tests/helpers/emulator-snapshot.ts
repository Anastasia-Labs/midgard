/**
 * Capturing a deployed emulator as plain data and restoring independent
 * copies of it, for fixtures that deploy once and hand every caller its own
 * chain (`run-shared-fixture-directory.ts`).
 */
import { type Emulator, type LucidEvolution } from "@lucid-evolution/lucid";

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

/** A lucid instance created exactly as the deployment created its own: an
 * emulator lucid reads its slot config from the emulator at creation, so the
 * emulator is put back at that instant while `create` runs. */
export const recreateLucid = async (
  emulator: Emulator,
  at: { readonly time: number; readonly slot: number },
  seedPhrase: string,
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
  return lucid;
};
