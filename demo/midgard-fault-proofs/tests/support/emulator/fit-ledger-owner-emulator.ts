import {
  type Assets,
  CML,
  type Emulator,
  type EmulatorAccount,
  generateEmulatorAccount,
  walletFromSeed,
} from "@lucid-evolution/lucid";

import {
  emulatorAccountFromSeed,
  fundedProverEmulatorAccount,
} from "./emulator-context.js";

/**
 * Fixed wallets and a fixed start time for the suites that write a
 * checked-in fit ledger. Random wallets and an emulator clock started at
 * `Date.now()` change transaction ids from run to run, and with them
 * hash-ordered positions, encoded lengths and execution budgets, so
 * `fit:regenerate --check` could not hold a ledger to a fresh run.
 *
 * A fit-ledger owner opts in once, at the top of its file, with
 * `fixFitLedgerOwnerEmulator()`. Each Vitest file runs in its own process
 * (`isolatedForksPool`), so the opt-in reaches only that file; every other
 * suite keeps random wallets and the wall-clock start. Each role has its own
 * phrase.
 */
export const FIT_LEDGER_OWNER_EMULATOR = {
  /** 2025-01-01T00:00:00Z, a whole second (`alignEmulatorToWholeSecond`). */
  emulatorTimeMs: 1_735_689_600_000,
  seedPhrases: {
    funder:
      "snake horror hour depth estate nominee pipe attend almost permit crack property unit wrestle sketch onion goddess turn adjust still make weekend yard space",
    prover:
      "jungle habit fame frozen panda quick stumble quarter buddy legal what excite gesture nephew across praise camera pudding shed rhythm erupt eight chest now",
    publisher:
      "spare empty rack valid peanut select poverty sweet push demise rubber joy retire pond obtain turn glory cliff provide wrestle palm tomato proud chalk",
    historyPair:
      "sister analyst scare morning disease toward country inherit path enjoy margin narrow need title topic embody liberty next unfair slice camp fall merit battle",
    deploymentCosigner:
      "welcome gravity model judge ensure dutch theory unhappy ethics deputy cook gold apology express awesome hip dentist barely hat elegant donkey match faith rigid",
    decodingOutsider:
      "key voice defy belt left system isolate just left history ketchup shift ride dawn lunch potato worth heavy vessel walnut into private trumpet next",
    withdrawalOwner:
      "around report desk boil inherit frog marble sing lemon clever sleep ceiling orbit type organ wealth orbit immense north ten need estate chief fiscal",
    cekContextPublisher:
      "clog else depart tissue link alpha impact stand radio shoulder test twenty cause idle before dust beef resource judge income wide mesh sea spoon",
    cekMaterialWallet:
      "letter federal heavy universe juice buddy unfold pretty spring pact young license usual model bronze simple stable income fantasy happy recycle armor quality urge",
    resolverPublisher:
      "service blast orient siren side season gallery exile permit ensure table heart rude crystal inside benefit cube laugh pattern pilot birth twice quantum fly",
  },
} as const;

export type FitLedgerOwnerRole =
  keyof typeof FIT_LEDGER_OWNER_EMULATOR.seedPhrases;

let fixed = false;

/** Fixes this file's emulator wallets and start time; see above. */
export const fixFitLedgerOwnerEmulator = (): void => {
  fixed = true;
};

/** `role`'s fixed phrase once the file opted in, otherwise undefined. */
export const fitLedgerOwnerSeedPhrase = (
  role: FitLedgerOwnerRole,
): string | undefined =>
  fixed ? FIT_LEDGER_OWNER_EMULATOR.seedPhrases[role] : undefined;

/** `role`'s fixed account once the file opted in, otherwise a random one. */
export const fitLedgerOwnerAccount = (
  role: FitLedgerOwnerRole,
  lovelace: bigint,
  assets: Assets = {},
): EmulatorAccount =>
  fixed
    ? {
        ...emulatorAccountFromSeed(
          lovelace,
          FIT_LEDGER_OWNER_EMULATOR.seedPhrases[role],
        ),
        assets: { lovelace, ...assets },
      }
    : generateEmulatorAccount({ lovelace, ...assets });

/**
 * The harness funder and prover: the caller's, else the fixed ones once the
 * file opted in, else random ones.
 */
export const fitLedgerOwnerHarnessAccounts = (accounts?: {
  readonly funder: EmulatorAccount;
  readonly prover: EmulatorAccount;
}): { funder: EmulatorAccount; prover: EmulatorAccount } => ({
  funder: accounts?.funder ?? fitLedgerOwnerAccount("funder", 40_000_000_000n),
  prover:
    accounts?.prover ??
    fundedProverEmulatorAccount(
      20_000_000_000n,
      fitLedgerOwnerSeedPhrase("prover"),
    ),
});

/** `role`'s fixed payment key once the file opted in, otherwise a random one. */
export const fitLedgerOwnerPrivateKey = (
  role: FitLedgerOwnerRole,
): CML.PrivateKey =>
  fixed
    ? CML.PrivateKey.from_bech32(
        walletFromSeed(FIT_LEDGER_OWNER_EMULATOR.seedPhrases[role], {
          addressType: "Enterprise",
          accountIndex: 0,
          network: "Custom",
        }).paymentKey,
      )
    : CML.PrivateKey.generate_ed25519();

/** The fixed start time once the file opted in, otherwise undefined. */
export const fitLedgerOwnerEmulatorTimeMs = (): number | undefined =>
  fixed ? FIT_LEDGER_OWNER_EMULATOR.emulatorTimeMs : undefined;

/**
 * Starts `emulator` at `emulatorTimeMs`, or at the fixed time once the file
 * opted in; otherwise leaves its wall-clock start.
 */
export const startFitLedgerOwnerEmulator = (
  emulator: Emulator,
  emulatorTimeMs = fitLedgerOwnerEmulatorTimeMs(),
): void => {
  if (emulatorTimeMs !== undefined) emulator.time = emulatorTimeMs;
};
