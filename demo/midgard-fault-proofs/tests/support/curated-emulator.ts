import { paymentCredentialOf, walletFromSeed } from "@lucid-evolution/lucid";

/**
 * The fixed seeds behind the keys `preprod-emulator-curated` lists, one per
 * allow-list. Test-only wallets: they hold nothing outside an emulator.
 */
export const CURATED_EMULATOR_SEEDS = {
  operator:
    "album cloth hello slot cluster patch glare idea excess refuse breeze dragon comfort cube fiscal frozen confirm enemy airport lift brain then render quote",
  fraudProver:
    "child swift buddy sun shaft bomb angry noble coil dolphin picture oblige intact engine deal spirit profit shrimp author ugly neck feel foot office",
  daChallenger:
    "diesel lunch angry skirt obvious skin fever all wage arm inside seed zebra enroll defense lecture subway outdoor torch follow slim spawn dragon turtle",
} as const;

const seedWallet = (seedPhrase: string) =>
  walletFromSeed(seedPhrase, {
    addressType: "Base",
    accountIndex: 0,
    network: "Custom",
  });

/** The payment key hash a seed's wallet signs with. */
export const curatedKeyHash = (seedPhrase: string): string =>
  paymentCredentialOf(seedWallet(seedPhrase).address).hash;

/** The seed's payment signing key, bech32. */
export const curatedPaymentKey = (seedPhrase: string): string =>
  seedWallet(seedPhrase).paymentKey;
