import { paymentCredentialOf, walletFromSeed } from "@lucid-evolution/lucid";

/**
 * The fixed seed behind the operator key `preprod-emulator-curated` lists.
 * Test-only wallet: it holds nothing outside an emulator.
 */
export const CURATED_EMULATOR_SEEDS = {
  operator:
    "album cloth hello slot cluster patch glare idea excess refuse breeze dragon comfort cube fiscal frozen confirm enemy airport lift brain then render quote",
} as const;

/** The payment key hash a seed's wallet signs with. */
export const curatedKeyHash = (seedPhrase: string): string =>
  paymentCredentialOf(
    walletFromSeed(seedPhrase, {
      addressType: "Base",
      accountIndex: 0,
      network: "Custom",
    }).address,
  ).hash;
