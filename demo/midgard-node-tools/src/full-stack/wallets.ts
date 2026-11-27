import { paymentCredentialOf, walletFromSeed } from "@lucid-evolution/lucid";

import type { StackConfig } from "./config.js";

export function deriveStackWallets(
  config: StackConfig,
  env: Record<string, string>,
) {
  const addresses: Record<string, string> = {};
  const credentials = new Set<string>();
  for (const [role, wallet] of Object.entries(config.wallets)) {
    const address = walletFromSeed(env[wallet.seedEnv]!, {
      network: "Preprod",
      addressType: ["prover", "availability"].includes(role)
        ? "Enterprise"
        : "Base",
    }).address;
    const credential = paymentCredentialOf(address).hash;
    if (credentials.has(credential))
      throw new Error(`Wallet ${role} shares a payment key with another role`);
    credentials.add(credential);
    addresses[role] = address;
  }
  return addresses;
}
