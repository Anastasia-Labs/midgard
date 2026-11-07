import { randomBytes } from "node:crypto";

import { CML, walletFromSeed } from "@lucid-evolution/lucid";
import { generateMnemonic } from "bip39";

import { createOnce } from "./durable.js";
import type { Layout } from "./layout.js";

/**
 * One distinct funded wallet per actor. Sharing a wallet between two actors
 * that build transactions independently makes them race for the same UTxOs,
 * so every submitting process gets its own.
 */
export const WALLET_ROLES = [
  "operator",
  "merge",
  "referenceScript",
  "settlement",
  "daCosigner",
  "daSubmitter0",
  "daSubmitter1",
  "daAvailability0",
  "daAvailability1",
  "watcherProver",
  "watcherAvailability",
  "userA",
  "userB",
  "userC",
] as const;
export type WalletRole = (typeof WALLET_ROLES)[number];
export const USER_ROLES = ["userA", "userB", "userC"] as const;
export type UserRole = (typeof USER_ROLES)[number];

export const LIBP2P_IDENTITIES = [
  "producer",
  "committee0",
  "committee1",
  "retained",
] as const;

export type Identities = {
  readonly schemaVersion: "midgard-devnet-identities-v1";
  readonly seeds: Record<WalletRole, string>;
  /** 32-byte hex seeds for `seed:` libp2p key sources. */
  readonly libp2p: Record<(typeof LIBP2P_IDENTITIES)[number], string>;
  readonly adminApiKey: string;
  readonly publicReaderPassword: string;
};

const hex32 = () => randomBytes(32).toString("hex");

const record = <K extends string, V>(keys: readonly K[], make: () => V) =>
  Object.fromEntries(keys.map((key) => [key, make()])) as Record<K, V>;

export const loadIdentities = (layout: Layout): Identities => {
  const identities = createOnce<Identities>(layout.identities, () => ({
    schemaVersion: "midgard-devnet-identities-v1",
    seeds: record(WALLET_ROLES, () => generateMnemonic(256)),
    libp2p: record(LIBP2P_IDENTITIES, hex32),
    adminApiKey: hex32(),
    publicReaderPassword: hex32(),
  }));
  const missing = WALLET_ROLES.filter((role) => !identities.seeds[role]);
  if (missing.length > 0)
    throw new Error(
      `${layout.identities} predates roles ${missing.join(", ")}; it is never rewritten, so this run cannot gain them`,
    );
  return identities;
};

export type WalletInfo = {
  readonly address: string;
  /** Raw Ed25519 payment verification key, hex. */
  readonly paymentVkey: string;
  readonly paymentKeyHash: string;
};

export const walletInfo = (seed: string): WalletInfo => {
  const wallet = walletFromSeed(seed, { network: "Custom" });
  const key = CML.PrivateKey.from_bech32(wallet.paymentKey);
  const vkey = key.to_public();
  return {
    address: wallet.address,
    paymentVkey: Buffer.from(vkey.to_raw_bytes()).toString("hex"),
    paymentKeyHash: vkey.hash().to_hex(),
  };
};

export const walletInfos = (identities: Identities) =>
  Object.fromEntries(
    WALLET_ROLES.map((role) => [role, walletInfo(identities.seeds[role])]),
  ) as Record<WalletRole, WalletInfo>;
