/**
 * The wallet seed: exactly one source, named by the user, never a default.
 * A seed file is read the way the node reads a mounted secret: an absolute
 * path to a nonempty regular file (not a symlink) of at most 64 KiB that
 * grants no group or other access.
 */
import { closeSync, constants, fstatSync, openSync, readSync } from "node:fs";
import { isAbsolute } from "node:path";

import {
  getAddressDetails,
  type Network,
  walletFromSeed,
} from "@lucid-evolution/lucid";

import { refuse, UserCliRefusal } from "./options.js";

const SEED_FILE_MAX_BYTES = 65_536;

export type SeedSource = {
  readonly env?: string;
  readonly file?: string;
};

const readSeedFile = (path: string): string => {
  if (!isAbsolute(path))
    return refuse("--wallet-seed-phrase-file must be an absolute path.");
  let fd: number | undefined;
  try {
    fd = openSync(path, constants.O_RDONLY | constants.O_NOFOLLOW);
    const stat = fstatSync(fd);
    if (!stat.isFile() || stat.size === 0 || stat.size > SEED_FILE_MAX_BYTES)
      return refuse(
        "--wallet-seed-phrase-file must be a nonempty regular file of at most 64 KiB.",
      );
    if ((stat.mode & 0o077) !== 0)
      return refuse(
        "--wallet-seed-phrase-file grants group or other access; make it mode 0600.",
      );
    const buffer = Buffer.alloc(SEED_FILE_MAX_BYTES + 1);
    let count = 0;
    while (count < buffer.length) {
      const read = readSync(fd, buffer, count, buffer.length - count, null);
      if (read === 0) break;
      count += read;
    }
    if (count > SEED_FILE_MAX_BYTES)
      return refuse("--wallet-seed-phrase-file exceeds 64 KiB.");
    return buffer.subarray(0, count).toString("utf8").trim();
  } catch (cause) {
    if (cause instanceof UserCliRefusal) throw cause;
    // The contents never reach an error message.
    return refuse(
      "--wallet-seed-phrase-file is missing, unreadable or a symbolic link.",
    );
  } finally {
    if (fd !== undefined) closeSync(fd);
  }
};

export const resolveSeedPhrase = (
  source: SeedSource,
  env: Readonly<Record<string, string | undefined>>,
): string => {
  if ((source.env === undefined) === (source.file === undefined))
    refuse(
      "Give exactly one of --wallet-seed-phrase-env NAME or --wallet-seed-phrase-file PATH.",
    );
  const seedPhrase =
    source.file !== undefined
      ? readSeedFile(source.file)
      : (env[source.env!.trim()]?.trim() ?? "");
  if (seedPhrase.length === 0)
    refuse(
      source.file !== undefined
        ? "--wallet-seed-phrase-file is empty."
        : `Environment variable "${source.env!.trim()}" does not hold a wallet seed phrase.`,
    );
  return seedPhrase;
};

export type UserWallet = {
  readonly address: string;
  readonly paymentKeyHash: string;
  /** Bech32 private key; signs withdrawal bodies. Never written anywhere. */
  readonly paymentKey: string;
};

/** The wallet Lucid selects from this seed (base address, account 0). */
export const walletOf = (seedPhrase: string, network: Network): UserWallet => {
  let wallet: ReturnType<typeof walletFromSeed>;
  try {
    wallet = walletFromSeed(seedPhrase, { network });
  } catch {
    // The seed never reaches an error message.
    return refuse("The wallet seed phrase is not a valid mnemonic.");
  }
  const paymentKeyHash = getAddressDetails(wallet.address).paymentCredential
    ?.hash;
  if (paymentKeyHash === undefined)
    return refuse("The wallet address has no payment key.");
  return {
    address: wallet.address,
    paymentKeyHash,
    paymentKey: wallet.paymentKey,
  };
};
