import { join } from "node:path";

import { type Network } from "@lucid-evolution/lucid";
import { generateMnemonic } from "bip39";

import {
  DEFAULT_STRESS_WALLET_ENV_PREFIX,
  ENV_NAME_PATTERN,
} from "./constants.js";

export const requireSafePositiveInteger = (
  value: number,
  label: string,
): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${label} must be a safe positive integer.`);
  }
  return value;
};

export const requireSafeNonNegativeInteger = (
  value: number,
  label: string,
): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${label} must be a safe non-negative integer.`);
  }
  return value;
};

export const parseStressWalletNetwork = (
  value: string | undefined,
  env: NodeJS.ProcessEnv = process.env,
): Network => {
  const normalized = (value ?? env.NETWORK ?? "Preprod").trim();
  if (
    normalized === "Mainnet" ||
    normalized === "Preprod" ||
    normalized === "Preview"
  ) {
    return normalized;
  }
  throw new Error(
    `Unsupported network "${normalized}". Expected Mainnet, Preprod, or Preview.`,
  );
};

export const parseStressWalletCount = (
  value: unknown,
  label: string,
): number => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a positive integer.`);
  }
  const parsed = Number(value);
  return requireSafePositiveInteger(parsed, label);
};

export const parseStressWalletNonNegativeMs = (
  value: unknown,
  label: string,
): number => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  const parsed = Number(value);
  return requireSafeNonNegativeInteger(parsed, label);
};

export const parseStressWalletLovelace = (
  value: unknown,
  label: string,
): bigint => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a positive integer.`);
  }
  const parsed = BigInt(value);
  if (parsed <= 0n) {
    throw new Error(`${label} must be greater than zero.`);
  }
  return parsed;
};

export const parseStressWalletNonNegativeLovelace = (
  value: unknown,
  label: string,
): bigint => {
  if (typeof value !== "string" || !/^\d+$/.test(value)) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  return BigInt(value);
};

export const normalizeEnvPrefix = (value: string | undefined): string => {
  const prefix = value?.trim() || DEFAULT_STRESS_WALLET_ENV_PREFIX;
  if (!ENV_NAME_PATTERN.test(`${prefix}_0001`)) {
    throw new Error(
      `Stress wallet env prefix "${prefix}" does not produce valid environment variable names.`,
    );
  }
  return prefix;
};

export const walletIndexLabel = (index: number): string =>
  index.toString().padStart(4, "0");

export const stressWalletFileName = (index: number): string =>
  `wallet-${walletIndexLabel(index)}.json`;

export const stressWalletEnvName = (envPrefix: string, index: number): string =>
  `${envPrefix}_${walletIndexLabel(index)}`;

export const walletPath = (outDir: string, index: number): string =>
  join(outDir, stressWalletFileName(index));

export const defaultGenerateSeedPhrase = (): string => generateMnemonic(256);

export const normalizeSeedPhrase = (seedPhrase: string): string =>
  seedPhrase.trim().replace(/\s+/g, " ");

export const stressWalletId = (index: number): string =>
  "stress-wallet-" + walletIndexLabel(index);
