/**
 * Shared lovelace and multi-asset CLI parsing helpers.
 * This module keeps command-line asset parsing pure so transfer/deposit command
 * surfaces can reuse the same validation rules without importing heavier
 * transaction-builder modules.
 */
import { type Assets } from "@lucid-evolution/lucid";

const LOVELACE_INTEGER_PATTERN = /^(?:0|[1-9]\d*)$/;
const HEX_PATTERN = /^[0-9a-fA-F]*$/;
const POLICY_ID_PATTERN = /^[0-9a-fA-F]{56}$/;

/**
 * Parses a CLI lovelace amount and enforces a strictly positive integer value.
 */
export const parseLovelaceAmount = (
  value: string,
  zeroAmountMessage = "Lovelace amount must be greater than zero.",
): bigint => {
  const normalized = value.trim();
  if (!LOVELACE_INTEGER_PATTERN.test(normalized)) {
    throw new Error(
      `Invalid lovelace amount "${value}". Use a positive integer number of lovelace.`,
    );
  }

  const lovelace = BigInt(normalized);
  if (lovelace <= 0n) {
    throw new Error(zeroAmountMessage);
  }
  return lovelace;
};

/**
 * Normalizes a user-supplied hex field and enforces even-length hex encoding.
 */
const normalizeHex = (value: string, field: string): string => {
  const normalized = value.trim();
  if (!HEX_PATTERN.test(normalized) || normalized.length % 2 !== 0) {
    throw new Error(`Invalid ${field}: expected even-length hex, got "${value}".`);
  }
  return normalized.toLowerCase();
};

/**
 * Parses one CLI multi-asset spec in `policyId.assetName:amount` form.
 */
export const parseAdditionalAssetSpec = (
  spec: string,
): {
  readonly unit: string;
  readonly amount: bigint;
} => {
  const trimmed = spec.trim();
  const colonIndex = trimmed.lastIndexOf(":");
  if (colonIndex <= 0 || colonIndex === trimmed.length - 1) {
    throw new Error(
      `Invalid asset spec "${spec}". Expected policyId.assetName:amount.`,
    );
  }

  const assetId = trimmed.slice(0, colonIndex);
  const amountString = trimmed.slice(colonIndex + 1).trim();
  const dotIndex = assetId.indexOf(".");
  if (dotIndex <= 0) {
    throw new Error(
      `Invalid asset spec "${spec}". Expected policyId.assetName:amount.`,
    );
  }

  const policyId = assetId.slice(0, dotIndex).trim();
  const assetName = assetId.slice(dotIndex + 1).trim();
  if (!POLICY_ID_PATTERN.test(policyId)) {
    throw new Error(
      `Invalid policy ID in asset spec "${spec}". Expected 56 hex characters.`,
    );
  }

  const normalizedAssetName = normalizeHex(assetName, "asset name");
  if (normalizedAssetName.length > 64) {
    throw new Error(
      `Invalid asset name in asset spec "${spec}". Cardano asset names must be at most 32 bytes.`,
    );
  }

  if (!/^\d+$/.test(amountString)) {
    throw new Error(
      `Invalid asset amount in "${spec}". Expected a positive integer quantity.`,
    );
  }
  const amount = BigInt(amountString);
  if (amount <= 0n) {
    throw new Error(
      `Invalid asset amount in "${spec}". Quantity must be greater than zero.`,
    );
  }

  return {
    unit: `${policyId.toLowerCase()}${normalizedAssetName}`,
    amount,
  };
};

/**
 * Parses multiple additional-asset specs and rejects duplicates by full unit.
 */
export const parseAdditionalAssetSpecs = (
  specs: readonly string[],
): Readonly<Assets> => {
  const assets: Assets = {};
  for (const spec of specs) {
    const { unit, amount } = parseAdditionalAssetSpec(spec);
    if (assets[unit] !== undefined) {
      throw new Error(
        `Duplicate additional asset "${unit}" provided. Combine quantities before submitting the command.`,
      );
    }
    assets[unit] = amount;
  }
  return assets;
};
