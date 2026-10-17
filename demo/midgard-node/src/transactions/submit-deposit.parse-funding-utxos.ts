import { normalizeHex as normalizeCoreHex } from "@al-ft/midgard-core/hex";
import {
  type Assets,
  getAddressDetails,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  parseAdditionalAssetSpecs,
  parseLovelaceAmount,
} from "../asset-specs.js";
import {
  MAX_DEPOSIT_BUILD_ADDITIONAL_ASSETS,
  MAX_DEPOSIT_BUILD_FUNDING_UTXOS,
  MAX_DEPOSIT_BUILD_UTXO_ASSET_ENTRIES,
  type SubmitDepositConfig,
} from "./submit-deposit.deposit-submission-attempt-from-completed-tx.js";
import {
  asObject,
  parseOptionalString,
  parseRequiredString,
} from "./submit-deposit.reconcile-deposit-submission-attempt-program.js";

const parsePositiveIntegerString = (value: string, field: string): bigint => {
  const normalized = value.trim();
  if (!/^[1-9]\d*$/.test(normalized)) {
    throw new Error(`${field} must be a positive integer string.`);
  }
  return BigInt(normalized);
};

const parseNonNegativeInteger = (value: unknown, field: string): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${field} must be a non-negative integer.`);
  }
  return value;
};

const expectedNetworkIdForAddressValidation = (
  network: string | undefined,
): number | undefined => {
  if (network === undefined || network === "Custom") {
    return undefined;
  }
  return network === "Mainnet" ? 1 : 0;
};

export const parseAddressString = ({
  value,
  field,
  expectedNetwork,
}: {
  readonly value: unknown;
  readonly field: string;
  readonly expectedNetwork?: string;
}): string => {
  const normalized = parseRequiredString(value, field);
  let details: ReturnType<typeof getAddressDetails>;
  try {
    details = getAddressDetails(normalized);
  } catch (cause) {
    throw new Error(`Invalid ${field} "${normalized}": ${String(cause)}`);
  }
  const expectedNetworkId =
    expectedNetworkIdForAddressValidation(expectedNetwork);
  if (
    expectedNetworkId !== undefined &&
    details.networkId !== expectedNetworkId
  ) {
    throw new Error(
      `${field} must target the configured ${expectedNetwork} network.`,
    );
  }
  return details.address.bech32;
};

const normalizeAssetUnit = (value: string, field: string): string => {
  const normalized = value.trim();
  const assetName = normalizeCoreHex(normalized.slice(56), {
    fieldName: `${field}.assetName`,
    allowEmpty: true,
  });
  if (assetName.length > 64) {
    throw new Error(
      `${field} must be a Cardano unit string (56 hex policy id plus optional asset-name hex).`,
    );
  }
  return `${normalizeCoreHex(normalized.slice(0, 56), {
    fieldName: `${field}.policyId`,
    byteLength: 28,
  })}${assetName}`;
};

const normalizeOptionalHexField = (
  value: unknown,
  field: string,
  byteLength?: number,
): string | null => {
  if (value === undefined || value === null) {
    return null;
  }
  if (typeof value !== "string") {
    throw new Error(`${field} must be a hex string when provided.`);
  }
  const normalized = value.trim();
  if (normalized.length === 0) {
    return null;
  }
  return normalizeCoreHex(normalized, { fieldName: field, byteLength });
};

const parseFundingAssets = (value: unknown, field: string): Assets => {
  const rawAssets = asObject(value, field);
  const entries = Object.entries(rawAssets);
  if (entries.length === 0) {
    throw new Error(`${field} must include at least lovelace.`);
  }
  if (entries.length > MAX_DEPOSIT_BUILD_UTXO_ASSET_ENTRIES) {
    throw new Error(
      `${field} exceeds the maximum asset entry count (${entries.length} > ${MAX_DEPOSIT_BUILD_UTXO_ASSET_ENTRIES}).`,
    );
  }

  const assets: Assets = {};
  for (const [unitKey, amountValue] of entries) {
    const unit =
      unitKey === "lovelace"
        ? "lovelace"
        : normalizeAssetUnit(unitKey, `${field}.${unitKey}`);
    if (assets[unit] !== undefined) {
      throw new Error(`Duplicate asset unit "${unit}" in ${field}.`);
    }
    assets[unit] = parsePositiveIntegerString(
      parseRequiredString(amountValue, `${field}.${unit}`),
      `${field}.${unit}`,
    );
  }
  if (assets.lovelace === undefined) {
    throw new Error(`${field} must include lovelace.`);
  }
  return assets;
};

export const parseAdditionalAssetsFromRequest = (
  value: unknown,
): Readonly<Assets> => {
  if (value === undefined || value === null) {
    return {};
  }
  if (!Array.isArray(value)) {
    throw new Error("additionalAssets must be an array when provided.");
  }
  if (value.length > MAX_DEPOSIT_BUILD_ADDITIONAL_ASSETS) {
    throw new Error(
      `additionalAssets exceeds the maximum entry count (${value.length} > ${MAX_DEPOSIT_BUILD_ADDITIONAL_ASSETS}).`,
    );
  }

  const assets: Assets = {};
  for (const [index, entry] of value.entries()) {
    const field = `additionalAssets[${index.toString()}]`;
    const raw = asObject(entry, field);
    const unit = normalizeAssetUnit(
      parseRequiredString(raw.unit, `${field}.unit`),
      `${field}.unit`,
    );
    if (assets[unit] !== undefined) {
      throw new Error(`Duplicate additional asset "${unit}" provided.`);
    }
    assets[unit] = parsePositiveIntegerString(
      parseRequiredString(raw.amount, `${field}.amount`),
      `${field}.amount`,
    );
  }
  return assets;
};

export const parseFundingUtxos = ({
  value,
  fundingAddress,
  expectedNetwork,
}: {
  readonly value: unknown;
  readonly fundingAddress: string;
  readonly expectedNetwork?: string;
}): readonly UTxO[] => {
  if (!Array.isArray(value)) {
    throw new Error("fundingUtxos must be an array.");
  }
  if (value.length === 0) {
    throw new Error("fundingUtxos must not be empty.");
  }
  if (value.length > MAX_DEPOSIT_BUILD_FUNDING_UTXOS) {
    throw new Error(
      `fundingUtxos exceeds the maximum count (${value.length} > ${MAX_DEPOSIT_BUILD_FUNDING_UTXOS}).`,
    );
  }

  const seenOutRefs = new Set<string>();
  return value.map((entry, index) => {
    const field = `fundingUtxos[${index.toString()}]`;
    const raw = asObject(entry, field);
    const txHash = normalizeCoreHex(
      parseRequiredString(raw.txHash, `${field}.txHash`),
      { fieldName: `${field}.txHash`, byteLength: 32 },
    );
    const outputIndex = parseNonNegativeInteger(
      raw.outputIndex,
      `${field}.outputIndex`,
    );
    const outRefKey = `${txHash}#${outputIndex.toString()}`;
    if (seenOutRefs.has(outRefKey)) {
      throw new Error(`Duplicate funding UTxO "${outRefKey}" provided.`);
    }
    seenOutRefs.add(outRefKey);

    const utxoAddress = parseAddressString({
      value: raw.address,
      field: `${field}.address`,
      expectedNetwork,
    });
    if (utxoAddress !== fundingAddress) {
      throw new Error(`${field}.address must match fundingAddress.`);
    }

    const datumHash = normalizeOptionalHexField(
      raw.datumHash,
      `${field}.datumHash`,
      32,
    );
    const datum = normalizeOptionalHexField(raw.datum, `${field}.datum`);
    if (parseOptionalString(raw.scriptRef, `${field}.scriptRef`) !== null) {
      throw new Error(
        `${field}.scriptRef is not supported for deposit build funding inputs.`,
      );
    }

    return {
      txHash,
      outputIndex,
      address: utxoAddress,
      assets: parseFundingAssets(raw.assets, `${field}.assets`),
      datumHash: datumHash ?? undefined,
      datum: datum ?? undefined,
      scriptRef: undefined,
    };
  });
};

export const buildSubmitDepositConfig = ({
  l2Address,
  l2Datum,
  lovelace,
  additionalAssets,
  expectedNetwork,
}: {
  readonly l2Address: unknown;
  readonly l2Datum?: unknown;
  readonly lovelace: unknown;
  readonly additionalAssets: Readonly<Assets>;
  readonly expectedNetwork?: string;
}): SubmitDepositConfig => {
  const normalizedL2Address = parseAddressString({
    value: l2Address,
    field: "l2Address",
    expectedNetwork,
  });
  const l2DatumHex = parseOptionalString(l2Datum, "l2Datum");

  return {
    l2Address: normalizedL2Address,
    l2Datum:
      l2DatumHex === null
        ? null
        : normalizeCoreHex(l2DatumHex, {
            fieldName: "L2 datum",
            allowEmpty: true,
          }),
    lovelace: parseLovelaceAmount(
      parseRequiredString(lovelace, "lovelace"),
      "Deposit lovelace amount must be greater than zero.",
    ),
    additionalAssets,
  };
};

export const parseSubmitDepositConfig = ({
  l2Address,
  l2Datum,
  lovelace,
  assetSpecs,
}: {
  readonly l2Address: string;
  readonly l2Datum?: string;
  readonly lovelace: string;
  readonly assetSpecs: readonly string[];
}): SubmitDepositConfig =>
  buildSubmitDepositConfig({
    l2Address,
    l2Datum,
    lovelace,
    additionalAssets: parseAdditionalAssetSpecs(assetSpecs),
  });
