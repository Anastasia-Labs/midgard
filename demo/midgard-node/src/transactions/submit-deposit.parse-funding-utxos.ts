import {
  parseAdditionalAssetSpecs,
  parseLovelaceAmount,
} from "@al-ft/midgard-core/asset-specs";
import { normalizeHex as normalizeCoreHex } from "@al-ft/midgard-core/hex";
import { type Assets, getAddressDetails } from "@lucid-evolution/lucid";

import { type SubmitDepositConfig } from "./submit-deposit.deposit-submission-attempt-from-completed-tx.js";
import {
  parseOptionalString,
  parseRequiredString,
} from "./submit-deposit.reconcile-deposit-submission-attempt-program.js";

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
