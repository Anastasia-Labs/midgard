/**
 * Pure parsers for the command-line values: each returns the value the run
 * uses or throws a refusal that names the flag.
 */
import {
  parseAdditionalAssetSpecs,
  parseLovelaceAmount,
} from "@al-ft/midgard-core/asset-specs";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  Data,
  getAddressDetails,
  type Network,
  type OutRef,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";

/** A refusal: the same arguments always get the same answer. */
export class UserCliRefusal extends Error {
  override readonly name = "UserCliRefusal";
}

export const refuse = (message: string): never => {
  throw new UserCliRefusal(message);
};

const attempt = <T>(read: () => T): T => {
  try {
    return read();
  } catch (cause) {
    if (cause instanceof UserCliRefusal) throw cause;
    return refuse(cause instanceof Error ? cause.message : String(cause));
  }
};

/** Same bound as the node's L2-transfer submission ids. */
export const parseSubmissionId = (value: string): string =>
  value.length >= 1 && value.length <= 128
    ? value
    : refuse("--submission-id must be 1 to 128 characters long.");

export const parseLovelace = (value: string, zeroMessage: string): bigint =>
  attempt(() => parseLovelaceAmount(value, zeroMessage));

export const parseOptionalLovelace = (
  value: string | undefined,
  flag: string,
): bigint | undefined =>
  value === undefined
    ? undefined
    : parseLovelace(value, `${flag} must be greater than zero.`);

export const parseAssets = (specs: readonly string[]): Readonly<Assets> =>
  attempt(() => parseAdditionalAssetSpecs(specs));

export const parsePositiveMs = (value: string, flag: string): number => {
  const parsed = /^[1-9]\d*$/u.test(value.trim()) ? Number(value) : NaN;
  return Number.isSafeInteger(parsed)
    ? parsed
    : refuse(`${flag} must be a positive integer number of milliseconds.`);
};

/** A Cardano address (deposit L2 recipients use the Cardano bech32 form). */
export const parseAddress = (
  value: string,
  flag: string,
  network: Network,
): string => {
  const normalized = value.trim();
  let networkId: number;
  try {
    networkId = getAddressDetails(normalized).networkId;
  } catch (cause) {
    return refuse(`Invalid ${flag} "${normalized}": ${String(cause)}`);
  }
  if (network !== "Custom" && networkId !== (network === "Mainnet" ? 1 : 0))
    refuse(`${flag} must target the deployment's ${network} network.`);
  return normalized;
};

/** The address as ledger data; a pointer address is refused (the node does
 * not map one, so its order would never be admitted). */
export const parseAddressData = (
  address: string,
  flag: string,
): SDK.AddressData => {
  const result = Effect.runSync(
    Effect.either(SDK.addressDataFromBech32(address.trim())),
  );
  return Either.isRight(result)
    ? result.right
    : refuse(`Invalid ${flag} "${address}": ${result.left.message}`);
};

/** Optional raw Plutus data hex; empty is the empty byte string's absence. */
export const parseOptionalDataHex = (
  value: string | undefined,
  flag: string,
): string | null => {
  const trimmed = value?.trim() ?? "";
  if (trimmed.length === 0) return null;
  const hex = attempt(() =>
    normalizeHex(trimmed, { fieldName: flag, allowEmpty: false }),
  );
  try {
    Data.from(hex);
  } catch (cause) {
    refuse(`${flag} must decode as Plutus data CBOR: ${String(cause)}`);
  }
  return hex;
};

/** The CardanoDatum CBOR a withdrawal carries: NoDatum, or the data inline,
 * its CBOR kept byte for byte. */
export const cardanoDatumCbor = (dataHex: string | null): string =>
  dataHex === null
    ? Data.to("NoDatum", SDK.CardanoDatum)
    : replacePlutusConstrFieldCbor(
        Data.to({ InlineDatum: { data: 0n } }, SDK.CardanoDatum),
        [0],
        dataHex,
      );

export const parseL2OutRef = (value: string): OutRef =>
  attempt(() => {
    try {
      return parseOutRefLabel(value);
    } catch (cause) {
      throw new Error(
        `--l2-out-ref must be <txHash>#<outputIndex>: ${cause instanceof Error ? cause.message : String(cause)}`,
      );
    }
  });
