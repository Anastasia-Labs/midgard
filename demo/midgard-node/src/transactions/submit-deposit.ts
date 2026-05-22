/**
 * Deposit submission flow for projecting deposit observations into Midgard
 * state.
 * This module owns node/API concerns and delegates production transaction
 * construction to the SDK user-event builders.
 */
import { normalizeHex as normalizeCoreHex } from "@al-ft/midgard-core/hex";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  getAddressDetails,
  Lucid as makeLucid,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import {
  parseAdditionalAssetSpecs,
  parseLovelaceAmount,
} from "@/asset-specs.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";

export type SubmitDepositReferenceScripts = SDK.SubmitDepositReferenceScripts;
export type SubmitDepositConfig = SDK.SubmitDepositConfig;

export type BuildDepositRequest = SubmitDepositConfig & {
  readonly fundingAddress: string;
  readonly fundingUtxos: readonly UTxO[];
};

export type BuiltUnsignedDepositTx = {
  readonly unsignedTxCbor: string;
};

export type DepositBuildMetadata = SDK.DepositBuildMetadata;

export type SubmittedDeposit = {
  readonly txHash: string;
  readonly metadata: DepositBuildMetadata;
};

export class SubmitDepositError extends EffectData.TaggedError(
  "SubmitDepositError",
)<{
  message: string;
  cause: unknown;
}> {}

const MAX_DEPOSIT_BUILD_FUNDING_UTXOS = 128;
const MAX_DEPOSIT_BUILD_UTXO_ASSET_ENTRIES = 64;
const MAX_DEPOSIT_BUILD_ADDITIONAL_ASSETS = 64;

const buildUnsignedDepositTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitDepositConfig,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly metadata: DepositBuildMetadata;
  },
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitDepositError
> =>
  SDK.buildUnsignedDepositTxWithMetadataProgram(lucid, contracts, config).pipe(
    Effect.catchTag("UserEventBuildError", (error) =>
      Effect.fail(
        new SubmitDepositError({
          message: error.message,
          cause: error.cause,
        }),
      ),
    ),
  );

export const buildUnsignedDepositTxProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitDepositConfig,
): Effect.Effect<
  TxSignBuilder,
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitDepositError
> =>
  buildUnsignedDepositTxWithMetadataProgram(lucid, contracts, config).pipe(
    Effect.map(({ tx }) => tx),
  );

export const buildUnsignedDepositTxFromFundingContextProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  request: BuildDepositRequest,
): Effect.Effect<
  BuiltUnsignedDepositTx,
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitDepositError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new SubmitDepositError({
          message:
            "Cardano network not found while preparing deposit transaction",
          cause: "Lucid network configuration is undefined",
        }),
      );
    }

    const externalLucid = yield* Effect.tryPromise({
      try: () => makeLucid(lucid.config().provider, network),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to initialize external-wallet deposit builder",
          cause,
        }),
    });
    yield* Effect.sync(() =>
      externalLucid.selectWallet.fromAddress(request.fundingAddress, [
        ...request.fundingUtxos,
      ]),
    );

    const { tx } = yield* buildUnsignedDepositTxWithMetadataProgram(
      externalLucid,
      contracts,
      request,
    );
    return { unsignedTxCbor: tx.toCBOR() };
  });

export const submitDepositWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitDepositConfig,
): Effect.Effect<
  SubmittedDeposit,
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitDepositError
  | TxSubmitError
  | TxConfirmError
  | TxSignError
> =>
  Effect.gen(function* () {
    const { tx, metadata } = yield* buildUnsignedDepositTxWithMetadataProgram(
      lucid,
      contracts,
      config,
    );
    const txHash = yield* handleSignSubmit(lucid, tx);
    return { txHash, metadata };
  });

type UnknownRecord = Record<string, unknown>;

const asObject = (value: unknown, field: string): UnknownRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object.`);
  }
  return value as UnknownRecord;
};

const parseRequiredString = (value: unknown, field: string): string => {
  if (typeof value !== "string") {
    throw new Error(`${field} must be a string.`);
  }
  const normalized = value.trim();
  if (normalized.length === 0) {
    throw new Error(`${field} must not be empty.`);
  }
  return normalized;
};

const parseOptionalString = (value: unknown, field: string): string | null => {
  if (value === undefined || value === null) {
    return null;
  }
  if (typeof value !== "string") {
    throw new Error(`${field} must be a string when provided.`);
  }
  const normalized = value.trim();
  return normalized.length === 0 ? null : normalized;
};

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

const parseAddressString = ({
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

const parseAdditionalAssetsFromRequest = (value: unknown): Readonly<Assets> => {
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

const parseFundingUtxos = ({
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

const buildSubmitDepositConfig = ({
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

export const parseBuildDepositRequest = (
  payload: unknown,
  options?: {
    readonly expectedNetwork?: string;
  },
): BuildDepositRequest => {
  const body = asObject(payload, "Deposit build request");
  const fundingAddress = parseAddressString({
    value: body.fundingAddress,
    field: "fundingAddress",
    expectedNetwork: options?.expectedNetwork,
  });
  const fundingUtxos = parseFundingUtxos({
    value: body.fundingUtxos,
    fundingAddress,
    expectedNetwork: options?.expectedNetwork,
  });

  return {
    ...buildSubmitDepositConfig({
      l2Address: body.l2Address,
      l2Datum: body.l2Datum,
      lovelace: body.lovelace,
      additionalAssets: parseAdditionalAssetsFromRequest(body.additionalAssets),
      expectedNetwork: options?.expectedNetwork,
    }),
    fundingAddress,
    fundingUtxos,
  };
};
