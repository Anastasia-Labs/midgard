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
  CML,
  coreToTxOutput,
  Data as LucidData,
  getAddressDetails,
  Lucid as makeLucid,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Option } from "effect";
import { Data as EffectData, Effect } from "effect";

import {
  parseAdditionalAssetSpecs,
  parseLovelaceAmount,
} from "@/asset-specs.js";
import {
  DepositSubmissionAttemptsDB,
  DepositsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { reconcileVisibleDepositUTxOs } from "@/fibers/fetch-and-insert-deposit-utxos.js";
import {
  Database,
  Lucid as LucidService,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import {
  awaitSubmittedTransactionConfirmation,
  signSubmitTransaction,
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
  readonly confirmationStatus:
    | "confirmed"
    | "reconciled_after_timeout"
    | "ambiguous";
};

export class SubmitDepositError extends EffectData.TaggedError(
  "SubmitDepositError",
)<{
  message: string;
  cause: unknown;
}> {}

export class DepositConfirmationUnknownError extends EffectData.TaggedError(
  "DepositConfirmationUnknownError",
)<{
  message: string;
  txHash: string;
  depositEventId: string;
  expectedDepositOutRef: string;
  reconciliation: DepositSubmissionReconciliationResult;
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

const sortedAssetEntries = (assets: Assets): [string, string][] =>
  Object.entries(assets)
    .map(([unit, quantity]) => [unit, quantity.toString()] as [string, string])
    .sort(([left], [right]) => left.localeCompare(right));

const serializeAssets = (
  assets: Assets,
): DepositSubmissionAttemptsDB.SerializedAssets =>
  Object.fromEntries(sortedAssetEntries(assets));

const sameSerializedAssets = (
  left: DepositSubmissionAttemptsDB.SerializedAssets,
  right: DepositSubmissionAttemptsDB.SerializedAssets,
): boolean => JSON.stringify(left) === JSON.stringify(right);

const inputOutRefsFromSignedTx = (tx: CML.Transaction): readonly string[] => {
  const inputs = tx.body().inputs();
  const outRefs: string[] = [];
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    outRefs.push(
      `${input.transaction_id().to_hex()}#${Number(input.index()).toString()}`,
    );
  }
  return outRefs;
};

const decodeDepositDatumEventId = (datumCbor: string): string => {
  const datum = LucidData.from(datumCbor, SDK.DepositDatum) as SDK.DepositDatum;
  return LucidData.to(datum.event.id, SDK.OutputReference);
};

export const depositSubmissionAttemptFromSignedTx = ({
  txHash,
  signedTxCbor,
  metadata,
  config,
}: {
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly metadata: DepositBuildMetadata;
  readonly config: SubmitDepositConfig;
}): DepositSubmissionAttemptsDB.InsertSubmittedInput => {
  const tx = CML.Transaction.from_cbor_hex(signedTxCbor);
  const outputs = tx.body().outputs();
  const matches: Array<{
    readonly outputIndex: number;
    readonly assets: Assets;
  }> = [];

  for (let outputIndex = 0; outputIndex < outputs.len(); outputIndex += 1) {
    const output = coreToTxOutput(outputs.get(outputIndex));
    if (output.address !== metadata.depositAddress) {
      continue;
    }
    if ((output.assets[metadata.depositAuthUnit] ?? 0n) !== 1n) {
      continue;
    }
    if (output.datum === undefined || output.datum === null) {
      throw new Error(
        `Deposit output ${txHash}#${outputIndex.toString()} is missing the inline deposit datum`,
      );
    }
    const actualEventId = decodeDepositDatumEventId(output.datum);
    if (actualEventId !== metadata.depositEventId) {
      continue;
    }
    matches.push({ outputIndex, assets: output.assets });
  }

  if (matches.length !== 1) {
    throw new Error(
      `Expected exactly one deposit output for event ${metadata.depositEventId} in signed tx ${txHash}; found ${matches.length.toString()}`,
    );
  }

  const match = matches[0]!;
  const expectedAssets: Assets = { ...match.assets };
  delete expectedAssets[metadata.depositAuthUnit];
  const expectedSerializedAssets = serializeAssets(expectedAssets);
  const requestedSerializedAssets = serializeAssets({
    ...config.additionalAssets,
    lovelace: config.lovelace,
  });
  if (!sameSerializedAssets(expectedSerializedAssets, requestedSerializedAssets)) {
    throw new Error(
      `Deposit output ${txHash}#${match.outputIndex.toString()} does not match requested projected assets`,
    );
  }

  return {
    [DepositSubmissionAttemptsDB.Columns.TX_HASH]: Buffer.from(txHash, "hex"),
    [DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID]: Buffer.from(
      metadata.depositEventId,
      "hex",
    ),
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF]: `${txHash}#${match.outputIndex.toString()}`,
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_L2_ADDRESS]: config.l2Address,
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_LOVELACE]:
      config.lovelace.toString(),
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_ASSETS]:
      expectedSerializedAssets,
    [DepositSubmissionAttemptsDB.Columns.METADATA]: {
      depositAddress: metadata.depositAddress,
      depositEventId: metadata.depositEventId,
      depositAssetName: metadata.depositAssetName,
      depositAuthUnit: metadata.depositAuthUnit,
      nonceInput: metadata.nonceInput,
      validTo: metadata.validTo,
      inclusionTime: metadata.inclusionTime,
    },
    [DepositSubmissionAttemptsDB.Columns.FUNDING_OUT_REFS]:
      inputOutRefsFromSignedTx(tx),
  };
};

export type DepositSubmissionReconciliationResult = {
  readonly txHash: string;
  readonly depositEventId: string;
  readonly status:
    | "confirmed"
    | "reconciled_after_timeout"
    | "ambiguous"
    | "missing_attempt";
  readonly expectedDepositOutRef?: string;
  readonly depositRowsFound: number;
  readonly reconciledCount: number;
  readonly nextSafeAction: string;
};

const findMatchingDepositEntry = (
  rows: readonly DepositsDB.Entry[],
  attempt: DepositSubmissionAttemptsDB.Row,
): DepositsDB.Entry | undefined =>
  rows.find((row) =>
    row[DepositsDB.Columns.ID].equals(
      attempt[DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID],
    ),
  );

export const reconcileDepositSubmissionAttemptProgram = (
  txHash: string,
): Effect.Effect<
  DepositSubmissionReconciliationResult,
  DatabaseError | SDK.LucidError,
  Database | MidgardContracts | LucidService | NodeConfig
> =>
  Effect.gen(function* () {
    const txHashBuffer = Buffer.from(txHash, "hex");
    const attemptOption =
      yield* DepositSubmissionAttemptsDB.retrieveByTxHash(txHashBuffer);
    if (Option.isNone(attemptOption)) {
      return {
        txHash,
        depositEventId: "",
        status: "missing_attempt",
        depositRowsFound: 0,
        reconciledCount: 0,
        nextSafeAction:
          "Do not blindly retry; inspect the transaction hash with provider tooling and only rebuild if the submitted transaction is not on-chain.",
      } as const;
    }

    const attempt = attemptOption.value;
    const beforeRows =
      yield* DepositsDB.retrieveByCardanoTxHash(txHashBuffer);
    const beforeMatch = findMatchingDepositEntry(beforeRows, attempt);
    if (beforeMatch !== undefined) {
      yield* DepositSubmissionAttemptsDB.markReconciled(txHashBuffer);
      return {
        txHash,
        depositEventId:
          attempt[DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID].toString(
            "hex",
          ),
        status: "reconciled_after_timeout",
        expectedDepositOutRef:
          attempt[DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF],
        depositRowsFound: beforeRows.length,
        reconciledCount: 0,
        nextSafeAction:
          "Deposit is already visible in deposits_utxos; continue the flow without resubmitting.",
      } as const;
    }

    const { reconciledCount } = yield* reconcileVisibleDepositUTxOs();
    const afterRows = yield* DepositsDB.retrieveByCardanoTxHash(txHashBuffer);
    const afterMatch = findMatchingDepositEntry(afterRows, attempt);
    if (afterMatch !== undefined) {
      yield* DepositSubmissionAttemptsDB.markReconciled(txHashBuffer);
      return {
        txHash,
        depositEventId:
          attempt[DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID].toString(
            "hex",
          ),
        status: "reconciled_after_timeout",
        expectedDepositOutRef:
          attempt[DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF],
        depositRowsFound: afterRows.length,
        reconciledCount,
        nextSafeAction:
          "Deposit was recovered from visible chain state; continue the flow without resubmitting.",
      } as const;
    }

    const reason =
      afterRows.length > 0
        ? `Cardano tx ${txHash} has deposit rows, but none match expected event ${attempt[
            DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID
          ].toString("hex")}.`
        : `Cardano tx ${txHash} is not visible in deposits_utxos after reconciliation.`;
    yield* DepositSubmissionAttemptsDB.markAmbiguous(txHashBuffer, reason);
    return {
      txHash,
      depositEventId:
        attempt[DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID].toString(
          "hex",
        ),
      status: "ambiguous",
      expectedDepositOutRef:
        attempt[DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF],
      depositRowsFound: afterRows.length,
      reconciledCount,
      nextSafeAction:
        "Do not resubmit yet; inspect provider confirmation and deposit contract UTxOs for this tx hash/event id.",
    } as const;
  });

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
  | DatabaseError
  | DepositConfirmationUnknownError,
  Database | MidgardContracts | LucidService | NodeConfig
> =>
  Effect.gen(function* () {
    const { tx, metadata } = yield* buildUnsignedDepositTxWithMetadataProgram(
      lucid,
      contracts,
      config,
    );
    const submission = yield* signSubmitTransaction(lucid, tx);
    const attempt = yield* Effect.try({
      try: () =>
        depositSubmissionAttemptFromSignedTx({
          txHash: submission.txHash,
          signedTxCbor: submission.signedTxCbor,
          metadata,
          config,
        }),
      catch: (cause) =>
        new SubmitDepositError({
          message:
            "Failed to derive expected deposit output from the submitted transaction",
          cause,
        }),
    });
    yield* DepositSubmissionAttemptsDB.insertSubmitted(attempt);

    const confirmationStatus =
      yield* awaitSubmittedTransactionConfirmation(lucid, submission).pipe(
        Effect.tap(() =>
          DepositSubmissionAttemptsDB.markConfirmed(
            Buffer.from(submission.txHash, "hex"),
          ),
        ),
        Effect.as("confirmed" as const),
        Effect.catchTag("TxConfirmError", (error) =>
          Effect.gen(function* () {
            yield* Effect.logWarning(
              `Deposit tx ${submission.txHash} confirmation timed out; reconciling visible deposit state before allowing retry.`,
            );
            const reconciliation = yield* reconcileDepositSubmissionAttemptProgram(
              submission.txHash,
            );
            if (reconciliation.status === "reconciled_after_timeout") {
              return reconciliation.status;
            }
            return yield* Effect.fail(
              new DepositConfirmationUnknownError({
                message:
                  "Deposit transaction confirmation is unknown after timeout and reconciliation did not prove the expected deposit output.",
                txHash: submission.txHash,
                depositEventId: metadata.depositEventId,
                expectedDepositOutRef:
                  attempt[
                    DepositSubmissionAttemptsDB.Columns
                      .EXPECTED_DEPOSIT_OUT_REF
                  ],
                reconciliation,
                cause: error,
              }),
            );
          }),
        ),
      );
    return { txHash: submission.txHash, metadata, confirmationStatus };
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
