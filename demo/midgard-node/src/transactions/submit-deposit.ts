/**
 * Deposit submission flow for projecting deposit observations into Midgard
 * state.
 * This module owns node/API concerns and delegates production transaction
 * construction to the SDK user-event builders.
 */
import { normalizeHex as normalizeCoreHex } from "@al-ft/midgard-core/hex";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  CML,
  coreToTxOutput,
  Data as LucidData,
  datumToHash,
  getAddressDetails,
  Lucid as makeLucid,
  type LucidEvolution,
  type Network,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Option } from "effect";
import { Data as EffectData, Effect } from "effect";

import {
  parseAdditionalAssetSpecs,
  parseLovelaceAmount,
} from "../asset-specs.js";
import { DepositSubmissionAttemptsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { persistDepositUTxOs } from "../fibers/fetch-and-insert-deposit-utxos.js";
import {
  Database,
  Lucid as LucidService,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import {
  historyAdmissionMetadata,
  historyIntentOptionsData,
  submitDurableEventHistoryProgram,
} from "./event-history-submission.js";

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
): boolean => {
  const entries = Object.entries(left);
  return (
    entries.length === Object.keys(right).length &&
    entries.every(([unit, amount]) => right[unit] === amount)
  );
};

const inputOutRefsFromCompletedTx = (
  tx: CML.Transaction,
): readonly string[] => {
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

export const depositSubmissionAttemptFromCompletedTx = ({
  txHash,
  transactionCbor,
  metadata,
  config,
}: {
  readonly txHash: string;
  readonly transactionCbor: string;
  readonly metadata: DepositBuildMetadata;
  readonly config: SubmitDepositConfig;
}): DepositSubmissionAttemptsDB.InsertSubmittedInput => {
  const tx = CML.Transaction.from_cbor_hex(transactionCbor);
  if (CML.hash_transaction(tx.body()).to_hex() !== txHash)
    throw new Error(
      "Deposit transaction hash does not match its completed body",
    );
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
    const node = LucidData.from(output.datum, SDK.EventHistoryNode);
    if (
      node.position === "Root" ||
      node.payload === "RootContent" ||
      !("Order" in node.payload)
    )
      throw new Error(
        "Expected an authenticated deposit Order in the completed transaction",
      );
    const facts = node.payload.Order.facts;
    const actualEventId = LucidData.to(facts.event_id, SDK.OutputReference);
    if (actualEventId !== metadata.depositEventId) {
      continue;
    }
    const key = datumToHash(actualEventId);
    if (
      node.position.Key[0] !== key ||
      metadata.depositAssetName !== key ||
      !metadata.depositAuthUnit.endsWith(key) ||
      metadata.depositAuthUnit.length !== 120 ||
      facts.structural_lovelace !== metadata.structuralLovelace ||
      facts.inclusion_time !== BigInt(metadata.inclusionTime) ||
      outputIndex !== metadata.orderOutputIndex
    )
      throw new Error(
        "Completed deposit Order does not match its full key, funding or inclusion metadata",
      );
    const assets = {
      ...output.assets,
      lovelace: (output.assets.lovelace ?? 0n) - facts.structural_lovelace,
    };
    if (assets.lovelace < 0n)
      throw new Error(
        "Completed deposit structural ADA exceeds its locked funds",
      );
    matches.push({ outputIndex, assets });
  }

  if (matches.length !== 1) {
    throw new Error(
      `Expected exactly one deposit output for event ${metadata.depositEventId} in completed tx ${txHash}; found ${matches.length.toString()}`,
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
  if (
    !sameSerializedAssets(expectedSerializedAssets, requestedSerializedAssets)
  ) {
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
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF]:
      `${txHash}#${match.outputIndex.toString()}`,
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
      structuralLovelace: metadata.structuralLovelace.toString(),
      orderOutputIndex: metadata.orderOutputIndex,
      l2DatumCbor: config.l2Datum,
      transactionCbor,
    },
    [DepositSubmissionAttemptsDB.Columns.FUNDING_OUT_REFS]:
      inputOutRefsFromCompletedTx(tx),
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

/** Compare current L1 facts with the persisted submission intent, independent
 * of the mutable history output location. A cache row alone cannot confirm it. */
export const matchesDepositSubmissionIntent = (
  deposit: SDK.DepositUTxO,
  attempt: DepositSubmissionAttemptsDB.InsertSubmittedInput,
  network: Network,
): boolean => {
  const metadata = attempt[DepositSubmissionAttemptsDB.Columns.METADATA];
  if (metadata.l2DatumCbor === undefined)
    throw new Error("Deposit submission intent is missing its L2 datum");
  const expectedAddress = Effect.runSync(
    SDK.addressDataFromBech32(
      attempt[DepositSubmissionAttemptsDB.Columns.EXPECTED_L2_ADDRESS],
    ),
  );
  const infoCbor = deposit.infoCbor.toString("hex");
  const info = LucidData.from(infoCbor, SDK.DepositInfo);
  const datumCbor = (cbor: string | null) =>
    cbor === null
      ? null
      : aikenSerialisedPlutusDataCborPreservingMapOrder(cbor);
  return (
    deposit.idCbor.equals(
      attempt[DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID],
    ) &&
    deposit.utxo.address === metadata.depositAddress &&
    deposit.assetName === metadata.depositAssetName &&
    deposit.utxo.assets[metadata.depositAuthUnit] === 1n &&
    deposit.facts.inclusion_time === BigInt(metadata.inclusionTime) &&
    deposit.facts.structural_lovelace === BigInt(metadata.structuralLovelace) &&
    info.l2_network_id === (network === "Mainnet" ? 1n : 0n) &&
    LucidData.to(info.l2_address, SDK.AddressData) ===
      LucidData.to(expectedAddress, SDK.AddressData) &&
    datumCbor(
      info.l2_datum === null ? null : plutusConstrFieldCbor(infoCbor, [2, 0]),
    ) === datumCbor(metadata.l2DatumCbor) &&
    sameSerializedAssets(
      serializeAssets(deposit.originalAssets),
      attempt[DepositSubmissionAttemptsDB.Columns.EXPECTED_ASSETS],
    )
  );
};

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
    const { api: lucid } = yield* LucidService;
    const contracts = yield* MidgardContracts;
    const nodeConfig = yield* NodeConfig;
    const deposits = yield* SDK.fetchDepositUTxOsProgram(
      lucid,
      SDK.eventHistoryDeploymentFromContracts(
        SDK.requireEventHistoryContracts(contracts).deposit,
      ),
    );
    const eventId =
      attempt[DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID];
    const observed = deposits.find((deposit) => deposit.idCbor.equals(eventId));
    const matchesIntent =
      observed === undefined
        ? false
        : yield* Effect.try({
            try: () =>
              matchesDepositSubmissionIntent(
                observed,
                attempt,
                nodeConfig.NETWORK,
              ),
            catch: (cause) =>
              new SDK.LucidError({
                message:
                  "Failed to compare authenticated deposit with its submission intent",
                cause,
              }),
          });
    if (matchesIntent) {
      const { reconciledCount } = yield* persistDepositUTxOs(
        deposits,
        nodeConfig.NETWORK,
      );
      yield* DepositSubmissionAttemptsDB.markReconciled(txHashBuffer);
      return {
        txHash,
        depositEventId: eventId.toString("hex"),
        status: "reconciled_after_timeout",
        expectedDepositOutRef:
          attempt[DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF],
        depositRowsFound: 1,
        reconciledCount,
        nextSafeAction:
          "Deposit matches the submission intent in current authenticated L1 history; continue without resubmitting.",
      } as const;
    }
    const reason =
      observed === undefined
        ? `Expected deposit event ${eventId.toString("hex")} is absent from current authenticated L1 history; absence after retirement does not establish submission failure.`
        : `Authenticated deposit event ${eventId.toString("hex")} differs from the persisted submission intent.`;
    yield* DepositSubmissionAttemptsDB.markAmbiguous(txHashBuffer, reason);
    return {
      txHash,
      depositEventId: eventId.toString("hex"),
      status: "ambiguous",
      expectedDepositOutRef:
        attempt[DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF],
      depositRowsFound: observed === undefined ? 0 : 1,
      reconciledCount: 0,
      nextSafeAction:
        "Do not resubmit yet; reconcile the original transaction receipt and current event history.",
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
      try: () => {
        const { provider, slotConfig } = lucid.config();
        return makeLucid(
          provider,
          network,
          slotConfig === undefined ? undefined : { slotConfig },
        );
      },
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

/** Exact semantic datum bytes remain part of the durable pre-nonce intent. */
export const depositSubmissionIntentHash = (
  config: SubmitDepositConfig,
): string =>
  datumToHash(
    LucidData.to([
      config.l2Address === ""
        ? ""
        : getAddressDetails(config.l2Address).address.hex,
      config.l2Datum === null
        ? []
        : [aikenSerialisedPlutusDataCborPreservingMapOrder(config.l2Datum)],
      config.lovelace,
      Object.entries(config.additionalAssets)
        .sort(([a], [b]) => a.localeCompare(b))
        .map(([unit, amount]) => [unit, amount]),
      config.structuralLovelace === undefined
        ? []
        : [config.structuralLovelace],
      historyIntentOptionsData(config),
    ]),
  );

export const submitDepositWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitDepositConfig,
  submissionId: string,
) =>
  Effect.gen(function* () {
    const intentHash = yield* Effect.try({
      try: () => depositSubmissionIntentHash(config),
      catch: (cause) =>
        new SubmitDepositError({
          message: "Invalid durable deposit intent",
          cause,
        }),
    });
    const metadataFrom = (
      attempt: SDK.EventHistorySubmissionAttempt,
      request: SDK.EventHistorySubmissionRequest,
    ): DepositBuildMetadata => {
      const admitted = historyAdmissionMetadata(lucid, attempt);
      return {
        depositAddress: admitted.output.address,
        depositEventId: SDK.outputReferenceToPlutusDataCbor(request.nonce),
        depositAssetName: admitted.key,
        depositAuthUnit: contracts.deposit.policyId + admitted.key,
        nonceInput: {
          txHash: request.nonce.txHash,
          outputIndex: request.nonce.outputIndex,
        },
        validTo: admitted.validTo,
        inclusionTime: Number(admitted.facts.inclusion_time),
        structuralLovelace: request.structuralLovelace,
        orderOutputIndex: attempt.outputIndex,
      };
    };
    const result = yield* submitDurableEventHistoryProgram({
      lucid,
      contracts,
      kind: "Deposit",
      submissionId,
      intentHash,
      nonceInput: config.nonceInput,
      scriptReference: config.referenceScripts?.depositMinting,
      prepare: (nonceInput) =>
        SDK.prepareDepositSubmissionProgram(lucid, contracts, {
          ...config,
          nonceInput,
        }),
      beforeAdmission: (attempt, request) =>
        Effect.gen(function* () {
          const input = yield* Effect.try({
            try: () =>
              depositSubmissionAttemptFromCompletedTx({
                txHash: attempt.txHash,
                transactionCbor: attempt.transactionCbor,
                metadata: metadataFrom(attempt, request),
                config,
              }),
            catch: (cause) =>
              new SubmitDepositError({
                message: "Invalid completed deposit intent",
                cause,
              }),
          });
          yield* DepositSubmissionAttemptsDB.insertSubmitted(input);
        }),
    });
    yield* DepositSubmissionAttemptsDB.markConfirmed(
      Buffer.from(result.admission.txHash, "hex"),
    );
    const metadata = yield* Effect.try({
      try: () => metadataFrom(result.admission, result.request),
      catch: (cause) =>
        new SubmitDepositError({
          message: "Invalid confirmed deposit receipt",
          cause,
        }),
    });
    return {
      submissionId,
      txHash: result.admission.txHash,
      metadata,
      confirmationStatus: "confirmed" as const,
    };
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
