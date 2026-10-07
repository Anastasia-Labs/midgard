import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data as LucidData,
  datumToHash,
  getAddressDetails,
  Lucid as makeLucid,
  type LucidEvolution,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Option } from "effect";
import { Effect } from "effect";

import { DepositSubmissionAttemptsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
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
import {
  type BuildDepositRequest,
  buildUnsignedDepositTxWithMetadataProgram,
  type BuiltUnsignedDepositTx,
  type DepositBuildMetadata,
  depositSubmissionAttemptFromCompletedTx,
  type DepositSubmissionReconciliationResult,
  matchesDepositSubmissionIntent,
  type SubmitDepositConfig,
  SubmitDepositError,
} from "./submit-deposit.deposit-submission-attempt-from-completed-tx.js";

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
      // The deposit row is the follower-change driver's to write (E-N1-2
      // ruling 1); this check only settles the submission attempt.
      yield* DepositSubmissionAttemptsDB.markReconciled(txHashBuffer);
      return {
        txHash,
        depositEventId: eventId.toString("hex"),
        status: "reconciled_after_timeout",
        expectedDepositOutRef:
          attempt[DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF],
        depositRowsFound: 1,
        reconciledCount: 0,
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

export const asObject = (value: unknown, field: string): UnknownRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object.`);
  }
  return value as UnknownRecord;
};

export const parseRequiredString = (value: unknown, field: string): string => {
  if (typeof value !== "string") {
    throw new Error(`${field} must be a string.`);
  }
  const normalized = value.trim();
  if (normalized.length === 0) {
    throw new Error(`${field} must not be empty.`);
  }
  return normalized;
};

export const parseOptionalString = (
  value: unknown,
  field: string,
): string | null => {
  if (value === undefined || value === null) {
    return null;
  }
  if (typeof value !== "string") {
    throw new Error(`${field} must be a string when provided.`);
  }
  const normalized = value.trim();
  return normalized.length === 0 ? null : normalized;
};
