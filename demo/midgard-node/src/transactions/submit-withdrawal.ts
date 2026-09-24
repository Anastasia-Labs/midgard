import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
/**
 * Withdrawal submission flow for creating authenticated withdrawal-order
 * events on L1.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  datumToHash,
  type LucidEvolution,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import {
  historyAdmissionMetadata,
  historyIntentOptionsData,
  submitDurableEventHistoryProgram,
} from "./event-history-submission.js";

export type SubmitWithdrawalReferenceScripts =
  SDK.SubmitWithdrawalReferenceScripts;
export type SubmitWithdrawalConfig = SDK.SubmitWithdrawalConfig;
export type WithdrawalBuildMetadata = SDK.WithdrawalBuildMetadata;

export class SubmitWithdrawalError extends EffectData.TaggedError(
  "SubmitWithdrawalError",
)<{
  message: string;
  cause: unknown;
}> {}

export const buildUnsignedWithdrawalTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitWithdrawalConfig,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly metadata: WithdrawalBuildMetadata;
  },
  | SDK.HubOracleError
  | SDK.LucidError
  | SDK.Bech32DeserializationError
  | SDK.HashingError
  | SubmitWithdrawalError
> =>
  SDK.buildUnsignedWithdrawalTxWithMetadataProgram(
    lucid,
    contracts,
    config,
  ).pipe(
    Effect.catchTag("UserEventBuildError", (error) =>
      Effect.fail(
        new SubmitWithdrawalError({
          message: error.message,
          cause: error.cause,
        }),
      ),
    ),
  );

/** Durable intent commits raw semantic body and refund bytes before nonce selection. */
export const withdrawalSubmissionIntentHash = (
  config: SubmitWithdrawalConfig,
): string => {
  if (
    (config.body !== undefined && config.bodyCbor !== undefined) ||
    (config.refundDatum !== undefined && config.refundDatumCbor !== undefined)
  )
    throw new Error(
      "Withdrawal intent must have exactly one encoding source per datum",
    );
  const body = config.bodyCbor ?? Data.to(config.body, SDK.WithdrawalBody);
  const refund =
    config.refundDatumCbor ??
    Data.to(config.refundDatum ?? "NoDatum", SDK.CardanoDatum);
  Data.from(body, SDK.WithdrawalBody);
  Data.from(refund, SDK.CardanoDatum);
  return datumToHash(
    Data.to([
      aikenSerialisedPlutusDataCborPreservingMapOrder(body),
      Data.from(Data.to(config.signature, SDK.WithdrawalSignature)),
      Data.from(Data.to(config.refundAddress, SDK.AddressData)),
      aikenSerialisedPlutusDataCborPreservingMapOrder(refund),
      config.lovelace === undefined ? [] : [config.lovelace],
      historyIntentOptionsData(config),
    ]),
  );
};

export const submitWithdrawalProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  config: SubmitWithdrawalConfig,
  submissionId: string,
) =>
  Effect.gen(function* () {
    const intentHash = yield* Effect.try({
      try: () => withdrawalSubmissionIntentHash(config),
      catch: (cause) =>
        new SubmitWithdrawalError({
          message: "Invalid durable withdrawal intent",
          cause,
        }),
    });
    const result = yield* submitDurableEventHistoryProgram({
      lucid,
      contracts,
      kind: "Withdrawal",
      submissionId,
      intentHash,
      nonceInput: config.nonceInput,
      scriptReference: config.referenceScripts?.withdrawalMinting,
      prepare: (nonceInput) =>
        SDK.prepareWithdrawalSubmissionProgram(lucid, contracts, {
          ...config,
          nonceInput,
        }),
    });
    const admitted = yield* Effect.try({
      try: () => historyAdmissionMetadata(lucid, result.admission),
      catch: (cause) =>
        new SubmitWithdrawalError({
          message: "Invalid confirmed withdrawal receipt",
          cause,
        }),
    });
    const metadata: WithdrawalBuildMetadata = {
      withdrawalAddress: admitted.output.address,
      withdrawalEventIdCbor: SDK.outputReferenceToPlutusDataCbor(
        result.request.nonce,
      ),
      withdrawalAuthUnit: contracts.withdrawal.policyId + admitted.key,
      nonceInput: {
        txHash: result.request.nonce.txHash,
        outputIndex: result.request.nonce.outputIndex,
      },
      validTo: admitted.validTo,
      inclusionTime: Number(admitted.facts.inclusion_time),
      lockedLovelace: result.request.assets.lovelace ?? 0n,
      orderOutputIndex: result.admission.outputIndex,
    };
    return { submissionId, txHash: result.admission.txHash, metadata };
  });
