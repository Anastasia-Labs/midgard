import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  Address,
  Assets,
  Constr,
  credentialToAddress,
  Data,
  fromText,
  LucidEvolution,
  PolicyId,
  scriptHashToCredential,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import {
  addressDataFromBech32,
  AddressSchema,
  AuthenticatedValidator,
  Bech32DeserializationError,
  GenericErrorFields,
  isHexString,
  LucidError,
  makeReturn,
  MidgardValidators,
  MintingValidator,
  ScriptHashSchema,
  UnspecifiedNetworkError,
} from "./common.js";
import {
  CORRECTION_LOCK_ASSET_NAME,
  CorrectionLockDatum,
} from "./correction-lock.js";
import {
  authenticateUTxOs,
  AuthenticUTxO,
  fetchSingleAuthenticUTxOProgram,
} from "./internals.js";

export type HubOracleConfig = {
  hubOracleAddress: Address;
  hubOraclePolicyId: PolicyId;
};

export const HUB_ORACLE_ASSET_NAME = fromText("MIDGARD_HUB_ORACLE");
export const HUB_ORACLE_ONE_SHOT_NONCE_DATUM_DOMAIN =
  "MidgardHubOracleOneShotNonceV1";

export type HubOracleOneShotNonceDatumParams = {
  readonly markerHex: string;
};

export type HubOracleOneShotNonceTxParams = {
  readonly address: Address;
  readonly amountLovelace: bigint;
  readonly markerHex: string;
};

export type IncompleteHubOracleOneShotNonceTx = {
  readonly txBuilder: TxBuilder;
  readonly inlineDatum: string;
};

export const HubOracleDatumSchema = Data.Object({
  registered_operators: ScriptHashSchema,
  active_operators: ScriptHashSchema,
  retired_operators: ScriptHashSchema,
  scheduler: ScriptHashSchema,
  state_queue: ScriptHashSchema,
  fraud_proof_catalogue: ScriptHashSchema,
  fraud_proof: ScriptHashSchema,
  deposit: ScriptHashSchema,
  withdrawal: ScriptHashSchema,
  tx_order: ScriptHashSchema,
  settlement: ScriptHashSchema,
  payout: ScriptHashSchema,
  registered_operators_addr: AddressSchema,
  active_operators_addr: AddressSchema,
  retired_operators_addr: AddressSchema,
  scheduler_addr: AddressSchema,
  state_queue_addr: AddressSchema,
  fraud_proof_catalogue_addr: AddressSchema,
  fraud_proof_addr: AddressSchema,
  deposit_addr: AddressSchema,
  withdrawal_addr: AddressSchema,
  tx_order_addr: AddressSchema,
  settlement_addr: AddressSchema,
  reserve_addr: AddressSchema,
  payout_addr: AddressSchema,
  reserve_observer: ScriptHashSchema,
});
export type HubOracleDatum = Data.Static<typeof HubOracleDatumSchema>;
export const HubOracleDatum = asDataType<HubOracleDatum>(HubOracleDatumSchema);

export type HubOracleUTxO = AuthenticUTxO<HubOracleDatum>;

export const utxosToHubOracleUTxOs = (
  utxos: UTxO[],
  nftPolicy: PolicyId,
): Effect.Effect<HubOracleUTxO[], LucidError> =>
  authenticateUTxOs<HubOracleDatum>(utxos, nftPolicy, HubOracleDatum);

/**
 * Parameters for the init transaction.
 */
export type HubOracleInitParams = {
  hubOracleMintValidator: MintingValidator;
  validators: HubOracleValidators;
  oneShotNonceUTxO: UTxO;
};

export type HubOracleValidators = Omit<
  MidgardValidators,
  "hubOracle" | "fraudProofs"
>;

export const makeHubOracleDatum = (
  validators: HubOracleValidators,
): Effect.Effect<HubOracleDatum, Bech32DeserializationError> =>
  Effect.gen(function* () {
    const [
      registeredOperatorsAddr,
      activeOperatorsAddr,
      retiredOperatorsAddr,
      schedulerAddr,
      stateQueueAddr,
      fraudProofCatalogueAddr,
      fraudProofAddr,
      depositAddr,
      withdrawalAddr,
      txOrderAddr,
      settlementAddr,
      payoutAddr,
    ] = yield* Effect.all(
      [
        validators.registeredOperators,
        validators.activeOperators,
        validators.retiredOperators,
        validators.scheduler,
        validators.stateQueue,
        validators.fraudProofCatalogue,
        validators.fraudProof,
        validators.deposit,
        validators.withdrawal,
        validators.txOrder,
        validators.settlement,
        validators.payout,
      ].map((authVal: AuthenticatedValidator) =>
        addressDataFromBech32(authVal.spendingScriptAddress),
      ),
      { concurrency: "unbounded" },
    );

    const reserveAddr = yield* addressDataFromBech32(
      validators.reserve.spendingScriptAddress,
    );

    return {
      registered_operators: validators.registeredOperators.policyId,
      active_operators: validators.activeOperators.policyId,
      retired_operators: validators.retiredOperators.policyId,
      scheduler: validators.scheduler.policyId,
      state_queue: validators.stateQueue.policyId,
      fraud_proof_catalogue: validators.fraudProofCatalogue.policyId,
      fraud_proof: validators.fraudProof.policyId,
      deposit: validators.deposit.policyId,
      withdrawal: validators.withdrawal.policyId,
      tx_order: validators.txOrder.policyId,
      settlement: validators.settlement.policyId,
      payout: validators.payout.policyId,
      registered_operators_addr: registeredOperatorsAddr,
      active_operators_addr: activeOperatorsAddr,
      retired_operators_addr: retiredOperatorsAddr,
      scheduler_addr: schedulerAddr,
      state_queue_addr: stateQueueAddr,
      fraud_proof_catalogue_addr: fraudProofCatalogueAddr,
      fraud_proof_addr: fraudProofAddr,
      deposit_addr: depositAddr,
      withdrawal_addr: withdrawalAddr,
      tx_order_addr: txOrderAddr,
      settlement_addr: settlementAddr,
      reserve_addr: reserveAddr,
      payout_addr: payoutAddr,
      reserve_observer: validators.reserve.withdrawalScriptHash,
    };
  });

/**
 * Creates a hub oracle init transaction builder.
 * Handles datum construction internally from validators.
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {HubOracleInitParams} params - All validators that need to be registered in the hub oracle
 * @returns {TxBuilder} Effect that produces a transaction builder.
 */
export const incompleteHubOracleInitTxProgram = (
  lucid: LucidEvolution,
  params: HubOracleInitParams,
): Effect.Effect<
  TxBuilder,
  Bech32DeserializationError | UnspecifiedNetworkError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network) {
      const datum = yield* makeHubOracleDatum(params.validators);
      const encodedDatum = Data.to<HubOracleDatum>(datum, HubOracleDatum);

      const hubOracleAssets: Assets = {
        [toUnit(params.hubOracleMintValidator.policyId, HUB_ORACLE_ASSET_NAME)]:
          1n,
      };
      const correctionLockAssets: Assets = {
        [toUnit(
          params.hubOracleMintValidator.policyId,
          CORRECTION_LOCK_ASSET_NAME,
        )]: 1n,
      };

      return lucid
        .newTx()
        .collectFrom([params.oneShotNonceUTxO])
        .mintAssets(
          {
            ...hubOracleAssets,
            ...correctionLockAssets,
          },
          Data.void(),
        )
        .pay.ToAddressWithData(
          credentialToAddress(
            network,
            scriptHashToCredential(params.hubOracleMintValidator.policyId),
          ),
          { kind: "inline", value: encodedDatum },
          hubOracleAssets,
        )
        .pay.ToContract(
          params.validators.correctionLock.spendingScriptAddress,
          { kind: "inline", value: Data.to("Idle", CorrectionLockDatum) },
          correctionLockAssets,
        )
        .attach.MintingPolicy(params.hubOracleMintValidator.mintingScript);
    } else {
      return yield* new UnspecifiedNetworkError({
        message: "",
        cause: "Cardano network not found",
      });
    }
  });

export class HubOracleError extends EffectData.TaggedError(
  "HubOracleError",
)<GenericErrorFields> {}

const validateHubOracleOneShotNonceMarkerHex = (
  markerHex: string,
): Effect.Effect<string, HubOracleError> => {
  if (markerHex.length === 0) {
    return Effect.fail(
      new HubOracleError({
        message: "Invalid hub-oracle one-shot nonce marker",
        cause: "markerHex must be non-empty hex bytes",
      }),
    );
  }
  if (markerHex.length % 2 !== 0) {
    return Effect.fail(
      new HubOracleError({
        message: "Invalid hub-oracle one-shot nonce marker",
        cause: "markerHex must contain an even number of hex characters",
      }),
    );
  }
  if (!isHexString(markerHex)) {
    return Effect.fail(
      new HubOracleError({
        message: "Invalid hub-oracle one-shot nonce marker",
        cause: "markerHex must contain only hex characters",
      }),
    );
  }
  return Effect.succeed(markerHex);
};

export const makeHubOracleOneShotNonceDatum = (
  params: HubOracleOneShotNonceDatumParams,
): Effect.Effect<string, HubOracleError> =>
  Effect.gen(function* () {
    const markerHex = yield* validateHubOracleOneShotNonceMarkerHex(
      params.markerHex,
    );
    return yield* Effect.try({
      try: () => Data.to(new Constr(0, [markerHex])),
      catch: (cause) =>
        new HubOracleError({
          message: "Failed to encode hub-oracle one-shot nonce datum",
          cause,
        }),
    });
  });

export const incompleteHubOracleOneShotNonceTxProgram = (
  lucid: LucidEvolution,
  params: HubOracleOneShotNonceTxParams,
): Effect.Effect<IncompleteHubOracleOneShotNonceTx, HubOracleError> =>
  Effect.gen(function* () {
    if (params.amountLovelace <= 0n) {
      return yield* new HubOracleError({
        message: "Invalid hub-oracle one-shot nonce amount",
        cause: "amountLovelace must be greater than zero",
      });
    }

    const inlineDatum = yield* makeHubOracleOneShotNonceDatum({
      markerHex: params.markerHex,
    });
    const txBuilder = yield* Effect.try({
      try: () =>
        lucid
          .newTx()
          .pay.ToAddressWithData(
            params.address,
            { kind: "inline", value: inlineDatum },
            { lovelace: params.amountLovelace },
          ),
      catch: (cause) =>
        new HubOracleError({
          message:
            "Failed to build hub-oracle one-shot nonce preparation transaction",
          cause,
        }),
    });

    return { txBuilder, inlineDatum };
  });

/**
 * Attempts fetching the hub oracle UTxO.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO} - The authentic hub oracle UTxO.
 */
export const fetchHubOracleUTxOProgram = (
  lucid: LucidEvolution,
  config: HubOracleConfig,
): Effect.Effect<HubOracleUTxO, HubOracleError | LucidError> =>
  fetchSingleAuthenticUTxOProgram<HubOracleUTxO, LucidError, HubOracleError>(
    lucid,
    {
      address: config.hubOracleAddress,
      policyId: config.hubOraclePolicyId,
      utxoLabel: "hub oracle",
      conversionFunction: utxosToHubOracleUTxOs,
      onUnexpectedAuthenticUTxOCount: () =>
        new HubOracleError({
          message: "Failed to fetch the hub oracle UTxO",
          cause:
            "Exactly one hub oracle UTxO was expected, but none or more were found",
        }),
    },
  );

export const fetchHubOracleUTxO = (
  lucid: LucidEvolution,
  config: HubOracleConfig,
) => makeReturn(fetchHubOracleUTxOProgram(lucid, config)).unsafeRun();
