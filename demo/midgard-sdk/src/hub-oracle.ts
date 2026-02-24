import { Effect, Data as EffectData } from "effect";
import {
  GenericErrorFields,
  LucidError,
  AddressSchema,
  PolicyIdSchema,
  makeReturn,
  addressDataFromBech32,
  Bech32DeserializationError,
  MidgardValidators,
  AuthenticatedValidator,
  ScriptHashSchema,
  UnspecifiedNetworkError,
} from "@/common.js";
import {
  Address,
  LucidEvolution,
  PolicyId,
  toUnit,
  TxBuilder,
  UTxO,
  Data,
  Assets,
} from "@lucid-evolution/lucid";
import { HUB_ORACLE_ASSET_NAME } from "@/constants.js";

export type HubOracleConfig = {
  hubOracleAddress: Address;
  hubOraclePolicyId: PolicyId;
};

// TODO: This should ideally come from Aiken env directory.
export const hubOracleAssetName = "";

export const HubOracleDatumSchema = Data.Object({
  registeredOperators: PolicyIdSchema,
  activeOperators: PolicyIdSchema,
  retiredOperators: PolicyIdSchema,
  scheduler: PolicyIdSchema,
  stateQueue: PolicyIdSchema,
  fraudProofCatalogue: PolicyIdSchema,
  fraudProof: PolicyIdSchema,
  deposit: PolicyIdSchema,
  withdrawal: PolicyIdSchema,
  txOrder: PolicyIdSchema,
  settlement: PolicyIdSchema,
  payout: PolicyIdSchema,
  registeredOperatorsAddr: AddressSchema,
  activeOperatorsAddr: AddressSchema,
  retiredOperatorsAddr: AddressSchema,
  schedulerAddr: AddressSchema,
  stateQueueAddr: AddressSchema,
  fraudProofCatalogueAddr: AddressSchema,
  fraudProofAddr: AddressSchema,
  depositAddr: AddressSchema,
  withdrawalAddr: AddressSchema,
  txOrderAddr: AddressSchema,
  settlementAddr: AddressSchema,
  payoutAddr: AddressSchema,
  reserveAddr: AddressSchema,
  reserveObserver: ScriptHashSchema,
});
export type HubOracleDatum = Data.Static<typeof HubOracleDatumSchema>;
export const HubOracleDatum = HubOracleDatumSchema as unknown as HubOracleDatum;

export type HubOracleInitParams = {
  hubOracleValidator: AuthenticatedValidator;
  validators: HubOracleValidators;
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
      registeredOperators: validators.registeredOperators.policyId,
      activeOperators: validators.activeOperators.policyId,
      retiredOperators: validators.retiredOperators.policyId,
      scheduler: validators.scheduler.policyId,
      stateQueue: validators.stateQueue.policyId,
      fraudProofCatalogue: validators.fraudProofCatalogue.policyId,
      fraudProof: validators.fraudProof.policyId,
      deposit: validators.deposit.policyId,
      withdrawal: validators.withdrawal.policyId,
      txOrder: validators.txOrder.policyId,
      settlement: validators.settlement.policyId,
      payout: validators.payout.policyId,
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
      reserveAddr,
      reserveObserver: validators.reserve.withdrawalScriptHash,
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

      const assets: Assets = {
        [toUnit(params.hubOracleValidator.policyId, HUB_ORACLE_ASSET_NAME)]: 1n,
      };

      return lucid
        .newTx()
        .mintAssets(assets, Data.void())
        .pay.ToAddressWithData(
          params.hubOracleValidator.spendingScriptAddress,
          { kind: "inline", value: encodedDatum },
          assets,
        )
        .attach.MintingPolicy(params.hubOracleValidator.mintingScript);
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

export const fetchHubOracleUTxOProgram = (
  lucid: LucidEvolution,
  config: HubOracleConfig,
): Effect.Effect<
  { utxo: UTxO; datum: HubOracleDatum },
  HubOracleError | LucidError
> =>
  Effect.gen(function* () {
    const errorMessage = "Failed to fetch the hub oracle UTxO";
    const hubOracleUTxOs: UTxO[] = yield* Effect.tryPromise({
      try: async () => {
        return await lucid.utxosAtWithUnit(
          config.hubOracleAddress,
          toUnit(config.hubOraclePolicyId, hubOracleAssetName),
        );
      },
      catch: (e) =>
        new LucidError({
          message: errorMessage,
          cause: e,
        }),
    });
    if (hubOracleUTxOs.length === 1) {
      const utxo = hubOracleUTxOs[0];
      const datum = yield* Effect.try({
        try: () => {
          if (utxo.datum) {
            const coerced = Data.from(utxo.datum, HubOracleDatum);
            return coerced;
          } else {
            throw new HubOracleError({
              message: errorMessage,
              cause: "Hub oracle UTxO datum is missing",
            });
          }
        },
        catch: (e) => {
          return new HubOracleError({
            message: errorMessage,
            cause: `Failed to parse the hub oracle datum: ${e}`,
          });
        },
      });
      return { utxo, datum };
    } else {
      return yield* Effect.fail(
        new HubOracleError({
          message: errorMessage,
          cause:
            "Exactly one hub oracle UTxO was expected, but none or more were found",
        }),
      );
    }
  });

/**
 * Attempts fetching the hub oracle UTxO.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO} - The authentic hub oracle UTxO.
 */
export const fetchHubOracleUTxO = (
  lucid: LucidEvolution,
  config: HubOracleConfig,
) => makeReturn(fetchHubOracleUTxOProgram(lucid, config)).unsafeRun();
