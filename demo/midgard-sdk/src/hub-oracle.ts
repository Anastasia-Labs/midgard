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
  MintingValidator,
  ScriptHashSchema,
  UnspecifiedNetworkError,
} from "@/common.js";
import {
  AuthenticUTxO,
  authenticateUTxOs,
  fetchSingleAuthenticUTxOProgram,
} from "@/internals.js";
import {
  Address,
  LucidEvolution,
  PolicyId,
  credentialToAddress,
  fromText,
  scriptHashToCredential,
  toUnit,
  TxBuilder,
  UTxO,
  Data,
  Assets,
} from "@lucid-evolution/lucid";

export type HubOracleConfig = {
  hubOracleAddress: Address;
  hubOraclePolicyId: PolicyId;
};

export const HUB_ORACLE_ASSET_NAME = fromText("MIDGARD_HUB_ORACLE");

export const hubOracleAssetName = HUB_ORACLE_ASSET_NAME;

export const HubOracleDatumSchema = Data.Object({
  registered_operators: PolicyIdSchema,
  active_operators: PolicyIdSchema,
  retired_operators: PolicyIdSchema,
  scheduler: PolicyIdSchema,
  state_queue: PolicyIdSchema,
  fraud_proof_catalogue: PolicyIdSchema,
  fraud_proof: PolicyIdSchema,
  deposit: PolicyIdSchema,
  withdrawal: PolicyIdSchema,
  tx_order: PolicyIdSchema,
  settlement: PolicyIdSchema,
  payout: PolicyIdSchema,
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
export const HubOracleDatum = HubOracleDatumSchema as unknown as HubOracleDatum;

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

      const assets: Assets = {
        [toUnit(params.hubOracleMintValidator.policyId, HUB_ORACLE_ASSET_NAME)]:
          1n,
      };

      return lucid
        .newTx()
        .collectFrom([params.oneShotNonceUTxO])
        .mintAssets(assets, Data.void())
        .pay.ToAddressWithData(
          credentialToAddress(
            network,
            scriptHashToCredential(params.hubOracleMintValidator.policyId),
          ),
          { kind: "inline", value: encodedDatum },
          assets,
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
