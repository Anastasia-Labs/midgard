import { Effect } from "effect";
import {
  Data,
  credentialToAddress,
  LucidEvolution,
  makeReturn,
  scriptHashToCredential,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import {
  Bech32DeserializationError,
  MidgardValidators,
  UnspecifiedNetworkError,
} from "@/common.js";
import {
  HubOracleDatum,
  HUB_ORACLE_ASSET_NAME,
  makeHubOracleDatum,
} from "@/hub-oracle.js";
import {
  INITIAL_SCHEDULER_DATUM,
  SchedulerDatum,
  SCHEDULER_ASSET_NAME,
  SchedulerMintRedeemer,
} from "@/scheduler.js";
import {
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
  GENESIS_UTXO_ROOT,
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
} from "@/state-queue.js";
import {
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ActiveOperatorMintRedeemer,
} from "@/active-operators.js";
import {
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  RegisteredOperatorMintRedeemer,
} from "@/registered-operators.js";
import {
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  RetiredOperatorMintRedeemer,
} from "@/retired-operators.js";
import {
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  FraudProofCatalogueDatum,
  FraudProofCatalogueMintRedeemer,
} from "@/fraud-proof/catalogue.js";
import {
  castConfirmedStateToData,
  ConfirmedState,
} from "@/ledger-state.js";
import {
  encodeLinkedListNodeView,
  LinkedListNodeView,
} from "@/linked-list.js";

export const ATOMIC_INIT_OUTPUT_INDEXES = {
  hubOracle: 0n,
  scheduler: 1n,
  stateQueue: 2n,
  registeredOperators: 3n,
  activeOperators: 4n,
  retiredOperators: 5n,
  fraudProofCatalogue: 6n,
} as const;

export type AtomicProtocolInitReferenceScripts = {
  readonly hubOracleMinting: UTxO;
  readonly schedulerMinting: UTxO;
  readonly stateQueueMinting: UTxO;
  readonly registeredOperatorsMinting: UTxO;
  readonly activeOperatorsMinting: UTxO;
  readonly retiredOperatorsMinting: UTxO;
  readonly fraudProofCatalogueMinting: UTxO;
};

export type InitializationParams = {
  midgardValidators: MidgardValidators;
  fraudProofCatalogueMerkleRoot: string;
  oneShotNonceUTxO: UTxO;
  validityRange: {
    readonly validFrom: bigint;
    readonly validTo: bigint;
  };
  referenceScripts?: AtomicProtocolInitReferenceScripts;
};

const encodeLinkedListRootDatum = (
  rootData: LinkedListNodeView["data"],
): string =>
  encodeLinkedListNodeView({
    key: "Empty",
    next: "Empty",
    data: rootData,
  });

export const incompleteInitializationTxProgram = (
  lucid: LucidEvolution,
  params: InitializationParams,
): Effect.Effect<
  TxBuilder,
  Bech32DeserializationError | UnspecifiedNetworkError
> =>
  Effect.gen(function* () {
    const network = lucid.config().network;
    if (network === undefined) {
      return yield* Effect.fail(
        new UnspecifiedNetworkError({
          message: "Failed to build atomic protocol initialization",
          cause: "lucid.config().network is undefined",
        }),
      );
    }

    const { midgardValidators } = params;
    const hubOracleDatum = yield* makeHubOracleDatum(midgardValidators);
    const encodedHubOracleDatum = Data.to(
      hubOracleDatum,
      HubOracleDatum,
    );
    const stateQueueGenesisTime = params.validityRange.validTo - 1n;
    const genesisConfirmedState: ConfirmedState = {
      headerHash: GENESIS_HEADER_HASH,
      prevHeaderHash: GENESIS_HEADER_HASH,
      utxoRoot: GENESIS_UTXO_ROOT,
      startTime: stateQueueGenesisTime,
      endTime: stateQueueGenesisTime,
      protocolVersion: GENESIS_PROTOCOL_VERSION,
    };

    const hubOracleAssets = {
      [toUnit(midgardValidators.hubOracle.policyId, HUB_ORACLE_ASSET_NAME)]: 1n,
    };
    const schedulerAssets = {
      [toUnit(midgardValidators.scheduler.policyId, SCHEDULER_ASSET_NAME)]: 1n,
    };
    const stateQueueAssets = {
      [toUnit(midgardValidators.stateQueue.policyId, STATE_QUEUE_ROOT_ASSET_NAME)]:
        1n,
    };
    const registeredOperatorsAssets = {
      [toUnit(
        midgardValidators.registeredOperators.policyId,
        REGISTERED_OPERATORS_ROOT_ASSET_NAME,
      )]: 1n,
    };
    const activeOperatorsAssets = {
      [toUnit(
        midgardValidators.activeOperators.policyId,
        ACTIVE_OPERATORS_ROOT_ASSET_NAME,
      )]: 1n,
    };
    const retiredOperatorsAssets = {
      [toUnit(
        midgardValidators.retiredOperators.policyId,
        RETIRED_OPERATORS_ROOT_ASSET_NAME,
      )]: 1n,
    };
    const fraudProofCatalogueAssets = {
      [toUnit(
        midgardValidators.fraudProofCatalogue.policyId,
        FRAUD_PROOF_CATALOGUE_ASSET_NAME,
      )]: 1n,
    };

    const tx = lucid
      .newTx()
      .validFrom(Number(params.validityRange.validFrom))
      .validTo(Number(params.validityRange.validTo))
      .collectFrom([params.oneShotNonceUTxO])
      .mintAssets(hubOracleAssets, Data.void())
      .pay.ToAddressWithData(
        credentialToAddress(
          network,
          scriptHashToCredential(midgardValidators.hubOracle.policyId),
        ),
        { kind: "inline", value: encodedHubOracleDatum },
        hubOracleAssets,
      )
      .mintAssets(
        schedulerAssets,
        Data.to("Init", SchedulerMintRedeemer),
      )
      .pay.ToContract(
        midgardValidators.scheduler.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(INITIAL_SCHEDULER_DATUM, SchedulerDatum),
        },
        schedulerAssets,
      )
      .mintAssets(
        stateQueueAssets,
        Data.to(
          {
            Init: {
              output_index: ATOMIC_INIT_OUTPUT_INDEXES.stateQueue,
            },
          },
          StateQueueRedeemer,
        ),
      )
      .pay.ToContract(
        midgardValidators.stateQueue.spendingScriptAddress,
        {
          kind: "inline",
          value: encodeLinkedListRootDatum(
            castConfirmedStateToData(
              genesisConfirmedState,
            ) as LinkedListNodeView["data"],
          ),
        },
        stateQueueAssets,
      )
      .mintAssets(
        registeredOperatorsAssets,
        Data.to(
          {
            Init: {
              output_index: ATOMIC_INIT_OUTPUT_INDEXES.registeredOperators,
            },
          },
          RegisteredOperatorMintRedeemer,
        ),
      )
      .pay.ToContract(
        midgardValidators.registeredOperators.spendingScriptAddress,
        { kind: "inline", value: encodeLinkedListRootDatum("") },
        registeredOperatorsAssets,
      )
      .mintAssets(
        activeOperatorsAssets,
        Data.to(
          {
            Init: {
              output_index: ATOMIC_INIT_OUTPUT_INDEXES.activeOperators,
            },
          },
          ActiveOperatorMintRedeemer,
        ),
      )
      .pay.ToContract(
        midgardValidators.activeOperators.spendingScriptAddress,
        { kind: "inline", value: encodeLinkedListRootDatum("") },
        activeOperatorsAssets,
      )
      .mintAssets(
        retiredOperatorsAssets,
        Data.to(
          {
            Init: {
              output_index: ATOMIC_INIT_OUTPUT_INDEXES.retiredOperators,
            },
          },
          RetiredOperatorMintRedeemer,
        ),
      )
      .pay.ToContract(
        midgardValidators.retiredOperators.spendingScriptAddress,
        { kind: "inline", value: encodeLinkedListRootDatum("") },
        retiredOperatorsAssets,
      )
      .mintAssets(
        fraudProofCatalogueAssets,
        Data.to("Init", FraudProofCatalogueMintRedeemer),
      )
      .pay.ToAddressWithData(
        midgardValidators.fraudProofCatalogue.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(
            params.fraudProofCatalogueMerkleRoot,
            FraudProofCatalogueDatum,
          ),
        },
        fraudProofCatalogueAssets,
      );

    if (params.referenceScripts !== undefined) {
      return tx.readFrom([
        params.referenceScripts.hubOracleMinting,
        params.referenceScripts.schedulerMinting,
        params.referenceScripts.stateQueueMinting,
        params.referenceScripts.registeredOperatorsMinting,
        params.referenceScripts.activeOperatorsMinting,
        params.referenceScripts.retiredOperatorsMinting,
        params.referenceScripts.fraudProofCatalogueMinting,
      ]);
    }

    return tx
      .attach.MintingPolicy(midgardValidators.hubOracle.mintingScript)
      .attach.Script(midgardValidators.scheduler.mintingScript)
      .attach.Script(midgardValidators.stateQueue.mintingScript)
      .attach.Script(midgardValidators.registeredOperators.mintingScript)
      .attach.Script(midgardValidators.activeOperators.mintingScript)
      .attach.Script(midgardValidators.retiredOperators.mintingScript)
      .attach.Script(midgardValidators.fraudProofCatalogue.mintingScript);
  });

export const unsignedInitializationTxProgram = (
  lucid: LucidEvolution,
  initParams: InitializationParams,
): Effect.Effect<
  TxBuilder,
  Bech32DeserializationError | UnspecifiedNetworkError
> => incompleteInitializationTxProgram(lucid, initParams);

/**
 * Builds the unsigned transaction builder for initializing all Midgard contracts.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param initParams - Parameters for initializing all Midgard contracts.
 * @returns A promise that resolves to a `TxBuilder` instance.
 */
export const unsignedInitializationTx = (
  lucid: LucidEvolution,
  initParams: InitializationParams,
): Promise<TxBuilder> =>
  makeReturn(unsignedInitializationTxProgram(lucid, initParams)).unsafeRun();
