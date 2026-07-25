import {
  isMidgardConsensusProfileV1,
  MIDGARD_PROTOCOL_V1_VERSION,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  credentialToAddress,
  Data,
  LucidEvolution,
  makeReturn,
  scriptHashToCredential,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ActiveOperatorMintRedeemer,
} from "@/active-operators.js";
import {
  Bech32DeserializationError,
  MidgardValidators,
  UnspecifiedNetworkError,
} from "@/common.js";
import {
  DaParamsDatum,
  type DaParamsDatum as DaParamsDatumType,
  daParamsUnit,
} from "@/da-attestation.js";
import {
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  FraudProofCatalogueDatum,
  FraudProofCatalogueMintRedeemer,
} from "@/fraud-proof/catalogue.js";
import {
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  makeHubOracleDatum,
} from "@/hub-oracle.js";
import {
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
} from "@/ledger-constants.js";
import { castConfirmedStateToData, ConfirmedState } from "@/ledger-state.js";
import { encodeLinkedListNodeView, LinkedListNodeView } from "@/linked-list.js";
import {
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  RegisteredOperatorMintRedeemer,
} from "@/registered-operators.js";
import {
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  RetiredOperatorMintRedeemer,
} from "@/retired-operators.js";
import {
  INITIAL_SCHEDULER_DATUM,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  SchedulerMintRedeemer,
} from "@/scheduler.js";
import {
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
} from "@/state-queue.js";

export type AtomicProtocolInitReferenceScripts = {
  readonly hubOracleMinting: UTxO;
  readonly daParamsGovernorMinting: UTxO;
  readonly schedulerMinting: UTxO;
  readonly stateQueueMinting: UTxO;
  readonly registeredOperatorsMinting: UTxO;
  readonly activeOperatorsMinting: UTxO;
  readonly retiredOperatorsMinting: UTxO;
  readonly fraudProofCatalogueMinting: UTxO;
};

export type InitializationParams = {
  midgardValidators: MidgardValidators;
  consensusProfile: MidgardConsensusProfileV1;
  fraudProofCatalogueMerkleRoot: string;
  daParams: DaParamsDatumType;
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

// Atomic initialization appends protocol root outputs in a fixed order before
// wallet change, and each Init validator independently verifies its output.
const encodeInitOutputRedeemer = <T>(outputIndex: bigint, schema: T): string =>
  Data.to({ Init: { output_index: outputIndex } } as never, schema as never);

/**
 * Builds the unsigned transaction builder for initializing all Midgard contracts.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param initParams - Parameters for initializing all Midgard contracts.
 * @returns A promise that resolves to a `TxBuilder` instance.
 */
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
    if (!isMidgardConsensusProfileV1(params.consensusProfile)) {
      throw new Error(
        "Protocol initialization requires an exact compiled consensus profile",
      );
    }
    const hubOracleDatum = yield* makeHubOracleDatum(midgardValidators);
    const encodedHubOracleDatum = Data.to(hubOracleDatum, HubOracleDatum);
    const stateQueueGenesisTime = params.validityRange.validTo - 1n;
    const genesisConfirmedState: ConfirmedState = {
      headerHash: GENESIS_HEADER_HASH,
      prevHeaderHash: GENESIS_HEADER_HASH,
      utxoRoot: EMPTY_MERKLE_TREE_ROOT,
      startTime: stateQueueGenesisTime,
      endTime: stateQueueGenesisTime,
      protocolVersion: BigInt(MIDGARD_PROTOCOL_V1_VERSION),
    };

    const hubOracleUnit = toUnit(
      midgardValidators.hubOracle.policyId,
      HUB_ORACLE_ASSET_NAME,
    );
    const schedulerUnit = toUnit(
      midgardValidators.scheduler.policyId,
      SCHEDULER_ASSET_NAME,
    );
    const stateQueueUnit = toUnit(
      midgardValidators.stateQueue.policyId,
      STATE_QUEUE_ROOT_ASSET_NAME,
    );
    const registeredOperatorsUnit = toUnit(
      midgardValidators.registeredOperators.policyId,
      REGISTERED_OPERATORS_ROOT_ASSET_NAME,
    );
    const activeOperatorsUnit = toUnit(
      midgardValidators.activeOperators.policyId,
      ACTIVE_OPERATORS_ROOT_ASSET_NAME,
    );
    const retiredOperatorsUnit = toUnit(
      midgardValidators.retiredOperators.policyId,
      RETIRED_OPERATORS_ROOT_ASSET_NAME,
    );
    const fraudProofCatalogueUnit = toUnit(
      midgardValidators.fraudProofCatalogue.policyId,
      FRAUD_PROOF_CATALOGUE_ASSET_NAME,
    );
    const daParamsGovernorUnit = daParamsUnit(
      midgardValidators.daParamsGovernor,
    );

    const hubOracleAssets = { [hubOracleUnit]: 1n };
    const schedulerAssets = { [schedulerUnit]: 1n };
    const stateQueueAssets = { [stateQueueUnit]: 1n };
    const registeredOperatorsAssets = { [registeredOperatorsUnit]: 1n };
    const activeOperatorsAssets = { [activeOperatorsUnit]: 1n };
    const retiredOperatorsAssets = { [retiredOperatorsUnit]: 1n };
    const fraudProofCatalogueAssets = { [fraudProofCatalogueUnit]: 1n };
    const daParamsGovernorAssets = { [daParamsGovernorUnit]: 1n };

    const tx = lucid
      .newTx()
      .validFrom(Number(params.validityRange.validFrom))
      .validTo(Number(params.validityRange.validTo))
      .collectFrom([params.oneShotNonceUTxO])
      .mintAssets(daParamsGovernorAssets, Data.void())
      .pay.ToContract(
        midgardValidators.daParamsGovernor.spendingScriptAddress,
        {
          kind: "inline",
          value: Data.to(params.daParams, DaParamsDatum),
        },
        daParamsGovernorAssets,
      )
      .mintAssets(hubOracleAssets, Data.void())
      .pay.ToAddressWithData(
        credentialToAddress(
          network,
          scriptHashToCredential(midgardValidators.hubOracle.policyId),
        ),
        { kind: "inline", value: encodedHubOracleDatum },
        hubOracleAssets,
      )
      .mintAssets(schedulerAssets, Data.to("Init", SchedulerMintRedeemer))
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
        Data.to({ InitV1: { output_index: 3n } }, StateQueueRedeemer),
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
        encodeInitOutputRedeemer(4n, RegisteredOperatorMintRedeemer),
      )
      .pay.ToContract(
        midgardValidators.registeredOperators.spendingScriptAddress,
        { kind: "inline", value: encodeLinkedListRootDatum("") },
        registeredOperatorsAssets,
      )
      .mintAssets(
        activeOperatorsAssets,
        encodeInitOutputRedeemer(5n, ActiveOperatorMintRedeemer),
      )
      .pay.ToContract(
        midgardValidators.activeOperators.spendingScriptAddress,
        { kind: "inline", value: encodeLinkedListRootDatum("") },
        activeOperatorsAssets,
      )
      .mintAssets(
        retiredOperatorsAssets,
        encodeInitOutputRedeemer(6n, RetiredOperatorMintRedeemer),
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
        params.referenceScripts.daParamsGovernorMinting,
        params.referenceScripts.hubOracleMinting,
        params.referenceScripts.schedulerMinting,
        params.referenceScripts.stateQueueMinting,
        params.referenceScripts.registeredOperatorsMinting,
        params.referenceScripts.activeOperatorsMinting,
        params.referenceScripts.retiredOperatorsMinting,
        params.referenceScripts.fraudProofCatalogueMinting,
      ]);
    }

    return tx.attach
      .Script(midgardValidators.daParamsGovernor.mintingScript)
      .attach.Script(midgardValidators.hubOracle.mintingScript)
      .attach.Script(midgardValidators.scheduler.mintingScript)
      .attach.Script(midgardValidators.stateQueue.mintingScript)
      .attach.Script(midgardValidators.registeredOperators.mintingScript)
      .attach.Script(midgardValidators.activeOperators.mintingScript)
      .attach.Script(midgardValidators.retiredOperators.mintingScript)
      .attach.Script(midgardValidators.fraudProofCatalogue.mintingScript);
  });

export const unsignedInitializationTx = (
  lucid: LucidEvolution,
  initParams: InitializationParams,
): Promise<TxBuilder> =>
  makeReturn(incompleteInitializationTxProgram(lucid, initParams)).unsafeRun();
