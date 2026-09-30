import {
  Address,
  Assets,
  type BuildTxWithRedeemer,
  Data,
  PolicyId,
  Script,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  DataCoercionError,
  MissingDatumError,
  UnauthenticUtxoError,
} from "./common.js";
import { type CorrectionLockUTxO } from "./correction-lock.js";
import { getStateToken } from "./internals.js";
import { ConfirmedState, Header } from "./ledger-state.js";
import {
  getLinkedListNodeViewFromUTxO,
  LinkedListNodeView,
  NodeKey,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "./linked-list.js";
import {
  type ActiveOperatorSpendTxRedeemer,
  type StateQueueUTxO,
  type StateQueueYieldWitness,
} from "./state-queue.state-queue-redeemer-schema.js";

/**
 * Extracts the block header hash from a state queue UTxO.
 *
 * If the UTxO is the confirmed state node (`datum.key === "Empty"`), it
 * returns `confirmedState.headerHash` extracted from datum.
 * Otherwise, it drops the canonical state-queue block prefix from `assetName`
 * and returns the suffix as the header hash.
 */
export const headerHashFromStateQueueUTxO = (
  stateQueueUTxO: StateQueueUTxO,
): Effect.Effect<string, DataCoercionError> =>
  stateQueueUTxO.datum.key === "Empty"
    ? getConfirmedStateFromStateQueueDatum(stateQueueUTxO.datum).pipe(
        Effect.andThen(({ data }) => data.headerHash),
      )
    : Effect.succeed(
        stateQueueUTxO.assetName.slice(
          STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
        ),
      );

export type StateQueueFetchConfig = {
  stateQueueAddress: Address;
  stateQueuePolicyId: PolicyId;
};

/**
 * Emulator/test helper for exercising the real state_queue CommitBlockHeader
 * mint redeemer. Final input, output, reference-input, and paired spend redeemer
 * indexes are resolved from Lucid's `BuildTxWithRedeemer` context after
 * balancing. This helper validates the state-queue side of the commit path;
 * callers still need the paired active-operators spend to be protocol-valid
 * outside focused tests.
 */
export type EmulatorStateQueueCommitBlockHeaderParams = {
  anchorUTxO: StateQueueUTxO;
  newHeader: Header;
  /**
   * The new node's lovelace; defaults to `STATE_QUEUE_NODE_MIN_LOVELACE`, the
   * on-chain floor. A value below the floor is refused by the commit arm.
   */
  headerNodeLovelace?: bigint;
  additionalInputs?: readonly UTxO[];
  validFrom?: bigint;
  validTo?: bigint;
  schedulerRefInput: UTxO;
  correctionLockRefInput: CorrectionLockUTxO;
  /** Required when `anchorUTxO` is a block node; omitted for an empty queue. */
  confirmedStateRefInput?: UTxO;
  /** Required when the current head differs from the consumed tail anchor. */
  headStateQueueNodeRefInput?: UTxO;
  additionalRefInputs?: readonly UTxO[];
  activeOperatorInput: UTxO;
  activeOperatorSpendRedeemer: ActiveOperatorSpendTxRedeemer;
  activeOperatorSpendingScript: Script;
  continuedActiveOperatorOutput?: {
    readonly address: Address;
    readonly datum: string;
    readonly assets: Assets;
  };
  stateQueueSpendingScript: Script;
  stateQueueMintingScript: Script;
  readonly yieldWitness: StateQueueYieldWitness;
};

type AlreadySlashedRemoveParams = {
  readonly kind: "operatorAlreadySlashed";
  readonly activeOperatorsElementRefInput: UTxO;
  readonly retiredOperatorsElementRefInput: UTxO;
};

/**
 * The fraud-prover reward a bond-consuming removal must route (F04 §2.3;
 * 2026-08-11 owner ruling 7, D3).
 *
 * `proverEnterpriseAddress` is the enterprise address of the `fraud_prover`
 * key hash carried by the fraud-proof token's datum — never a submitter,
 * change, or stake-delegated address, which the on-chain guard refuses.
 * `lovelace` must equal the compiled `env.fraud_prover_reward`; there is no
 * default here because this SDK is not an economics authority (F04 §2.5), and
 * omitting the plan altogether is correct only while that compiled value is
 * zero.
 */
export type FraudProverRewardPlan = {
  readonly proverEnterpriseAddress: Address;
  readonly lovelace: bigint;
};

type SlashActiveOperatorRemoveParams = {
  readonly kind: "slashActiveOperator";
  /** Reward routing; omit only while `env.fraud_prover_reward` is zero. */
  readonly fraudProverReward?: FraudProverRewardPlan;
  /**
   * Supports the active-operators `SlashOperator` mint path. For full
   * active-operator validation, provide the anchor/node inputs, continued
   * anchor output, hub-oracle reference input, and scheduler sync data
   * required by the slashing redeemer.
   */
  readonly activeOperatorsAssetsToBurn: Assets;
  readonly activeOperatorsMintRedeemer: BuildTxWithRedeemer;
  readonly activeOperatorsMintingScript: Script;
  readonly activeOperatorInputs: readonly UTxO[];
  readonly activeOperatorSpendingScript?: Script;
  readonly activeOperatorSpendRedeemer?: ActiveOperatorSpendTxRedeemer;
  readonly continuedActiveOperatorAnchorOutput?: {
    readonly address: Address;
    readonly datum: string;
    readonly assets: Assets;
  };
  readonly schedulerSpend?: {
    readonly input: UTxO;
    readonly redeemer: BuildTxWithRedeemer;
    readonly script: Script;
    readonly continuedOutput: {
      readonly address: Address;
      readonly datum: string;
      readonly assets: Assets;
    };
  };
};

type SlashRetiredOperatorRemoveParams = {
  readonly kind: "slashRetiredOperator";
  /** Reward routing; omit only while `env.fraud_prover_reward` is zero. */
  readonly fraudProverReward?: FraudProverRewardPlan;
  readonly retiredOperatorsAssetsToBurn: Assets;
  readonly retiredOperatorsMintRedeemer: BuildTxWithRedeemer;
  readonly retiredOperatorsMintingScript: Script;
  readonly retiredOperatorInputs: readonly UTxO[];
  readonly retiredOperatorSpendingScript?: Script;
  readonly retiredOperatorSpendRedeemer?: string | BuildTxWithRedeemer;
  readonly continuedRetiredOperatorAnchorOutput?: {
    readonly address: Address;
    readonly datum: string;
    readonly assets: Assets;
  };
};

export type EmulatorStateQueueRemoveSlashingParams =
  | AlreadySlashedRemoveParams
  | SlashActiveOperatorRemoveParams
  | SlashRetiredOperatorRemoveParams;

/**
 * Emulator/test helper for RemoveFraudulentBlockHeader +
 * RemoveLastFraudulentBlock. Final layout-sensitive indexes are resolved from
 * Lucid's `BuildTxWithRedeemer` context after balancing.
 */
type EmulatorStateQueueRemoveLastFraudulentBlockHeaderCommonParams = {
  anchorUTxO: StateQueueUTxO;
  fraudulentBlockUTxO: StateQueueUTxO;
  additionalInputs?: readonly UTxO[];
  validFrom?: bigint;
  validTo?: bigint;
  fraudulentOperator: string;
  fraudulentBlocksHeaderHash?: string;
  fraudProofRefInput: UTxO;
  fraudProofPolicyId: PolicyId;
  hubOracleRefInput: UTxO;
  correctionLockInput: CorrectionLockUTxO;
  correctionLockSpendingScript: Script;
  additionalRefInputs?: readonly UTxO[];
  stateQueueSpendingScript: Script;
  stateQueueMintingScript: Script;
  readonly yieldWitness: StateQueueYieldWitness;
  referenceScripts?: StateQueueRemoveReferenceScriptUTxOs;
  slashing: EmulatorStateQueueRemoveSlashingParams;
  stateQueueMintRedeemer?: BuildTxWithRedeemer;
};

export type EmulatorStateQueueRemoveLastFraudulentBlockHeaderParams =
  EmulatorStateQueueRemoveLastFraudulentBlockHeaderCommonParams;

type EmulatorStateQueueRemoveFraudulentBlocksLinkParams = {
  fraudulentBlockUTxO: StateQueueUTxO;
  removedBlockUTxO: StateQueueUTxO;
  additionalInputs?: readonly UTxO[];
  validFrom?: bigint;
  validTo?: bigint;
  fraudulentOperator: string;
  fraudulentBlocksHeaderHash: string;
  fraudProofRefInput: UTxO;
  fraudProofPolicyId: PolicyId;
  hubOracleRefInput: UTxO;
  correctionLockInput: CorrectionLockUTxO;
  correctionLockSpendingScript: Script;
  additionalRefInputs?: readonly UTxO[];
  stateQueueSpendingScript: Script;
  stateQueueMintingScript: Script;
  readonly yieldWitness: StateQueueYieldWitness;
  referenceScripts?: StateQueueRemoveReferenceScriptUTxOs;
  slashing: EmulatorStateQueueRemoveSlashingParams;
  stateQueueMintRedeemer?: BuildTxWithRedeemer;
};

export type EmulatorStateQueueRemoveFraudulentBlocksLinkHeaderParams =
  EmulatorStateQueueRemoveFraudulentBlocksLinkParams;

export type StateQueueRemoveReferenceScriptUTxOs = {
  readonly correctionLockSpend?: UTxO;
  readonly stateQueueSpend?: UTxO;
  readonly stateQueueMint?: UTxO;
  readonly activeOperatorsSpend?: UTxO;
  readonly activeOperatorsMint?: UTxO;
  readonly retiredOperatorsSpend?: UTxO;
  readonly retiredOperatorsMint?: UTxO;
  readonly schedulerSpend?: UTxO;
};

/**
 * Validates correctness of datum, and having a single NFT.
 */
export const utxoToStateQueueUTxO = (
  utxo: UTxO,
  nftPolicy: string,
): Effect.Effect<
  StateQueueUTxO,
  DataCoercionError | MissingDatumError | UnauthenticUtxoError
> =>
  Effect.gen(function* () {
    const datum = yield* getLinkedListNodeViewFromUTxO(utxo);
    const [sym, assetName] = yield* getStateToken(utxo.assets);
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: "Failed to convert UTxO to `StateQueueUTxO`",
          cause: "UTxO's NFT policy ID is not the same as the state queue's",
        }),
      );
    }
    return { utxo, datum, assetName };
  });

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToStateQueueUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<StateQueueUTxO[]> => {
  const effects = utxos.map((u) => utxoToStateQueueUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};

/**
 * Given a StateQueue datum, this function confirms the node is root
 * (i.e. no keys in its datum), and attempts to coerce its underlying data into
 * a `ConfirmedState`.
 */
export const getConfirmedStateFromStateQueueDatum = (
  nodeDatum: LinkedListNodeView,
): Effect.Effect<
  { data: ConfirmedState; link: NodeKey },
  DataCoercionError
> => {
  try {
    if (nodeDatum.key === "Empty") {
      const confirmedState = Data.castFrom(nodeDatum.data, ConfirmedState);
      return Effect.succeed({
        data: confirmedState,
        link: nodeDatum.next,
      });
    } else {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce to a root node datum`,
          cause: `Given UTxO is not root`,
        }),
      );
    }
  } catch (e) {
    return Effect.fail(
      new DataCoercionError({
        message: `Could not coerce to a node datum`,
        cause: e,
      }),
    );
  }
};
