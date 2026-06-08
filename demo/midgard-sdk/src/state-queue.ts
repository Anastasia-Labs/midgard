import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  Address,
  Assets,
  type BuildTxWithRedeemer,
  Data,
  fromUnit,
  fromText,
  LucidEvolution,
  paymentCredentialOf,
  PolicyId,
  Script,
  toUnit,
  TxBuilder,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import { ActiveOperatorSpendRedeemer } from "@/active-operators.js";
import {
  AuthenticatedValidator,
  DataCoercionError,
  GenericErrorFields,
  HashingError,
  MerkleRoot,
  MerkleRootSchema,
  MissingDatumError,
  OutputReferenceSchema,
  outputReferenceFromUTxO,
  POSIXTime,
  UnauthenticUtxoError,
  utxosAtByNFTPolicyId,
} from "@/common.js";
import { LucidError, makeReturn } from "@/common.js";
import { getStateToken } from "@/internals.js";
import {
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
} from "@/ledger-constants.js";
import {
  castStateQueueNodeToData,
  ConfirmedState,
  getHeaderFromStateQueueDatum,
  hashBlockHeader,
  Header,
  HeaderHashSchema,
  NO_DA_ATTESTATION,
} from "@/ledger-state.js";
import {
  encodeLinkedListNodeView,
  getLinkedListNodeViewFromUTxO,
  incompleteInitLinkedListTxProgram,
  LinkedListError,
  LinkedListNodeView,
  NodeKey,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "@/linked-list.js";
import { completeTxWithLocalUPLCEvalProgram } from "@/tx-completion.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
} from "@/tx-context-redeemer.js";
import { dedupeAndSortUtxos } from "@/tx-out-ref-order.js";

export const STATE_QUEUE_ROOT_ASSET_NAME = fromText("MIDGARD_CONFIRMED_STATE");

type ActiveOperatorSpendTxRedeemer =
  | "ListStateTransition"
  | BuildTxWithRedeemer;

const encodeActiveOperatorSpendRedeemer = (
  redeemer: ActiveOperatorSpendTxRedeemer,
): string | BuildTxWithRedeemer =>
  typeof redeemer === "function"
    ? redeemer
    : Data.to(redeemer as never, ActiveOperatorSpendRedeemer as never);

export const SlashingApproachSchema = Data.Enum([
  Data.Object({
    SlashActiveOperator: Data.Object({
      active_operators_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    SlashRetiredOperator: Data.Object({
      retired_operators_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    OperatorAlreadySlashed: Data.Object({
      active_operators_element_ref_input_index: Data.Integer(),
      retired_operators_element_ref_input_index: Data.Integer(),
    }),
  }),
]);
export type SlashingApproach = Data.Static<typeof SlashingApproachSchema>;
export const SlashingApproach =
  SlashingApproachSchema as unknown as SlashingApproach;

export const BlockRemovalApproachSchema = Data.Enum([
  Data.Object({
    RemoveLastFraudulentBlock: Data.Object({
      anchor_element_input_outref: OutputReferenceSchema,
      anchor_element_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveFraudulentBlocksLink: Data.Object({
      fraudulent_node_input_outref: OutputReferenceSchema,
      fraudulent_node_output_index: Data.Integer(),
    }),
  }),
]);
export type BlockRemovalApproach = Data.Static<
  typeof BlockRemovalApproachSchema
>;
export const BlockRemovalApproach =
  BlockRemovalApproachSchema as unknown as BlockRemovalApproach;

export const StateQueueRedeemerSchema = Data.Enum([
  Data.Object({
    Init: Data.Object({
      output_index: Data.Integer(),
    }),
  }),
  Data.Literal("Deinit"),
  Data.Object({
    CommitBlockHeader: Data.Object({
      new_block_output_index: Data.Integer(),
      continued_latest_block_output_index: Data.Integer(),
      operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      scheduler_ref_input_index: Data.Integer(),
      active_operators_input_index: Data.Integer(),
      active_operators_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    MergeToConfirmedState: Data.Object({
      header_node_key: Data.Bytes(),
      confirmed_state_input_outref: OutputReferenceSchema,
      confirmed_state_output_index: Data.Integer(),
      m_settlement_redeemer_index: Data.Nullable(Data.Integer()),
      merged_block_transactions_root: MerkleRootSchema,
      merged_block_deposits_root: MerkleRootSchema,
      merged_block_withdrawals_root: MerkleRootSchema,
    }),
  }),
  Data.Object({
    RemoveFraudulentBlockHeader: Data.Object({
      fraudulent_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      fraudulent_blocks_header_hash: HeaderHashSchema,
      slashing_approach: SlashingApproachSchema,
      fraud_proof_ref_input_index: Data.Integer(),
      block_removal_approach: BlockRemovalApproachSchema,
    }),
  }),
]);
export type StateQueueRedeemer = Data.Static<typeof StateQueueRedeemerSchema>;
export const StateQueueRedeemer =
  StateQueueRedeemerSchema as unknown as StateQueueRedeemer;

export const StateQueueSpendRedeemerSchema = Data.Enum([
  Data.Literal("LinkedListMutation"),
  Data.Object({
    AttachDaAttestation: Data.Object({
      state_queue_input_index: Data.Integer(),
      da_attestation_mint_redeemer_index: Data.Integer(),
    }),
  }),
]);
export type StateQueueSpendRedeemer = Data.Static<
  typeof StateQueueSpendRedeemerSchema
>;
export const StateQueueSpendRedeemer =
  StateQueueSpendRedeemerSchema as unknown as StateQueueSpendRedeemer;

const STATE_QUEUE_LINKED_LIST_MUTATION_REDEEMER = Data.to(
  "LinkedListMutation" as never,
  StateQueueSpendRedeemer as never,
);

export type StateQueueUTxO = {
  utxo: UTxO;
  datum: LinkedListNodeView;
  assetName: string;
};

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

export type StateQueueInitParams = {
  validator: AuthenticatedValidator;
  genesisTime: POSIXTime; // Just pass the time, not the full state
  lovelace?: bigint;
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
  additionalInputs?: readonly UTxO[];
  validFrom?: bigint;
  validTo?: bigint;
  schedulerRefInput: UTxO;
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
};

type AlreadySlashedRemoveParams = {
  readonly kind: "operatorAlreadySlashed";
  readonly activeOperatorsElementRefInput: UTxO;
  readonly retiredOperatorsElementRefInput: UTxO;
};

type SlashActiveOperatorRemoveParams = {
  readonly kind: "slashActiveOperator";
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
  additionalRefInputs?: readonly UTxO[];
  stateQueueSpendingScript: Script;
  stateQueueMintingScript: Script;
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
  additionalRefInputs?: readonly UTxO[];
  stateQueueSpendingScript: Script;
  stateQueueMintingScript: Script;
  referenceScripts?: StateQueueRemoveReferenceScriptUTxOs;
  slashing: EmulatorStateQueueRemoveSlashingParams;
  stateQueueMintRedeemer?: BuildTxWithRedeemer;
};

export type EmulatorStateQueueRemoveFraudulentBlocksLinkHeaderParams =
  EmulatorStateQueueRemoveFraudulentBlocksLinkParams;

export type StateQueueRemoveReferenceScriptUTxOs = {
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

export const findLinkStateQueueUTxO = (
  link: NodeKey,
  utxos: StateQueueUTxO[],
): Effect.Effect<StateQueueUTxO, LinkedListError> => {
  const errorMessage = `Failed to find link state queue UTxO`;
  if (link === "Empty") {
    return Effect.fail(
      new LinkedListError({
        message: errorMessage,
        cause: `Given link is "Empty"`,
      }),
    );
  }

  const foundLink = utxos.find(
    (u: StateQueueUTxO) =>
      u.datum.key !== "Empty" && u.datum.key.Key.key === link.Key.key,
  );
  if (foundLink) {
    return Effect.succeed(foundLink);
  }

  return Effect.fail(
    new LinkedListError({
      message: errorMessage,
      cause: `Link not found among given state queue UTxOs`,
    }),
  );
};

/**
 * Returns a sorted array of `StateQueueUTxO`s where the confirmed state's UTxO
 * is the head element, and the following elements are linked from their
 * previous elements.
 *
 * TODO: Make it more efficient. Currently that same list of all state queue
 *       UTxOs is traversed to find the next link UTxO multiple times. It might
 *       be better to drop link UTxOs when found so that subsequent lookups
 *       become cheaper.
 */
export const sortStateQueueUTxOs = (
  stateQueueUTxOs: StateQueueUTxO[],
): Effect.Effect<StateQueueUTxO[], LinkedListError> =>
  Effect.gen(function* () {
    const filteredForConfirmedState = yield* Effect.allSuccesses(
      stateQueueUTxOs.map((u) =>
        Effect.gen(function* () {
          const dataAndLink = yield* getConfirmedStateFromStateQueueDatum(
            u.datum,
          );
          return { ...dataAndLink, utxo: u };
        }),
      ),
    );
    if (filteredForConfirmedState.length !== 1) {
      return yield* Effect.fail(
        new LinkedListError({
          message: `Failed to sort state queue UTxOs`,
          cause: `Confirmed state (root node) not found among state queue UTxOs`,
        }),
      );
    }

    const { utxo: confirmedStateUTxO, link: linkToOldestBlock } =
      filteredForConfirmedState[0];
    const sorted: StateQueueUTxO[] = [confirmedStateUTxO];
    let link = linkToOldestBlock;
    while (link !== "Empty") {
      const linkUTxO = yield* findLinkStateQueueUTxO(link, stateQueueUTxOs);
      sorted.push(linkUTxO);
      link = linkUTxO.datum.next;
    }
    return sorted;
  });

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

const outputDatumCborMatches = (
  output: { readonly datum?: string | null },
  datumCbor: string,
): boolean =>
  output.datum != null &&
  canonicalPlutusDataCbor(output.datum) === canonicalPlutusDataCbor(datumCbor);

const requireSingleNonAdaPolicyId = (
  assets: Assets,
  label: string,
): PolicyId => {
  const policyIds = new Set(
    Object.entries(assets)
      .filter(([unit, quantity]) => unit !== "lovelace" && quantity !== 0n)
      .map(([unit]) => fromUnit(unit).policyId),
  );
  if (policyIds.size !== 1) {
    throw new Error(
      `${label} expected exactly one non-ADA policy, got ${policyIds.size.toString()}`,
    );
  }
  return [...policyIds][0]!;
};

const resolveRemoveSlashingApproach = (
  ctx: Parameters<BuildTxWithRedeemer>[0],
  slashing: EmulatorStateQueueRemoveSlashingParams,
): SlashingApproach => {
  switch (slashing.kind) {
    case "operatorAlreadySlashed":
      return {
        OperatorAlreadySlashed: {
          active_operators_element_ref_input_index: requireReferenceInputIndex(
            ctx,
            slashing.activeOperatorsElementRefInput,
            "state-queue remove active-operators slashed witness",
          ),
          retired_operators_element_ref_input_index: requireReferenceInputIndex(
            ctx,
            slashing.retiredOperatorsElementRefInput,
            "state-queue remove retired-operators slashed witness",
          ),
        },
      };
    case "slashActiveOperator":
      return {
        SlashActiveOperator: {
          active_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            requireSingleNonAdaPolicyId(
              slashing.activeOperatorsAssetsToBurn,
              "state-queue remove active-operators burn",
            ),
            "state-queue remove active-operators burn",
          ),
        },
      };
    case "slashRetiredOperator":
      return {
        SlashRetiredOperator: {
          retired_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            requireSingleNonAdaPolicyId(
              slashing.retiredOperatorsAssetsToBurn,
              "state-queue remove retired-operators burn",
            ),
            "state-queue remove retired-operators burn",
          ),
        },
      };
  }
};

const removeSlashingReferenceInputs = (
  slashing: EmulatorStateQueueRemoveSlashingParams,
): readonly UTxO[] =>
  slashing.kind === "operatorAlreadySlashed"
    ? [
        slashing.activeOperatorsElementRefInput,
        slashing.retiredOperatorsElementRefInput,
      ]
    : [];

const collectRemoveSlashingInputs = (
  tx: TxBuilder,
  slashing: EmulatorStateQueueRemoveSlashingParams,
  referenceScripts: StateQueueRemoveReferenceScriptUTxOs | undefined,
): TxBuilder => {
  if (slashing.kind === "operatorAlreadySlashed") {
    return tx;
  }

  if (slashing.kind === "slashActiveOperator") {
    let updated = tx
      .collectFrom(
        [...slashing.activeOperatorInputs],
        encodeActiveOperatorSpendRedeemer(
          slashing.activeOperatorSpendRedeemer ?? "ListStateTransition",
        ),
      )
      .mintAssets(
        slashing.activeOperatorsAssetsToBurn,
        slashing.activeOperatorsMintRedeemer,
      );
    if (referenceScripts?.activeOperatorsMint === undefined) {
      updated = updated.attach.Script(slashing.activeOperatorsMintingScript);
    }
    if (slashing.continuedActiveOperatorAnchorOutput !== undefined) {
      updated = updated.pay.ToContract(
        slashing.continuedActiveOperatorAnchorOutput.address,
        {
          kind: "inline",
          value: slashing.continuedActiveOperatorAnchorOutput.datum,
        },
        slashing.continuedActiveOperatorAnchorOutput.assets,
      );
    }
    if (slashing.schedulerSpend !== undefined) {
      updated = updated.pay
        .ToContract(
          slashing.schedulerSpend.continuedOutput.address,
          {
            kind: "inline",
            value: slashing.schedulerSpend.continuedOutput.datum,
          },
          slashing.schedulerSpend.continuedOutput.assets,
        )
        .collectFrom(
          [slashing.schedulerSpend.input],
          slashing.schedulerSpend.redeemer,
        );
      if (referenceScripts?.schedulerSpend === undefined) {
        updated = updated.attach.Script(slashing.schedulerSpend.script);
      }
    }
    if (
      slashing.activeOperatorSpendingScript !== undefined &&
      referenceScripts?.activeOperatorsSpend === undefined
    ) {
      updated = updated.attach.Script(slashing.activeOperatorSpendingScript);
    }
    return updated;
  }

  let updated = tx
    .collectFrom(
      [...slashing.retiredOperatorInputs],
      slashing.retiredOperatorSpendRedeemer ?? Data.void(),
    )
    .mintAssets(
      slashing.retiredOperatorsAssetsToBurn,
      slashing.retiredOperatorsMintRedeemer,
    );
  if (referenceScripts?.retiredOperatorsMint === undefined) {
    updated = updated.attach.Script(slashing.retiredOperatorsMintingScript);
  }
  if (slashing.continuedRetiredOperatorAnchorOutput !== undefined) {
    updated = updated.pay.ToContract(
      slashing.continuedRetiredOperatorAnchorOutput.address,
      {
        kind: "inline",
        value: slashing.continuedRetiredOperatorAnchorOutput.datum,
      },
      slashing.continuedRetiredOperatorAnchorOutput.assets,
    );
  }
  if (
    slashing.retiredOperatorSpendingScript !== undefined &&
    referenceScripts?.retiredOperatorsSpend === undefined
  ) {
    updated = updated.attach.Script(slashing.retiredOperatorSpendingScript);
  }
  return updated;
};

type StateQueueRemovalTxAssemblyParams = {
  readonly collectedStateQueueInputs: readonly UTxO[];
  readonly continuedOutput: {
    readonly datum: string;
    readonly assets: Assets;
  };
  readonly assetsToBurn: Assets;
  readonly stateQueueMintRedeemer: BuildTxWithRedeemer;
  readonly additionalInputs?: readonly UTxO[];
  readonly validFrom?: bigint;
  readonly validTo?: bigint;
  readonly fraudProofRefInput: UTxO;
  readonly additionalRefInputs?: readonly UTxO[];
  readonly stateQueueSpendingScript: Script;
  readonly stateQueueMintingScript: Script;
  readonly referenceScripts?: StateQueueRemoveReferenceScriptUTxOs;
  readonly slashing: EmulatorStateQueueRemoveSlashingParams;
};

const buildStateQueueRemovalTx = (
  lucid: LucidEvolution,
  stateQueueAddress: Address,
  params: StateQueueRemovalTxAssemblyParams,
): TxBuilder => {
  const additionalInputs = params.additionalInputs ?? [];
  const referenceScriptInputs = Object.values(
    params.referenceScripts ?? {},
  ).filter((utxo): utxo is UTxO => utxo !== undefined);
  const referenceInputs = dedupeAndSortUtxos([
    params.fraudProofRefInput,
    ...(params.additionalRefInputs ?? []),
    ...removeSlashingReferenceInputs(params.slashing),
    ...referenceScriptInputs,
  ]);
  let tx = lucid.newTx();
  if (params.validFrom !== undefined) {
    tx = tx.validFrom(Number(params.validFrom));
  }
  if (params.validTo !== undefined) {
    tx = tx.validTo(Number(params.validTo));
  }
  if (additionalInputs.length > 0) {
    tx = tx.collectFrom([...additionalInputs]);
  }
  tx = tx
    .collectFrom(
      [...params.collectedStateQueueInputs],
      STATE_QUEUE_LINKED_LIST_MUTATION_REDEEMER,
    )
    .readFrom(referenceInputs)
    .pay.ToContract(
      stateQueueAddress,
      { kind: "inline", value: params.continuedOutput.datum },
      params.continuedOutput.assets,
    )
    .mintAssets(params.assetsToBurn, params.stateQueueMintRedeemer);

  if (params.referenceScripts?.stateQueueSpend === undefined) {
    tx = tx.attach.Script(params.stateQueueSpendingScript);
  }
  if (params.referenceScripts?.stateQueueMint === undefined) {
    tx = tx.attach.Script(params.stateQueueMintingScript);
  }

  return collectRemoveSlashingInputs(
    tx,
    params.slashing,
    params.referenceScripts,
  );
};

export const incompleteEmulatorCommitBlockHeaderTxProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
  params: EmulatorStateQueueCommitBlockHeaderParams,
): Effect.Effect<TxBuilder, HashingError> =>
  Effect.gen(function* () {
    const newHeaderHash = yield* hashBlockHeader(params.newHeader);
    const newBlockAssetName =
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + newHeaderHash;
    const newBlockAssets: Assets = {
      [toUnit(config.stateQueuePolicyId, newBlockAssetName)]: 1n,
    };
    const continuedAnchorDatum: LinkedListNodeView = {
      ...params.anchorUTxO.datum,
      next: { Key: { key: newHeaderHash } },
    };
    const newBlockDatum: LinkedListNodeView = {
      key: { Key: { key: newHeaderHash } },
      next: "Empty",
      data: castStateQueueNodeToData({
        header: params.newHeader,
        da_attestation: NO_DA_ATTESTATION,
      }) as LinkedListNodeView["data"],
    };
    const newBlockDatumCbor = encodeLinkedListNodeView(newBlockDatum);
    const continuedAnchorDatumCbor =
      encodeLinkedListNodeView(continuedAnchorDatum);
    const continuedAnchorUnit = toUnit(
      config.stateQueuePolicyId,
      params.anchorUTxO.assetName,
    );
    const stateQueueCommitRedeemer = ((ctx) =>
      Data.to(
        {
          CommitBlockHeader: {
            new_block_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === config.stateQueueAddress &&
                outputDatumCborMatches(output, newBlockDatumCbor) &&
                (output.assets[
                  toUnit(config.stateQueuePolicyId, newBlockAssetName)
                ] ?? 0n) === 1n,
              "emulator state-queue commit new block",
            ),
            continued_latest_block_output_index: requireUniqueOutputIndex(
              ctx.outputs,
              (output) =>
                output.address === config.stateQueueAddress &&
                outputDatumCborMatches(output, continuedAnchorDatumCbor) &&
                (output.assets[continuedAnchorUnit] ?? 0n) === 1n,
              "emulator state-queue commit continued latest block",
            ),
            operator: params.newHeader.operatorVkey,
            scheduler_ref_input_index: requireReferenceInputIndex(
              ctx,
              params.schedulerRefInput,
              "emulator state-queue commit scheduler",
            ),
            active_operators_input_index: requireInputIndex(
              ctx,
              params.activeOperatorInput,
              "emulator state-queue commit active operator",
            ),
            active_operators_redeemer_index: requireSpendRedeemerIndex(
              ctx,
              params.activeOperatorInput,
              "emulator state-queue commit active operator",
            ),
          },
        } satisfies StateQueueRedeemer,
        StateQueueRedeemer,
      )) satisfies BuildTxWithRedeemer;

    const additionalInputs = params.additionalInputs ?? [];
    let tx = lucid.newTx();
    if (additionalInputs.length > 0) {
      tx = tx.collectFrom([...additionalInputs]);
    }
    tx = tx
      .collectFrom(
        [params.anchorUTxO.utxo],
        STATE_QUEUE_LINKED_LIST_MUTATION_REDEEMER,
      )
      .collectFrom(
        [params.activeOperatorInput],
        encodeActiveOperatorSpendRedeemer(params.activeOperatorSpendRedeemer),
      )
      .readFrom([
        ...(params.additionalRefInputs ?? []),
        params.schedulerRefInput,
      ])
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: newBlockDatumCbor },
        newBlockAssets,
      )
      .pay.ToContract(
        config.stateQueueAddress,
        {
          kind: "inline",
          value: continuedAnchorDatumCbor,
        },
        params.anchorUTxO.utxo.assets,
      )
      .mintAssets(newBlockAssets, stateQueueCommitRedeemer)
      .addSignerKey(params.newHeader.operatorVkey)
      .attach.Script(params.stateQueueSpendingScript)
      .attach.Script(params.stateQueueMintingScript)
      .attach.Script(params.activeOperatorSpendingScript);

    if (params.validFrom !== undefined) {
      tx = tx.validFrom(Number(params.validFrom));
    }
    if (params.validTo !== undefined) {
      tx = tx.validTo(Number(params.validTo));
    }
    if (params.continuedActiveOperatorOutput !== undefined) {
      tx = tx.pay.ToContract(
        params.continuedActiveOperatorOutput.address,
        {
          kind: "inline",
          value: params.continuedActiveOperatorOutput.datum,
        },
        params.continuedActiveOperatorOutput.assets,
      );
    }

    return tx;
  });

/**
 * Production-shaped RemoveFraudulentBlockHeader + RemoveLastFraudulentBlock
 * builder. Reference-script UTxOs are read as ordinary reference inputs; when
 * one is absent the matching inline script is attached for test/emulator flows.
 */
export const incompleteRemoveLastFraudulentBlockHeaderTxProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
  params: EmulatorStateQueueRemoveLastFraudulentBlockHeaderParams,
): TxBuilder => {
  const fraudulentBlocksHeaderHash =
    params.fraudulentBlocksHeaderHash ??
    params.fraudulentBlockUTxO.assetName.slice(
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
    );
  const assetsToBurn: Assets = {
    [toUnit(config.stateQueuePolicyId, params.fraudulentBlockUTxO.assetName)]:
      -1n,
  };
  const continuedAnchorDatum: LinkedListNodeView = {
    ...params.anchorUTxO.datum,
    next: "Empty",
  };
  const continuedAnchorDatumCbor =
    encodeLinkedListNodeView(continuedAnchorDatum);
  const continuedAnchorUnit = toUnit(
    config.stateQueuePolicyId,
    params.anchorUTxO.assetName,
  );
  const defaultStateQueueMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      config.stateQueuePolicyId,
      "emulator state-queue remove burn",
    );
    return Data.to(
      {
        RemoveFraudulentBlockHeader: {
          fraudulent_operator: params.fraudulentOperator,
          fraudulent_blocks_header_hash: fraudulentBlocksHeaderHash,
          slashing_approach: resolveRemoveSlashingApproach(
            ctx,
            params.slashing,
          ),
          fraud_proof_ref_input_index: requireReferenceInputIndex(
            ctx,
            params.fraudProofRefInput,
            "emulator state-queue remove fraud proof",
          ),
          block_removal_approach: {
            RemoveLastFraudulentBlock: {
              anchor_element_input_outref: outputReferenceFromUTxO(
                params.anchorUTxO.utxo,
              ),
              anchor_element_output_index: requireUniqueOutputIndex(
                ctx.outputs,
                (output) =>
                  output.address === config.stateQueueAddress &&
                  outputDatumCborMatches(output, continuedAnchorDatumCbor) &&
                  (output.assets[continuedAnchorUnit] ?? 0n) === 1n,
                "emulator state-queue remove continued anchor",
              ),
            },
          },
        },
      } satisfies StateQueueRedeemer,
      StateQueueRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  return buildStateQueueRemovalTx(lucid, config.stateQueueAddress, {
    collectedStateQueueInputs: [
      params.anchorUTxO.utxo,
      params.fraudulentBlockUTxO.utxo,
    ],
    continuedOutput: {
      datum: continuedAnchorDatumCbor,
      assets: params.anchorUTxO.utxo.assets,
    },
    assetsToBurn,
    stateQueueMintRedeemer:
      params.stateQueueMintRedeemer ?? defaultStateQueueMintRedeemer,
    additionalInputs: params.additionalInputs,
    validFrom: params.validFrom,
    validTo: params.validTo,
    fraudProofRefInput: params.fraudProofRefInput,
    additionalRefInputs: params.additionalRefInputs,
    stateQueueSpendingScript: params.stateQueueSpendingScript,
    stateQueueMintingScript: params.stateQueueMintingScript,
    referenceScripts: params.referenceScripts,
    slashing: params.slashing,
  });
};

/**
 * Production-shaped RemoveFraudulentBlockHeader +
 * RemoveFraudulentBlocksLink builder. This removes the immediate successor of
 * a fraud-proved state-queue block and preserves the fraud-proved block with
 * its successor link spliced forward.
 */
export const incompleteRemoveFraudulentBlocksLinkTxProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
  params: EmulatorStateQueueRemoveFraudulentBlocksLinkHeaderParams,
): TxBuilder => {
  const removedBlockHash = params.removedBlockUTxO.assetName.slice(
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
  );
  if (
    params.fraudulentBlockUTxO.datum.next === "Empty" ||
    params.fraudulentBlockUTxO.datum.next.Key.key !== removedBlockHash
  ) {
    throw new Error(
      "RemoveFraudulentBlocksLink requires the removed block to be the immediate successor of the fraud-proved block.",
    );
  }

  const assetsToBurn: Assets = {
    [toUnit(config.stateQueuePolicyId, params.removedBlockUTxO.assetName)]: -1n,
  };
  const continuedFraudulentNodeDatum: LinkedListNodeView = {
    ...params.fraudulentBlockUTxO.datum,
    next: params.removedBlockUTxO.datum.next,
  };
  const continuedFraudulentNodeDatumCbor = encodeLinkedListNodeView(
    continuedFraudulentNodeDatum,
  );
  const continuedFraudulentNodeUnit = toUnit(
    config.stateQueuePolicyId,
    params.fraudulentBlockUTxO.assetName,
  );
  const defaultStateQueueMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      config.stateQueuePolicyId,
      "state-queue remove fraudulent successor burn",
    );
    return Data.to(
      {
        RemoveFraudulentBlockHeader: {
          fraudulent_operator: params.fraudulentOperator,
          fraudulent_blocks_header_hash: params.fraudulentBlocksHeaderHash,
          slashing_approach: resolveRemoveSlashingApproach(
            ctx,
            params.slashing,
          ),
          fraud_proof_ref_input_index: requireReferenceInputIndex(
            ctx,
            params.fraudProofRefInput,
            "state-queue remove fraud proof",
          ),
          block_removal_approach: {
            RemoveFraudulentBlocksLink: {
              fraudulent_node_input_outref: outputReferenceFromUTxO(
                params.fraudulentBlockUTxO.utxo,
              ),
              fraudulent_node_output_index: requireUniqueOutputIndex(
                ctx.outputs,
                (output) =>
                  output.address === config.stateQueueAddress &&
                  outputDatumCborMatches(
                    output,
                    continuedFraudulentNodeDatumCbor,
                  ) &&
                  (output.assets[continuedFraudulentNodeUnit] ?? 0n) === 1n,
                "state-queue remove continued fraud-proved block",
              ),
            },
          },
        },
      } satisfies StateQueueRedeemer,
      StateQueueRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  return buildStateQueueRemovalTx(lucid, config.stateQueueAddress, {
    collectedStateQueueInputs: [
      params.fraudulentBlockUTxO.utxo,
      params.removedBlockUTxO.utxo,
    ],
    continuedOutput: {
      datum: continuedFraudulentNodeDatumCbor,
      assets: params.fraudulentBlockUTxO.utxo.assets,
    },
    assetsToBurn,
    stateQueueMintRedeemer:
      params.stateQueueMintRedeemer ?? defaultStateQueueMintRedeemer,
    additionalInputs: params.additionalInputs,
    validFrom: params.validFrom,
    validTo: params.validTo,
    fraudProofRefInput: params.fraudProofRefInput,
    additionalRefInputs: params.additionalRefInputs,
    stateQueueSpendingScript: params.stateQueueSpendingScript,
    stateQueueMintingScript: params.stateQueueMintingScript,
    referenceScripts: params.referenceScripts,
    slashing: params.slashing,
  });
};

/**
 * Given the latest block in state queue, along with the required tree roots,
 * this function returns the updated datum of the latest block, along with the
 * new `Header` that should be included in the new block's datum.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param latestBlocksDatum - Datum of the UTxO of the latest block in queue.
 * @param newUTxOsRoot - MPF root of the updated ledger.
 * @param transactionsRoot - MPF root of the transactions included in the new block.
 * @param depositsRoot - MPF root of the deposit transactions included in the new block.
 * @param withdrawalsRoot - MPF root of the withdrawal transactions included in the new block.
 * @param endTime - POSIX time of the new block's closing range.
 */
export const updateLatestBlocksDatumAndGetTheNewHeaderProgram = (
  lucid: LucidEvolution,
  latestBlocksDatum: LinkedListNodeView,
  newUTxOsRoot: MerkleRoot,
  transactionsRoot: MerkleRoot,
  depositsRoot: MerkleRoot,
  withdrawalsRoot: MerkleRoot,
  endTime: POSIXTime,
): Effect.Effect<
  { nodeDatum: LinkedListNodeView; header: Header },
  DataCoercionError | LucidError | HashingError
> =>
  Effect.gen(function* () {
    const walletAddress: string = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (e) =>
        new LucidError({ message: `Failed to find the wallet`, cause: e }),
    });

    const pubKeyHash = paymentCredentialOf(walletAddress).hash;
    if (latestBlocksDatum.key === "Empty") {
      const { data: confirmedState } =
        yield* getConfirmedStateFromStateQueueDatum(latestBlocksDatum);
      const newHeader = {
        prevUtxosRoot: confirmedState.utxoRoot,
        utxosRoot: newUTxOsRoot,
        transactionsRoot,
        depositsRoot,
        withdrawalsRoot,
        startTime: confirmedState.endTime,
        endTime,
        prevHeaderHash: confirmedState.headerHash,
        operatorVkey: pubKeyHash,
        protocolVersion: confirmedState.protocolVersion,
      };
      const newHeaderHash = yield* hashBlockHeader(newHeader);
      return {
        nodeDatum: {
          ...latestBlocksDatum,
          next: { Key: { key: newHeaderHash } },
        },
        header: newHeader,
      };
    } else {
      const latestHeader =
        yield* getHeaderFromStateQueueDatum(latestBlocksDatum);
      const prevHeaderHash = yield* hashBlockHeader(latestHeader);
      const newHeader = {
        ...latestHeader,
        prevUtxosRoot: latestHeader.utxosRoot,
        utxosRoot: newUTxOsRoot,
        transactionsRoot,
        depositsRoot,
        withdrawalsRoot,
        startTime: latestHeader.endTime,
        endTime,
        prevHeaderHash,
        operatorVkey: pubKeyHash,
      };
      const newHeaderHash = yield* hashBlockHeader(newHeader);
      return {
        nodeDatum: {
          ...latestBlocksDatum,
          next: { Key: { key: newHeaderHash } },
        },
        header: newHeader,
      };
    }
  });

/**
 * Given the latest block in state queue, along with the required tree roots,
 * this function returns the updated datum of the latest block, along with the
 * new `Header` that should be included in the new block's datum.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param latestBlocksDatum - Datum of the UTxO of the latest block in queue.
 * @param newUTxOsRoot - MPF root of the updated ledger.
 * @param transactionsRoot - MPF root of the transactions included in the new block.
 * @param depositsRoot - MPF root of the deposit transactions included in the new block.
 * @param withdrawalsRoot - MPF root of the withdrawal transactions included in the new block.
 * @param endTime - POSIX time of the new block's closing range.
 */
export const updateLatestBlocksDatumAndGetTheNewHeader = (
  lucid: LucidEvolution,
  latestBlocksDatum: LinkedListNodeView,
  newUTxOsRoot: MerkleRoot,
  transactionsRoot: MerkleRoot,
  depositsRoot: MerkleRoot,
  withdrawalsRoot: MerkleRoot,
  endTime: POSIXTime,
): Promise<{ nodeDatum: LinkedListNodeView; header: Header }> =>
  makeReturn(
    updateLatestBlocksDatumAndGetTheNewHeaderProgram(
      lucid,
      latestBlocksDatum,
      newUTxOsRoot,
      transactionsRoot,
      depositsRoot,
      withdrawalsRoot,
      endTime,
    ),
  ).unsafeRun();

export const fetchUnsortedStateQueueUTxOsProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
): Effect.Effect<StateQueueUTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(config.stateQueueAddress),
      catch: (e) => {
        return new LucidError({
          message: `Failed to fetch state queue UTxOs at: ${config.stateQueueAddress}`,
          cause: e,
        });
      },
    });
    return yield* utxosToStateQueueUTxOs(allUTxOs, config.stateQueuePolicyId);
  });

export const fetchSortedStateQueueUTxOsProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
): Effect.Effect<StateQueueUTxO[], LucidError | LinkedListError> =>
  Effect.gen(function* () {
    const unsorted = yield* fetchUnsortedStateQueueUTxOsProgram(lucid, config);
    return yield* sortStateQueueUTxOs(unsorted);
  });

/**
 * Attempts fetching the whole state queue linked list.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO[]} - All the authentic node UTxOs.
 */
export const fetchSortedStateQueueUTxOs = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
) => makeReturn(fetchSortedStateQueueUTxOsProgram(lucid, config)).unsafeRun();

export const fetchUnsortedStateQueueUTxOs = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
) => makeReturn(fetchUnsortedStateQueueUTxOsProgram(lucid, config)).unsafeRun();

export const fetchConfirmedStateAndItsLinkProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
): Effect.Effect<
  { confirmed: StateQueueUTxO; link: StateQueueUTxO },
  StateQueueError | LucidError | LinkedListError
> =>
  Effect.gen(function* () {
    const allUTxOs = yield* fetchUnsortedStateQueueUTxOsProgram(lucid, config);
    const filteredForConfirmedState = yield* Effect.allSuccesses(
      allUTxOs.map((u) =>
        Effect.gen(function* () {
          const dataAndLink = yield* getConfirmedStateFromStateQueueDatum(
            u.datum,
          );
          return {
            ...dataAndLink,
            utxo: u,
          };
        }),
      ),
    );
    if (filteredForConfirmedState.length === 1) {
      const { utxo: confirmedStateUTxO, link: confirmedStatesLink } =
        filteredForConfirmedState[0];
      const linkUTxO = yield* findLinkStateQueueUTxO(
        confirmedStatesLink,
        allUTxOs,
      );
      return {
        confirmed: confirmedStateUTxO,
        link: linkUTxO,
      };
    } else {
      return yield* Effect.fail(
        new StateQueueError({
          message: "Failed to fetch confirmed state and its link",
          cause: "Exactly 1 authentic confirmed state UTxO was expected",
        }),
      );
    }
  });

/**
 * Attempts fetching the confirmed state, i.e. the root node of the state queue
 * linked list, along with its link (i.e. first non-root node in the list).
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO} - The authentic UTxO which is the root node.
 */
export const fetchConfirmedStateAndItsLink = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
) =>
  makeReturn(fetchConfirmedStateAndItsLinkProgram(lucid, config)).unsafeRun();

export const fetchLatestCommittedBlockProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
): Effect.Effect<StateQueueUTxO, StateQueueError | LucidError> =>
  Effect.gen(function* () {
    const errorMessage = `Failed to fetch latest committed block`;
    const allBlocks = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId,
    );
    yield* Effect.logInfo("allBlocks", allBlocks.length);
    const filtered: StateQueueUTxO[] = yield* Effect.allSuccesses(
      allBlocks.map(({ utxo: u }) => {
        const stateQueueUTxOEffect = utxoToStateQueueUTxO(
          u,
          config.stateQueuePolicyId,
        );
        return Effect.andThen(stateQueueUTxOEffect, (squ: StateQueueUTxO) =>
          squ.datum.next === "Empty"
            ? Effect.succeed(squ)
            : Effect.fail(
                new StateQueueError({
                  message: errorMessage,
                  cause: "Not a tail node",
                }),
              ),
        );
      }),
    );
    if (filtered.length === 1) {
      return filtered[0];
    } else {
      return yield* Effect.fail(
        new StateQueueError({
          message: errorMessage,
          cause: "Latest block not found",
        }),
      );
    }
  });

/**
 * Attempts fetching the committed block at the very end of the state queue
 * linked list.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param config - Configuration values required to know where to look for which NFT.
 * @returns {UTxO} - The authentic UTxO which links to no other nodes.
 */
export const fetchLatestCommittedBlock = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
) => makeReturn(fetchLatestCommittedBlockProgram(lucid, config)).unsafeRun();

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteInitStateQueueTxProgram = (
  lucid: LucidEvolution,
  params: StateQueueInitParams,
): Effect.Effect<TxBuilder, never> =>
  Effect.gen(function* () {
    const stateQueueData: ConfirmedState = {
      headerHash: GENESIS_HEADER_HASH,
      prevHeaderHash: GENESIS_HEADER_HASH,
      utxoRoot: EMPTY_MERKLE_TREE_ROOT,
      startTime: params.genesisTime,
      endTime: params.genesisTime,
      protocolVersion: GENESIS_PROTOCOL_VERSION,
    };

    return yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.validator,
      rootAssetName: STATE_QUEUE_ROOT_ASSET_NAME,
      data: Data.castTo(stateQueueData, ConfirmedState),
      redeemer: (outputIndex) =>
        Data.to({ Init: { output_index: outputIndex } }, StateQueueRedeemer),
      lovelace: params.lovelace,
    });
  });

export const unsignedInitStateQueueTxProgram = (
  lucid: LucidEvolution,
  initParams: StateQueueInitParams,
): Effect.Effect<TxSignBuilder, LucidError> =>
  Effect.gen(function* () {
    const commitTx = yield* incompleteInitStateQueueTxProgram(
      lucid,
      initParams,
    );
    const completedTx: TxSignBuilder =
      yield* completeTxWithLocalUPLCEvalProgram(
        commitTx,
        (e) =>
          new LucidError({
            message: `Failed to build the init state queue transaction: ${e}`,
            cause: e,
          }),
      );
    return completedTx;
  });

/**
 * Builds completed tx for initializing the state queue.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param initParams - Parameters for minting the initialization NFT.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedInitStateQueueTx = (
  lucid: LucidEvolution,
  initParams: StateQueueInitParams,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedInitStateQueueTxProgram(lucid, initParams)).unsafeRun();

export class StateQueueError extends EffectData.TaggedError(
  "StateQueueError",
)<GenericErrorFields> {}
