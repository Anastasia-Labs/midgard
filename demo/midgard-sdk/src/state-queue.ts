import {
  Address,
  Assets,
  Data,
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

import {
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
} from "@/active-operators.js";
import {
  AuthenticatedValidator,
  DataCoercionError,
  GenericErrorFields,
  HashingError,
  MerkleRoot,
  MerkleRootSchema,
  MissingDatumError,
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
  ConfirmedState,
  getHeaderFromStateQueueDatum,
  hashBlockHeader,
  Header,
  HeaderHashSchema,
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
import { SchedulerSpendRedeemer } from "@/scheduler.js";
import { completeTxWithLocalUPLCEvalProgram } from "@/tx-completion.js";
import { dedupeAndSortUtxos } from "@/tx-out-ref-order.js";

export const STATE_QUEUE_ROOT_ASSET_NAME = fromText("MIDGARD_CONFIRMED_STATE");

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
      anchor_element_input_index: Data.Integer(),
      anchor_element_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveFraudulentBlocksLink: Data.Object({
      fraudulent_node_output_index: Data.Integer(),
      removed_block_input_index: Data.Integer(),
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
  Data.Object({
    Deinit: Data.Object({
      input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    CommitBlockHeader: Data.Object({
      latest_block_input_index: Data.Integer(),
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
      header_node_input_index: Data.Integer(),
      confirmed_state_input_index: Data.Integer(),
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
      fraudulent_node_input_index: Data.Integer(),
      fraud_proof_ref_input_index: Data.Integer(),
      block_removal_approach: BlockRemovalApproachSchema,
    }),
  }),
]);
export type StateQueueRedeemer = Data.Static<typeof StateQueueRedeemerSchema>;
export const StateQueueRedeemer =
  StateQueueRedeemerSchema as unknown as StateQueueRedeemer;

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
  outputIndex?: bigint;
  lovelace?: bigint;
};

/**
 * Emulator/test helper for exercising the real state_queue CommitBlockHeader
 * mint redeemer. The output layout is fixed and reflected in the redeemer:
 * output 0 is the new block node, output 1 is the continued anchor node. This
 * helper validates the state-queue side of the commit path; callers still need
 * the paired active-operators spend to be protocol-valid outside focused tests.
 */
export type EmulatorStateQueueCommitBlockHeaderParams = {
  anchorUTxO: StateQueueUTxO;
  newHeader: Header;
  additionalInputs?: readonly UTxO[];
  validFrom?: bigint;
  validTo?: bigint;
  schedulerRefInput: UTxO;
  schedulerRefInputIndex: bigint;
  additionalRefInputs?: readonly UTxO[];
  activeOperatorInput: UTxO;
  activeOperatorInputIndex: bigint;
  activeOperatorSpendRedeemer: ActiveOperatorSpendRedeemer;
  /** Tx-info redeemer-list index, not the full transaction input index. */
  activeOperatorSpendRedeemerTxInfoIndex: bigint;
  activeOperatorSpendingScript: Script;
  continuedActiveOperatorOutput?: {
    readonly address: Address;
    readonly datum: string;
    readonly assets: Assets;
  };
  stateQueueSpendingScript: Script;
  stateQueueMintingScript: Script;
  latestBlockInputIndex: bigint;
};

export type EmulatorStateQueueRemoveSlashingParams =
  | {
      readonly kind: "operatorAlreadySlashed";
      readonly activeOperatorsElementRefInput: UTxO;
      readonly activeOperatorsElementRefInputIndex: bigint;
      readonly retiredOperatorsElementRefInput: UTxO;
      readonly retiredOperatorsElementRefInputIndex: bigint;
    }
  | {
      readonly kind: "slashActiveOperator";
      /**
       * Supports the active-operators `SlashOperator` mint path. For full
       * active-operator validation, provide the anchor/node inputs, continued
       * anchor output, hub-oracle reference input, and scheduler sync data
       * required by the slashing redeemer.
       */
      readonly activeOperatorsRedeemerTxInfoIndex: bigint;
      readonly activeOperatorsAssetsToBurn: Assets;
      readonly activeOperatorsMintRedeemer: ActiveOperatorMintRedeemer;
      readonly activeOperatorsMintingScript: Script;
      readonly activeOperatorInputs: readonly UTxO[];
      readonly activeOperatorSpendingScript?: Script;
      readonly activeOperatorSpendRedeemer?: ActiveOperatorSpendRedeemer;
      readonly continuedActiveOperatorAnchorOutput?: {
        readonly address: Address;
        readonly datum: string;
        readonly assets: Assets;
      };
      readonly schedulerSpend?: {
        readonly input: UTxO;
        readonly redeemer: SchedulerSpendRedeemer;
        readonly script: Script;
        readonly continuedOutput: {
          readonly address: Address;
          readonly datum: string;
          readonly assets: Assets;
        };
      };
    };

/**
 * Emulator/test helper for RemoveFraudulentBlockHeader +
 * RemoveLastFraudulentBlock. Output 0 is the continued anchor node.
 */
export type EmulatorStateQueueRemoveLastFraudulentBlockHeaderParams = {
  anchorUTxO: StateQueueUTxO;
  fraudulentBlockUTxO: StateQueueUTxO;
  additionalInputs?: readonly UTxO[];
  validFrom?: bigint;
  validTo?: bigint;
  fraudulentOperator: string;
  fraudulentBlocksHeaderHash?: string;
  fraudProofRefInput: UTxO;
  fraudProofRefInputIndex: bigint;
  additionalRefInputs?: readonly UTxO[];
  slashing: EmulatorStateQueueRemoveSlashingParams;
  anchorElementInputIndex: bigint;
  anchorElementOutputIndex?: bigint;
  fraudulentNodeInputIndex: bigint;
  stateQueueSpendingScript: Script;
  stateQueueMintingScript: Script;
  referenceScripts?: StateQueueRemoveReferenceScriptUTxOs;
};

export type StateQueueRemoveReferenceScriptUTxOs = {
  readonly stateQueueSpend?: UTxO;
  readonly stateQueueMint?: UTxO;
  readonly activeOperatorsSpend?: UTxO;
  readonly activeOperatorsMint?: UTxO;
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
      data: Data.castTo(params.newHeader, Header),
    };
    const redeemer: StateQueueRedeemer = {
      CommitBlockHeader: {
        latest_block_input_index: params.latestBlockInputIndex,
        new_block_output_index: 0n,
        continued_latest_block_output_index: 1n,
        operator: params.newHeader.operatorVkey,
        scheduler_ref_input_index: params.schedulerRefInputIndex,
        active_operators_input_index: params.activeOperatorInputIndex,
        active_operators_redeemer_index:
          params.activeOperatorSpendRedeemerTxInfoIndex,
      },
    };

    const additionalInputs = params.additionalInputs ?? [];
    let tx = lucid.newTx();
    if (additionalInputs.length > 0) {
      tx = tx.collectFrom([...additionalInputs]);
    }
    tx = tx
      .collectFrom(
        [params.anchorUTxO.utxo],
        Data.to(redeemer, StateQueueRedeemer),
      )
      .collectFrom(
        [params.activeOperatorInput],
        Data.to(
          params.activeOperatorSpendRedeemer,
          ActiveOperatorSpendRedeemer,
        ),
      )
      .readFrom([
        ...(params.additionalRefInputs ?? []),
        params.schedulerRefInput,
      ])
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: encodeLinkedListNodeView(newBlockDatum) },
        newBlockAssets,
      )
      .pay.ToContract(
        config.stateQueueAddress,
        {
          kind: "inline",
          value: encodeLinkedListNodeView(continuedAnchorDatum),
        },
        params.anchorUTxO.utxo.assets,
      )
      .mintAssets(newBlockAssets, Data.to(redeemer, StateQueueRedeemer))
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
  params: EmulatorStateQueueRemoveLastFraudulentBlockHeaderParams & {
    readonly referenceScripts?: StateQueueRemoveReferenceScriptUTxOs;
  },
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
  const slashingApproach: SlashingApproach =
    params.slashing.kind === "operatorAlreadySlashed"
      ? {
          OperatorAlreadySlashed: {
            active_operators_element_ref_input_index:
              params.slashing.activeOperatorsElementRefInputIndex,
            retired_operators_element_ref_input_index:
              params.slashing.retiredOperatorsElementRefInputIndex,
          },
        }
      : {
          SlashActiveOperator: {
            active_operators_redeemer_index:
              params.slashing.activeOperatorsRedeemerTxInfoIndex,
          },
        };
  const redeemer: StateQueueRedeemer = {
    RemoveFraudulentBlockHeader: {
      fraudulent_operator: params.fraudulentOperator,
      fraudulent_blocks_header_hash: fraudulentBlocksHeaderHash,
      slashing_approach: slashingApproach,
      fraudulent_node_input_index: params.fraudulentNodeInputIndex,
      fraud_proof_ref_input_index: params.fraudProofRefInputIndex,
      block_removal_approach: {
        RemoveLastFraudulentBlock: {
          anchor_element_input_index: params.anchorElementInputIndex,
          anchor_element_output_index: params.anchorElementOutputIndex ?? 0n,
        },
      },
    },
  };

  const additionalInputs = params.additionalInputs ?? [];
  const referenceScriptInputs = Object.values(
    params.referenceScripts ?? {},
  ).filter((utxo): utxo is UTxO => utxo !== undefined);
  const slashingReferenceInputs =
    params.slashing.kind === "operatorAlreadySlashed"
      ? [
          params.slashing.activeOperatorsElementRefInput,
          params.slashing.retiredOperatorsElementRefInput,
        ]
      : [];
  const referenceInputs = dedupeAndSortUtxos([
    params.fraudProofRefInput,
    ...(params.additionalRefInputs ?? []),
    ...slashingReferenceInputs,
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
      [params.anchorUTxO.utxo, params.fraudulentBlockUTxO.utxo],
      Data.void(),
    )
    .readFrom(referenceInputs)
    .pay.ToContract(
      config.stateQueueAddress,
      { kind: "inline", value: encodeLinkedListNodeView(continuedAnchorDatum) },
      params.anchorUTxO.utxo.assets,
    )
    .mintAssets(assetsToBurn, Data.to(redeemer, StateQueueRedeemer));

  if (params.referenceScripts?.stateQueueSpend === undefined) {
    tx = tx.attach.Script(params.stateQueueSpendingScript);
  }
  if (params.referenceScripts?.stateQueueMint === undefined) {
    tx = tx.attach.Script(params.stateQueueMintingScript);
  }

  if (params.slashing.kind === "operatorAlreadySlashed") {
    return tx;
  } else {
    tx = tx
      .collectFrom(
        [...params.slashing.activeOperatorInputs],
        Data.to(
          params.slashing.activeOperatorSpendRedeemer ?? "ListStateTransition",
          ActiveOperatorSpendRedeemer,
        ),
      )
      .mintAssets(
        params.slashing.activeOperatorsAssetsToBurn,
        Data.to(
          params.slashing.activeOperatorsMintRedeemer,
          ActiveOperatorMintRedeemer,
        ),
      );
    if (params.referenceScripts?.activeOperatorsMint === undefined) {
      tx = tx.attach.Script(params.slashing.activeOperatorsMintingScript);
    }
    if (params.slashing.continuedActiveOperatorAnchorOutput !== undefined) {
      tx = tx.pay.ToContract(
        params.slashing.continuedActiveOperatorAnchorOutput.address,
        {
          kind: "inline",
          value: params.slashing.continuedActiveOperatorAnchorOutput.datum,
        },
        params.slashing.continuedActiveOperatorAnchorOutput.assets,
      );
    }
    if (params.slashing.schedulerSpend !== undefined) {
      tx = tx.pay
        .ToContract(
          params.slashing.schedulerSpend.continuedOutput.address,
          {
            kind: "inline",
            value: params.slashing.schedulerSpend.continuedOutput.datum,
          },
          params.slashing.schedulerSpend.continuedOutput.assets,
        )
        .collectFrom(
          [params.slashing.schedulerSpend.input],
          Data.to(
            params.slashing.schedulerSpend.redeemer,
            SchedulerSpendRedeemer,
          ),
        );
      if (params.referenceScripts?.schedulerSpend === undefined) {
        tx = tx.attach.Script(params.slashing.schedulerSpend.script);
      }
    }
    if (
      params.slashing.activeOperatorSpendingScript !== undefined &&
      params.referenceScripts?.activeOperatorsSpend === undefined
    ) {
      tx = tx.attach.Script(params.slashing.activeOperatorSpendingScript);
    }
  }

  return tx;
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
      redeemer: Data.to(
        { Init: { output_index: params.outputIndex ?? 0n } },
        StateQueueRedeemer,
      ),
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
