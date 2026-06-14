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
import { ActiveOperatorUpdateCommitmentTimeParams } from "@/operator-directory/active-operators.js";
import { Data as EffectData, Effect } from "effect";
import {
  DataCoercionError,
  GenericErrorFields,
  HashingError,
  MerkleRoot,
  OutputReferenceSchema,
  POSIXTime,
  POSIXTimeSchema,
  hashHexWithBlake2b224,
  AuthenticatedValidator,
} from "@/common.js";
import { LucidError, makeReturn } from "@/common.js";
import {
  LinkedListError,
  incompleteInitLinkedListTxProgram,
  sortElementUTxOs,
  ElementUTxO,
  LinkedListParameters,
  utxosToElementUTxOs,
  utxoToElementUTxO,
  getRootData,
  getNodeDataAndKey,
} from "@/linked-list.js";
import { ConfirmedState, Header } from "@/ledger-state.js";
import { Element } from "@/linked-list.js";

export const GENESIS_HASH_28 = "00".repeat(28);
export const GENESIS_HASH_32 = "00".repeat(32);
export const INITIAL_PROTOCOL_VERSION = 0n;
export const CONFIRMED_STATE_ROOT_KEY: string = fromText(
  "MIDGARD_CONFIRMED_STATE",
);
export const BLOCK_ASSET_NAME_PREFIX: string = fromText("MBLC");

export const STATE_QUEUE_LINKED_LIST_PARAMETERS = (
  nftPolicy: PolicyId,
): LinkedListParameters<ConfirmedState, Header> => ({
  nftPolicy,
  rootKey: CONFIRMED_STATE_ROOT_KEY,
  nodeKeyPrefix: BLOCK_ASSET_NAME_PREFIX,
  rootType: ConfirmedState,
  nodeType: Header,
});

export const StateQueueConfigSchema = Data.Object({
  initUTxO: OutputReferenceSchema,
  refundWaitingPeriod: POSIXTimeSchema,
});
export type StateQueueConfig = Data.Static<typeof StateQueueConfigSchema>;
export const StateQueueConfig =
  StateQueueConfigSchema as unknown as StateQueueConfig;

export const SlashingApproachSchema = Data.Enum([
  Data.Object({
    SlashActiveOperator: Data.Object({
      activeOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    SlashRetiredOperator: Data.Object({
      retiredOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    OperatorAlreadySlashed: Data.Object({
      activeOperatorsElementRefInputIndex: Data.Integer(),
      retiredOperatorsElementRefInputIndex: Data.Integer(),
    }),
  }),
]);
export type SlashingApproach = Data.Static<typeof SlashingApproachSchema>;
export const SlashingApproach =
  SlashingApproachSchema as unknown as SlashingApproach;

export const BlockRemovalApproachSchema = Data.Enum([
  Data.Object({
    RemoveLastFraudulentBlock: Data.Object({
      anchorElementInputIndex: Data.Integer(),
      anchorElementOutputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveFraudulentBlocksLink: Data.Object({
      fraudulentNodeOutputIndex: Data.Integer(),
      removedBlockInputIndex: Data.Integer(),
    }),
  }),
]);
export type BlockRemovalApproach = Data.Static<
  typeof BlockRemovalApproachSchema
>;
export const BlockRemovalApproach =
  BlockRemovalApproachSchema as unknown as BlockRemovalApproach;

export const StateQueueMintRedeemerSchema = Data.Enum([
  Data.Object({ Init: Data.Object({ outputIndex: Data.Integer() }) }),
  Data.Object({ Deinit: Data.Object({ inputIndex: Data.Integer() }) }),
  Data.Object({
    CommitBlockHeader: Data.Object({
      latestBlockInputIndex: Data.Integer(),
      newBlockOutputIndex: Data.Integer(),
      continuedLatestBlockOutputIndex: Data.Integer(),
      operator: Data.Bytes(),
      schedulerRefInputIndex: Data.Integer(),
      activeOperatorsInputIndex: Data.Integer(),
      activeOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    MergeToConfirmedState: Data.Object({
      headerNodeKey: Data.Bytes(),
      headerNodeInputIndex: Data.Integer(),
      confirmedStateInputIndex: Data.Integer(),
      confirmedStateOutputIndex: Data.Integer(),
      mSettlementRedeemerIndex: Data.Nullable(Data.Integer()),
    }),
  }),
  Data.Object({
    RemoveFraudulentBlockHeader: Data.Object({
      fraudulentOperator: Data.Bytes(),
      fraudulentblocksHeaderHash: Data.Bytes(),
      slashingApproach: SlashingApproachSchema,
      fraudulentNodeInputIndex: Data.Integer(),
      fraudProofRefInputIndex: Data.Integer(),
      blockRemovalApproach: BlockRemovalApproachSchema,
    }),
  }),
]);
export type StateQueueMintRedeemer = Data.Static<
  typeof StateQueueMintRedeemerSchema
>;
export const StateQueueMintRedeemer =
  StateQueueMintRedeemerSchema as unknown as StateQueueMintRedeemer;

export type StateQueueDatum = Element;
export const StateQueueDatum = Element;

export type StateQueueUTxO = ElementUTxO<ConfirmedState, Header>;

export const utxoToStateQueueUTxO = (utxo: UTxO, nftPolicy: PolicyId) =>
  utxoToElementUTxO(utxo, {
    nftPolicy,
    rootKey: CONFIRMED_STATE_ROOT_KEY,
    nodeKeyPrefix: BLOCK_ASSET_NAME_PREFIX,
    rootType: ConfirmedState,
    nodeType: Header,
  });

export type StateQueueFetchConfig = {
  stateQueueAddress: Address;
  stateQueuePolicyId: PolicyId;
};

export type StateQueueCommitBlockParams = {
  anchorUTxO: StateQueueUTxO;
  updatedAnchorDatum: StateQueueDatum;
  newHeader: Header;
  stateQueueSpendingScript: Script;
  policyId: PolicyId;
  stateQueueMintingScript: Script;
};

export type StateQueueMergeParams = {
  confirmedUTxO: StateQueueUTxO;
  firstBlockUTxO: StateQueueUTxO;
  stateQueueSpendingScript: Script;
  stateQueueMintingScript: Script;
};

export type StateQueueInitParams = {
  validator: AuthenticatedValidator;
  genesisTime: POSIXTime;
};

export type StateQueueDeinitParams = {};

export type StateQueueRemoveBlockParams = {};

/**
 * Returns a sorted array of `StateQueueUTxO`s where the confirmed state's UTxO
 * is the head element, and the following elements are linked from their
 * previous elements. The last element is guaranteed to have no link.
 */
export const sortStateQueueUTxOs = (
  stateQueueUTxOs: StateQueueUTxO[],
): Effect.Effect<
  { root: StateQueueUTxO; nodes: StateQueueUTxO[] },
  LinkedListError
> => sortElementUTxOs<ConfirmedState, Header, StateQueueUTxO>(stateQueueUTxOs);

/**
 * Given a StateQueue datum, this function confirms the node is root
 * (its element data is `Root`), and attempts to coerce its underlying data into
 * a `ConfirmedState`.
 */
export const getConfirmedStateFromStateQueueDatum = (
  elementDatum: StateQueueDatum,
): Effect.Effect<
  { data: ConfirmedState; link: string | null },
  DataCoercionError
> => {
  try {
    if ("Root" in elementDatum.data) {
      const confirmedState = Data.castFrom(
        elementDatum.data.Root,
        ConfirmedState,
      );
      return Effect.succeed({
        data: confirmedState,
        link: elementDatum.link,
      });
    } else {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce to a root datum`,
          cause: `Given UTxO is not root`,
        }),
      );
    }
  } catch (e) {
    return Effect.fail(
      new DataCoercionError({
        message: `Could not coerce element data to a ConfirmedState`,
        cause: e,
      }),
    );
  }
};

export const getConfirmedStateFromStateQueueUTxO = (utxo: StateQueueUTxO) =>
  getRootData<ConfirmedState, Header, StateQueueUTxO>(utxo);

export const getHeaderFromStateQueueUTxO = (utxo: StateQueueUTxO) =>
  getNodeDataAndKey<ConfirmedState, Header, StateQueueUTxO>(utxo);

// TODO: this function is from ledger-state, mb it should be moved from here
export const hashBlockHeader = (
  header: Header,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b224(Data.to(header, Header));

/**
 * Given the latest block in state queue, along with the required tree roots,
 * this function returns the updated datum of the latest block, along with the
 * new `Header` that should be included in the new block's datum.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param latestBlock - StateQueueUTxO of the latest element (either root or node).
 * @param newUTxOsRoot - MPF root of the updated ledger.
 * @param transactionsRoot - MPF root of the transactions included in the new block.
 * @param depositsRoot - MPF root of the deposit transactions included in the new block.
 * @param withdrawalsRoot - MPF root of the withdrawal transactions included in the new block.
 * @param endTime - POSIX time of the new block's closing range.
 */
export const updateLatestBlocksDatumAndGetTheNewHeaderProgram = (
  lucid: LucidEvolution,
  latestBlock: StateQueueUTxO,
  newUTxOsRoot: MerkleRoot,
  transactionsRoot: MerkleRoot,
  depositsRoot: MerkleRoot,
  withdrawalsRoot: MerkleRoot,
  endTime: POSIXTime,
): Effect.Effect<
  { nodeDatum: StateQueueDatum; header: Header },
  DataCoercionError | LucidError | HashingError
> =>
  Effect.gen(function* () {
    const walletAddress: string = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (e) =>
        new LucidError({ message: `Failed to find the wallet`, cause: e }),
    });

    const pubKeyHash = paymentCredentialOf(walletAddress).hash;
    if ("key" in latestBlock) {
      const latestHeader = latestBlock.coercedData;
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
      return {
        nodeDatum: {
          ...latestBlock.datum,
          data: { Node: Data.to(newHeader, Header) },
        },
        header: newHeader,
      };
    } else {
      const confirmedState = latestBlock.coercedData;
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
      return {
        nodeDatum: {
          ...latestBlock.datum,
          data: { Node: Data.to(newHeader, Header) },
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
 * @param latestBlocksUTxO - StateQueueUTxO of the latest block in queue.
 * @param newUTxOsRoot - MPF root of the updated ledger.
 * @param transactionsRoot - MPF root of the transactions included in the new block.
 * @param depositsRoot - MPF root of the deposit transactions included in the new block.
 * @param withdrawalsRoot - MPF root of the withdrawal transactions included in the new block.
 * @param endTime - POSIX time of the new block's closing range.
 */
export const updateLatestBlocksDatumAndGetTheNewHeader = (
  lucid: LucidEvolution,
  latestBlocksUTxO: StateQueueUTxO,
  newUTxOsRoot: MerkleRoot,
  transactionsRoot: MerkleRoot,
  depositsRoot: MerkleRoot,
  withdrawalsRoot: MerkleRoot,
  endTime: POSIXTime,
): Promise<{ nodeDatum: StateQueueDatum; header: Header }> =>
  makeReturn(
    updateLatestBlocksDatumAndGetTheNewHeaderProgram(
      lucid,
      latestBlocksUTxO,
      newUTxOsRoot,
      transactionsRoot,
      depositsRoot,
      withdrawalsRoot,
      endTime,
    ),
  ).unsafeRun();

/**
 * Builds portions of a tx required for submitting a new block, using the
 * provided `LucidEvolution` instance, fetch config, and required parameters.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param fetchConfig - Configuration values required to know where to look for which NFT.
 * @param commitParams - Parameters required for committing to state queue.
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteCommitBlockHeaderTxProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
  {
    anchorUTxO: latestBlock,
    updatedAnchorDatum: updatedNodeDatum,
    newHeader,
    stateQueueSpendingScript,
    policyId,
    stateQueueMintingScript,
  }: StateQueueCommitBlockParams,
): Effect.Effect<TxBuilder, HashingError> =>
  Effect.gen(function* () {
    const newHeaderHash = yield* hashBlockHeader(newHeader);
    const assets: Assets = {
      [toUnit(policyId, BLOCK_ASSET_NAME_PREFIX + newHeaderHash)]: 1n,
    };

    const newNodeDatum: StateQueueDatum = {
      data: {
        Node: Data.to(newHeader, Header),
      },
      link: null,
    };

    // Note that we are not specifying a validity range (TODO?).
    const tx = lucid
      .newTx()
      .collectFrom([latestBlock.utxo], Data.void())
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: Data.to(newNodeDatum, StateQueueDatum) },
        assets,
      )
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: Data.to(updatedNodeDatum, StateQueueDatum) },
        latestBlock.utxo.assets,
      )
      .mintAssets(assets, Data.void())
      .attach.Script(stateQueueSpendingScript)
      .attach.Script(stateQueueMintingScript);
    return tx;
  });

export const unsignedCommitBlockHeaderTxProgram = (
  lucid: LucidEvolution,
  fetchConfig: StateQueueFetchConfig,
  sqCommitParams: StateQueueCommitBlockParams,
  aoUpdateParams: ActiveOperatorUpdateCommitmentTimeParams,
): Effect.Effect<TxSignBuilder, StateQueueError | HashingError> =>
  Effect.gen(function* () {
    const commitTx = yield* incompleteCommitBlockHeaderTxProgram(
      lucid,
      fetchConfig,
      sqCommitParams,
    );
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () =>
        commitTx
          // .compose(
          //   ActiveOperators.updateCommitmentTimeTxBuilder(lucid, aoUpdateParams)
          // )
          .complete({ localUPLCEval: false }),
      catch: (e) =>
        new StateQueueError({
          message: `Failed to build block header commitment transaction: ${e}`,
          cause: e,
        }),
    });
    return completedTx;
  });

/**
 * Builds completed tx for submitting a new block using the provided
 * `LucidEvolution` instance, fetch config, and required parameters.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param fetchConfig - Configuration values required to know where to look for which NFT.
 * @param sqCommitParams - Parameters required for committing to state queue.
 * @param aoUpdateParams - Parameters required for updating the active operator's commitment time.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedCommitBlockHeaderTx = (
  lucid: LucidEvolution,
  fetchConfig: StateQueueFetchConfig,
  sqCommitParams: StateQueueCommitBlockParams,
  aoUpdateParams: ActiveOperatorUpdateCommitmentTimeParams,
): Promise<TxSignBuilder> =>
  makeReturn(
    unsignedCommitBlockHeaderTxProgram(
      lucid,
      fetchConfig,
      sqCommitParams,
      aoUpdateParams,
    ),
  ).unsafeRun();

export const fetchUnsortedStateQueueUTxOsProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
): Effect.Effect<StateQueueUTxO[], LucidError> =>
  Effect.tryPromise({
    try: () => lucid.utxosAt(config.stateQueueAddress),
    catch: (e) => {
      return new LucidError({
        message: `Failed to fetch state queue UTxOs at: ${config.stateQueueAddress}`,
        cause: e,
      });
    },
  }).pipe(
    Effect.andThen((allUTxOs) =>
      utxosToElementUTxOs<ConfirmedState, Header>(
        allUTxOs,
        STATE_QUEUE_LINKED_LIST_PARAMETERS(config.stateQueuePolicyId),
      ),
    ),
  );

/**
 * Fetches all UTxOs at state queue, filters the authentic ones, and returns the
 * complete queue, starting with the confirmed state UTxO, up until the last
 * block in the queue.
 */
export const fetchSortedStateQueueUTxOsProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
): Effect.Effect<
  { root: StateQueueUTxO; nodes: StateQueueUTxO[] },
  LucidError | LinkedListError
> =>
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

export const fetchLatestCommittedBlockProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
): Effect.Effect<StateQueueUTxO, LinkedListError | LucidError> =>
  Effect.gen(function* () {
    const { root, nodes } = yield* fetchSortedStateQueueUTxOsProgram(
      lucid,
      config,
    );
    if (nodes.length <= 0) {
      return root;
    } else {
      return nodes[nodes.length - 1];
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
      headerHash: GENESIS_HASH_28,
      prevHeaderHash: GENESIS_HASH_28,
      utxoRoot: GENESIS_HASH_32,
      startTime: params.genesisTime,
      endTime: params.genesisTime,
      protocolVersion: INITIAL_PROTOCOL_VERSION,
    };

    const mintRedeemer = Data.to(
      { Init: { outputIndex: 0n } },
      StateQueueMintRedeemer,
    );

    return yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.validator,
      rootData: Data.castTo(stateQueueData, ConfirmedState),
      redeemer: mintRedeemer,
      rootKey: CONFIRMED_STATE_ROOT_KEY,
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
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () => commitTx.complete({ localUPLCEval: false }),
      catch: (e) =>
        new LucidError({
          message: `Failed to build the init state queue transaction: ${e}`,
          cause: e,
        }),
    });
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

/**
 * Deinit
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteDeinitStateQueueTxProgram = (
  lucid: LucidEvolution,
  params: StateQueueDeinitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Remove a block
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteRemoveBlockStateQueueTxProgram = (
  lucid: LucidEvolution,
  params: StateQueueRemoveBlockParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Merge
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param fetchConfig - Configuration values required to know where to look for which NFT.
 * @param mergeParams - Parameters needed for building the merge transaction.
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteStateQueueMergeTxProgram = (
  lucid: LucidEvolution,
  fetchConfig: StateQueueFetchConfig,
  {
    confirmedUTxO,
    firstBlockUTxO,
    stateQueueSpendingScript,
    stateQueueMintingScript,
  }: StateQueueMergeParams,
): Effect.Effect<
  TxBuilder,
  HashingError | DataCoercionError | LucidError | LinkedListError
> =>
  Effect.gen(function* () {
    const currentConfirmedState =
      yield* getConfirmedStateFromStateQueueUTxO(confirmedUTxO);
    const { nodeData: blockHeader, key: firstBlocksKey } =
      yield* getHeaderFromStateQueueUTxO(firstBlockUTxO);
    const headerHash = yield* hashBlockHeader(blockHeader);
    const newConfirmedState = {
      ...currentConfirmedState,
      headerHash,
      prevHeaderHash: currentConfirmedState.headerHash,
      utxoRoot: blockHeader.utxosRoot,
      startTime: currentConfirmedState.endTime,
      endTime: blockHeader.endTime,
      protocolVersion: blockHeader.protocolVersion,
    };

    const newConfirmedElementDatum: StateQueueDatum = {
      ...confirmedUTxO.datum,
      data: {
        Root: Data.to(newConfirmedState, ConfirmedState),
      },
      link: firstBlockUTxO.datum.link,
    };
    const assetsToBurn: Assets = {
      [toUnit(fetchConfig.stateQueuePolicyId, firstBlockUTxO.assetName)]: -1n,
    };

    const redeemer: StateQueueMintRedeemer = {
      MergeToConfirmedState: {
        headerNodeKey: firstBlocksKey,
        headerNodeInputIndex: 0n,
        confirmedStateInputIndex: 0n,
        confirmedStateOutputIndex: 0n,
        mSettlementRedeemerIndex: 0n,
      },
    };
    const redeemerCBOR = Data.to(redeemer, StateQueueMintRedeemer);

    const tx = lucid
      .newTx()
      .collectFrom([confirmedUTxO.utxo, firstBlockUTxO.utxo], redeemerCBOR)
      .pay.ToContract(
        fetchConfig.stateQueueAddress,
        {
          kind: "inline",
          value: Data.to(newConfirmedElementDatum, StateQueueDatum),
        },
        confirmedUTxO.utxo.assets,
      )
      .mintAssets(assetsToBurn, Data.void())
      .attach.Script(stateQueueSpendingScript)
      .attach.Script(stateQueueMintingScript);
    return tx;
  });

export const mergeToConfirmedStateProgram = (
  lucid: LucidEvolution,
  fetchConfig: StateQueueFetchConfig,
  mergeParams: StateQueueMergeParams,
): Effect.Effect<
  TxSignBuilder,
  DataCoercionError | LucidError | HashingError | LinkedListError
> =>
  Effect.gen(function* () {
    const completedTx = yield* incompleteStateQueueMergeTxProgram(
      lucid,
      fetchConfig,
      mergeParams,
    );
    return yield* completedTx.completeProgram().pipe(
      Effect.mapError(
        (e) =>
          new LucidError({
            message:
              "Failed to finalize the transaction for merging oldest block into confirmed state",
            cause: e,
          }),
      ),
    );
  });

/**
 * Builds completed tx for merging the first block in queue to be merged into
 * the confirmed state.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param fetchConfig - Configuration values required to know where to look for which NFT.
 * @param mergeParams - Parameters needed for building the merge transaction.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const mergeToConfirmedState = (
  lucid: LucidEvolution,
  fetchConfig: StateQueueFetchConfig,
  mergeParams: StateQueueMergeParams,
): Promise<TxSignBuilder> =>
  makeReturn(
    mergeToConfirmedStateProgram(lucid, fetchConfig, mergeParams),
  ).unsafeRun();

export class StateQueueError extends EffectData.TaggedError(
  "StateQueueError",
)<GenericErrorFields> {}
