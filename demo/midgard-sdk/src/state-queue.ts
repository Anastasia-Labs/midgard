import {
  Address,
  Assets,
  Data,
  LucidEvolution,
  paymentCredentialOf,
  PolicyId,
  Script,
  toUnit,
  TxBuilder,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { ActiveOperatorUpdateCommitmentTimeParams } from "@/active-operators.js";
import { Data as EffectData, Effect } from "effect";
import {
  DataCoercionError,
  GenericErrorFields,
  HashingError,
  MissingDatumError,
  UnauthenticUtxoError,
  MerkleRoot,
  OutputReferenceSchema,
  POSIXTime,
  POSIXTimeSchema,
  hashHexWithBlake2b224,
  AuthenticatedValidator,
  utxosAtByNFTPolicyId,
} from "@/common.js";
import { LucidError, makeReturn } from "@/common.js";
import { getStateToken } from "@/internals.js";
import {
  NodeDatum,
  NodeDatumSchema,
  NODE_ASSET_NAME,
  NodeKey,
  getNodeDatumFromUTxO,
  LinkedListError,
  incompleteInitLinkedListTxProgram,
} from "@/linked-list.js";
import { ConfirmedState, Header } from "@/ledger-state.js";

export const GENESIS_HASH_28 = "00".repeat(28);
export const GENESIS_HASH_32 = "00".repeat(32);
export const INITIAL_PROTOCOL_VERSION = 0n;

export const StateQueueConfigSchema = Data.Object({
  initUTxO: OutputReferenceSchema,
  refundWaitingPeriod: POSIXTimeSchema,
});
export type StateQueueConfig = Data.Static<typeof StateQueueConfigSchema>;
export const StateQueueConfig =
  StateQueueConfigSchema as unknown as StateQueueConfig;

export const StateQueueRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Object({
    CommitBlockHeader: Data.Object({
      operator: Data.Bytes(),
      scheduler_ref_input_index: Data.Integer(),
      active_node_input_index: Data.Integer(),
      header_node_output_index: Data.Integer(),
      previous_header_node_output_index: Data.Integer(),
      active_operators_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Literal("MergeToConfirmedState"),
  Data.Object({
    RemoveFraudulentBlockHeader: Data.Object({
      fraudulentOperator: Data.Bytes(),
    }),
  }),
]);
export type StateQueueRedeemer = Data.Static<typeof StateQueueRedeemerSchema>;
export const StateQueueRedeemer =
  StateQueueRedeemerSchema as unknown as StateQueueRedeemer;

export type StateQueueDatum = Data.Static<typeof NodeDatumSchema>;
export const StateQueueDatum = NodeDatumSchema as unknown as StateQueueDatum;

export type StateQueueUTxO = {
  utxo: UTxO;
  datum: StateQueueDatum;
  assetName: string;
};

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
  genesisTime: POSIXTime; // Just pass the time, not the full state
};

export type StateQueueDeinitParams = {};

export type StateQueueRemoveBlockParams = {};

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
    const datum = yield* getNodeDatumFromUTxO(utxo);
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
  } else {
    const foundLink = utxos.find(
      (u: StateQueueUTxO) =>
        u.datum.key !== "Empty" && u.datum.key.Key.key === link.Key.key,
    );
    if (foundLink) {
      return Effect.succeed(foundLink);
    } else {
      return Effect.fail(
        new LinkedListError({
          message: errorMessage,
          cause: `Link not found among given state queue UTxOs`,
        }),
      );
    }
  }
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
    if (filteredForConfirmedState.length === 1) {
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
    } else {
      yield* Effect.fail(
        new LinkedListError({
          message: `Failed to sort state queue UTxOs`,
          cause: `Confirmed state (root node) not found among state queue UTxOs`,
        }),
      );
      return [];
    }
  });

/**
 * Given a StateQueue datum, this function confirms the node is root
 * (i.e. no keys in its datum), and attempts to coerce its underlying data into
 * a `ConfirmedState`.
 */
export const getConfirmedStateFromStateQueueDatum = (
  nodeDatum: StateQueueDatum,
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

// TODO: this function is from ledger-state, mb it should be moved from here
export const getHeaderFromStateQueueDatum = (
  nodeDatum: StateQueueDatum,
): Effect.Effect<Header, DataCoercionError> =>
  Effect.gen(function* () {
    const header: Header = yield* Effect.try({
      try: () => Data.castFrom(nodeDatum.data, Header),
      catch: (e) =>
        new DataCoercionError({
          message: "Failed coercing block's datum data to `Header`",
          cause: e,
        }),
    });
    return header;
  });

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
 * @param latestBlocksDatum - Datum of the UTxO of the latest block in queue.
 * @param newUTxOsRoot - MPF root of the updated ledger.
 * @param transactionsRoot - MPF root of the transactions included in the new block.
 * @param depositsRoot - MPF root of the deposit transactions included in the new block.
 * @param withdrawalsRoot - MPF root of the withdrawal transactions included in the new block.
 * @param endTime - POSIX time of the new block's closing range.
 */
export const updateLatestBlocksDatumAndGetTheNewHeaderProgram = (
  lucid: LucidEvolution,
  latestBlocksDatum: StateQueueDatum,
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
        header: {
          ...latestHeader,
          prevUtxosRoot: latestHeader.utxosRoot,
          utxosRoot: newUTxOsRoot,
          transactionsRoot,
          startTime: latestHeader.endTime,
          endTime,
          prevHeaderHash,
          operatorVkey: pubKeyHash,
        },
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
  latestBlocksDatum: StateQueueDatum,
  newUTxOsRoot: MerkleRoot,
  transactionsRoot: MerkleRoot,
  depositsRoot: MerkleRoot,
  withdrawalsRoot: MerkleRoot,
  endTime: POSIXTime,
): Promise<{ nodeDatum: StateQueueDatum; header: Header }> =>
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
      [toUnit(policyId, NODE_ASSET_NAME + newHeaderHash)]: 1n,
    };

    const newNodeDatum: StateQueueDatum = {
      key: updatedNodeDatum.next,
      next: "Empty",
      data: Data.castTo(newHeader, Header),
    };

    // Add 1 minute
    const endTime = Date.now();
    const endTimePlusOneMinute = endTime + 60000;
    const tx = lucid
      .newTx()
      .validTo(endTimePlusOneMinute)
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
  { confirmed: StateQueueUTxO; link?: StateQueueUTxO },
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
      headerHash: GENESIS_HASH_28,
      prevHeaderHash: GENESIS_HASH_28,
      utxoRoot: GENESIS_HASH_32,
      startTime: params.genesisTime,
      endTime: params.genesisTime,
      protocolVersion: INITIAL_PROTOCOL_VERSION,
    };

    return yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.validator,
      data: Data.castTo(stateQueueData, ConfirmedState),
      redeemer: Data.to("Init", StateQueueRedeemer),
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
): Effect.Effect<TxBuilder, HashingError | DataCoercionError> =>
  Effect.gen(function* () {
    const { data: currentConfirmedState } =
      yield* getConfirmedStateFromStateQueueDatum(confirmedUTxO.datum);
    const blockHeader: Header = yield* getHeaderFromStateQueueDatum(
      firstBlockUTxO.datum,
    );
    const headerHash = yield* hashBlockHeader(blockHeader);
    const newConfirmedState = {
      ...currentConfirmedState,
      headerHash,
      prevHeaderHash: currentConfirmedState.headerHash,
      utxoRoot: blockHeader.utxosRoot,
      startTime: currentConfirmedState.endTime,
      endTime: blockHeader.endTime,
    };
    const newConfirmedNodeDatum: NodeDatum = {
      ...confirmedUTxO.datum,
      data: Data.castTo(newConfirmedState, ConfirmedState),
      next: firstBlockUTxO.datum.next,
    };
    const assetsToBurn: Assets = {
      [toUnit(fetchConfig.stateQueuePolicyId, firstBlockUTxO.assetName)]: -1n,
    };
    const tx = lucid
      .newTx()
      .collectFrom(
        [confirmedUTxO.utxo, firstBlockUTxO.utxo],
        Data.to("MergeToConfirmedState", StateQueueRedeemer),
      )
      .pay.ToContract(
        fetchConfig.stateQueueAddress,
        {
          kind: "inline",
          value: Data.to(newConfirmedNodeDatum, NodeDatum),
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
  DataCoercionError | LucidError | HashingError
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
