import {
  Address,
  Assets,
  Data,
  LucidEvolution,
  fromText,
  paymentCredentialOf,
  PolicyId,
  Script,
  toUnit,
  TxBuilder,
  TxSignBuilder,
  UTxO,
  OutputDatum,
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
  getStateToken,
  hashHexWithBlake2b224,
  utxosAtByNFTPolicyId,
  AuthenticatedValidator,
} from "@/common.js";
import { LucidError, makeReturn } from "@/common.js";
import {
  NodeDatum,
  NodeDatumSchema,
  NodeKey,
  getNodeDatumFromUTxO,
  LinkedListError,
  incompleteInitLinkedListTxProgram,
} from "@/linked-list.js";
import { ConfirmedState, Header } from "@/ledger-state.js";
import {
  GENESIS_HASH_28,
  GENESIS_HASH_32,
  INITIAL_PROTOCOL_VERSION,
  NODE_ASSET_NAME,
} from "./constants.js";

/**
 * SDK helpers for the state-queue linked list.
 *
 * The state queue is the protocol's ordered backlog of committed block headers
 * rooted at the confirmed state. This file therefore mixes three concerns:
 * authentication of queue UTxOs, linked-list traversal helpers, and
 * transaction builders for queue mutations.
 */
const STATE_QUEUE_INIT_LOVELACE = 5_000_000n;
const STATE_QUEUE_HEADER_NODE_LOVELACE = 5_000_000n;

/**
 * Static configuration stored alongside the state queue.
 */
export const StateQueueConfigSchema = Data.Object({
  initUTxO: OutputReferenceSchema,
  refundWaitingPeriod: POSIXTimeSchema,
});
export type StateQueueConfig = Data.Static<typeof StateQueueConfigSchema>;
export const StateQueueConfig =
  StateQueueConfigSchema as unknown as StateQueueConfig;

/**
 * Redeemers accepted by the state-queue validator/policy pair.
 */
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
  Data.Object({
    MergeToConfirmedState: Data.Object({
      header_node_key: Data.Bytes(),
      header_node_input_index: Data.Integer(),
      confirmed_state_node_input_index: Data.Integer(),
      confirmed_state_node_output_index: Data.Integer(),
      settlement_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveFraudulentBlockHeader: Data.Object({
      fraudulentOperator: Data.Bytes(),
    }),
  }),
]);
export type StateQueueRedeemer = Data.Static<typeof StateQueueRedeemerSchema>;
export const StateQueueRedeemer =
  StateQueueRedeemerSchema as unknown as StateQueueRedeemer;

/**
 * Datum stored at each state-queue node.
 */
export type StateQueueDatum = Data.Static<typeof NodeDatumSchema>;
export const StateQueueDatum = NodeDatumSchema as unknown as StateQueueDatum;

/**
 * Authenticated state-queue UTxO paired with its decoded datum and node asset
 * name.
 */
export type StateQueueUTxO = {
  utxo: UTxO;
  datum: StateQueueDatum;
  assetName: string;
};

/**
 * Location of the state-queue state machine on chain.
 */
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
 * Authenticates one raw UTxO as a state-queue node and decodes its datum.
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
 * Converts raw UTxOs into authenticated state-queue nodes, silently dropping
 * malformed or unauthenticated entries.
 */
export const utxosToStateQueueUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<StateQueueUTxO[]> => {
  const effects = utxos.map((u) => utxoToStateQueueUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};

/**
 * Resolves a linked-list key to the corresponding authenticated queue node.
 */
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
          cause: `Expected exactly one confirmed-state root node, found ${filteredForConfirmedState.length}`,
        }),
      );
      return [];
    }
  });

/**
 * Confirms that a state-queue node is the root node and decodes its payload as
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

/**
 * Decodes a non-root state-queue node as a committed block header.
 *
 * TODO: This concept belongs more naturally in `ledger-state.ts`; keep it here
 * until the queue helpers are reorganized.
 */
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

/**
 * Computes the canonical hash of a committed block header.
 *
 * TODO: This concept belongs more naturally in `ledger-state.ts`; keep it here
 * until the queue helpers are reorganized.
 */
export const hashBlockHeader = (
  header: Header,
): Effect.Effect<string, HashingError> =>
  hashHexWithBlake2b224(Data.to(header, Header));

/**
 * Given the latest block in state queue, along with the required tree roots,
 * this function returns the updated datum of the latest block, along with the
 * new `Header` that should be stored in the newly appended tail node.
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
 * Promise-style wrapper around
 * `updateLatestBlocksDatumAndGetTheNewHeaderProgram`.
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
 * The transaction both appends a new tail node and rewrites the previous tail
 * so its `next` pointer links to the new block header.
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
    const mintedAssets: Assets = {
      [toUnit(policyId, NODE_ASSET_NAME + newHeaderHash)]: 1n,
    };
    const outputAssets: Assets = {
      lovelace: STATE_QUEUE_HEADER_NODE_LOVELACE,
      ...mintedAssets,
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
        outputAssets,
      )
      .pay.ToContract(
        config.stateQueueAddress,
        { kind: "inline", value: Data.to(updatedNodeDatum, StateQueueDatum) },
        latestBlock.utxo.assets,
      )
      .mintAssets(mintedAssets, Data.void())
      .attach.Script(stateQueueSpendingScript)
      .attach.Script(stateQueueMintingScript);
    return tx;
  });

/**
 * Completes the block-header commit transaction with local UPLC evaluation
 * enabled.
 */
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
          .complete({ localUPLCEval: true }),
      catch: (e) =>
        new StateQueueError({
          message: `Failed to build block header commitment transaction: ${e}`,
          cause: e,
        }),
    });
    return completedTx;
    });

/**
 * Promise-style wrapper for block-header commit transactions.
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

/**
 * Fetches, authenticates, and topologically sorts the entire state queue.
 */
export const fetchSortedStateQueueUTxOsProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
): Effect.Effect<StateQueueUTxO[], LucidError | LinkedListError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId,
    );
    const unsorted = yield* utxosToStateQueueUTxOs(
      allUTxOs,
      config.stateQueuePolicyId,
    );
    return yield* sortStateQueueUTxOs(unsorted);
  });

/**
 * Fetches and authenticates the state queue without attempting to sort it.
 */
export const fetchUnsortedStateQueueUTxOsProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
): Effect.Effect<StateQueueUTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId,
    );
    return yield* utxosToStateQueueUTxOs(allUTxOs, config.stateQueuePolicyId);
  });

/**
 * Promise-style wrapper for fetching the sorted state queue.
 */
export const fetchSortedStateQueueUTxOs = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
) => makeReturn(fetchSortedStateQueueUTxOsProgram(lucid, config)).unsafeRun();

/**
 * Promise-style wrapper for fetching authenticated but unsorted state-queue
 * nodes.
 */
export const fetchUnsortedStateQueueUTxOs = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
) => makeReturn(fetchUnsortedStateQueueUTxOsProgram(lucid, config)).unsafeRun();

/**
 * Fetches the confirmed-state root node and, when present, the first queued
 * block linked from it.
 */
export const fetchConfirmedStateAndItsLinkProgram = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
): Effect.Effect<
  { confirmed: StateQueueUTxO; link?: StateQueueUTxO },
  StateQueueError | LucidError | LinkedListError
> =>
  Effect.gen(function* () {
    const initUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.stateQueueAddress,
      config.stateQueuePolicyId,
    );
    const allUTxOs = yield* utxosToStateQueueUTxOs(
      initUTxOs,
      config.stateQueuePolicyId,
    );
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
    if (filteredForConfirmedState.length !== 1) {
      return yield* Effect.fail(
        new StateQueueError({
          message: "Failed to fetch confirmed state and its link",
          cause:
            filteredForConfirmedState.length === 0
              ? "No authentic confirmed state UTxO was found"
              : `Expected exactly one confirmed-state root node, found ${filteredForConfirmedState.length}`,
        }),
      );
    }
    const selectedEntry = filteredForConfirmedState[0];
    const confirmedStateUTxO = selectedEntry.utxo;
    if (selectedEntry.link === "Empty") {
      return {
        confirmed: confirmedStateUTxO,
        link: undefined,
      };
    }
    const linkUTxO = yield* findLinkStateQueueUTxO(selectedEntry.link, allUTxOs);
    return {
      confirmed: confirmedStateUTxO,
      link: linkUTxO,
    };
  });

/**
 * Promise-style wrapper for fetching the confirmed-state root and its first
 * link.
 */
export const fetchConfirmedStateAndItsLink = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
) =>
  makeReturn(fetchConfirmedStateAndItsLinkProgram(lucid, config)).unsafeRun();

/**
 * Fetches the unique tail node, i.e. the most recently committed block header.
 */
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
      allBlocks.map((u: UTxO) => {
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
    } else if (filtered.length > 1) {
      return yield* Effect.fail(
        new StateQueueError({
          message: errorMessage,
          cause: `Expected exactly one tail node, found ${filtered.length}`,
        }),
      );
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
 * Promise-style wrapper for fetching the latest committed block.
 */
export const fetchLatestCommittedBlock = (
  lucid: LucidEvolution,
  config: StateQueueFetchConfig,
) => makeReturn(fetchLatestCommittedBlockProgram(lucid, config)).unsafeRun();

/**
 * Builds the initialization fragment for the state queue.
 *
 * Genesis state is represented as a root linked-list node whose payload is the
 * initial `ConfirmedState`.
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
      lovelace: STATE_QUEUE_INIT_LOVELACE,
    });
  });

/**
 * Completes the state-queue initialization transaction with local UPLC
 * evaluation enabled.
 */
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
      try: () => commitTx.complete({ localUPLCEval: true }),
      catch: (e) =>
        new LucidError({
          message: `Failed to build the init state queue transaction: ${e}`,
          cause: e,
        }),
    });
    return completedTx;
  });

/**
 * Promise-style wrapper for state-queue initialization transactions.
 */
export const unsignedInitStateQueueTx = (
  lucid: LucidEvolution,
  initParams: StateQueueInitParams,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedInitStateQueueTxProgram(lucid, initParams)).unsafeRun();

/**
 * Stub entry point for tearing down the state queue.
 */
export const incompleteDeinitStateQueueTxProgram = (
  lucid: LucidEvolution,
  params: StateQueueDeinitParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for removing a fraudulent or invalid block from the queue.
 */
export const incompleteRemoveBlockStateQueueTxProgram = (
  lucid: LucidEvolution,
  params: StateQueueRemoveBlockParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Builds the transaction fragment that merges the oldest queued block into the
 * confirmed state and burns the consumed header-node NFT.
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
    const headerHash =
      firstBlockUTxO.datum.key === "Empty"
        ? yield* hashBlockHeader(blockHeader)
        : firstBlockUTxO.datum.key.Key.key;
    const newConfirmedState = {
      ...currentConfirmedState,
      headerHash,
      prevHeaderHash: currentConfirmedState.headerHash,
      // On-chain merge currently preserves utxoRoot from prior confirmed state.
      utxoRoot: currentConfirmedState.utxoRoot,
      startTime: blockHeader.startTime,
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
      .validFrom(Date.now())
      .collectFrom(
        [confirmedUTxO.utxo, firstBlockUTxO.utxo],
        Data.to(
          {
            MergeToConfirmedState: {
              header_node_key: headerHash,
              header_node_input_index: 1n,
              confirmed_state_node_input_index: 0n,
              confirmed_state_node_output_index: 0n,
              settlement_redeemer_index: 0n,
            },
          },
          StateQueueRedeemer,
        ),
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

/**
 * Completes the merge transaction that advances confirmed state.
 */
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
 * Promise-style wrapper for merging the oldest queued block into confirmed
 * state.
 */
export const mergeToConfirmedState = (
  lucid: LucidEvolution,
  fetchConfig: StateQueueFetchConfig,
  mergeParams: StateQueueMergeParams,
): Promise<TxSignBuilder> =>
  makeReturn(
    mergeToConfirmedStateProgram(lucid, fetchConfig, mergeParams),
  ).unsafeRun();

/**
 * State-queue-specific tagged error used by the higher-level queue helpers.
 */
export class StateQueueError extends EffectData.TaggedError(
  "StateQueueError",
)<GenericErrorFields> {}
