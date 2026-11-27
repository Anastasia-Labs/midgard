import { LucidEvolution, paymentCredentialOf } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  DataCoercionError,
  HashingError,
  MerkleRoot,
  POSIXTime,
  utxosAtByNFTPolicyId,
} from "./common.js";
import { LucidError, makeReturn } from "./common.js";
import {
  confirmedStateNextHeaderProtocolVersion,
  getHeaderFromStateQueueDatum,
  hashBlockHeader,
  Header,
  HeaderTransitionCommitments,
  HeaderTransitionCommitmentsError,
  validateHeaderTransitionCommitmentsProgram,
} from "./ledger-state.js";
import { LinkedListError, LinkedListNodeView } from "./linked-list.js";
import { StateQueueError } from "./state-queue.build-state-queue-removal-tx.js";
import {
  findLinkStateQueueUTxO,
  sortStateQueueUTxOs,
} from "./state-queue.collect-remove-slashing-inputs.js";
import {
  getConfirmedStateFromStateQueueDatum,
  type StateQueueFetchConfig,
  utxosToStateQueueUTxOs,
  utxoToStateQueueUTxO,
} from "./state-queue.emulator-state-queue-commit-block-header-params.js";
import { type StateQueueUTxO } from "./state-queue.state-queue-redeemer-schema.js";

/**
 * Builds the canonical V1 state-queue header and linked-list update. It
 * accepts only StateQueueNodeV1 and commits the validation-trace root/count.
 */
export const updateLatestBlocksDatumAndGetTheNewHeaderProgram = (
  lucid: LucidEvolution,
  latestBlocksDatum: LinkedListNodeView,
  newUTxOsRoot: MerkleRoot,
  transactionsRoot: MerkleRoot,
  depositsRoot: MerkleRoot,
  withdrawalsRoot: MerkleRoot,
  transitionCommitments: HeaderTransitionCommitments,
  endTime: POSIXTime,
  validationContext: Pick<
    Header,
    "blockSlot" | "expectedNetworkId" | "minFeeA" | "minFeeB"
  >,
): Effect.Effect<
  { nodeDatum: LinkedListNodeView; header: Header },
  | DataCoercionError
  | HeaderTransitionCommitmentsError
  | LucidError
  | HashingError
> =>
  Effect.gen(function* () {
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new LucidError({
          message: "Failed to find the wallet",
          cause,
        }),
    });
    const operatorVkey = paymentCredentialOf(walletAddress).hash;
    const commitments = yield* validateHeaderTransitionCommitmentsProgram({
      ...transitionCommitments,
      withdrawalsRoot,
      transactionsRoot,
      depositsRoot,
    });

    if (latestBlocksDatum.key === "Empty") {
      const { data: confirmedState } =
        yield* getConfirmedStateFromStateQueueDatum(latestBlocksDatum);
      const nextProtocolVersion =
        confirmedStateNextHeaderProtocolVersion(confirmedState);
      if (nextProtocolVersion === null) {
        return yield* Effect.fail(
          new DataCoercionError({
            message:
              "Proof-profile state queue root has an invalid protocol identity",
            cause: `protocol_version=${confirmedState.protocolVersion.toString()},header_hash=${confirmedState.headerHash}`,
          }),
        );
      }
      const newHeader: Header = {
        prevUtxosRoot: confirmedState.utxoRoot,
        utxosRoot: newUTxOsRoot,
        withdrawalsRoot,
        forcedTransactionsRoot: commitments.forcedTransactionsRoot,
        transactionsRoot,
        depositsRoot,
        transitionTraceRoot: commitments.transitionTraceRoot,
        eventToStepRoot: commitments.eventToStepRoot,
        validationTracesRoot: commitments.validationTracesRoot,
        withdrawalCount: commitments.withdrawalCount,
        forcedTransactionCount: commitments.forcedTransactionCount,
        l2TransactionCount: commitments.l2TransactionCount,
        depositCount: commitments.depositCount,
        totalEventCount: commitments.totalEventCount,
        transitionStepCount: commitments.transitionStepCount,
        validationTraceCount: commitments.validationTraceCount,
        startTime: confirmedState.endTime,
        endTime,
        ...validationContext,
        prevHeaderHash: confirmedState.headerHash,
        operatorVkey,
        protocolVersion: nextProtocolVersion,
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

    const latestHeader = yield* getHeaderFromStateQueueDatum(latestBlocksDatum);
    const prevHeaderHash = yield* hashBlockHeader(latestHeader);
    const newHeader: Header = {
      ...latestHeader,
      prevUtxosRoot: latestHeader.utxosRoot,
      utxosRoot: newUTxOsRoot,
      withdrawalsRoot,
      forcedTransactionsRoot: commitments.forcedTransactionsRoot,
      transactionsRoot,
      depositsRoot,
      transitionTraceRoot: commitments.transitionTraceRoot,
      eventToStepRoot: commitments.eventToStepRoot,
      validationTracesRoot: commitments.validationTracesRoot,
      withdrawalCount: commitments.withdrawalCount,
      forcedTransactionCount: commitments.forcedTransactionCount,
      l2TransactionCount: commitments.l2TransactionCount,
      depositCount: commitments.depositCount,
      totalEventCount: commitments.totalEventCount,
      transitionStepCount: commitments.transitionStepCount,
      validationTraceCount: commitments.validationTraceCount,
      startTime: latestHeader.endTime,
      endTime,
      ...validationContext,
      prevHeaderHash,
      operatorVkey,
    };
    const newHeaderHash = yield* hashBlockHeader(newHeader);
    return {
      nodeDatum: {
        ...latestBlocksDatum,
        next: { Key: { key: newHeaderHash } },
      },
      header: newHeader,
    };
  });

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
