import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type AvailabilityCommitFixture,
  availabilityQueueElements,
  liveAvailabilityAnchors,
  liveAvailabilityTarget,
} from "./availability-challenge-emulator.build-availability-timeout.js";
import {
  AVAILABILITY_COLLATERAL_COIN_LOVELACE,
  AVAILABILITY_PROFILE,
} from "./availability-challenge-emulator.measure-availability-transaction.js";
import { type AvailabilityFixture } from "./availability-challenge-emulator.open-availability.js";

/**
 * `f` with the live root, correction lock and block node, for a helper that
 * spends them (`buildAvailabilityTimeout`) after other transactions moved
 * them.
 */
export const withLiveAvailabilityQueue = async (
  f: AvailabilityFixture,
): Promise<AvailabilityFixture> => {
  const target = await liveAvailabilityTarget(f);
  if (target === undefined)
    throw new Error(`Block ${f.target.headerHash} is no longer queued`);
  return { ...f, ...(await liveAvailabilityAnchors(f)), target };
};

export type AvailabilityCommittedBlock = AvailabilityFixture & {
  readonly commitTxHash: string;
  /** The header's `end_time`: the commit's inclusive validity upper bound. */
  readonly headerEndTime: bigint;
};

const hashHeader = (header: SDK.Header): Promise<string> =>
  Effect.runPromise(SDK.hashBlockHeader(header));

/**
 * Commits one block with the real `CommitBlockHeader` (the SDK builder the
 * node uses) after the queue's tail, signed by the responder as the
 * scheduler's active operator. The header is empty (no events), valid for
 * `validForMs` from now (or over an explicit `validity` interval), with
 * `end_time = validTo - 1`, and carries over from its anchor (the confirmed
 * state for an empty queue, else the tail).
 *
 * Returns an `AvailabilityFixture` for the committed block: `target`,
 * `payload`, `commitment` and `queueUnit` are the block's, and the root and
 * correction lock are live as of the commit.
 */
export const commitAvailabilityBlock = async (
  f: AvailabilityCommitFixture,
  options: {
    payloadBytes?: number;
    validForMs?: number;
    validity?: { readonly validFromMs: number; readonly validToMs: number };
  } = {},
): Promise<AvailabilityCommittedBlock> => {
  const { lucid, contracts } = f;
  const payloadBytes = options.payloadBytes ?? 14_021;
  const elements = await availabilityQueueElements(f);
  const root = elements.find((element) => element.view.key === "Empty");
  const tail = elements.find((element) => element.view.next === "Empty");
  if (!root || !tail) throw new Error("The state queue has no root or tail");
  const queueIsEmpty = tail === root;
  const headKey = root.view.next;
  const head =
    headKey === "Empty"
      ? undefined
      : elements.find(
          (element) =>
            element.view.key !== "Empty" &&
            element.view.key.Key.key === headKey.Key.key,
        );
  if (!queueIsEmpty && head === undefined)
    throw new Error("The state queue's head node is missing");
  let anchor: { headerHash: string; utxosRoot: string; endTime: bigint };
  if (tail.view.key === "Empty") {
    const confirmed = Data.castFrom(tail.view.data, SDK.ConfirmedState);
    anchor = {
      headerHash: confirmed.headerHash,
      utxosRoot: confirmed.utxoRoot,
      endTime: confirmed.endTime,
    };
  } else {
    const node = Data.castFrom(tail.view.data, SDK.StateQueueNode);
    anchor = {
      headerHash: tail.view.key.Key.key,
      utxosRoot: node.header.utxosRoot,
      endTime: node.header.endTime,
    };
  }
  const validFrom = options.validity?.validFromMs ?? f.emulator.now();
  const validTo =
    options.validity?.validToMs ?? validFrom + (options.validForMs ?? 60_000);
  const header: SDK.Header = {
    prevUtxosRoot: anchor.utxosRoot,
    utxosRoot: anchor.utxosRoot,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
    startTime: anchor.endTime,
    endTime: BigInt(validTo - 1),
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 44n,
    minFeeB: 155381n,
    prevHeaderHash: anchor.headerHash,
    operatorVkey: f.responderKey,
    protocolVersion: 1n,
  };
  const [schedulerRefInput] = await lucid.utxosAtWithUnit(
    contracts.scheduler.spendingScriptAddress,
    contracts.scheduler.policyId + SDK.SCHEDULER_ASSET_NAME,
  );
  const [activeOperatorInput] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    contracts.activeOperators.policyId +
      SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX +
      f.responderKey,
  );
  if (!schedulerRefInput || !activeOperatorInput?.datum)
    throw new Error("Not a commit fixture: no scheduler or operator node");
  const { correctionLockUtxo } = await liveAvailabilityAnchors(f);
  lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
  // One funding coin, never the collateral coin.
  const [funding] = (await lucid.wallet().getUtxos())
    .filter(
      (u) =>
        u.assets.lovelace !== AVAILABILITY_COLLATERAL_COIN_LOVELACE &&
        Object.keys(u.assets).length === 1 &&
        !u.datum &&
        !u.datumHash &&
        !u.scriptRef,
    )
    .sort((a, b) => (a.assets.lovelace > b.assets.lovelace ? -1 : 1));
  if (!funding) throw new Error("The responder holds no funding coin");
  const { tx, newHeaderHash } = await Effect.runPromise(
    SDK.buildCommitBlockHeaderTxProgram({
      lucid,
      contracts,
      latestBlock: {
        utxo: tail.utxo,
        datum: tail.view,
        assetName: tail.assetName,
      },
      updatedNodeDatum: {
        ...tail.view,
        next: { Key: { key: await hashHeader(header) } },
      },
      newHeader: header,
      validFrom,
      validTo,
      witness: {
        operatorKeyHash: f.responderKey,
        schedulerRefInput,
        hubOracleRefInput: f.hubOracleRefInput,
        correctionLockRefInput: {
          utxo: correctionLockUtxo,
          datum: "Idle",
          assetName: SDK.CORRECTION_LOCK_ASSET_NAME,
        },
        ...(queueIsEmpty
          ? {}
          : {
              confirmedStateRefInput: root.utxo,
              ...(head === undefined || head === tail
                ? {}
                : { headStateQueueNodeRefInput: head.utxo }),
            }),
        activeOperatorInput: {
          ...activeOperatorInput,
          datum: activeOperatorInput.datum,
        },
        activeOperatorsSpendingScript: contracts.activeOperators.spendingScript,
        activeOperatorsSpendingScriptRef: f.reference(
          "active-operators spending",
        ),
        stateQueueSpendingScriptRef: f.reference("state-queue spending"),
        stateQueueMintingScriptRef: f.reference("state-queue minting"),
        stateQueueCommitYieldScriptRef: f.reference(
          "state-queue commit withdrawal",
        ),
        operatorWalletView: { knownUtxos: [funding], consumedOutRefs: [] },
      },
      activeOperatorMaturityDurationMs: BigInt(
        AVAILABILITY_PROFILE.timing.block_maturity_ms,
      ),
    }),
  );
  const signed = await tx.sign.withWallet().complete();
  const commitTxHash = await signed.submit();
  f.emulator.awaitBlock(1);
  const queueUnit =
    contracts.stateQueue.policyId +
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
    newHeaderHash;
  const [queueUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    queueUnit,
  );
  if (!queueUtxo) throw new Error("The commit produced no queue node");
  const datum = await Effect.runPromise(
    SDK.getLinkedListNodeViewFromUTxO(queueUtxo),
  );
  const payload = Uint8Array.from(
    { length: payloadBytes },
    (_, i) => (i * 17 + 3) % 256,
  );
  const commitment = SDK.buildDaAvailabilityCommitment({
    deploymentIdentity: contracts.hubOracle.policyId,
    headerHash: newHeaderHash,
    payload,
    responseGeometry: SDK.availabilityResponseGeometry({
      chunkByteLength: Number(f.parameters.response_geometry.chunk_byte_length),
      trancheByteLength: Number(
        f.parameters.response_geometry.tranche_byte_length,
      ),
      maxTrancheCount: Number(f.parameters.response_geometry.max_tranche_count),
    }),
  });
  return {
    ...f,
    ...(await liveAvailabilityAnchors(f)),
    target: {
      headerHash: newHeaderHash,
      stateQueueNode: Data.castFrom(datum.data, SDK.StateQueueNode),
      stateQueueUtxo: {
        utxo: queueUtxo,
        datum,
        assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + newHeaderHash,
      },
    },
    payload,
    commitment,
    queueUnit,
    commitTxHash,
    headerEndTime: header.endTime,
  };
};
