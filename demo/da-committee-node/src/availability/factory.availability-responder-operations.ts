import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  type CommitteeConfig,
  type CommitteeL1ClientConfig,
} from "../config.js";
import {
  type CanonicalChainPoint,
  type ChainSyncCursor,
  kupmiosChainPointResolver,
  kupmiosCurrentChainPointResolver,
} from "../l1/provider.js";
import {
  L1SourceIntegrityError,
  StateQueueHistoryNotExtendingAnchorError,
} from "../l1/source-integrity.js";
import {
  fetchAncestor,
  fetchOgmiosTipBlockNo,
  fetchSpend,
  readTransaction,
  type StateQueueReplayFetch,
  type StateQueueReplayWebSocket,
  type StateQueueReplayWebSocketFactory,
} from "../l1/state-queue-replay-provider.js";

export type AvailabilityResponderL1Readers = Readonly<{
  /** The aligned Kupmios tip: Kupo and Ogmios at one chain point. */
  currentPoint: () => Promise<CanonicalChainPoint>;
  /** The committee node's chain-sync cursor and rollback generation. */
  currentCursor: () => Promise<ChainSyncCursor>;
  /** Ogmios's tip block height (`queryNetwork/blockHeight`). */
  tipBlockNo: () => Promise<number>;
  resolveInclusion: (
    output: UTxO,
  ) => Promise<Readonly<{ slot?: number; blockHash?: string; depth?: number }>>;
  foreignSpend: Omit<SDK.DaAvailabilityForeignSpendReaders, "readBoundary">;
}>;

/**
 * The responder's live L1 readers over its kupmios query provider. The
 * configured network magic reaches both Ogmios identity checks (the aligned
 * tip and the confirmation-depth query), which a Custom network needs.
 */
export const availabilityResponderL1ReadersFromConfig = (input: {
  readonly config: Pick<
    CommitteeL1ClientConfig,
    "network" | "finalityDepth" | "cardanoL1Source"
  >;
  readonly lucid: LucidEvolution;
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly currentCursor: () => Promise<ChainSyncCursor>;
}): AvailabilityResponderL1Readers => {
  const { config, lucid, kupoUrl, ogmiosUrl } = input;
  return {
    currentPoint: kupmiosCurrentChainPointResolver(
      config.network,
      kupoUrl,
      ogmiosUrl,
      config.cardanoL1Source.networkMagic,
    ),
    currentCursor: input.currentCursor,
    tipBlockNo: () => fetchOgmiosTipBlockNo(ogmiosUrl, fetch),
    resolveInclusion: kupmiosChainPointResolver(
      lucid,
      kupoUrl,
      fetch,
      ogmiosUrl,
      config.network,
      config.finalityDepth,
      config.cardanoL1Source.networkMagic,
    ),
    foreignSpend: availabilityForeignSpendReaders({ kupoUrl, ogmiosUrl }),
  };
};

/**
 * The responder's canonical boundary, operation context and reconcile step,
 * built on injected L1 readers.
 *
 * The boundary is the point where the committee's chain-sync cursor and the
 * aligned Kupmios tip agree, with that point's block height read between two
 * tip reads that must both still be the cursor's point. The same boundary
 * brackets inclusion reads and the verified foreign-spend check, so a
 * responder whose Publish, Settle or Close lost its race to another
 * transaction (every one of them spends only protocol UTxOs) expires that
 * intent once the rival spend is final, instead of waiting on it forever.
 */
export const availabilityResponderOperations = (input: {
  readonly lucid: LucidEvolution;
  readonly readers: AvailabilityResponderL1Readers;
  readonly assertSourceHealthy: () => Promise<void>;
  readonly context: Omit<
    SDK.DaAvailabilityOperationContext,
    "observe" | "assertActuationCurrent"
  >;
}) => {
  const { readers } = input;
  let expectedRollbackGeneration: number | undefined;
  const awaitingScan = () =>
    new Error(
      "Availability responder awaits the next canonical committee node L1 scan before acting",
    );
  const readBoundary = async (): Promise<
    SDK.DaAvailabilityCanonicalBoundary &
      Readonly<{ blockHash: string; blockNo: number }>
  > => {
    const point = await readers.currentPoint();
    const cursor = await readers.currentCursor();
    if (
      expectedRollbackGeneration !== undefined &&
      cursor.rollbackGeneration !== expectedRollbackGeneration
    ) {
      throw new Error(
        "Availability responder canonical generation changed; durable operations must reconcile on the next scan",
      );
    }
    const atCursor = (tip: CanonicalChainPoint) =>
      cursor.point.slot === tip.slot &&
      cursor.point.blockHash === tip.blockHash &&
      cursor.point.network === tip.network;
    if (!atCursor(point)) throw awaitingScan();
    // The tip's height, bound to the cursor's point by a tip read on each
    // side; the tip's own optional height is never read.
    const tipBefore = await readers.currentPoint();
    const blockNo = await readers.tipBlockNo();
    const tipAfter = await readers.currentPoint();
    if (!atCursor(tipBefore) || !atCursor(tipAfter)) throw awaitingScan();
    return {
      pointId: `${point.slot}:${point.blockHash}`,
      slot: point.slot,
      blockHash: point.blockHash,
      blockNo,
    };
  };
  const assertActuationCurrent = async (): Promise<void> => {
    await input.assertSourceHealthy();
    await readBoundary();
  };
  const context: SDK.DaAvailabilityOperationContext = {
    ...input.context,
    assertActuationCurrent,
    observe: SDK.createDaAvailabilityOperationObserver({
      lucid: input.lucid,
      readBoundary,
      resolveInclusion: readers.resolveInclusion,
      resolveForeignSpend: (outRef) =>
        SDK.resolveDaAvailabilityForeignSpend({
          ...readers.foreignSpend,
          outRef,
          readBoundary,
        }),
    }),
  };
  const reconcile = async (): Promise<"ready" | "pending"> => {
    // A new tick may adopt a recovered generation only for reconciliation;
    // no new action is selected until every durable intent is checked.
    expectedRollbackGeneration = (await readers.currentCursor())
      .rollbackGeneration;
    const results = await SDK.reconcileDaAvailabilityOperations(context);
    if (results.some((result) => result.status === "conflict"))
      throw new Error(
        "Availability responder journal contains a conflicting transaction; authenticated recovery is required",
      );
    return results.some(
      (result) =>
        result.status !== "confirmed" &&
        result.status !== "included" &&
        result.status !== "expired",
    )
      ? "pending"
      : "ready";
  };
  return { readBoundary, assertActuationCurrent, context, reconcile };
};

/**
 * The committee's readers for the SDK's verified foreign-spend check: the
 * same Kupo and Ogmios reads its state-queue replay trusts. A Kupo match set
 * with no entry for the ref is no evidence, and a transaction missing from
 * the block Kupo named fails verification; chain-moved and rollback errors
 * propagate so the next tick retries.
 */
export const availabilityForeignSpendReaders = (input: {
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly fetchImpl?: StateQueueReplayFetch;
  readonly webSocketFactory?: StateQueueReplayWebSocketFactory;
}): Omit<SDK.DaAvailabilityForeignSpendReaders, "readBoundary"> => {
  const fetchImpl = input.fetchImpl ?? fetch;
  const webSocketFactory =
    input.webSocketFactory ??
    ((url: string) =>
      new WebSocket(url) as unknown as StateQueueReplayWebSocket);
  return {
    fetchSpend: async (outRef) => {
      try {
        const spend = await fetchSpend(
          input.kupoUrl,
          `${outRef.txHash}#${outRef.outputIndex.toString()}`,
          fetchImpl,
        );
        return spend === null
          ? undefined
          : { transactionId: spend.transactionHash, point: spend.point };
      } catch (error) {
        if (error instanceof StateQueueHistoryNotExtendingAnchorError)
          return undefined;
        throw error;
      }
    },
    fetchAncestor: (slot) => fetchAncestor(input.kupoUrl, slot, fetchImpl),
    readTransaction: async ({ ancestor, point, txHash }) => {
      try {
        const transaction = await readTransaction(
          input.ogmiosUrl,
          ancestor,
          { transactionHash: txHash, point },
          webSocketFactory,
        );
        return {
          txHash: transaction.transactionHash,
          point: {
            slot: transaction.slot,
            blockHash: transaction.blockHash,
            blockNo: transaction.blockNo,
          },
          ...(transaction.cbor === undefined ? {} : { cbor: transaction.cbor }),
        };
      } catch (error) {
        if (error instanceof L1SourceIntegrityError) return undefined;
        throw error;
      }
    },
  };
};

/**
 * The deployment's `ParametersV1` from the manifest-pinned configuration: the
 * value compiled into the availability and DA attestation validators, shared
 * by the responder and by the attestation apply's pool check.
 */
export const availabilityParametersFromConfig = (
  config: Pick<CommitteeConfig, "availabilityChallenge">,
): SDK.DaAvailabilityParameters => {
  const p = config.availabilityChallenge;
  return SDK.daAvailabilityParameters({
    responseGeometry: SDK.availabilityResponseGeometry(p.responseGeometry),
    daBondLovelace: BigInt(p.daBondLovelace),
    daSlashPenaltyLovelace: BigInt(p.daSlashPenaltyLovelace),
    daBondMinTopUpLovelace: BigInt(p.daBondMinTopUpLovelace),
    daBondPoolFloorLovelace: BigInt(p.daBondPoolFloorLovelace),
    challengeRecordLovelace: BigInt(p.challengeRecordLovelace),
    challengerBondLovelace: BigInt(p.challengerBondLovelace),
    maxOpenFeeLovelace: BigInt(p.maxOpenFeeLovelace),
    maxPublicationFeeLovelace: BigInt(p.maxPublicationFeeLovelace),
    maxSettlementFeeLovelace: BigInt(p.maxSettlementFeeLovelace),
    maxCloseFeeLovelace: BigInt(p.maxCloseFeeLovelace),
    maxTimeoutFeeLovelace: BigInt(p.maxTimeoutFeeLovelace),
  });
};

export const availabilityResponderCollateral = async (
  lucid: Pick<LucidEvolution, "config" | "utxosAt"> & {
    readonly wallet: () => { readonly address: () => Promise<string> };
  },
  fee: bigint,
): Promise<readonly UTxO[]> => {
  const protocol = lucid.config().protocolParameters;
  if (protocol === undefined)
    throw new Error("Availability responder requires live protocol parameters");
  const required = (fee * BigInt(protocol.collateralPercentage) + 99n) / 100n;
  const address = await lucid.wallet().address();
  const candidates = (await lucid.utxosAt(address))
    .filter(
      (utxo) =>
        !utxo.datum &&
        !utxo.datumHash &&
        !utxo.scriptRef &&
        Object.keys(utxo.assets).length === 1 &&
        (utxo.assets.lovelace ?? 0n) >= required,
    )
    .sort((a, b) =>
      `${a.txHash}#${a.outputIndex}`.localeCompare(
        `${b.txHash}#${b.outputIndex}`,
      ),
    );
  if (candidates[0] === undefined)
    throw new Error(
      `Availability responder wallet lacks separate plain-ADA collateral of at least ${required} lovelace`,
    );
  return [candidates[0]];
};

/** A challenge record discovery left out, and why. */
export type AvailabilityResponderSkippedRecord = Readonly<{
  outRef: string;
  /** True when its state-queue node is gone or challenged by another record. */
  stranded: boolean;
  reason: string;
}>;

/** A DACH asset name is 32 bytes: the 4-byte prefix and a 28-byte identity. */
export const DACH_SUFFIX_HEX_LENGTH = 56;
