import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  type CommitteeConfig,
  type CommitteeL1ClientConfig,
} from "../config.js";
import { scopedKupmiosCurrentPoint } from "../l1/availability-scoped-boundary.js";
import { committeeScopedTransactionStatus } from "../l1/availability-scoped-transaction-status.js";
import { committeeScopedOutRefs } from "../l1/availability-scoped-utxos.js";
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
import { committeeBoundReadContext } from "./committee-owned-read-transports.js";
import type { AvailabilityDiscoveryObservation } from "./consistent-discovery.js";
import { AvailabilityResponderAwaitingScanError } from "./responder.js";
import {
  committeeScopedFetch,
  committeeScopedOgmiosRpc,
  committeeScopedWebSocketFactory,
  type CommitteeSourceReadLimits,
} from "./scoped-transports.js";

export type AvailabilityResponderL1Readers = Readonly<{
  /** The aligned Kupmios tip: Kupo and Ogmios at one chain point. */
  currentPoint: (
    scope?: SDK.DaAvailabilityReadScope,
  ) => Promise<CanonicalChainPoint>;
  /** The committee node's chain-sync cursor and rollback generation. */
  currentCursor: (
    scope?: SDK.DaAvailabilityReadScope,
  ) => Promise<ChainSyncCursor>;
  /** Ogmios's tip block height (`queryNetwork/blockHeight`). */
  tipBlockNo: (scope?: SDK.DaAvailabilityReadScope) => Promise<number>;
  readTransactionStatus?: (
    txHash: string,
    scope?: SDK.DaAvailabilityReadScope,
  ) => ReturnType<LucidEvolution["transactionStatus"]>;
  readInputs?: (
    refs: readonly Readonly<{ txHash: string; outputIndex: number }>[],
    scope?: SDK.DaAvailabilityReadScope,
  ) => Promise<UTxO[]>;
  resolveInclusion: (
    output: UTxO,
    scope?: SDK.DaAvailabilityReadScope,
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
  readonly currentCursor: (
    scope?: SDK.DaAvailabilityReadScope,
  ) => Promise<ChainSyncCursor>;
  readonly sourceReadLimits?: CommitteeSourceReadLimits;
}): AvailabilityResponderL1Readers => {
  const { config, lucid, kupoUrl, ogmiosUrl } = input;
  const defaultInclusion = kupmiosChainPointResolver(
    lucid,
    kupoUrl,
    fetch,
    ogmiosUrl,
    config.network,
    config.finalityDepth,
    config.cardanoL1Source.networkMagic,
  );
  const readStatus = (txHash: string, scope?: SDK.DaAvailabilityReadScope) =>
    scope && input.sourceReadLimits
      ? committeeScopedTransactionStatus({
          kupoUrl,
          limits: input.sourceReadLimits,
        })(txHash, scope)
      : lucid.transactionStatus(txHash);
  return {
    currentPoint: (scope) =>
      scope && input.sourceReadLimits
        ? scopedKupmiosCurrentPoint({
            network: config.network,
            kupoUrl,
            ogmiosUrl,
            networkMagic: config.cardanoL1Source.networkMagic,
            scope,
            limits: input.sourceReadLimits,
          })
        : kupmiosCurrentChainPointResolver(
            config.network,
            kupoUrl,
            ogmiosUrl,
            config.cardanoL1Source.networkMagic,
          )(),
    currentCursor: (scope) =>
      scope
        ? scope.read(() => input.currentCursor(scope))
        : input.currentCursor(),
    tipBlockNo: (scope) =>
      fetchOgmiosTipBlockNo(
        ogmiosUrl,
        scope && input.sourceReadLimits
          ? committeeScopedFetch(scope, input.sourceReadLimits)
          : fetch,
      ),
    readTransactionStatus: readStatus,
    readInputs: (refs, scope) =>
      scope && input.sourceReadLimits
        ? committeeScopedOutRefs({ kupoUrl, limits: input.sourceReadLimits })(
            refs,
            scope,
          )
        : lucid.utxosByOutRef([...refs]),
    resolveInclusion: (output, scope) => {
      if (!scope || !input.sourceReadLimits) return defaultInclusion(output);
      const limits = input.sourceReadLimits;
      // This proxy owns only the status callback; it cannot mutate the shared wallet/cache.
      const scopedLucid = new Proxy(lucid, {
        get(target, property, receiver) {
          return property === "transactionStatus"
            ? (hash: string) => readStatus(hash, scope)
            : Reflect.get(target, property, receiver);
        },
      });
      return kupmiosChainPointResolver(
        scopedLucid,
        kupoUrl,
        committeeScopedFetch(scope, limits),
        ogmiosUrl,
        config.network,
        config.finalityDepth,
        config.cardanoL1Source.networkMagic,
        {
          openSession: (url) => committeeScopedOgmiosRpc(url, scope, limits),
          readAlignedTip: () =>
            scopedKupmiosCurrentPoint({
              network: config.network,
              kupoUrl,
              ogmiosUrl,
              networkMagic: config.cardanoL1Source.networkMagic,
              scope,
              limits,
            }),
        },
      )(output);
    },
    foreignSpend: availabilityForeignSpendReaders({
      kupoUrl,
      ogmiosUrl,
      sourceReadLimits: input.sourceReadLimits,
    }),
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
  // Still thrown, so the SDK aborts the step and releases its lease; the
  // responder tick reports it as a wait, not a failure.
  const awaitingScan = () => new AvailabilityResponderAwaitingScanError();
  const readDiscoveryObservation = async (
    scope?: SDK.DaAvailabilityReadScope,
  ): Promise<AvailabilityDiscoveryObservation> => {
    scope?.assertCurrent();
    const point = await readers.currentPoint(scope);
    const cursor = await readers.currentCursor(scope);
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
    const tipBefore = await readers.currentPoint(scope);
    const blockNo = await readers.tipBlockNo(scope);
    const tipAfter = await readers.currentPoint(scope);
    if (!atCursor(tipBefore) || !atCursor(tipAfter)) throw awaitingScan();
    if (
      scope !== undefined &&
      ((tipBefore.blockHeight !== undefined &&
        tipBefore.blockHeight !== blockNo) ||
        (tipAfter.blockHeight !== undefined &&
          tipAfter.blockHeight !== blockNo))
    )
      throw new Error(
        "Scoped boundary native height differs from its selected-chain tip",
      );
    scope?.assertCurrent();
    return {
      cursor,
      boundary: {
        pointId: `${point.slot}:${point.blockHash}`,
        slot: point.slot,
        blockHash: point.blockHash,
        blockNo,
      },
    };
  };
  const readBoundary = async (scope?: SDK.DaAvailabilityReadScope) =>
    (await readDiscoveryObservation(scope)).boundary;
  const readDiscoveryInputs = async (
    refs: readonly Readonly<{ txHash: string; outputIndex: number }>[],
    scope?: SDK.DaAvailabilityReadScope,
  ) =>
    readers.readInputs
      ? readers.readInputs(refs, scope)
      : input.lucid.utxosByOutRef([...refs]);
  const assertActuationCurrent = async (
    scope?: SDK.DaAvailabilityReadScope,
  ): Promise<void> => {
    if (scope) await scope.read(() => input.assertSourceHealthy());
    else await input.assertSourceHealthy();
    await readBoundary(scope);
  };
  const context: SDK.DaAvailabilityOperationContext = {
    ...input.context,
    assertActuationCurrent,
    readBoundary,
    observe: SDK.createDaAvailabilityOperationObserver({
      lucid: input.lucid,
      readBoundary,
      readTransactionStatus: readers.readTransactionStatus,
      readInputs: readers.readInputs,
      resolveInclusion: readers.resolveInclusion,
      resolveForeignSpend: (outRef, scope) =>
        SDK.resolveDaAvailabilityForeignSpend({
          ...readers.foreignSpend,
          outRef,
          readBoundary,
          scope,
        }),
    }),
  };
  const reconcile = async (
    scope?: SDK.DaAvailabilityReadScope,
  ): Promise<"ready" | "pending" | Readonly<{ held: string }>> => {
    // A new tick may adopt a recovered generation only for reconciliation;
    // no new action is selected until every durable intent is checked.
    expectedRollbackGeneration = (await readers.currentCursor(scope))
      .rollbackGeneration;
    scope?.assertCurrent();
    // The owning routine awaits durable reconciliation; only its evidence
    // reads inherit this absolute scope through the SDK context.
    const results = await SDK.reconcileDaAvailabilityOperations(
      scope === undefined
        ? context
        : {
            ...committeeBoundReadContext(context, scope),
            observationSignal: scope.signal,
            observationTimeoutMs: Math.max(
              1,
              Math.ceil(
                Math.min(
                  context.observationTimeoutMs ?? scope.remainingMs(),
                  scope.remainingMs(),
                ),
              ),
            ),
          },
    );
    scope?.assertCurrent();
    // A held or conflicting intent stops new signing with its reason; it is
    // read afresh on every pass, so evidence that clears it clears the hold.
    const held = results.find(
      (result) => result.status === "held" || result.status === "conflict",
    );
    if (held !== undefined)
      return {
        held: `${held.txHash}: ${held.detail ?? "A conflicting transaction spends this intent's inputs"}`,
      };
    return results.some(
      (result) =>
        result.status !== "confirmed" &&
        result.status !== "included" &&
        result.status !== "expired",
    )
      ? "pending"
      : "ready";
  };
  return {
    readBoundary,
    readDiscoveryObservation,
    readDiscoveryInputs,
    assertActuationCurrent,
    context,
    reconcile,
  };
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
  readonly sourceReadLimits?: CommitteeSourceReadLimits;
}): Omit<SDK.DaAvailabilityForeignSpendReaders, "readBoundary"> => {
  const fetchImpl = input.fetchImpl ?? fetch;
  const transportFetch: typeof fetch = (request, init) =>
    fetchImpl(
      typeof request === "string"
        ? request
        : request instanceof URL
          ? request.toString()
          : request.url,
      init,
    );
  const webSocketFactory =
    input.webSocketFactory ??
    ((url: string) =>
      new WebSocket(url) as unknown as StateQueueReplayWebSocket);
  return {
    fetchSpend: async (outRef, scope) => {
      try {
        const spend = await fetchSpend(
          input.kupoUrl,
          `${outRef.txHash}#${outRef.outputIndex.toString()}`,
          scope && input.sourceReadLimits
            ? committeeScopedFetch(
                scope,
                input.sourceReadLimits,
                transportFetch,
              )
            : fetchImpl,
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
    fetchAncestor: (slot, scope) =>
      fetchAncestor(
        input.kupoUrl,
        slot,
        scope && input.sourceReadLimits
          ? committeeScopedFetch(scope, input.sourceReadLimits, transportFetch)
          : fetchImpl,
      ),
    readTransaction: async ({ ancestor, point, txHash }, scope) => {
      try {
        const transaction = await readTransaction(
          input.ogmiosUrl,
          ancestor,
          { transactionHash: txHash, point },
          scope && input.sourceReadLimits
            ? committeeScopedWebSocketFactory(
                scope,
                input.sourceReadLimits,
                webSocketFactory,
              )
            : webSocketFactory,
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
