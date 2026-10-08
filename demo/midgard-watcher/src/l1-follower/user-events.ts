import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import type { FactStore } from "@al-ft/midgard-l1-follower";
import {
  eventAdmittedThrough,
  type EventCutoff,
  eventKeyOfId,
} from "@al-ft/midgard-l1-follower/events";
import {
  OutputReference,
  TxOrderDatum,
  TxOrderEvent,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

import type { WatcherStateQueueHeaderObservation } from "../indexers/authenticated-state-queue-observation.js";
import {
  admitWatcherUserEventAuthority,
  type WatcherUserEvent,
  type WatcherUserEventAdmission,
  type WatcherUserEventAuthority,
  type WatcherUserEventAuthorityRead,
  type WatcherUserEventHeaderCutoff,
  type WatcherUserEventKind,
  type WatcherUserEventNetwork,
} from "../verification/user-event.js";
import type { WatcherProofRetention } from "./proof-retention.js";
import type { FollowerRawReads } from "./raw-reads.types.js";
import { rawPointOf } from "./raw-reads.types.js";

/**
 * The watcher's user events read from the L1 follower's facts (W3): the
 * deposit and withdrawal events from the shared event projection
 * (`@al-ft/midgard-l1-follower/events`), a forced order from the tx-order
 * unit's history and the stored mint transaction. Every read is pure over
 * the facts at the state-queue header's cutoff: the block that carries the
 * header's commit, through the commit's own transaction index.
 *
 * A read is current until a rewind removes the cutoff block, or the store
 * resets; a rewind above the cutoff leaves it standing, since nothing the
 * read used lies above the cutoff. A read the facts cannot answer (the
 * event is not admitted by the cutoff, or its rows were pruned) throws: the
 * capture that asked fails, and the decision is retried from the facts.
 */

export type WatcherUserEventIdentity = Readonly<{
  deploymentManifestId: string;
  blueprintHash: string;
  network: WatcherUserEventNetwork;
}>;

export type WatcherUserEventScripts = Readonly<{
  depositPolicyId: string;
  withdrawalPolicyId: string;
  forcedOrderPolicyId: string;
  /** The tx-order spending address, as raw address bytes (hex). */
  forcedOrderAddressHex: string;
}>;

export type WatcherUserEventRequest = Readonly<{
  kind: WatcherUserEventKind;
  /** The event id, `OutputReference` CBOR (hex). */
  eventId: string;
  throughHeader: WatcherStateQueueHeaderObservation;
}>;

export type WatcherUserEvents = Readonly<{
  deploymentManifestId: string;
  blueprintHash: string;
  /** The event `eventId` as of `throughHeader`'s cutoff, as a fenced capability. */
  eventAuthority(
    request: WatcherUserEventRequest,
  ): Promise<WatcherUserEventAuthority>;
  close(): void;
}>;

/** The event is not in the facts at the cutoff, or its rows are gone. */
export class WatcherUserEventUnavailable extends Error {
  constructor(message: string) {
    super(message);
    this.name = "WatcherUserEventUnavailable";
  }
}

const unavailable = (message: string): never => {
  throw new WatcherUserEventUnavailable(message);
};

/** Rewinds heard, kept for fencing; older reads than the window are stale. */
const REWIND_WINDOW = 1024;

type HeardRewind = Readonly<{ seq: number; toSlot: number; reset: boolean }>;

const cutoffOf = (
  header: WatcherStateQueueHeaderObservation,
  transactionIndex: number,
): WatcherUserEventHeaderCutoff =>
  Object.freeze({
    headerHash: header.headerHash,
    headerCborHex: header.headerCborHex,
    queueOutRef: header.queueOutRef,
    observedTransactionHash: header.observedTransactionHash,
    observedBlockHash: header.observedBlockHash,
    observedSlot: header.observedSlot,
    observedBlockNo: header.observedBlockNo,
    transactionIndex: transactionIndex.toString(),
  });

const nonceOutRefOf = (eventId: string): string => {
  const id = Data.from(eventId, OutputReference);
  return `${id.transactionId}#${id.outputIndex.toString()}`;
};

const unitAmount = (
  assets: ReadonlyMap<string, ReadonlyMap<string, bigint>>,
  policyId: string,
  assetName: string,
): bigint => assets.get(policyId)?.get(assetName) ?? 0n;

export const createWatcherFollowerUserEvents = (
  input: Readonly<{
    store: Pick<
      FactStore,
      "onGeneration" | "txByHash" | "transaction" | "dialect"
    >;
    rawReads: Pick<FollowerRawReads, "unitHistoryAtPoint">;
    proofRetention: Pick<WatcherProofRetention, "holdUnits">;
    identity: WatcherUserEventIdentity;
    scripts: WatcherUserEventScripts;
  }>,
): WatcherUserEvents => {
  const { store, rawReads, proofRetention, identity, scripts } = input;
  let seq = 0;
  let heard: HeardRewind[] = [];
  /** Closed: rewinds are no longer heard, so no read can stand. */
  let closed = false;
  const unsubscribe = store.onGeneration(({ rewound }) => {
    seq += 1;
    heard.push({ seq, toSlot: rewound.to.slot, reset: rewound.reset === true });
    if (heard.length > REWIND_WINDOW) heard = heard.slice(-REWIND_WINDOW);
  });
  /** Whether no rewind after `since` removed the slot `cutoffSlot`. */
  const standing = (since: number, cutoffSlot: number): boolean => {
    if (closed) return false;
    if (seq === since) return true;
    if (heard.length === 0 || heard[0]!.seq > since + 1) return false;
    return heard.every(
      (entry) =>
        entry.seq <= since || (!entry.reset && entry.toSlot >= cutoffSlot),
    );
  };

  /** The header's cutoff: its commit transaction's block and index in it. */
  const cutoffAt = async (
    header: WatcherStateQueueHeaderObservation,
  ): Promise<EventCutoff> => {
    const commit = await store.txByHash(
      Buffer.from(header.observedTransactionHash, "hex"),
    );
    if (commit === null || commit.blockSlot !== Number(header.observedSlot))
      return unavailable(
        `header ${header.headerHash}'s commit transaction is not stored at slot ${header.observedSlot}`,
      );
    return {
      point: {
        slot: commit.blockSlot,
        hash: Buffer.from(header.observedBlockHash, "hex"),
      },
      txIndex: commit.blockTxIndex,
    };
  };

  const listEvent = async (
    kind: "deposit" | "withdrawal",
    eventId: string,
    cutoff: EventCutoff,
  ): Promise<WatcherUserEvent> => {
    const read = await eventAdmittedThrough(
      store as FactStore,
      kind,
      eventId,
      cutoff,
    );
    if (read.kind !== "ok")
      return unavailable(
        `${kind} ${eventId} cannot be read at the cutoff: ${read.kind}${"detail" in read ? `: ${read.detail}` : ""}`,
      );
    const row = read.value;
    if (row === null)
      return unavailable(
        `${kind} ${eventId} is not admitted by the cutoff, or its rows were pruned`,
      );
    const eventCborHex = aikenSerialisedPlutusDataCborPreservingMapOrder(
      plutusConstrFieldCbor(row.payloadCbor, [0]),
    );
    const payloadId = plutusConstrFieldCbor(eventCborHex, [0]);
    if (
      Data.to(Data.from(payloadId, OutputReference), OutputReference) !==
      eventId
    )
      throw new Error(`${kind} ${eventId}'s payload names another event id`);
    const admission: WatcherUserEventAdmission = Object.freeze({
      blockHash: row.admission.blockHash,
      slot: row.admission.slot.toString(),
      blockNo: row.admission.height.toString(),
      transactionHash: row.admission.txHash,
      transactionIndex: row.admission.txIndex.toString(),
      outputIndex: row.admission.outRef.index.toString(),
    });
    return Object.freeze({
      kind,
      eventId,
      nonceOutRef: nonceOutRefOf(eventId),
      policyId:
        kind === "deposit"
          ? scripts.depositPolicyId
          : scripts.withdrawalPolicyId,
      assetNameHex: row.key,
      inclusionTime: row.inclusionTime.toString(),
      eventCborHex,
      originalAssetsCborHex: kind === "deposit" ? row.originalAssetsCbor : null,
      admission,
    });
  };

  /**
   * A forced order: the transaction that minted its tx-order token (the
   * on-chain tx-order policy admitted it), and the order output it created.
   */
  const forcedOrder = async (
    eventId: string,
    header: WatcherStateQueueHeaderObservation,
    cutoff: EventCutoff,
  ): Promise<WatcherUserEvent> => {
    const assetNameHex = eventKeyOfId(Buffer.from(eventId, "hex")).toString(
      "hex",
    );
    const unit = scripts.forcedOrderPolicyId + assetNameHex;
    const hold = await proofRetention.holdUnits(header.headerHash, [unit]);
    if (hold.kind === "already_pruned")
      return unavailable(`forced order ${eventId}'s history was pruned`);
    const history = await rawReads.unitHistoryAtPoint(
      unit,
      rawPointOf({
        slot: cutoff.point.slot,
        hash: cutoff.point.hash,
        height: Number(header.observedBlockNo),
      }),
    );
    if (history.kind !== "ok")
      return unavailable(
        `forced order ${eventId}'s history cannot be read: ${history.reason}: ${history.detail}`,
      );
    const minted: WatcherUserEvent[] = [];
    for (const entry of history.value.transactions) {
      const stored = await store.txByHash(Buffer.from(entry.txHash, "hex"));
      if (stored === null)
        return unavailable(
          `forced order transaction ${entry.txHash} is not stored`,
        );
      if (
        !stored.isValid ||
        unitAmount(stored.mint, scripts.forcedOrderPolicyId, assetNameHex) !==
          1n
      )
        continue;
      if (
        stored.blockSlot === cutoff.point.slot &&
        stored.blockTxIndex > cutoff.txIndex
      )
        continue;
      const outputs = CML.TransactionBody.from_cbor_bytes(
        stored.bodyCbor,
      ).outputs();
      for (let index = 0; index < outputs.len(); index += 1) {
        const output = outputs.get(index);
        const quantity =
          output
            .amount()
            .multi_asset()
            .get_assets(CML.ScriptHash.from_hex(scripts.forcedOrderPolicyId))
            ?.get(CML.AssetName.from_hex(assetNameHex)) ?? 0n;
        if (
          output.address().to_hex() !== scripts.forcedOrderAddressHex ||
          quantity !== 1n
        )
          continue;
        const datumCbor = output.datum()?.as_datum()?.to_cbor_hex();
        if (datumCbor === undefined)
          throw new Error(
            `forced order ${eventId}'s output has no inline datum`,
          );
        const datum = Data.from(datumCbor, TxOrderDatum);
        if (Data.to(datum.event.id, OutputReference) !== eventId)
          throw new Error(`forced order ${eventId}'s datum names another id`);
        minted.push(
          Object.freeze({
            kind: "forced_order",
            eventId,
            nonceOutRef: nonceOutRefOf(eventId),
            policyId: scripts.forcedOrderPolicyId,
            assetNameHex,
            inclusionTime: datum.inclusion_time.toString(),
            eventCborHex: Data.to(datum.event, TxOrderEvent),
            originalAssetsCborHex: null,
            admission: Object.freeze({
              blockHash: entry.inclusionPoint.blockHash,
              slot: entry.inclusionPoint.slot,
              blockNo: entry.inclusionPoint.blockNo,
              transactionHash: entry.txHash,
              transactionIndex: stored.blockTxIndex.toString(),
              outputIndex: index.toString(),
            }),
          }),
        );
      }
    }
    if (minted.length === 0)
      return unavailable(
        `forced order ${eventId} is not admitted by the cutoff`,
      );
    if (minted.length !== 1)
      throw new Error(`forced order ${eventId} is admitted more than once`);
    return minted[0]!;
  };

  const readEvent = async (
    request: WatcherUserEventRequest,
  ): Promise<WatcherUserEventAuthorityRead> => {
    const cutoff = await cutoffAt(request.throughHeader);
    const event =
      request.kind === "forced_order"
        ? await forcedOrder(request.eventId, request.throughHeader, cutoff)
        : await listEvent(request.kind, request.eventId, cutoff);
    return Object.freeze({
      deploymentManifestId: identity.deploymentManifestId,
      blueprintHash: identity.blueprintHash,
      network: identity.network,
      event,
      throughHeader: cutoffOf(request.throughHeader, cutoff.txIndex),
    });
  };

  return Object.freeze({
    deploymentManifestId: identity.deploymentManifestId,
    blueprintHash: identity.blueprintHash,
    eventAuthority: async (request) => {
      const since = seq;
      const first = await readEvent(request);
      const cutoffSlot = Number(first.throughHeader.observedSlot);
      if (!standing(since, cutoffSlot))
        return unavailable("an L1 rewind removed the cutoff during the read");
      return admitWatcherUserEventAuthority({
        current: () => standing(since, cutoffSlot),
        read: async () => {
          const again = await readEvent(request);
          if (
            again.event.eventCborHex !== first.event.eventCborHex ||
            again.event.originalAssetsCborHex !==
              first.event.originalAssetsCborHex ||
            again.throughHeader.transactionIndex !==
              first.throughHeader.transactionIndex
          )
            throw new Error("the event read differs from its first read");
          return first;
        },
      });
    },
    close: () => {
      closed = true;
      unsubscribe();
    },
  });
};
