import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { Data, datumToHash, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { hashHexWithBlake2b, OutputReference } from "../common.js";
import {
  EVENT_HISTORY_MAX_PROTECTION_TIME,
  EventHistoryNode,
  EventHistoryPayload,
} from "./history.js";
import { EventHistoryData } from "./history-data.js";

export type EventHistoryDeployment = {
  readonly policyId: string;
  readonly address: string;
  readonly retentionAddress: string;
  readonly inlineLimitBytes: bigint;
};

export type AuthenticatedHistoryNode = {
  readonly utxo: UTxO;
  readonly node: EventHistoryNode;
  readonly key: string | null;
};

/** Whether a refused history read can clear on a later read.
 * `snapshot-stale`: the provider's view is incomplete or straddles a change
 * (a partial list, retained data not yet or no longer visible).
 * `authenticated-state-invalid`: authenticated state breaks a rule no read
 * repairs. `invalid-request`: the caller asked for a malformed key. */
export type EventHistoryReadClassification =
  | "snapshot-stale"
  | "authenticated-state-invalid"
  | "invalid-request";

/** A classified refusal. Its `name` and `message` are those of the plain
 * `Error` it replaces, so rendered text is unchanged. */
export class EventHistoryReadError extends Error {
  readonly _tag = "EventHistoryReadError";
  readonly retryable: boolean;

  constructor(
    message: string,
    readonly classification: EventHistoryReadClassification,
  ) {
    super(message);
    this.retryable = classification === "snapshot-stale";
  }
}

const RETAINED_DATA_UNAVAILABLE =
  "Authenticated retained event data is unavailable on L1";

const stale = (message: string) =>
  new EventHistoryReadError(message, "snapshot-stale");

const invalid = (message: string) =>
  new EventHistoryReadError(message, "authenticated-state-invalid");

/** The classified refusal `error` is, or wraps one level down (the
 * `LucidError` the history event readers fail with). */
export const eventHistoryReadErrorOf = (
  error: unknown,
): EventHistoryReadError | undefined =>
  error instanceof EventHistoryReadError
    ? error
    : typeof error === "object" &&
        error !== null &&
        (error as { cause?: unknown }).cause instanceof EventHistoryReadError
      ? ((error as { cause?: unknown }).cause as EventHistoryReadError)
      : undefined;

export const eventHistoryKey = (id: OutputReference) =>
  hashHexWithBlake2b(Data.to(id, OutputReference), 32);

/** Ignore donations without our NFT; reject malformed authenticated state. */
export const authenticateHistoryNodes = (
  utxos: readonly UTxO[],
  deployment: EventHistoryDeployment,
): AuthenticatedHistoryNode[] => {
  const result: AuthenticatedHistoryNode[] = [];
  const seen = new Set<string | null>();
  for (const utxo of utxos) {
    const tokens = Object.entries(utxo.assets).filter(([unit]) =>
      unit.startsWith(deployment.policyId),
    );
    if (tokens.length === 0) continue;
    if (
      utxo.address !== deployment.address ||
      utxo.scriptRef != null ||
      utxo.datum == null
    ) {
      throw invalid("Invalid authenticated history output shape");
    }
    const node = Data.from(utxo.datum, EventHistoryNode);
    if (node.protected_until > EVENT_HISTORY_MAX_PROTECTION_TIME)
      throw invalid(
        "History protection timestamp exceeds its funded encoding width",
      );
    const key = node.position === "Root" ? null : node.position.Key[0];
    if (
      tokens.length !== 1 ||
      tokens[0]![0] !== deployment.policyId + (key ?? "") ||
      tokens[0]![1] !== 1n
    ) {
      throw invalid("History token does not authenticate its complete key");
    }
    if (
      (key === null) !== (node.payload === "RootContent") ||
      (key !== null && node.next !== null && key >= node.next)
    ) {
      throw invalid("Invalid history role or successor ordering");
    }
    if (seen.has(key)) throw invalid("Duplicate authenticated history key");
    seen.add(key);
    result.push({ utxo, node, key });
  }
  return result;
};

export type EventHistoryWitness =
  | { readonly kind: "Absent"; readonly anchor: AuthenticatedHistoryNode }
  | {
      readonly kind: "Present";
      readonly anchor: AuthenticatedHistoryNode;
      readonly payload: EventHistoryPayload;
      readonly payloadCbor: string;
      readonly retainedDataUtxo?: UTxO;
    };

/** Select a current authenticated presence or strict-gap/equal-filler witness. */
export const selectHistoryWitness = (
  nodes: readonly AuthenticatedHistoryNode[],
  key: string,
): AuthenticatedHistoryNode => {
  if (!/^[0-9a-f]{64}$/u.test(key))
    throw new EventHistoryReadError(
      "History key must be a full 32-byte lowercase hash",
      "invalid-request",
    );
  const candidates = nodes.filter(
    (entry) =>
      entry.key === key ||
      ((entry.key === null || entry.key < key) &&
        (entry.node.next === null || key < entry.node.next)),
  );
  if (candidates.length !== 1)
    throw stale(
      "History snapshot has no unique authenticated witness; refresh the L1 snapshot",
    );
  return candidates[0]!;
};

export type EventHistoryPresence = Extract<
  EventHistoryWitness,
  { kind: "Present" }
>;

/** Open only a node authenticated by its complete list NFT. Payload bytes and
 * operator archives alone never confer authority. */
const openHistoryOrder = (
  anchor: AuthenticatedHistoryNode,
  deployment: EventHistoryDeployment,
  retainedUtxos: readonly UTxO[],
): EventHistoryPresence => {
  if (
    anchor.key === null ||
    anchor.node.payload === "RootContent" ||
    !("Order" in anchor.node.payload)
  )
    throw invalid("History presence requires an authenticated Order");
  const facts = anchor.node.payload.Order.facts;
  if (datumToHash(Data.to(facts.event_id, OutputReference)) !== anchor.key) {
    throw invalid("History order identity differs from its authenticated key");
  }
  let loaded: { payload: EventHistoryPayload; retainedDataUtxo?: UTxO };
  let rawPayload: string;
  if ("Inline" in facts.location) {
    rawPayload = plutusConstrFieldCbor(anchor.utxo.datum!, [3, 0, 2, 0]);
    loaded = { payload: Data.from(rawPayload, EventHistoryPayload) };
  } else {
    const expectedHash = facts.location.External.storage_datum_hash;
    const candidates = retainedUtxos;
    const utxo = candidates.find(
      (candidate) =>
        candidate.address === deployment.retentionAddress &&
        candidate.scriptRef == null &&
        candidate.datum != null &&
        datumToHash(
          aikenSerialisedPlutusDataCborPreservingMapOrder(candidate.datum),
        ) === expectedHash,
    );
    if (utxo?.datum == null) throw stale(RETAINED_DATA_UNAVAILABLE);
    const retained = Data.from(utxo.datum, EventHistoryData);
    if (retained.event_key !== anchor.key)
      throw invalid(
        "Retained event data does not bind its authenticated order",
      );
    rawPayload = plutusConstrFieldCbor(utxo.datum, [1]);
    loaded = {
      payload: Data.from(rawPayload, EventHistoryPayload),
      retainedDataUtxo: utxo,
    };
  }
  const { payload } = loaded;
  // Definite maps match the validators' serialiseData commitments and bound.
  const payloadCbor =
    aikenSerialisedPlutusDataCborPreservingMapOrder(rawPayload);
  if (
    "Inline" in facts.location &&
    BigInt(payloadCbor.length / 2) > deployment.inlineLimitBytes
  ) {
    throw invalid("Authenticated inline payload exceeds the deployment bound");
  }
  const payloadId =
    "DepositPayload" in payload
      ? payload.DepositPayload.event.id
      : payload.WithdrawalPayload.event.id;
  if (
    payloadId.transactionId !== facts.event_id.transactionId ||
    payloadId.outputIndex !== facts.event_id.outputIndex
  ) {
    throw invalid("Payload identity differs from the authenticated order");
  }
  return { kind: "Present", anchor, payloadCbor, ...loaded };
};

/** Opens the one authenticated Order `orderUtxo` holds, for a caller that
 * found it by its key. An external payload is read from `retainedUtxos`.
 * Refuses exactly as a full read refuses that Order. */
export const readEventHistoryOrder = (
  orderUtxo: UTxO,
  retainedUtxos: readonly UTxO[],
  deployment: EventHistoryDeployment,
): EventHistoryPresence => {
  const nodes = authenticateHistoryNodes([orderUtxo], deployment);
  if (nodes.length !== 1)
    throw invalid("History presence requires an authenticated Order");
  return openHistoryOrder(nodes[0]!, deployment, retainedUtxos);
};

/** A full event scan must not silently accept a partial provider snapshot. */
const completeHistorySnapshot = (
  nodes: readonly AuthenticatedHistoryNode[],
) => {
  const ordered = [...nodes].sort((left, right) =>
    left.key === null
      ? -1
      : right.key === null
        ? 1
        : left.key.localeCompare(right.key),
  );
  if (ordered[0]?.key !== null)
    throw stale(
      "Authenticated history Root is unavailable; refresh the L1 snapshot",
    );
  for (let index = 0; index < ordered.length; index++) {
    if (ordered[index]!.node.next !== (ordered[index + 1]?.key ?? null))
      throw stale(
        "Authenticated history snapshot has missing or disconnected nodes; refresh L1",
      );
  }
  return ordered;
};

/** Convert a complete current list plus actual retention UTxOs. Roots and
 * fillers are structural nodes, and are never returned as user events. */
export const readEventHistoryOrders = (
  utxos: readonly UTxO[],
  retainedUtxos: readonly UTxO[],
  deployment: EventHistoryDeployment,
): EventHistoryPresence[] =>
  completeHistorySnapshot(authenticateHistoryNodes(utxos, deployment))
    .filter(
      (entry) =>
        entry.node.payload !== "RootContent" && "Order" in entry.node.payload,
    )
    .map((anchor) => openHistoryOrder(anchor, deployment, retainedUtxos));

type HistoryProvider = { utxosAt(address: string): Promise<UTxO[]> };

/** List reads one history read takes at most while the list keeps moving
 * under its retention read. */
const HISTORY_SNAPSHOT_LIST_READ_LIMIT = 3;

const sameOutRefs = (left: readonly UTxO[], right: readonly UTxO[]) => {
  const keys = (utxos: readonly UTxO[]) =>
    utxos.map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`).sort();
  const [a, b] = [keys(left), keys(right)];
  return a.length === b.length && a.every((key, index) => key === b[index]);
};

/**
 * The list and retention reads are separate provider reads, so a retirement
 * and reclaim landing between them leave a listed order whose retained data
 * is gone. On that refusal the list is read again: a list that moved is
 * returned to be opened afresh, while an unchanged list, any other refusal or
 * a list that keeps moving rethrows `refusal`.
 */
const relistAfterRetentionMiss = async (
  provider: HistoryProvider,
  deployment: EventHistoryDeployment,
  listUtxos: readonly UTxO[],
  listReads: number,
  refusal: unknown,
): Promise<UTxO[]> => {
  if (
    !(refusal instanceof EventHistoryReadError) ||
    refusal.message !== RETAINED_DATA_UNAVAILABLE ||
    listReads >= HISTORY_SNAPSHOT_LIST_READ_LIMIT
  )
    throw refusal;
  const relisted = await provider.utxosAt(deployment.address);
  if (sameOutRefs(listUtxos, relisted)) throw refusal;
  return relisted;
};

export const fetchEventHistoryOrders = async (
  provider: HistoryProvider,
  deployment: EventHistoryDeployment,
): Promise<EventHistoryPresence[]> => {
  let listUtxos = await provider.utxosAt(deployment.address);
  for (let listReads = 1; ; listReads++) {
    const nodes = completeHistorySnapshot(
      authenticateHistoryNodes(listUtxos, deployment),
    );
    const orders = nodes.filter(
      (entry) =>
        entry.node.payload !== "RootContent" && "Order" in entry.node.payload,
    );
    const external = orders.some(
      (entry) =>
        entry.node.payload !== "RootContent" &&
        "Order" in entry.node.payload &&
        "External" in entry.node.payload.Order.facts.location,
    );
    const retained = external
      ? await provider.utxosAt(deployment.retentionAddress)
      : [];
    try {
      return orders.map((anchor) =>
        openHistoryOrder(anchor, deployment, retained),
      );
    } catch (refusal) {
      listUtxos = await relistAfterRetentionMiss(
        provider,
        deployment,
        listUtxos,
        listReads,
        refusal,
      );
    }
  }
};

/** Canonical chain UTxOs provide authority. Returned bytes are only preimages. */
export const fetchEventHistoryWitness = async (
  provider: HistoryProvider,
  deployment: EventHistoryDeployment,
  id: OutputReference,
): Promise<EventHistoryWitness> => {
  const key = await Effect.runPromise(eventHistoryKey(id));
  let listUtxos = await provider.utxosAt(deployment.address);
  for (let listReads = 1; ; listReads++) {
    const nodes = authenticateHistoryNodes(listUtxos, deployment);
    const anchor = selectHistoryWitness(nodes, key);
    if (
      anchor.key !== key ||
      anchor.node.payload === "RootContent" ||
      "Filler" in anchor.node.payload
    )
      return { kind: "Absent", anchor };
    const retained =
      "External" in anchor.node.payload.Order.facts.location
        ? await provider.utxosAt(deployment.retentionAddress)
        : [];
    try {
      return openHistoryOrder(anchor, deployment, retained);
    } catch (refusal) {
      listUtxos = await relistAfterRetentionMiss(
        provider,
        deployment,
        listUtxos,
        listReads,
        refusal,
      );
    }
  }
};
