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
      throw new Error("Invalid authenticated history output shape");
    }
    const node = Data.from(utxo.datum, EventHistoryNode);
    if (node.protected_until > EVENT_HISTORY_MAX_PROTECTION_TIME)
      throw new Error(
        "History protection timestamp exceeds its funded encoding width",
      );
    const key = node.position === "Root" ? null : node.position.Key[0];
    if (
      tokens.length !== 1 ||
      tokens[0]![0] !== deployment.policyId + (key ?? "") ||
      tokens[0]![1] !== 1n
    ) {
      throw new Error("History token does not authenticate its complete key");
    }
    if (
      (key === null) !== (node.payload === "RootContent") ||
      (key !== null && node.next !== null && key >= node.next)
    ) {
      throw new Error("Invalid history role or successor ordering");
    }
    if (seen.has(key)) throw new Error("Duplicate authenticated history key");
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
    throw new Error("History key must be a full 32-byte lowercase hash");
  const candidates = nodes.filter(
    (entry) =>
      entry.key === key ||
      ((entry.key === null || entry.key < key) &&
        (entry.node.next === null || key < entry.node.next)),
  );
  if (candidates.length !== 1)
    throw new Error(
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
    throw new Error("History presence requires an authenticated Order");
  const facts = anchor.node.payload.Order.facts;
  if (datumToHash(Data.to(facts.event_id, OutputReference)) !== anchor.key) {
    throw new Error(
      "History order identity differs from its authenticated key",
    );
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
    if (utxo?.datum == null)
      throw new Error("Authenticated retained event data is unavailable on L1");
    const retained = Data.from(utxo.datum, EventHistoryData);
    if (retained.event_key !== anchor.key)
      throw new Error(
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
    throw new Error(
      "Authenticated inline payload exceeds the deployment bound",
    );
  }
  const payloadId =
    "DepositPayload" in payload
      ? payload.DepositPayload.event.id
      : payload.WithdrawalPayload.event.id;
  if (
    payloadId.transactionId !== facts.event_id.transactionId ||
    payloadId.outputIndex !== facts.event_id.outputIndex
  ) {
    throw new Error("Payload identity differs from the authenticated order");
  }
  return { kind: "Present", anchor, payloadCbor, ...loaded };
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
    throw new Error(
      "Authenticated history Root is unavailable; refresh the L1 snapshot",
    );
  for (let index = 0; index < ordered.length; index++) {
    if (ordered[index]!.node.next !== (ordered[index + 1]?.key ?? null))
      throw new Error(
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

export const fetchEventHistoryOrders = async (
  provider: { utxosAt(address: string): Promise<UTxO[]> },
  deployment: EventHistoryDeployment,
): Promise<EventHistoryPresence[]> => {
  const nodes = completeHistorySnapshot(
    authenticateHistoryNodes(
      await provider.utxosAt(deployment.address),
      deployment,
    ),
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
  return orders.map((anchor) => openHistoryOrder(anchor, deployment, retained));
};

/** Canonical chain UTxOs provide authority. Returned bytes are only preimages. */
export const fetchEventHistoryWitness = async (
  provider: { utxosAt(address: string): Promise<UTxO[]> },
  deployment: EventHistoryDeployment,
  id: OutputReference,
): Promise<EventHistoryWitness> => {
  const key = await Effect.runPromise(eventHistoryKey(id));
  const nodes = authenticateHistoryNodes(
    await provider.utxosAt(deployment.address),
    deployment,
  );
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
  return openHistoryOrder(anchor, deployment, retained);
};
