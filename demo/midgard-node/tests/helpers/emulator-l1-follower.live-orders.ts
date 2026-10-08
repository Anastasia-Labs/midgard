/**
 * The event lists' live Orders as the follower's derivation (`openOrder`)
 * admits them, read from the emulator's list and retention outputs: what
 * the follower stand-in (`emulator-l1-follower.ts`) projects and mirrors.
 */
import {
  EVENT_KINDS,
  type EventListConfig,
  eventProjectionConfigFromContracts,
  openOrder,
  type ProjectedEvent,
} from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

export type ListContracts = {
  readonly config: EventListConfig;
  readonly listAddress: string;
  readonly retentionAddress: string;
};

export const listContracts = (
  contracts: SDK.MidgardValidators,
  networkId: 0 | 1,
): readonly ListContracts[] => {
  const pair = SDK.requireEventHistoryContracts(contracts);
  const projection = eventProjectionConfigFromContracts(pair, networkId);
  return EVENT_KINDS.map((kind) => ({
    config: projection.lists.find((list) => list.kind === kind)!,
    listAddress: pair[kind].list.spendingScriptAddress,
    retentionAddress: pair[kind].retention.spendingScriptAddress,
  }));
};

export type OpenedOrder = {
  readonly kind: ProjectedEvent["kind"];
  readonly utxo: UTxO;
  readonly opened: Exclude<ReturnType<typeof openOrder>, "not_an_order">;
};

/** The live Orders the follower's derivation admits, per list. */
export const liveOrders = async (
  lucid: LucidEvolution,
  lists: readonly ListContracts[],
): Promise<OpenedOrder[]> => {
  const orders: OpenedOrder[] = [];
  for (const list of lists) {
    const retained = await lucid.utxosAt(list.retentionAddress);
    for (const utxo of await lucid.utxosAt(list.listAddress)) {
      const names = Object.keys(utxo.assets)
        .filter((unit) => unit.startsWith(list.config.policyId))
        .map((unit) => unit.slice(list.config.policyId.length));
      if (!names.some((name) => name.length === 64)) continue;
      let opened: ReturnType<typeof openOrder>;
      try {
        opened = openOrder(utxo, list.config, retained);
      } catch {
        continue; // the follower refuses it as malformed
      }
      if (opened !== "not_an_order")
        orders.push({ kind: list.config.kind, utxo, opened });
    }
  }
  return orders;
};
