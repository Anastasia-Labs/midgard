/**
 * The node's operator set (NC14): the active and registered operator lists,
 * the scheduler, the hub oracle, this operator's own retired node and its own
 * active-node history, kept in memory from the follower facts.
 *
 * - The first read, and the first after a rewind (a generation change) or a
 *   prune past the last read, loads the live outputs under the active,
 *   registered, scheduler and hub-oracle policies and the own retired node
 *   by its exact asset.
 * - Every later read takes only the rows a block after the last read
 *   created or spent (`changedUtxosIn`). The retired list is never read: the
 *   only retired row the set holds is the own one, by its exact asset, and
 *   it only grows by retirements.
 * - Within one generation a row the facts hold never changes except by its
 *   spend, and pruning removes only spent rows at or below the retained
 *   window, which the set drops too: applying the changed rows to the last
 *   read gives the read a fresh load would give, so the set equals a replay
 *   whenever the facts do.
 *
 * Every read runs in one follower read transaction (a snapshot), at the
 * view that transaction sees.
 */
import {
  type Assets,
  changedUtxosIn,
  type Dialect,
  liveUtxosIn,
  type SqlTx,
  type StoredOutput,
  tipIn,
  type UtxoFilter,
  type UtxoRead,
  type View,
  walkLinkedQueue,
} from "@al-ft/midgard-l1-follower";
import { toLucidUtxo } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";

import {
  OPERATOR_LIST_MAX_NODES,
  type OperatorListContract,
  type OperatorSetConfig,
  type OperatorSetContract,
} from "./config.js";

/** One output of this operator's active node, live or spent. */
export type OwnActivity = Readonly<{
  outRef: string;
  /** The slot of the block that created it (null for a seeded row). */
  createdSlot: number | null;
  /** The slot of the block that spent it (null while live). */
  spentSlot: number | null;
}>;

/** The operator set at one follower view. */
export type OperatorSet = Readonly<{
  view: View;
  ownKey: string;
  /** The registered list, root first, in list order. */
  registered: readonly SDK.RegisteredOperatorNode[];
  /** The active list, root first, in list order. */
  active: readonly SDK.ActiveOperatorNode[];
  /** This operator's retired node, and the slot it was created at. */
  ownRetired: Readonly<{
    node: SDK.RetiredOperatorNode;
    createdSlot: number | null;
  }> | null;
  scheduler: SDK.SchedulerUTxO | null;
  hubOracle: SDK.HubOracleUTxO | null;
  /** This operator's active-node outputs within the follower's retention. */
  ownActivity: readonly OwnActivity[];
  /**
   * Null when the lists, the scheduler and the hub oracle are each one
   * well-formed whole; otherwise what is wrong (a malformed output, a broken
   * list, a missing or duplicated singleton).
   */
  unhealthy: string | null;
}>;

export type OperatorSetRead =
  | Readonly<{
      kind: "ok";
      set: OperatorSet;
      /** The fact rows this read took (0 when nothing changed). */
      rowsRead: number;
      /** Whether this read loaded the live set instead of the changes. */
      loaded: boolean;
    }>
  | Readonly<{ kind: "not_initialized"; detail: string }>;

type Entry =
  | Readonly<{ kind: "registered"; node: SDK.RegisteredOperatorNode }>
  | Readonly<{ kind: "active"; node: SDK.ActiveOperatorNode }>
  | Readonly<{
      kind: "retired";
      node: SDK.RetiredOperatorNode;
      createdSlot: number | null;
    }>
  | Readonly<{ kind: "scheduler"; utxo: SDK.SchedulerUTxO }>
  | Readonly<{ kind: "hubOracle"; utxo: SDK.HubOracleUTxO }>
  | Readonly<{ kind: "invalid"; source: Source; detail: string }>;

type Source = "registered" | "active" | "retired" | "scheduler" | "hubOracle";

const outRefText = (stored: StoredOutput): string =>
  `${stored.outRef.txHash.toString("hex")}#${stored.outRef.index.toString()}`;

/** The one asset name an authentication output holds under `policyId`. */
const beaconName = (assets: Assets, policyId: string): string | null => {
  const names = assets.get(policyId);
  if (names === undefined || names.size !== 1 || assets.size !== 1) return null;
  const [name, quantity] = [...names.entries()][0]!;
  return quantity === 1n ? name : null;
};

const decoded = <A, E>(
  program: Effect.Effect<A, E>,
): Either.Either<A, string> =>
  Effect.runSync(
    Effect.either(Effect.mapError(program, (error) => String(error))),
  );

const singleton = <A>(
  program: Effect.Effect<A[], unknown>,
): Either.Either<A, string> =>
  Either.flatMap(decoded(program), (found) =>
    found.length === 1
      ? Either.right(found[0]!)
      : Either.left("not an authentic output"),
  );

/** One fact row as an entry of the set (null when the set does not hold it). */
const entryOf = (
  source: Source,
  contract: OperatorSetContract,
  stored: StoredOutput,
): Entry | null => {
  if (!stored.output.address.equals(Buffer.from(contract.address, "hex")))
    return null;
  if (!stored.output.assets.has(contract.policyId)) return null;
  const invalid = (detail: string): Entry => ({
    kind: "invalid",
    source,
    detail: `${outRefText(stored)}: ${detail}`,
  });
  const name = beaconName(stored.output.assets, contract.policyId);
  if (name === null) return invalid("not one authentication token");
  const utxo: UTxO = toLucidUtxo(stored.outRef, stored.output);
  const result: Either.Either<Entry, string> = (() => {
    switch (source) {
      case "registered":
        return Either.map(
          decoded(SDK.registeredOperatorNodeFromUTxO(utxo, name)),
          (node): Entry => ({ kind: "registered", node }),
        );
      case "active":
        return Either.map(
          decoded(SDK.activeOperatorNodeFromUTxO(utxo, name)),
          (node): Entry => ({ kind: "active", node }),
        );
      case "retired":
        return Either.map(
          decoded(SDK.retiredOperatorNodeFromUTxO(utxo, name)),
          (node): Entry => ({
            kind: "retired",
            node,
            createdSlot: stored.created?.slot ?? stored.seedSlot,
          }),
        );
      case "scheduler":
        return Either.map(
          singleton(SDK.utxosToSchedulerUTxOs([utxo], contract.policyId)),
          (found): Entry => ({ kind: "scheduler", utxo: found }),
        );
      case "hubOracle":
        return Either.map(
          singleton(SDK.utxosToHubOracleUTxOs([utxo], contract.policyId)),
          (found): Entry => ({ kind: "hubOracle", utxo: found }),
        );
    }
  })();
  return Either.isRight(result) ? result.right : invalid(result.left);
};

/** A node's linked-list key (null for the root). */
const keyOf = (node: SDK.NodeWithDatum): string | null =>
  node.datum.key === "Empty" ? null : node.datum.key.Key.key;

/**
 * One list in list order, or why its live nodes are not one list: a node
 * whose asset name does not match its key is malformed.
 */
const walkList = <N extends SDK.NodeWithDatum>(
  name: string,
  contract: OperatorListContract,
  nodes: readonly N[],
): Readonly<{ nodes: readonly N[]; problem: string | null }> => {
  const byId = new Map<string, N>();
  const entries = nodes.map((node, index) => {
    const id = index.toString();
    byId.set(id, node);
    const key = keyOf(node);
    const wellNamed =
      key === null
        ? node.assetName === contract.rootAssetName
        : node.assetName === `${contract.nodePrefix}${key}`;
    return {
      id,
      kind: wellNamed ? (key === null ? "root" : "node") : "invalid",
      key,
      next: node.datum.next === "Empty" ? null : node.datum.next.Key.key,
    } as const;
  });
  const walk = walkLinkedQueue(entries, { maxNodes: OPERATOR_LIST_MAX_NODES });
  const ordered = [
    ...(walk.root === null ? [] : [walk.root]),
    ...walk.nodes,
  ].map((entry) => byId.get(entry.id)!);
  return {
    nodes: ordered,
    problem: walk.healthy
      ? null
      : `${name} list ${walk.reason ?? "unhealthy"}${walk.detail === null ? "" : ` (${walk.detail})`}`,
  };
};

/** The mirror's state as an operator set at its view. */
const setOf = (
  view: View,
  ownKey: string,
  config: OperatorSetConfig,
  entries: ReadonlyMap<string, Entry>,
  activity: ReadonlyMap<string, OwnActivity>,
): OperatorSet => {
  const all = [...entries.values()];
  const pick = <K extends Entry["kind"]>(kind: K) =>
    all.filter(
      (entry): entry is Extract<Entry, { kind: K }> => entry.kind === kind,
    );
  const registered = walkList(
    "registered",
    config.registered,
    pick("registered").map((entry) => entry.node),
  );
  const active = walkList(
    "active",
    config.active,
    pick("active").map((entry) => entry.node),
  );
  const schedulers = pick("scheduler");
  const hubs = pick("hubOracle");
  const retired = pick("retired");
  const problems = [
    ...pick("invalid").map((entry) => `${entry.source}: ${entry.detail}`),
    ...(registered.problem === null ? [] : [registered.problem]),
    ...(active.problem === null ? [] : [active.problem]),
    ...(schedulers.length === 1
      ? []
      : [`${schedulers.length.toString()} scheduler outputs`]),
    ...(hubs.length === 1
      ? []
      : [`${hubs.length.toString()} hub oracle outputs`]),
    ...(retired.length <= 1
      ? []
      : [`${retired.length.toString()} own retired nodes`]),
  ];
  return {
    view,
    ownKey,
    registered: registered.nodes,
    active: active.nodes,
    ownRetired:
      retired[0] === undefined
        ? null
        : { node: retired[0].node, createdSlot: retired[0].createdSlot },
    scheduler: schedulers.length === 1 ? schedulers[0]!.utxo : null,
    hubOracle: hubs.length === 1 ? hubs[0]!.utxo : null,
    ownActivity: [...activity.values()],
    unhealthy: problems.length === 0 ? null : problems.join("; "),
  };
};

/**
 * The operator set kept from the follower facts. `refresh` reads the facts
 * in the caller's read transaction and returns the set at the view that
 * transaction sees.
 */
export type OperatorSetMirror = Readonly<{
  refresh: (tx: SqlTx, dialect: Dialect) => Promise<OperatorSetRead>;
  /** The view of the last refresh (null before the first). */
  view: () => View | null;
}>;

export const createOperatorSetMirror = (options: {
  readonly config: OperatorSetConfig;
  /** This operator's key hash (56 hex). */
  readonly ownKey: string;
}): OperatorSetMirror => {
  const { config, ownKey } = options;
  const ownActiveName = `${config.active.nodePrefix}${ownKey}`;
  const ownRetiredName = `${config.retired.nodePrefix}${ownKey}`;
  const unit = (
    contract: OperatorSetContract,
    assetName?: string,
  ): UtxoFilter => ({
    by: "unit",
    policyId: Buffer.from(contract.policyId, "hex"),
    ...(assetName === undefined
      ? {}
      : { assetName: Buffer.from(assetName, "hex") }),
  });
  // What the set reads: the four policies it holds whole, and the own
  // retired node by its exact asset.
  const sources: readonly (readonly [
    Source,
    OperatorSetContract,
    UtxoFilter,
  ])[] = [
    ["registered", config.registered, unit(config.registered)],
    ["active", config.active, unit(config.active)],
    ["scheduler", config.scheduler, unit(config.scheduler)],
    ["hubOracle", config.hubOracle, unit(config.hubOracle)],
    ["retired", config.retired, unit(config.retired, ownRetiredName)],
  ];
  const ownActive = unit(config.active, ownActiveName);
  let view: View | null = null;
  let entries = new Map<string, Entry>();
  let activity = new Map<string, OwnActivity>();

  const activeAddress = Buffer.from(config.active.address, "hex");
  const holdsOwnActive = (stored: StoredOutput): boolean =>
    stored.output.address.equals(activeAddress) &&
    stored.output.assets.get(config.active.policyId)?.has(ownActiveName) ===
      true;
  const apply = (
    into: Map<string, Entry>,
    source: Source,
    contract: OperatorSetContract,
    stored: StoredOutput,
  ): void => {
    const id = outRefText(stored);
    if (stored.spent !== null) into.delete(id);
    else {
      const entry = entryOf(source, contract, stored);
      if (entry === null) into.delete(id);
      else into.set(id, entry);
    }
  };
  const record = (into: Map<string, OwnActivity>, stored: StoredOutput) => {
    if (!holdsOwnActive(stored)) return;
    into.set(outRefText(stored), {
      outRef: outRefText(stored),
      createdSlot: stored.created?.slot ?? stored.seedSlot,
      spentSlot: stored.spent?.slot ?? null,
    });
  };

  const load = async (
    tx: SqlTx,
    dialect: Dialect,
  ): Promise<
    | Readonly<{
        entries: Map<string, Entry>;
        activity: Map<string, OwnActivity>;
        rows: number;
      }>
    | Readonly<{ kind: "refused"; detail: string }>
  > => {
    const nextEntries = new Map<string, Entry>();
    const nextActivity = new Map<string, OwnActivity>();
    let rows = 0;
    for (const [source, contract, filter] of sources) {
      const read = await liveUtxosIn(tx, dialect, filter);
      if (read.kind !== "ok") return { kind: "refused", detail: read.detail };
      rows += read.utxos.length;
      for (const stored of read.utxos)
        apply(nextEntries, source, contract, stored);
    }
    const history = await changedUtxosIn(tx, dialect, ownActive, null);
    if (history.kind !== "ok")
      return { kind: "refused", detail: history.detail };
    rows += history.utxos.length;
    for (const stored of history.utxos) record(nextActivity, stored);
    return { entries: nextEntries, activity: nextActivity, rows };
  };

  const refresh = async (
    tx: SqlTx,
    dialect: Dialect,
  ): Promise<OperatorSetRead> => {
    const tip = await tipIn(tx, dialect);
    if (tip === null)
      return {
        kind: "not_initialized",
        detail: "the follower has no view yet",
      };
    const current: View = {
      generation: tip.generation,
      point: tip.point,
      height: tip.height,
    };
    let rows = 0;
    let loaded = false;
    const changed: (readonly [Source, OperatorSetContract, UtxoRead])[] = [];
    if (view !== null && view.generation === current.generation) {
      for (const [source, contract, filter] of sources) {
        const read = await changedUtxosIn(tx, dialect, filter, view.point.slot);
        if (read.kind !== "ok") break;
        changed.push([source, contract, read]);
      }
    }
    if (changed.length === sources.length) {
      for (const [source, contract, read] of changed) {
        if (read.kind !== "ok") continue;
        rows += read.utxos.length;
        for (const stored of read.utxos) {
          apply(entries, source, contract, stored);
          if (source === "active") record(activity, stored);
        }
      }
    } else {
      // The first read, a rewind or a prune past the last read: load.
      const fresh = await load(tx, dialect);
      if ("kind" in fresh)
        return { kind: "not_initialized", detail: fresh.detail };
      entries = fresh.entries;
      activity = fresh.activity;
      rows = fresh.rows;
      loaded = true;
    }
    // A spent own row at or below the retained window is pruned (or about
    // to be): the set holds what a load at this view holds.
    for (const [id, own] of activity)
      if (own.spentSlot !== null && own.spentSlot <= tip.prunedThroughSlot)
        activity.delete(id);
    view = current;
    return {
      kind: "ok",
      set: setOf(current, ownKey, config, entries, activity),
      rowsRead: rows,
      loaded,
    };
  };

  return { refresh, view: () => view };
};
