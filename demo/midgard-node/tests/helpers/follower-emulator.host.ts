/**
 * The one follower host of a test process: the node's Postgres follower
 * store in this worker's database (`testDatabaseName()`), following one
 * emulator chain at a time (`follower-emulator.chain.ts`). Every block of
 * the chain is applied once, by the store's own writer (`applyBlock`), with
 * the node's projections once a deployment is bound (`bindNodeFollower`):
 * their derivations write the node's follower tables exactly as the node's
 * follower does. The provider (`NodeL1Provider`), the driver
 * (`planCurrentView`) and every other reader read this store.
 *
 * - Origin: the chain's origin (genesis, or a ledger restored without a
 *   chain) with the tracked outputs it holds seeded; its key-credential
 *   addresses are the own wallets.
 * - Tracked set: the node's (`nodeIntentTrackedSet` and its projections'),
 *   and the script credentials and policies the origin holds. Every payment
 *   credential an output pays to (a key one outside the origin's wallets)
 *   and every policy a transaction mints under is tracked from the block
 *   that first shows it, so every transaction the suites submit is a fact.
 * - Sync: the store moves to the chain of the emulator asked for. A chain
 *   that shares blocks with the stored one rewinds to the last shared block
 *   (a rollback, or another copy of one snapshot) and applies the rest. A
 *   different origin or binding, a store whose cursor another writer moved
 *   (`resetApplicationTables` empties it), a rewind the store refuses, or a
 *   failed sync resets the store to its origin (`resetToOrigin`) and replays
 *   the chain, as a tracked-set reset does; the replay mark is cleared
 *   once the replay is back at the tip.
 * - Store check (`onFollowerHostCheck`): runs after each sync, on the store
 *   at the synced emulator's chain, and before a sync moves the store off
 *   the chain of the emulator it last followed, when it may first bring the
 *   store to that emulator's chain as it now stands (only the blocks it made
 *   since its last sync). A check that fails while leaving leaves the next
 *   sync a reset.
 * - Every sync is serialized; a failure leaves the next sync a reset.
 */
import {
  decodeBlock,
  decodeLedgerUtxos,
  type FactStore,
  type FollowerProjection,
  isTrackedOutput,
  mergeTrackedSets,
  openPostgresBackend,
  openPostgresFactStore,
  projectionStoreOptions,
  resetToOrigin,
  type TrackedSet,
} from "@al-ft/midgard-l1-follower";
import type * as SDK from "@al-ft/midgard-sdk";
import {
  type Emulator,
  getAddressDetails,
  type Network,
} from "@lucid-evolution/lucid";

import { protocolPaymentCredentials } from "../../src/services/intent-journal.tracked-set.js";
import { nodeIntentTrackedSet } from "../../src/services/l1-follower.intents.js";
import {
  type L1FollowerPlan,
  l1FollowerPlan,
} from "../../src/services/l1-follower.plan.js";
import { nodeFollowerProjections } from "../../src/services/l1-follower.projections.js";
import { nodeDatabaseConnectionString } from "../../src/services/l1-provider.js";
import { applyMidgardNodeTestEnv } from "../test-env.js";
import {
  followedBlockBytes,
  type FollowedChain,
  followedChainOf,
  followedPoint,
} from "./follower-emulator.chain.js";
import { trackedItemsOf, utxoAnswer } from "./follower-emulator.ledger.js";

/** k for a chain no deployment bound. */
const DEFAULT_SECURITY_PARAMETER = 2_160;
const PLACEHOLDER_HASH = "00".repeat(32);

export type NodeFollowerPlan = Extract<L1FollowerPlan, { kind: "run" }>;

/** A deployment the node follows on an emulator chain. */
type Binding = Readonly<{
  key: string;
  plan: NodeFollowerPlan;
  projections: readonly FollowerProjection[];
  trackedSet: TrackedSet;
}>;

const bindings = new WeakMap<Emulator, Binding>();

const connectionString = (): string => {
  applyMidgardNodeTestEnv();
  const env = process.env;
  return nodeDatabaseConnectionString({
    POSTGRES_HOST: env.POSTGRES_HOST!,
    POSTGRES_PORT: Number(env.POSTGRES_PORT),
    POSTGRES_USER: env.POSTGRES_USER!,
    POSTGRES_PASSWORD: env.POSTGRES_PASSWORD!,
    POSTGRES_DB: env.POSTGRES_DB!,
  });
};

/**
 * The node's follower over `emulator`'s chain for `contracts`: the plan the
 * node makes from its configuration (`l1FollowerPlan`; the local node
 * settings and the hub oracle's one-shot are placeholders the store never
 * reads) and its projections. Returns the plan.
 */
export const bindNodeFollower = (
  emulator: Emulator,
  input: Readonly<{
    contracts: SDK.MidgardValidators;
    network: Network;
    securityParameter?: number;
  }>,
): NodeFollowerPlan => {
  const securityParameter =
    input.securityParameter ?? DEFAULT_SECURITY_PARAMETER;
  const chain = followedChainOf(emulator);
  const plan = l1FollowerPlan({
    config: {
      L1_NATIVE_LEDGER: {
        socketPath: "/dev/null",
        nodeConfigPath: "/dev/null",
        binaryPath: "/dev/null",
      },
      L1_ORIGIN: { slot: chain.origin.slot, blockHash: chain.origin.hash },
      HUB_ORACLE_ONE_SHOT_TX_HASH: PLACEHOLDER_HASH,
      HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: 0,
      L1_TX_CONTENT_SOURCES: [],
      NETWORK: input.network,
      POSTGRES_HOST: "",
      POSTGRES_PORT: 0,
      POSTGRES_USER: "",
      POSTGRES_PASSWORD: "",
      POSTGRES_DB: "",
    },
    contracts: input.contracts,
    securityParameter,
  });
  if (plan.kind !== "run")
    throw new Error(`the node's follower does not run: ${plan.detail}`);
  // The host never prunes, so the landed frontier's prune floor never runs.
  const { projections } = nodeFollowerProjections(plan, () =>
    Promise.reject(new Error("the emulator follower host never prunes")),
  );
  const trackedSet = nodeIntentTrackedSet({
    protocolPaymentCredentials: protocolPaymentCredentials(input.contracts),
    hubOraclePolicyId: plan.hubOraclePolicyId,
  });
  bindings.set(emulator, {
    key: [
      input.network,
      securityParameter.toString(),
      plan.hubOraclePolicyId,
      plan.stateQueue.policyId,
      ...[...trackedSet.paymentCredentials].sort(),
    ].join(":"),
    plan,
    projections,
    trackedSet,
  });
  return plan;
};

type Open = {
  readonly key: string;
  readonly originHash: string;
  readonly binding: Binding | undefined;
  readonly store: FactStore;
  readonly wallets: readonly Buffer[];
  /** The key payment credentials of the origin's wallets, as hex. */
  readonly walletCredentials: ReadonlySet<string>;
  /** The hashes of the applied blocks, by height - 1. */
  readonly applied: string[];
  replaying: boolean;
};

let open: Open | undefined;
/** The emulator whose chain `open` holds, as of its last sync. */
let followed: Emulator | undefined;
let storeCheck: StoreCheck | undefined;
let lane: Promise<unknown> = Promise.resolve();

const serialized = <A>(work: () => Promise<A>): Promise<A> => {
  const next = lane.then(work);
  lane = next.catch(() => undefined);
  return next;
};

const originItems = (chain: FollowedChain) => {
  const wallets = new Map<string, Buffer>();
  const walletCredentials = new Set<string>();
  const credentials = new Set<string>();
  const policies = new Set<string>();
  for (const { address, assets } of chain.origin.ledger) {
    const details = getAddressDetails(address);
    if (details.paymentCredential?.type === "Key") {
      wallets.set(details.address.hex, Buffer.from(details.address.hex, "hex"));
      walletCredentials.add(details.paymentCredential.hash);
    } else if (details.paymentCredential?.type === "Script")
      credentials.add(details.paymentCredential.hash);
    for (const unit of Object.keys(assets))
      if (unit !== "lovelace") policies.add(unit.slice(0, 56));
  }
  return {
    wallets: [...wallets.values()],
    walletCredentials,
    trackedSet: {
      addresses: new Set<string>(),
      paymentCredentials: credentials,
      policies,
    } satisfies TrackedSet,
  };
};

const required = <T extends { kind: string }, K extends T["kind"]>(
  what: string,
  result: T | null,
  kind: K,
): Extract<T, { kind: K }> => {
  if (result?.kind !== kind)
    throw new Error(
      `the follower store did not ${what}: ${result?.kind ?? "uninitialized"}${result !== null && "detail" in result ? `: ${String(result.detail)}` : ""}`,
    );
  return result as Extract<T, { kind: K }>;
};

const close = async (): Promise<void> => {
  const closing = open;
  open = undefined;
  followed = undefined;
  await closing?.store.close().catch(() => undefined);
};

/** Whether this process's host has written the store since its last reset. */
let written = false;

/** Empties the follower's tables (`resetToOrigin`). */
const resetStore = async (connection: { connectionString: string }) => {
  const backend = openPostgresBackend(connection);
  try {
    required("reset", await resetToOrigin(backend), "reset");
  } finally {
    await backend.close();
  }
};

/** Resets the store to `chain`'s origin with `binding`'s projections. */
const reopen = async (
  chain: FollowedChain,
  binding: Binding | undefined,
  key: string,
): Promise<Open> => {
  await close();
  const connection = { connectionString: connectionString() };
  await resetStore(connection);
  written = true;
  const origin = originItems(chain);
  const trackedSet = mergeTrackedSets(
    origin.trackedSet,
    ...(binding === undefined ? [] : [binding.trackedSet]),
  );
  const store = openPostgresFactStore({
    ...projectionStoreOptions(
      binding?.projections ?? [],
      {
        securityParameter:
          binding?.plan.securityParameter ?? DEFAULT_SECURITY_PARAMETER,
        trackedSet,
      },
      "postgres",
    ),
    wallets: origin.wallets,
    connection: { ...connection, maxConnections: 4 },
  });
  try {
    const started = required("start", await store.start(), "ready");
    const point = followedPoint(chain, -1);
    required(
      "initialize",
      await store.initialize({ point, height: 0 }),
      "initialized",
    );
    // The tracked outputs the origin holds predate it: seed them.
    const tracked = store.trackedSet();
    const seeds = decodeLedgerUtxos(utxoAnswer(chain.origin.ledger)).filter(
      ({ output }) => isTrackedOutput(output, tracked),
    );
    if (seeds.length > 0)
      required("seed", await store.insertSeedOutputs(point, seeds), "seeded");
    open = {
      key,
      originHash: chain.origin.hash,
      binding,
      store,
      wallets: origin.wallets,
      walletCredentials: origin.walletCredentials,
      applied: [],
      replaying: started.replaying,
    };
    return open;
  } catch (error) {
    await store.close().catch(() => undefined);
    throw error;
  }
};

/** Applies `chain`'s block at `index` on the stored chain's tip. */
const apply = async (
  current: Open,
  chain: FollowedChain,
  index: number,
): Promise<void> => {
  const block = decodeBlock(followedBlockBytes(chain, index));
  const expected = chain.blocks[index]!;
  if (block.point.hash.toString("hex") !== expected.hash)
    throw new Error("the follower block changed its hash");
  const items = trackedItemsOf(block);
  const tracked = current.store.trackedSet();
  current.store.setTrackedSet({
    addresses: tracked.addresses,
    paymentCredentials: new Set([
      ...tracked.paymentCredentials,
      ...items.scripts,
      // A key credential the origin's wallets do not hold has no output
      // before this block.
      ...items.keys.filter((key) => !current.walletCredentials.has(key)),
    ]),
    policies: new Set([...tracked.policies, ...items.policies]),
  });
  required("apply a block", await current.store.applyBlock(block), "applied");
  current.applied.push(expected.hash);
};

/** The applied blocks `chain` shares, from the origin. */
const sharedBlocks = (current: Open, chain: FollowedChain): number => {
  let shared = 0;
  while (
    shared < current.applied.length &&
    shared < chain.blocks.length &&
    current.applied[shared] === chain.blocks[shared]!.hash
  )
    shared += 1;
  return shared;
};

/** Runs the store check if the sync to `emulator` leaves another's chain. */
const leave = async (emulator: Emulator): Promise<void> => {
  const leaving = followed;
  if (
    open === undefined ||
    leaving === undefined ||
    leaving === emulator ||
    storeCheck === undefined
  )
    return;
  try {
    await storeCheck(leaving, async () => (await follow(leaving)).store);
  } catch {
    // What the check did not read stays unchecked.
    await close();
  }
};

const follow = async (emulator: Emulator): Promise<Open> => {
  await leave(emulator);
  followed = undefined;
  const chain = followedChainOf(emulator);
  const binding =
    bindings.get(emulator) ??
    (open?.originHash === chain.origin.hash ? open.binding : undefined);
  const key = `${chain.origin.hash}:${binding?.key ?? "unbound"}`;
  let current = open;
  if (current?.key === key) {
    const cursor = await current.store.cursor();
    const tip = current.applied.at(-1) ?? chain.origin.hash;
    if (cursor?.point.hash.toString("hex") !== tip) current = undefined;
  } else current = undefined;
  current ??= await reopen(chain, binding, key);
  const shared = sharedBlocks(current, chain);
  if (shared < current.applied.length) {
    const rewound =
      shared === 0
        ? undefined
        : await current.store.rewind(followedPoint(chain, shared - 1));
    if (rewound?.kind === "rewound") current.applied.splice(shared);
    else current = await reopen(chain, binding, key);
  }
  for (let index = current.applied.length; index < chain.blocks.length; index++)
    await apply(current, chain, index);
  if (current.replaying) {
    const ended = await current.store.endTrackedSetReplay();
    current.replaying = ended === "below_replay_height";
  }
  followed = emulator;
  return current;
};

/**
 * Brings the store to `emulator`'s chain and returns it. The store object
 * is replaced by a reset; read it through the sync that returned it.
 */
export const syncFollowerHost = (emulator: Emulator): Promise<FactStore> =>
  withFollowerHost(emulator, (store) => Promise.resolve(store));

/**
 * `read` over the store at `emulator`'s chain, before any later sync can
 * move it to another chain.
 */
export const withFollowerHost = <A>(
  emulator: Emulator,
  read: (store: FactStore) => Promise<A>,
): Promise<A> =>
  serialized(async () => {
    let store: FactStore;
    try {
      store = (await follow(emulator)).store;
    } catch (error) {
      await close();
      throw error;
    }
    // What a failed check did not read stays unchecked.
    await storeCheck?.(emulator, () => Promise.resolve(store)).catch(
      () => undefined,
    );
    return read(store);
  });

/**
 * `read` over the store where it stands (no sync), as the node's other
 * readers read its follower between runs; `undefined` before any sync.
 */
export const readFollowerHost = <A>(
  read: (store: FactStore) => Promise<A>,
): Promise<A | undefined> =>
  serialized(() =>
    open === undefined ? Promise.resolve(undefined) : read(open.store),
  );

/**
 * A check of `emulator`'s chain: `chain` returns the store at that chain as
 * it now stands (bringing it there first when the store is leaving it).
 */
export type StoreCheck = (
  emulator: Emulator,
  chain: () => Promise<FactStore>,
) => Promise<void>;

/**
 * Runs `check` after each sync and before each sync that moves the store off
 * the chain of the emulator it last followed, until the returned unregister
 * runs.
 */
export const onFollowerHostCheck = (check: StoreCheck): (() => void) => {
  storeCheck = check;
  return () => {
    if (storeCheck === check) storeCheck = undefined;
  };
};

/** The own wallets of the store's current origin. */
export const followerHostWallets = (): readonly Buffer[] => open?.wallets ?? [];

/** The plan `emulator` was bound with (`bindNodeFollower`). */
export const boundNodeFollower = (
  emulator: Emulator,
): NodeFollowerPlan | undefined => bindings.get(emulator)?.plan;

/** Releases the store's connections and its writer lease. */
export const closeFollowerHost = (): Promise<void> => serialized(() => close());

/**
 * `closeFollowerHost`, then the follower's tables emptied if this process's
 * host wrote them: the next file in this worker's database finds no chain
 * of this one.
 */
export const releaseFollowerHost = (): Promise<void> =>
  serialized(async () => {
    await close();
    if (!written) return;
    await resetStore({ connectionString: connectionString() });
    written = false;
  });
