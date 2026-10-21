/**
 * The validation-trace Open scene (the fault-proofs package's test support)
 * under the node's follower, intent journal and defender:
 *
 * - the scene's operator is the node's: its Base wallet and its defense
 *   pocket are the node's own wallets, tracked from the follower's origin
 *   (attached after the scene's setup, so it sees every later
 *   transaction), and the scene's protocol outputs are seeded;
 * - the node's landed rows hold the scene's three blocks, each retained, on
 *   a confirmed frontier at the failing block's parent;
 * - the defender's port plays with the node's own Lucid (the operator wallet
 *   with its pocket selected) against the scene's published references;
 * - a tick runs while the chain moves: whenever a transaction waits in the
 *   emulator's mempool, a block is made and the follower applies it, so a
 *   held Defend sees its own landing;
 * - another scope can hold the L1 control plane, so a tick's Defend waits
 *   on it.
 */
import {
  type Block,
  type ExpiryReferences,
  l1SlotConfig,
  publishExpiryReferences,
  type Scene,
  setUpScene,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import * as SDK from "@al-ft/midgard-sdk";
import type { SqlClient } from "@effect/sql";
import {
  CML,
  Data,
  generateEmulatorAccount,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";

import { foreignRetainedDaInsert } from "../../src/da/foreign-retained-da.js";
import { ConfirmedLedgerDB, DaPayloadsDB } from "../../src/database/index.js";
import {
  ledgerMap,
  ledgerRows,
  netDelta,
} from "../../src/landed-blocks/ledger.js";
import { Frontier, insertRow } from "../../src/landed-blocks/store.js";
import { Globals } from "../../src/services/globals.js";
import { withL1ControlPlane } from "../../src/services/index.js";
import { IntentJournal } from "../../src/services/intent-journal.js";
import {
  defensePocketAddress,
  nodeOwnWallets,
} from "../../src/services/intent-journal.tracked-set.js";
import type {
  ValidationTraceDefensePort,
  ValidationTraceDefenseReferences,
} from "../../src/validation-trace-defense/deployment.js";
import {
  VALIDATION_TRACE_DEFENSE_LEASE_HOLDER,
  type ValidationTraceDefenseHealth,
  validationTraceDefenseReadiness,
} from "../../src/validation-trace-defense/health.js";
import { validationTraceDefenseTick } from "../../src/validation-trace-defense/tick.js";
import {
  attachIntentFollower,
  type IntentEmulator,
} from "./intent-journal-emulator.js";
import type { testDatabases } from "./l1-events-store.js";

type Entry = Readonly<{ outRef: Buffer; output: Buffer }>;
const minimal = (entries: readonly Entry[]) =>
  entries.map(({ outRef, output }) => ({ outref: outRef, output }));

/** The node's wallet configuration with `seed` as its operator seed. */
export const operatorConfig = (seed: string) => ({
  NETWORK: "Custom" as const,
  L1_OPERATOR_SEED_PHRASE: seed,
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: "",
  L1_SETTLEMENT_SEED_PHRASE: "",
  L1_REFERENCE_SCRIPT_SEED_PHRASE: "",
});

/**
 * Retains `block`'s payload as the node does. The fixture's body carries
 * no validation traces, while a retained row of a block with transactions
 * must count one per transaction under a non-empty root; the row takes
 * those from the header's counts and a stand-in root (the defense reads
 * only the payload).
 */
export const retain = (block: Block) => {
  const { header, headerHash } = block.evidence;
  return DaPayloadsDB.upsertAvailable(
    foreignRetainedDaInsert(
      headerHash,
      header,
      header.endTime,
      Buffer.from(block.payloadEnvelopeCbor),
      { validationTracesRoot: "5a".repeat(32) },
      {
        validationTraceCount:
          header.forcedTransactionCount + header.l2TransactionCount,
      },
    ),
  );
};

/** The confirmed frontier at the failing block's parent. */
export const frontierAtStart = (scene: Scene) =>
  Frontier.upsert({
    headerHash: scene.failing.evidence.header.prevHeaderHash,
    utxosRoot: scene.failing.evidence.header.prevUtxosRoot,
  });

/** The node's landed rows: the frontier at the failing block's parent,
 * then the scene's three blocks as processed own rows, each retained. */
export const seedLanded = (scene: Scene) =>
  Effect.gen(function* () {
    const { failing, passing, forced } = scene;
    yield* frontierAtStart(scene);
    yield* ConfirmedLedgerDB.insertMultiple(
      yield* ledgerRows(minimal(failing.predecessorEntries), new Map()),
    );
    for (const block of [failing, passing, forced]) {
      const { header, headerHash } = block.evidence;
      yield* insertRow({
        headerHash,
        parentHeaderHash: header.prevHeaderHash,
        parentUtxosRoot: header.prevUtxosRoot,
        utxosRoot: header.utxosRoot,
        kind: "own",
        state: "processed",
        applied: true,
        ...netDelta(
          ledgerMap(minimal(block.predecessorEntries)),
          minimal(block.postEntries),
        ),
        depositIds: [],
        withdrawals: [],
        forcedIds: [],
        txIds: [],
      });
      yield* retain(block);
    }
  });

/** The out-refs `txCbor` spends, as `<tx hash>#<index>`. */
export const inputsOf = (txCbor: Buffer | string): string[] => {
  const inputs = CML.Transaction.from_cbor_hex(
    typeof txCbor === "string" ? txCbor : txCbor.toString("hex"),
  )
    .body()
    .inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

export const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The bond hold an active-operator node carries. */
export const bondUnlockTimeOf = (node: UTxO): bigint | null => {
  const { data } = Data.from(node.datum!, SDK.LinkedListDatum);
  if (!("Node" in data)) throw new Error("not an operator node");
  return Data.castFrom(data.Node.data, SDK.ActiveOperatorDatum)
    .bond_unlock_time;
};

/** When the case `caseUtxo` started (L1 time). */
export const startedAtOf = (caseUtxo: UTxO): number =>
  Number(Data.from(caseUtxo.datum!, SDK.Case).started_at);

/** What the node runs a defender effect with. */
type NodeServices = SqlClient.SqlClient | Globals | IntentJournal;

export type DefenseScene = Awaited<ReturnType<typeof openDefenseScene>>;

/**
 * The scene, its expiry references published, under the node's follower
 * with the landed rows seeded, and the defender's port.
 */
export const openDefenseScene = async (
  databases: ReturnType<typeof testDatabases>,
) => {
  const scene = await setUpScene();
  const expiry: ExpiryReferences = await publishExpiryReferences(scene);
  const references: ValidationTraceDefenseReferences = {
    case: scene.references.case,
    game: scene.references.game,
    timeout: scene.references.timeout,
    boundary: expiry.boundary,
    threadExpiry: expiry.threadExpiry,
    activeOperators: scene.references.activeOperators,
  };
  const config = operatorConfig(scene.funder.seedPhrase);
  const env: IntentEmulator = await attachIntentFollower(databases, {
    emulator: scene.h.emulator,
    own: scene.funder,
    payee: generateEmulatorAccount({ lovelace: 0n }),
    nodeSchema: true,
    ownWallets: nodeOwnWallets(config),
    protocol: {
      contracts: scene.h.contracts,
      referenceScriptAddresses: [
        ...new Set(Object.values(references).map(({ address }) => address)),
      ],
    },
  });
  // The first pass seeds the own wallets at the origin.
  await env.stage.run();
  const globals = await Effect.runPromise(
    Effect.provide(Globals, Globals.Default),
  );
  const inNode = <A, E>(effect: Effect.Effect<A, E, NodeServices>) =>
    env.runtime.runPromise(
      effect.pipe(
        Effect.provide(env.journalLayer),
        Effect.provideService(Globals, globals),
      ),
    );
  await inNode(seedLanded(scene));

  const lucid: LucidEvolution = await env.wallet({ defensePocket: true });
  const pocketAddress = defensePocketAddress(config);
  const port: ValidationTraceDefensePort = {
    lucid,
    network: "Custom",
    dispute: scene.chain,
    activeOperatorsPolicyId: scene.h.contracts.activeOperators.policyId,
    hubOraclePolicyId: scene.h.contracts.hubOracle.policyId,
    ourVkh: scene.operator.vkey,
    baseAddress: scene.funder.address,
    pocketAddress,
    l1SlotConfig,
    references: Effect.succeed(references),
    lease: { ttlMs: 600_000, renewIntervalMs: 60_000 },
    tickMs: 1_000,
    pollMs: 25,
    cache: new Map(),
  };

  const pending = () =>
    Object.values(scene.h.emulator.transactionHistory).some(
      ({ status }) => status === "pending",
    );
  /** Makes a block of what waits in the mempool, and follows it. */
  const land = async () => {
    scene.h.emulator.awaitBlock(1);
    await env.follow();
  };
  /**
   * Runs `run` while the chain moves: whenever a transaction waits in the
   * emulator's mempool, `onPending` looks, then a block is made and the
   * follower applies it.
   */
  const whileChainMoves = async <A>(
    run: () => Promise<A>,
    onPending: () => Promise<void> = async () => undefined,
  ): Promise<A> => {
    let settled = false;
    const running = run().finally(() => {
      settled = true;
    });
    while (!settled) {
      await new Promise((resolve) => setTimeout(resolve, 10));
      if (!settled && pending()) {
        await onPending();
        await land();
      }
    }
    return await running;
  };
  /**
   * One tick of `withPort`'s defender (the scene's by default), while the
   * chain moves.
   */
  const tick = async (
    withPort: ValidationTraceDefensePort = port,
    onPending?: () => Promise<void>,
  ) => {
    await env.follow();
    return await whileChainMoves(
      () => inNode(validationTraceDefenseTick(withPort)),
      onPending,
    );
  };
  /**
   * Holds the L1 control plane as another scope (a commit, say) until
   * `release`; resolves once it holds it.
   */
  const holdControlPlane = async () => {
    let release!: () => void;
    const released = new Promise<void>((resolve) => {
      release = resolve;
    });
    const held = inNode(
      withL1ControlPlane(
        globals,
        { scope: "another-l1-scope" },
        Effect.promise(() => released),
      ),
    );
    const holder = async () =>
      (await Effect.runPromise(Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY)))
        .holder?.scope;
    while ((await holder()) !== "another-l1-scope")
      await new Promise((resolve) => setTimeout(resolve, 10));
    return {
      release: async () => {
        release();
        await held;
      },
    };
  };
  /** Waits until a Defend waits for the L1 control plane. */
  const defendWaitsForPlane = async () => {
    const waiting = async () =>
      [
        ...(
          await Effect.runPromise(Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY))
        ).waiters.values(),
      ].some(({ scope }) => scope === VALIDATION_TRACE_DEFENSE_LEASE_HOLDER);
    while (!(await waiting()))
      await new Promise((resolve) => setTimeout(resolve, 10));
  };
  const health = (): Promise<ValidationTraceDefenseHealth> =>
    Effect.runPromise(Ref.get(globals.VALIDATION_TRACE_DEFENSE_HEALTH));
  const readiness = async () =>
    validationTraceDefenseReadiness(await health(), Date.now());
  /** The confirmed transaction `hash`'s signed bytes and when it landed (L1 time). */
  const landed = (hash: string) => {
    const status = scene.h.emulator.transactionHistory[hash];
    const cbor = env.accepted.get(hash);
    if (status?.status !== "confirmed" || cbor === undefined)
      throw new Error(`${hash} has not landed`);
    return { cbor, at: lucid.slotToUnixTime(status.slot) };
  };
  /** The live output holding `unit` on the emulator's ledger. */
  const liveByUnit = (unit: string): UTxO | undefined =>
    Object.values(scene.h.emulator.ledger)
      .filter(({ spent }) => !spent)
      .map(({ utxo }) => utxo)
      .find((utxo) => (utxo.assets[unit] ?? 0n) > 0n);
  return {
    scene,
    expiry,
    references,
    env,
    globals,
    inNode,
    lucid,
    pocketAddress,
    port,
    pending,
    land,
    whileChainMoves,
    tick,
    holdControlPlane,
    defendWaitsForPlane,
    health,
    readiness,
    landed,
    liveByUnit,
  };
};
