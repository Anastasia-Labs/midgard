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
 *   on it;
 * - the node can be killed at its next submission, before the chain holds
 *   the bytes or once it does, and restarted: a fresh process's Globals,
 *   defender cache and Lucid over the same database and chain;
 * - a submission can reach only a fork the emulator never sees, which the
 *   test applies to the follower;
 * - the intents journaled under a workflow key, with their bytes and
 *   status, can be read.
 */
import { isPlainFundingOutput } from "@al-ft/midgard-fault-proofs";
import {
  type ExpiryReferences,
  l1SlotConfig,
  landWith,
  publishExpiryReferences,
  setUpScene,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import {
  postgresDialect,
  readIntentsByWorkflowIn,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import {
  CML,
  Data,
  generateEmulatorAccount,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";

import { NODE_PROCESS_STATE_QUEUE_LEASE_HOLDERS } from "../../src/commands/listen-startup.release-state-queue-leases-of-previous-node-process.js";
import { followerSqlTx } from "../../src/database/follower-schema.js";
import { StateQueueMutationLeasesDB } from "../../src/database/index.js";
import { Globals } from "../../src/services/globals.js";
import { withL1ControlPlane } from "../../src/services/index.js";
import {
  IntentJournal,
  readIntentStatus,
} from "../../src/services/intent-journal.js";
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
import {
  operatorConfig,
  seedLanded,
} from "./validation-trace-defense-scene.landed.js";

export {
  frontierAtStart,
  operatorConfig,
  retain,
  seedLanded,
} from "./validation-trace-defense-scene.landed.js";

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
  const freshGlobals = () =>
    Effect.runPromise(Effect.provide(Globals, Globals.Default));
  /** This node process's Globals: a restart replaces them. */
  let globals = await freshGlobals();
  /** Runs `effect` in the node process; `signal` kills it. */
  const inNode = <A, E>(
    effect: Effect.Effect<A, E, NodeServices>,
    signal?: AbortSignal,
  ) =>
    env.runtime.runPromise(
      effect.pipe(
        Effect.provide(env.journalLayer),
        Effect.provideService(Globals, globals),
      ),
      { signal },
    );
  await inNode(seedLanded(scene));

  const lucid: LucidEvolution = await env.wallet({ defensePocket: true });
  const pocketAddress = defensePocketAddress(config);
  /** This node process's defender port: a restart replaces it. */
  let port: ValidationTraceDefensePort = {
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
   * One tick of `withPort`'s defender (this process's by default), while
   * the chain moves.
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
  /**
   * Arms a kill at the node's next submission to the chain: `before` the
   * chain holds the bytes (they never reach it), or `after` it does. The
   * promise resolves with the bytes once the point is reached; the
   * submission never returns.
   */
  const killAtSubmit = (when: "before" | "after"): Promise<Buffer> => {
    const { emulator } = scene.h;
    const submit = emulator.submitTx;
    return new Promise((reached) => {
      emulator.submitTx = async (tx) => {
        emulator.submitTx = submit;
        if (when === "after") await submit.call(emulator, tx);
        reached(Buffer.from(tx, "hex"));
        return await new Promise<string>(() => undefined);
      };
    });
  };
  /**
   * The node's next submission reaches only a fork the emulator never sees:
   * the promise resolves with its bytes, for the test to apply to the
   * follower.
   */
  const forkOnlySubmit = (): Promise<Buffer> => {
    const { emulator } = scene.h;
    const submit = emulator.submitTx;
    return new Promise((reached) => {
      emulator.submitTx = (tx) => {
        emulator.submitTx = submit;
        reached(Buffer.from(tx, "hex"));
        return Promise.resolve(
          CML.hash_transaction(
            CML.Transaction.from_cbor_hex(tx).body(),
          ).to_hex(),
        );
      };
    });
  };
  /**
   * One tick of this process's defender, killed once `point` is reached;
   * resolves with the bytes `point` gave.
   */
  const tickKilledAt = async (point: Promise<Buffer>): Promise<Buffer> => {
    await env.follow();
    const kill = new AbortController();
    const running = inNode(validationTraceDefenseTick(port), kill.signal);
    const reached = await Promise.race([
      point,
      running.then(() => {
        throw new Error("the tick ended before its kill");
      }),
    ]);
    kill.abort();
    await running.catch(() => undefined);
    return reached;
  };
  /**
   * The node process restarts over the same database and chain: its
   * startup retires the leases the ended process held (as startup does,
   * outside the follower write gate this scene does not run), and it runs
   * with fresh Globals, a fresh defender cache and a fresh Lucid.
   */
  const restart = async () => {
    globals = await freshGlobals();
    port = {
      ...port,
      lucid: await env.wallet({ defensePocket: true }),
      cache: new Map(),
    };
    await inNode(
      StateQueueMutationLeasesDB.retireActiveLeasesOfHolders(
        NODE_PROCESS_STATE_QUEUE_LEASE_HOLDERS,
        "retired at the scene's restart",
      ),
    );
  };
  /** The intents journaled under `key`: their hash, bytes and status now. */
  const journaled = (key: string) =>
    env.runtime.runPromise(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const intents = yield* sql.withTransaction(
          Effect.flatMap(followerSqlTx, (tx) =>
            Effect.promise(() =>
              readIntentsByWorkflowIn(tx, postgresDialect, key),
            ),
          ),
        );
        const read = [];
        for (const { txHash, txCbor } of intents)
          read.push({
            txHash: txHash.toString("hex"),
            txCbor,
            status: (yield* readIntentStatus(txHash.toString("hex")))?.kind,
          });
        return read;
      }),
    );
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
    inNode,
    pocketAddress,
    get port() {
      return port;
    },
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
    killAtSubmit,
    forkOnlySubmit,
    tickKilledAt,
    restart,
    journaled,
  };
};

/** The live outputs at `address` on the emulator's ledger. */
export const at = (s: DefenseScene, address: string): UTxO[] =>
  Object.values(s.scene.h.emulator.ledger)
    .filter(({ spent, utxo }) => !spent && utxo.address === address)
    .map(({ utxo }) => utxo);

export const lovelace = (utxos: readonly UTxO[]) =>
  utxos.reduce((sum, utxo) => sum + (utxo.assets.lovelace ?? 0n), 0n);

/** Moves the operator wallet's plain ADA away, leaving it `keep`. */
export const drainBase = async (s: DefenseScene, keep: bigint) => {
  const { funderLucid } = s.scene.h;
  const plain = at(s, s.scene.funder.address).filter(isPlainFundingOutput);
  await landWith(
    funderLucid,
    await funderLucid
      .newTx()
      .collectFrom(plain)
      .pay.ToAddress(generateEmulatorAccount({ lovelace: 0n }).address, {
        lovelace: lovelace(plain) - keep,
      })
      .complete({ presetWalletInputs: plain }),
  );
  await s.env.follow();
};
