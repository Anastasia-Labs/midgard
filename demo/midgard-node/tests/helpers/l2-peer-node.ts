/**
 * A Midgard node for L2 transaction sharing tests: the production `/submit`,
 * `/peer/txs` and `/peer/inventory` handlers behind the production HTTP
 * ingress on a real local HTTP server, over its own Postgres database, with
 * the production tx-queue processor (inline Phase A) drained on demand and,
 * when it has peers, the production forwarder fiber running.
 *
 * Each node reads `POSTGRES_DB` once, when its runtime is built, so two nodes
 * in one test file each keep their own database.
 */
import { once } from "node:events";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";

import { encodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpRouter } from "@effect/platform";
import { NodeHttpServer } from "@effect/platform-node";
import { SqlClient } from "@effect/sql";
import {
  assetsToValue,
  CML,
  type LucidEvolution,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect, Fiber, Layer, ManagedRuntime, Ref } from "effect";

import type { NodeUtxo } from "../../src/commands/command-utils.js";
import {
  postPeerInventoryHandler,
  postPeerTxsHandler,
} from "../../src/commands/http/peer-routes.js";
import { V1_SUBMISSION_MEDIA_TYPE } from "../../src/commands/http/submission-capacity.js";
import { submissionCheckConfig } from "../../src/commands/http/submission-check.js";
import {
  isSubmitRequest,
  makeSubmitRequestHandler,
} from "../../src/commands/http/submit.js";
import { submittedTxRules } from "../../src/commands/http/transaction-admission.js";
import {
  createHttpIngress,
  withHttpIngress,
} from "../../src/commands/listen-http-ingress.js";
import { buildTransferTxWithMinFee } from "../../src/commands/transfer-build-core.js";
import { MempoolLedgerDB } from "../../src/database/index.js";
import { l2TxPeerForwarderFiber } from "../../src/fibers/l2-tx-peer-forwarder.js";
import { txQueueProcessorDrainOnce } from "../../src/fibers/tx-queue-processor.js";
import {
  encodePeerList,
  PEER_MEDIA_TYPE,
  peerAuthHeaders,
} from "../../src/l2-peer/protocol.js";
import {
  AdmissionWriter,
  AdmissionWriterLive,
} from "../../src/services/admission-writer.js";
import { Database } from "../../src/services/database.js";
import type { HttpRateLimitPolicy } from "../../src/services/http-rate-limits.js";
import {
  ContractDeploymentIdentity,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
  type NodeConfigDep,
  publishMempoolLedgerDelta,
  rebuildAdmissionInputClaimsOnStartup,
  ValidationPool,
  type ValidationPoolService,
} from "../../src/services/index.js";
import { mempoolLedgerCacheLayer } from "../../src/services/mempool-ledger-cache.js";
import { createSubmitCheckPool } from "../../src/services/submit-check-pool.js";
import { WriteBehindLive } from "../../src/services/write-behind.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "../midgard-output-helpers.js";
import { provideDatabaseLayers, resetApplicationTables } from "../utils.js";
import { testDriverRecompute, testWrite } from "./driver-recompute.js";
import { withPostgresDb } from "./l2-peer-database.js";
import { fetchLocalUtxos } from "./local-l2-transfer.js";
import { testingInlineCekVerdicts } from "./validation-pool-cek.js";

/** The hub-oracle policy id of the deployment these nodes belong to. */
export const PEER_TEST_DEPLOYMENT = "5e".repeat(28);

/** Validation on the calling fiber, as a pool of no workers runs it. */
const inlineValidationPool = {
  ...testingInlineCekVerdicts,
  poolSize: 0,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  ready: Effect.void,
  runPhaseAChunk: () => Effect.die("the inline path never reaches a worker"),
} as unknown as ValidationPoolService;

/** An operator: its seed phrase, signing key and key hash. */
export const operatorFromSeed = (seedPhrase: string) => {
  const wallet = walletFromSeed(seedPhrase, { network: "Preprod" });
  const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
  const publicKey = privateKey.to_public();
  return {
    seedPhrase,
    address: wallet.address,
    paymentKey: wallet.paymentKey,
    keyHash: publicKey.hash().to_hex(),
    signer: {
      verificationKeyHex: Buffer.from(publicKey.to_raw_bytes()).toString("hex"),
      sign: (message: Uint8Array) => privateKey.sign(message).to_hex(),
    },
  };
};
export type TestOperator = ReturnType<typeof operatorFromSeed>;

/** One spendable L2 output of `owner`, as a node ledger holds it. */
export const l2Output = (
  owner: TestOperator | { readonly address: string },
  txHashByte: number,
  lovelace: bigint,
) => {
  const txHash = txHashByte.toString(16).padStart(2, "0").repeat(32);
  const outputCbor = Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(owner.address),
      assetsToValue({ lovelace }),
    ).to_cbor_bytes(),
  );
  return {
    txHash,
    outputIndex: 0,
    outrefCbor: makeOutRefCbor(txHash, 0),
    outputCbor,
    address: owner.address,
    assets: { lovelace },
  };
};
export type L2Output = NodeUtxo;

/** A signed self-transfer of `lovelace` by `owner`, spending `inputs`. */
export const buildTransfer = (
  owner: TestOperator,
  inputs: readonly L2Output[],
  lovelace: bigint,
  config: Pick<NodeConfigDep, "MIN_FEE_A" | "MIN_FEE_B">,
) =>
  buildTransferTxWithMinFee({
    senderAddress: owner.address,
    destinationAddress: owner.address,
    signer: CML.PrivateKey.from_bech32(owner.paymentKey),
    availableUtxos: inputs,
    requestedAssets: { lovelace },
    network: "Preprod",
    networkId: 0n,
    minFeeA: config.MIN_FEE_A,
    minFeeB: config.MIN_FEE_B,
  });

export type PeerNodeOptions = {
  /** The node's database; the file's test shard when omitted. */
  readonly database?: string;
  readonly operator: TestOperator;
  /** Key hashes of the active operator set this node has read from L1. */
  readonly activeOperators: readonly string[];
  /** `L2_TX_PEERS`; with any, the forwarder fiber runs. */
  readonly peers?: readonly string[];
  readonly lucid: LucidEvolution;
  /** The rest of the node's `Lucid` service, beside `api: lucid`. */
  readonly l1?: Record<string, unknown>;
  readonly contracts?: unknown;
  readonly config?: Partial<NodeConfigDep>;
  /** The port to serve on; any free one when omitted. */
  readonly port?: number;
  /** The HTTP ingress rate-limit policy; the public defaults when omitted. */
  readonly ingressPolicy?: HttpRateLimitPolicy;
  /** The check worker module; the production one, loaded first, if omitted. */
  readonly checkWorker?: URL;
};

export type PostReply = {
  readonly status: number;
  readonly headers: Headers;
  readonly body: Buffer;
};

/** Posts `body` to `url`, as an HTTP client does. */
export const post = async (
  url: string,
  body: Uint8Array,
  headers: Record<string, string>,
): Promise<PostReply> => {
  const response = await fetch(url, { method: "POST", headers, body });
  return {
    status: response.status,
    headers: response.headers,
    body: Buffer.from(await response.arrayBuffer()),
  };
};

/** A peer request signed by `signer` for `path` of `deployment`. */
export const signedPeerPost = (
  baseUrl: string,
  path: string,
  items: readonly Uint8Array[],
  signer: TestOperator["signer"],
  { deployment = PEER_TEST_DEPLOYMENT, headers = {} } = {} as {
    readonly deployment?: string;
    readonly headers?: Record<string, string>;
  },
) => {
  const body = encodePeerList(items);
  return post(`${baseUrl}${path}`, body, {
    "content-type": PEER_MEDIA_TYPE,
    ...peerAuthHeaders(signer, path, deployment, body),
    ...headers,
  });
};

/** Submits a transaction to `/submit` as a user does. */
export const submitTx = (
  baseUrl: string,
  txCbor: Uint8Array,
  headers: Record<string, string> = {},
) =>
  post(
    `${baseUrl}/submit`,
    encodeMidgardProofSubmission({ transactionCbor: Buffer.from(txCbor) }),
    { "content-type": V1_SUBMISSION_MEDIA_TYPE, ...headers },
  );

const ADMISSION_SETTLE_MS = 30_000;

export const startPeerNode = async (options: PeerNodeOptions) => {
  const database = options.database ?? process.env.POSTGRES_DB ?? "";
  const configLayer = Layer.effect(
    NodeConfig,
    Effect.map(NodeConfig, (config) => ({
      ...config,
      POSTGRES_DB: database,
      L1_OPERATOR_SEED_PHRASE: options.operator.seedPhrase,
      L2_TX_PEERS: options.peers ?? [],
      ...options.config,
    })),
  ).pipe(Layer.provide(NodeConfig.layer));
  const base = Layer.mergeAll(configLayer, Database.layer, Globals.Default);
  const layer = Layer.mergeAll(
    WriteBehindLive,
    AdmissionWriterLive,
    mempoolLedgerCacheLayer,
    Layer.succeed(ValidationPool, inlineValidationPool),
    Layer.succeed(Lucid, {
      api: options.lucid,
      ...options.l1,
    } as unknown as Lucid),
    Layer.succeed(
      MidgardContracts,
      (options.contracts ?? {
        hubOracle: { policyId: PEER_TEST_DEPLOYMENT },
      }) as MidgardContracts,
    ),
    Layer.succeed(
      ContractDeploymentIdentity,
      ContractDeploymentIdentity.make({
        kind: "derived",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      }),
    ),
  ).pipe(Layer.provideMerge(base));
  const runtime = await withPostgresDb(database, async () => {
    const built = ManagedRuntime.make(layer);
    await built.runtime();
    return built;
  });
  const run = <A, E>(
    effect: Effect.Effect<
      A,
      E,
      ManagedRuntime.ManagedRuntime.Context<typeof runtime>
    >,
  ) => runtime.runPromise(effect);
  const globals = await run(Globals);
  const config = await run(NodeConfig);
  await run(
    Ref.set(globals.OPERATOR_SET, {
      set: {
        active: options.activeOperators.map((key) => ({
          datum: { key: { Key: { key } } },
        })),
      },
    } as never),
  );
  // The node's follower view, as its driver applies one at startup.
  await run(
    Effect.flatMap(testDriverRecompute(), (driver) =>
      driver.run("peer node start"),
    ),
  );
  // The backlog's input claims, as the node rebuilds them before serving.
  await run(rebuildAdmissionInputClaimsOnStartup());

  // One check worker per node, shared by `/submit` and `/peer/txs`.
  const checkPool = createSubmitCheckPool({
    size: 1,
    maxQueued: config.SUBMIT_INGRESS_WORKER_QUEUE,
    maxQueuedBytes: config.SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES,
    config: submissionCheckConfig(config, await run(submittedTxRules)),
    ...(options.checkWorker && { entry: options.checkWorker }),
  });
  if (options.checkWorker === undefined && !(await checkPool.whenReady(30_000)))
    throw new Error("the submit check worker did not load");
  // Every request this node's server receives, by path.
  const received = new Map<string, number>();
  const app = HttpRouter.empty.pipe(
    HttpRouter.post("/peer/txs", postPeerTxsHandler(Effect.void, checkPool)),
    HttpRouter.post("/peer/inventory", postPeerInventoryHandler),
  );
  // `/submit` is the raw handler, as in the node; both charge one ingress.
  const ingress = createHttpIngress(
    options.ingressPolicy === undefined
      ? {}
      : { policy: options.ingressPolicy },
  );
  const handler = await run(
    NodeHttpServer.makeHandler(withHttpIngress(app, ingress)),
  );
  const submitHandler = await run(
    makeSubmitRequestHandler({
      wakeTxQueueProcessor: Effect.void,
      checkPool,
      ingress,
    }),
  );
  const server = createServer((request, response) => {
    const path = (request.url ?? "").split("?", 1)[0]!;
    received.set(path, (received.get(path) ?? 0) + 1);
    if (isSubmitRequest(request)) submitHandler(request, response);
    else handler(request, response);
  });
  server.listen(options.port ?? 0, "127.0.0.1");
  await once(server, "listening");
  const port = (server.address() as AddressInfo).port;
  const url = `http://127.0.0.1:${port.toString()}`;

  const forwarder =
    (options.peers ?? []).length > 0
      ? runtime.runFork(l2TxPeerForwarderFiber)
      : undefined;

  // `/submit` and `/peer/txs` answer before the row is written: every read
  // of the admission table first waits for what the writer acknowledged.
  const admissions = () =>
    run(
      Effect.gen(function* () {
        yield* (yield* AdmissionWriter).flush;
        const sql = yield* SqlClient.SqlClient;
        return yield* sql<{
          tx_id: Buffer;
          status: string;
          submit_source: string;
          reject_code: string | null;
        }>`SELECT tx_id, status, submit_source, reject_code
          FROM tx_admissions ORDER BY arrival_seq`;
      }),
    );
  const mempool = () =>
    run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql<{ tx_id: Buffer; tx: Buffer }>`
          SELECT tx_id, tx FROM mempool ORDER BY tx_id`;
      }),
    );

  let closed = false;

  /** Drains the admission queue until no row is queued or validating. */
  const drain = async () => {
    const deadline = performance.now() + ADMISSION_SETTLE_MS;
    for (;;) {
      await run(Effect.flatMap(AdmissionWriter, (writer) => writer.flush));
      await run(txQueueProcessorDrainOnce());
      const open = (await admissions()).filter(
        ({ status }) => status === "queued" || status === "validating",
      );
      if (open.length === 0) return;
      if (performance.now() >= deadline)
        throw new Error(
          `Admission left ${open.length.toString()} rows open after draining`,
        );
      await new Promise((resolve) => setTimeout(resolve, 20));
    }
  };

  return {
    url,
    port,
    config,
    globals,
    operator: options.operator,
    run,
    drain,
    admissions,
    mempool,
    /** How many requests this node's server received at `path`. */
    received: (path: string) => received.get(path) ?? 0,
    /** Seeds this node's ledger with spendable L2 outputs, and publishes
     * them to its validation cache as a deposit projection does. */
    seedLedger: (outputs: readonly L2Output[]) =>
      run(
        testWrite(
          MempoolLedgerDB.insert(
            outputs.map((output) => ({
              tx_id: Buffer.from(output.txHash, "hex"),
              outref: output.outrefCbor,
              output: output.outputCbor,
              address: output.address,
              source_event_id: null,
            })),
          ),
        ).pipe(
          Effect.zipRight(
            publishMempoolLedgerDelta(
              globals,
              {
                full: false,
                upserts: outputs.map((output) => [
                  output.outrefCbor.toString("hex"),
                  output.outputCbor,
                ]),
                deletes: [],
              },
              config.VALIDATION_LEDGER_DELTA_LOG_MAX,
            ),
          ),
        ),
      ),
    /** This node's spendable L2 outputs of `address`, from its ledger. */
    spendable: (address: string) => run(fetchLocalUtxos(address)),
    /** Stops serving, as a node that is down: connections are refused. */
    stopServing: () =>
      new Promise<void>((resolve, reject) => {
        server.closeAllConnections();
        server.close((error) => (error ? reject(error) : resolve()));
      }),
    /** Serves again at the same address. */
    resumeServing: async () => {
      server.listen(port, "127.0.0.1");
      await once(server, "listening");
    },
    close: async () => {
      if (closed) return;
      closed = true;
      if (forwarder !== undefined)
        await runtime.runPromise(Fiber.interrupt(forwarder));
      if (server.listening) {
        server.closeAllConnections();
        await new Promise<void>((resolve) => server.close(() => resolve()));
      }
      await checkPool.close();
      await runtime.dispose();
    },
  };
};
export type PeerNode = Awaited<ReturnType<typeof startPeerNode>>;

/** A node on this file's test shard, its tables emptied first. */
export const startShardNode = async (options: PeerNodeOptions) => {
  await Effect.runPromise(provideDatabaseLayers(resetApplicationTables));
  return startPeerNode(options);
};

/**
 * A parent transfer of `user` spending `funding`, and a child spending the
 * parent's output, built against a scratch node that accepted the parent.
 * The scratch node is closed; `start` must give a node holding `funding`.
 */
export const buildChain = async (
  start: () => Promise<PeerNode>,
  user: TestOperator,
  funding: L2Output,
) => {
  const node = await start();
  try {
    const parent = await buildTransfer(
      user,
      [funding],
      10_000_000n,
      node.config,
    );
    if ((await submitTx(node.url, parent.txCbor)).status !== 202)
      throw new Error("The scratch node did not admit the parent");
    await node.drain();
    const outputs = (await node.spendable(user.address)).filter(
      (utxo) => utxo.txHash === parent.txIdHex,
    );
    const child = await buildTransfer(user, outputs, 5_000_000n, node.config);
    return { parent, child };
  } finally {
    await node.close();
  }
};
