import { randomBytes } from "node:crypto";
import { join } from "node:path";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import { generateMnemonic } from "bip39";

import { psql } from "./chain.js";
import type { DeployContext } from "./deploy.js";
import { PREFLIGHT_L1_ORIGIN, recordedL1Origin } from "./deployment-origin.js";
import { readJsonIfPresent, writeDurableJson } from "./durable.js";
import {
  FUNDING_POLICY,
  ROLE_COLLATERAL_LOVELACE,
  roleBudgetLovelace,
  totalRoleFundingLovelace,
} from "./funding-budget.js";
import { walletInfo } from "./identities.js";
import {
  furtherNodePorts,
  type Layout,
  type RunEnv,
  servicePorts,
} from "./layout.js";
import { lockOwner } from "./lock.js";
import {
  type HubOracleOneShot,
  nodeEnvironment,
  PREFLIGHT_ONE_SHOT,
  requireNodeAccepts,
} from "./node-env.js";
import type { ServiceSpec } from "./supervisor.js";

/**
 * A further node is another operator's node on the run's deployment: its own
 * wallets, libp2p identity, database, ledger store and ports. It holds no DA
 * committee key; the run's committee admits it as a producer. The stack's own
 * node is node 1, so further nodes count from 2.
 */
export const FURTHER_NODE_ROLES = [
  "operator",
  "merge",
  "referenceScript",
  "settlement",
] as const;
export type FurtherNodeRole = (typeof FURTHER_NODE_ROLES)[number];

export type FurtherNode = {
  readonly node: number;
  readonly seeds: Record<FurtherNodeRole, string>;
  /** 32-byte hex seed of the node's `seed:` libp2p key source. */
  readonly libp2p: string;
};

/** What the funding transaction pays one further node's four wallets. */
const FURTHER_NODE_FUNDING_LOVELACE = FURTHER_NODE_ROLES.reduce(
  (sum, role) => sum + roleBudgetLovelace(role) + ROLE_COLLATERAL_LOVELACE,
  0n,
);
/** The genesis UTxO, less a margin for the funding transaction's own fee. */
const GENESIS_SPENDABLE_LOVELACE =
  BigInt(FUNDING_POLICY.totalSupplyLovelace) -
  BigInt(FUNDING_POLICY.delegatedSupplyLovelace) -
  1_000_000_000n;

type FurtherNodesRecord = {
  readonly schemaVersion: "midgard-devnet-further-nodes-v1";
  readonly nodes: readonly Omit<FurtherNode, "node">[];
};

const recordPath = (layout: Layout) => join(layout.state, "further-nodes.json");

/**
 * The run's further nodes. `up --further-nodes` fixes their number on the
 * run's first start: the funding transaction pays their wallets and the
 * committee's manifests admit their libp2p identities, and neither is ever
 * rewritten. Without `requested` this reads what the run recorded.
 */
export const loadFurtherNodes = (
  layout: Layout,
  requested?: { readonly count: number; readonly runFunded: boolean },
): readonly FurtherNode[] => {
  let record = readJsonIfPresent<FurtherNodesRecord>(recordPath(layout));
  const recorded = record?.nodes.length ?? 0;
  if (requested !== undefined && requested.count !== recorded) {
    if (record !== undefined || requested.runFunded)
      throw new Error(
        `${layout.runDir} was created with ${recorded} further node(s); a run's further nodes are fixed when it is created`,
      );
    const affordable =
      (GENESIS_SPENDABLE_LOVELACE - totalRoleFundingLovelace()) /
      FURTHER_NODE_FUNDING_LOVELACE;
    if (BigInt(requested.count) > affordable)
      throw new Error(
        `the genesis allocation funds at most ${affordable} further node(s)`,
      );
    record = {
      schemaVersion: "midgard-devnet-further-nodes-v1",
      nodes: Array.from({ length: requested.count }, () => ({
        seeds: Object.fromEntries(
          FURTHER_NODE_ROLES.map((role) => [role, generateMnemonic(256)]),
        ) as Record<FurtherNodeRole, string>,
        libp2p: randomBytes(32).toString("hex"),
      })),
    };
    writeDurableJson(recordPath(layout), record);
  }
  return (record?.nodes ?? []).map((node, index) => ({
    ...node,
    node: index + 2,
  }));
};

export const furtherNode = (layout: Layout, node: number): FurtherNode => {
  const found = loadFurtherNodes(layout).find((entry) => entry.node === node);
  if (found === undefined)
    throw new Error(
      `${layout.runDir} has no node ${node}; create the run with up --further-nodes`,
    );
  return found;
};

export const furtherNodeDatabase = (run: RunEnv, node: number) =>
  `${run.postgresDatabase}_node_${node}`;

/** The node's own paths; everything else is the run's. */
export const furtherNodeLayout = (layout: Layout, node: number): Layout => {
  const state = join(layout.state, `node-${node}`);
  return {
    ...layout,
    state,
    nodeData: join(state, "data"),
    producerManifest: join(
      layout.deploymentInfo,
      `da-producer-${node}-manifest.json`,
    ),
    supervisorPid: join(state, "supervisor.pid"),
    supervisorSpecs: join(state, "supervisor.specs"),
    supervisorEvents: join(state, "logs/supervisor.ndjson"),
    serviceLog: (name: string) => join(state, `logs/${name}.log`),
  };
};

/**
 * The run as node `further.node` sees it: its wallets, libp2p identity,
 * database and paths in place of the stack node's. Never derive the committee
 * from it: the committee's first member is the stack node's operator key.
 */
export const furtherNodeContext = (
  context: DeployContext,
  further: FurtherNode,
): DeployContext => ({
  ...context,
  furtherNode: further.node,
  // The deployment manifest names the address its reference scripts were
  // published to, and a node refuses a manifest that names another.
  referenceScriptDeployAddress: walletInfo(
    context.identities.seeds.referenceScript,
  ).address,
  layout: furtherNodeLayout(context.layout, further.node),
  run: {
    ...context.run,
    postgresDatabase: furtherNodeDatabase(context.run, further.node),
  },
  identities: {
    ...context.identities,
    seeds: { ...context.identities.seeds, ...further.seeds },
    libp2p: { ...context.identities.libp2p, producer: further.libp2p },
  },
});

/** One budget and one collateral output per wallet of every further node. */
export const furtherNodeFundingOutputs = (
  nodes: readonly FurtherNode[],
): string[] =>
  nodes.flatMap((node) =>
    FURTHER_NODE_ROLES.flatMap((role) => {
      const { address } = walletInfo(node.seeds[role]);
      return [
        "--tx-out",
        `${address}+${roleBudgetLovelace(role)}`,
        "--tx-out",
        `${address}+${ROLE_COLLATERAL_LOVELACE}`,
      ];
    }),
  );

const producerMultiaddr = async (port: number, libp2pSeed: string) =>
  `/ip4/127.0.0.1/tcp/${port}/p2p/${(await loadDaLibp2pIdentity(`seed:${libp2pSeed}`)).peerId}`;

/** The live supervisors of the run's further nodes. */
export const liveFurtherNodes = (
  layout: Layout,
): { readonly name: string; readonly pid: number }[] =>
  loadFurtherNodes(layout).flatMap(({ node }) => {
    const pid = lockOwner(furtherNodeLayout(layout, node).supervisorPid);
    return pid === undefined ? [] : [{ name: `node-${node}`, pid }];
  });

/** Where the committee and the other nodes reach a further node's libp2p host. */
export const furtherNodeMultiaddr = (run: RunEnv, further: FurtherNode) =>
  producerMultiaddr(furtherNodePorts(run, further.node).libp2p, further.libp2p);

export const nodeHttpUrl = (run: RunEnv, node: number) =>
  `http://127.0.0.1:${node === 1 ? servicePorts(run).nodeHttp : furtherNodePorts(run, node).http}`;

/** The base URLs of every node of the run but `node`. */
export const otherNodeUrls = (
  layout: Layout,
  run: RunEnv,
  node: number,
): string[] =>
  [1, ...loadFurtherNodes(layout).map((entry) => entry.node)]
    .filter((other) => other !== node)
    .map((other) => nodeHttpUrl(run, other));

/** Created once, never recreated; the node migrates it when it starts. */
export const ensureFurtherNodeDatabase = async (
  layout: Layout,
  run: RunEnv,
  node: number,
) => {
  const name = furtherNodeDatabase(run, node);
  await psql(
    layout,
    run,
    "postgres",
    `SELECT 'CREATE DATABASE ${name}' WHERE NOT EXISTS (SELECT 1 FROM pg_database WHERE datname = '${name}')\\gexec\n`,
    `create-${name}`,
  );
};

/**
 * A further node's `listen` environment. A node that starts without a ledger
 * asks every other node of the run for the confirmed one, the stack's node
 * first.
 */
export const furtherNodeListenEnvironment = async (
  context: DeployContext,
  further: FurtherNode,
  deployment: {
    readonly oneShot: HubOracleOneShot;
    readonly l1Origin: L1Origin;
  },
): Promise<Record<string, string>> => {
  const { layout, run, identities } = context;
  const snapshotPeers = [
    await producerMultiaddr(
      servicePorts(run).producerLibp2p,
      identities.libp2p.producer,
    ),
    ...(await Promise.all(
      loadFurtherNodes(layout)
        .filter((other) => other.node !== further.node)
        .map((other) => furtherNodeMultiaddr(run, other)),
    )),
  ];
  return nodeEnvironment({
    ...furtherNodeContext(context, further),
    ...deployment,
    role: "listen",
    snapshotPeers,
    txPeers: otherNodeUrls(layout, run, further.node),
  });
};

/**
 * Runs the node's configuration parser over every further node's `listen`
 * environment, with stand-ins for the facts only the deployment fixes
 * (`preflightNodeConfig`).
 */
export const preflightFurtherNodeConfigs = async (
  context: DeployContext,
): Promise<void> => {
  for (const further of loadFurtherNodes(context.layout))
    await requireNodeAccepts(
      await furtherNodeListenEnvironment(context, further, {
        oneShot: PREFLIGHT_ONE_SHOT,
        l1Origin: PREFLIGHT_L1_ORIGIN,
      }),
    );
};

/** The `listen` process of a further node. */
export const furtherNodeSpec = async (
  context: DeployContext,
  oneShot: HubOracleOneShot,
  further: FurtherNode,
): Promise<ServiceSpec> => {
  const { layout, run } = context;
  const base = nodeHttpUrl(run, further.node);
  return {
    name: `node-${further.node}`,
    command: process.execPath,
    args: [join(layout.nodeRoot, "dist/index.js"), "listen"],
    cwd: layout.nodeRoot,
    env: await furtherNodeListenEnvironment(context, further, {
      oneShot,
      l1Origin: recordedL1Origin(layout, oneShot),
    }),
    healthUrl: `${base}/healthz`,
    readyUrl: `${base}/readyz`,
  };
};
