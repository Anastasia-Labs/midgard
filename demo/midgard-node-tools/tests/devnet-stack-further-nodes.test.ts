import { existsSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { ConfigProvider, Effect } from "effect";
import { ledgerSnapshotPeersConfig } from "midgard-node/services/config.ledger-snapshot-peers";
import { afterEach, describe, expect, it } from "vitest";

import { DEPLOYMENT_PROFILE } from "../src/devnet-stack/build.js";
import { PREFLIGHT_L1_ORIGIN } from "../src/devnet-stack/deployment-origin.js";
import {
  FUNDING_POLICY,
  totalRoleFundingLovelace,
} from "../src/devnet-stack/funding-budget.js";
import {
  furtherNodeContext,
  furtherNodeFundingOutputs,
  furtherNodeListenEnvironment,
  furtherNodeMultiaddr,
  loadFurtherNodes,
  otherNodeUrls,
  preflightFurtherNodeConfigs,
} from "../src/devnet-stack/further-nodes.js";
import { loadIdentities, walletInfo } from "../src/devnet-stack/identities.js";
import { makeLayout, type RunEnv } from "../src/devnet-stack/layout.js";
import {
  nodeEnvironment,
  PREFLIGHT_ONE_SHOT,
} from "../src/devnet-stack/node-env.js";
import { supervisorPaths } from "../src/devnet-stack/services.js";

const dirs: string[] = [];
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const run: RunEnv = {
  runId: "further",
  composeProject: "further",
  networkMagic: 424242,
  ogmiosPort: 2337,
  kupoPort: 1442,
  postgresPort: 5432,
  postgresUser: "midgard",
  postgresPassword: "midgard",
  postgresDatabase: "midgard",
  cardanoImage: "cardano-node",
  postgresImage: "postgres",
  portOffset: 0,
};

/** A generated run with its identities and nothing else. */
const generatedRun = (furtherNodes?: number) => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-stack-further-"));
  dirs.push(dir);
  const layout = makeLayout(dir);
  const identities = loadIdentities(layout);
  if (furtherNodes !== undefined)
    loadFurtherNodes(layout, { count: furtherNodes, runFunded: false });
  return {
    layout,
    run,
    identities,
    artifacts: {
      nativeOwnerBinary: join(layout.bin, "architecture-g-owner"),
      nativeOwnerSha256: "00".repeat(32),
      transportBinary: join(layout.bin, "midgard-l1-node-transport"),
    },
  };
};
const deployment = {
  oneShot: PREFLIGHT_ONE_SHOT,
  l1Origin: PREFLIGHT_L1_ORIGIN,
};

describe("a run's further nodes", () => {
  it("are fixed when the run is created and read back unchanged", () => {
    const { layout } = generatedRun(2);
    const nodes = loadFurtherNodes(layout);
    expect(nodes.map((node) => node.node)).toEqual([2, 3]);
    const secrets = nodes.flatMap((node) => [
      ...Object.values(node.seeds),
      node.libp2p,
    ]);
    expect(new Set(secrets).size).toBe(10);
    expect(loadFurtherNodes(layout, { count: 2, runFunded: true })).toEqual(
      nodes,
    );
    expect(() =>
      loadFurtherNodes(layout, { count: 3, runFunded: false }),
    ).toThrow("was created with 2 further node(s)");
  });

  it("cannot be added once the run's funding transaction exists", () => {
    const { layout } = generatedRun();
    expect(() =>
      loadFurtherNodes(layout, { count: 1, runFunded: true }),
    ).toThrow("was created with 0 further node(s)");
    expect(loadFurtherNodes(layout)).toEqual([]);
    expect(existsSync(join(layout.state, "further-nodes.json"))).toBe(false);
  });

  it("are paid a budget and a collateral output per wallet, inside the genesis allocation", () => {
    const { layout } = generatedRun(2);
    const outputs = furtherNodeFundingOutputs(loadFurtherNodes(layout));
    const values = outputs.filter((argument) => argument !== "--tx-out");
    // Two nodes, four wallets each, two outputs per wallet.
    expect(values).toHaveLength(16);
    expect(new Set(values.map((value) => value.split("+")[0])).size).toBe(8);
    const paid = values.reduce(
      (sum, value) => sum + BigInt(value.split("+")[1]!),
      0n,
    );
    expect(totalRoleFundingLovelace() + paid + 1_000_000_000n).toBeLessThan(
      BigInt(FUNDING_POLICY.totalSupplyLovelace) -
        BigInt(FUNDING_POLICY.delegatedSupplyLovelace),
    );
  });

  it("are refused beyond what the genesis allocation funds", () => {
    const { layout } = generatedRun();
    expect(() =>
      loadFurtherNodes(layout, { count: 1_000, runFunded: false }),
    ).toThrow("the genesis allocation funds at most");
    expect(loadFurtherNodes(layout)).toEqual([]);
  });
});

describe("a further node's environment", () => {
  it("is another operator's node on the same deployment", async () => {
    const context = generatedRun(2);
    const [second, third] = loadFurtherNodes(context.layout);
    const stack = nodeEnvironment({
      ...context,
      ...deployment,
      role: "listen",
    });
    const env = await furtherNodeListenEnvironment(
      context,
      second!,
      deployment,
    );
    // Its own keys, database, store, ports and producer manifest.
    for (const own of [
      "L1_OPERATOR_SEED_PHRASE",
      "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
      "L1_SETTLEMENT_SEED_PHRASE",
      "L1_REFERENCE_SCRIPT_SEED_PHRASE",
      "DA_LIBP2P_PRIVATE_KEY_SOURCE",
      "POSTGRES_DB",
      "LEDGER_MPF_DB_PATH",
      "PORT",
      "MIDGARD_NODE_URL",
      "PROM_METRICS_PORT",
      "MIDGARD_DEPLOYMENT_MANIFEST_PATH",
    ])
      expect(env[own], own).not.toBe(stack[own]);
    expect(env.L1_OPERATOR_SEED_PHRASE).toBe(second!.seeds.operator);
    expect(env.POSTGRES_DB).toBe("midgard_node_2");
    // The deployment's reference scripts are at the stack node's wallet; the
    // node refuses a deployment manifest that names another address.
    const published = walletInfo(
      stack.L1_REFERENCE_SCRIPT_SEED_PHRASE!,
    ).address;
    expect(stack.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS).toBeUndefined();
    expect(env.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS).toBe(published);
    expect(
      nodeEnvironment({
        ...furtherNodeContext(context, second!),
        ...deployment,
        role: "command",
      }).L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS,
    ).toBe(published);
    // The committee's second key stays with the stack's node.
    expect(stack.DA_COSIGNER_SEED_PHRASE).toBeDefined();
    expect(env.DA_COSIGNER_SEED_PHRASE).toBeUndefined();
    // The deployment is the stack's.
    for (const shared of [
      "MIDGARD_DEPLOYMENT_PROFILE",
      "HUB_ORACLE_ONE_SHOT_TX_HASH",
      "L1_ORIGIN",
      "L1_NODE_SOCKET_PATH",
      "DA_THRESHOLD",
    ])
      expect(env[shared], shared).toBe(stack[shared]);
    // A node without a ledger asks the stack's node, then the other node.
    const peers = env.LEDGER_SNAPSHOT_PEERS!.split(",");
    expect(peers).toHaveLength(2);
    expect(peers[0]).toMatch(/^\/ip4\/127\.0\.0\.1\/tcp\/39002\/p2p\//);
    expect(peers[1]).toBe(await furtherNodeMultiaddr(run, third!));
    const parsed = await Effect.runPromise(
      ledgerSnapshotPeersConfig.pipe(
        Effect.withConfigProvider(
          ConfigProvider.fromMap(new Map(Object.entries(env))),
        ),
      ),
    );
    expect(parsed.map((peer) => peer.multiaddr)).toEqual(peers);
    expect(env.L2_TX_PEERS).toBe("http://127.0.0.1:3000,http://127.0.0.1:3002");
  });

  it("gives every node of the run the others as transaction peers", () => {
    const { layout } = generatedRun(2);
    expect(otherNodeUrls(layout, run, 1)).toEqual([
      "http://127.0.0.1:3001",
      "http://127.0.0.1:3002",
    ]);
    expect(otherNodeUrls(layout, run, 3)).toEqual([
      "http://127.0.0.1:3000",
      "http://127.0.0.1:3001",
    ]);
    expect(otherNodeUrls(generatedRun().layout, run, 1)).toEqual([]);
  });

  it("keeps its supervisor's records apart from the stack's", () => {
    const context = generatedRun(1);
    const own = furtherNodeContext(
      context,
      loadFurtherNodes(context.layout)[0]!,
    );
    const stack = supervisorPaths(context);
    const further = supervisorPaths(own);
    // The stack's supervisor stops every service it finds in its own records.
    expect(further.pidDir).not.toBe(stack.pidDir);
    expect(further.events).not.toBe(stack.events);
    expect(own.layout.supervisorPid).not.toBe(context.layout.supervisorPid);
    expect(further.runDir).toBe(stack.runDir);
  });

  // Widened: the compiled profile's literal type depends on the build.
  const compiledProfile: string = SELECTED_DEPLOYMENT_PROFILE.name;
  it.skipIf(compiledProfile !== DEPLOYMENT_PROFILE)(
    `is accepted by the node's configuration parser (needs ${DEPLOYMENT_PROFILE} compiled)`,
    async () => {
      await preflightFurtherNodeConfigs(generatedRun(2));
    },
  );
});
