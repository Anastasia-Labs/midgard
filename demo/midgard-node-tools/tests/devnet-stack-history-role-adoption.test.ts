import { createHash, randomInt } from "node:crypto";
import { cpSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { createServer } from "node:net";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { admitWatcherNativeRollForwardBlock } from "midgard-watcher";
import { build } from "tsup";
import { beforeAll, expect, it } from "vitest";

import { loadIdentities } from "../src/devnet-stack/identities.js";
import { makeLayout } from "../src/devnet-stack/layout.js";
import { HISTORY_ROLES } from "../src/devnet-stack/watcher-history.js";
import { createHistoryChainFollower } from "../src/devnet-stack/watcher-history-chain.js";
import { readFinalizedManifest } from "../src/devnet-stack/watcher-release.js";
import { windowFixture } from "./helpers/history-native-window-fixture.js";
import {
  type HistoryOwnedChild,
  historyOwnedChild,
} from "./helpers/history-owned-child.js";

const root = fileURLToPath(new URL("../", import.meta.url));
const isLocalProfile = (name: string) => name === "local-devnet-testing";
beforeAll(async () => {
  if (!isLocalProfile(SELECTED_DEPLOYMENT_PROFILE.name)) return;
  await build({
    config: join(root, "tsup.config.ts"),
    esbuildOptions: (options) => {
      options.keepNames = true;
      options.conditions = [...(options.conditions ?? []), "midgard-source"];
      options.loader = { ...(options.loader ?? {}), ".sql": "text" };
    },
    entry: [
      "tests/helpers/history-custom-role-fixture.ts",
      "tests/helpers/history-role-controller-fixture.ts",
    ],
    outDir: ".probe-dist/history-custom-role-fixture",
    clean: true,
    target: "node22",
    noExternal: [/^midgard-node(\/|$)/, /^@al-ft\/midgard-test-support(\/|$)/],
    esbuildPlugins: [
      {
        name: "owned-stable-poll-service-scope",
        setup(builder) {
          builder.onLoad(
            { filter: /[/\\]devnet-stack[/\\]services\.ts$/u },
            async (args) => {
              const source = await readFile(args.path, "utf8");
              return {
                loader: "ts",
                resolveDir: dirname(args.path),
                contents:
                  source.replace(
                    "export const serviceSpecs =",
                    "const originalServiceSpecs =",
                  ) +
                  "\nexport const serviceSpecs = (...args: Parameters<typeof originalServiceSpecs>) => globalThis.historyStableFixtureServiceSpecs ?? originalServiceSpecs(...args);\n",
              };
            },
          );
        },
      },
      {
        name: "fixture-json-bigint",
        setup(builder) {
          builder.onResolve({ filter: /^json-bigint$/ }, () => ({
            path: join(
              root,
              "../midgard-node/node_modules/json-bigint/index.js",
            ),
            external: true,
          }));
        },
      },
    ],
  });
}, 30000);

const exerciseRoles = async (
  mode:
    | "registry"
    | "supervisor"
    | "supervisor-cas-drift"
    | "unix"
    | "unix-stable"
    | "child-refusal"
    | "registry-drift",
) => {
  const f = await windowFixture(2161);
  const layout = makeLayout(join(f.root, "run"));
  const children: HistoryOwnedChild[] = [];
  const setupStarted = performance.now();
  const rejectDatabase = createServer((socket) => socket.destroy());
  try {
    mkdirSync(layout.runDir, { recursive: true });
    const prepared = historyOwnedChild(
      process.execPath,
      [
        join(
          root,
          ".probe-dist/history-custom-role-fixture/history-custom-role-fixture.js",
        ),
        layout.runDir,
        mode,
      ],
      { cwd: root },
    );
    children.push(prepared);
    const preparationTimer = setTimeout(() => void prepared.close(), 20000);
    try {
      expect((await prepared.done).code, prepared.output()).toBe(0);
    } finally {
      clearTimeout(preparationTimer);
    }
    await new Promise<void>((resolve) =>
      rejectDatabase.listen(0, "127.0.0.1", resolve),
    );
    const address = rejectDatabase.address();
    if (address === null || typeof address === "string")
      throw Error("owned rejecting fixture port absent");
    const offset = randomInt(1000, 15000);
    writeFileSync(
      layout.runEnv,
      Object.entries({
        MIDGARD_PHASE4_RUN_DIR: layout.runDir,
        MIDGARD_PHASE4_RUN_ID: "synthetic-history-binding",
        MIDGARD_PHASE4_COMPOSE_PROJECT: "synthetic-history",
        MIDGARD_PHASE4_NETWORK_MAGIC: 42,
        MIDGARD_PHASE4_OGMIOS_PORT: 2337 + offset,
        MIDGARD_PHASE4_KUPO_PORT: 2442 + offset,
        MIDGARD_PHASE4_POSTGRES_PORT: address.port,
        MIDGARD_PHASE4_POSTGRES_USER: "unused",
        MIDGARD_PHASE4_POSTGRES_PASSWORD: "unused-synthetic",
        MIDGARD_PHASE4_POSTGRES_DATABASE: "unused",
        MIDGARD_PHASE4_CARDANO_NODE_IMAGE: "unused",
        MIDGARD_PHASE4_POSTGRES_IMAGE: "unused",
      })
        .map(([key, value]) => `${key}=${value}`)
        .join("\n"),
    );
    console.info(
      "owned fixture public preparation ms",
      Math.round(performance.now() - setupStarted),
    );
    const parsed = f.watcherConfig;
    if (parsed.l1.source.sourceMode !== "local_node")
      throw Error("synthetic local source required");
    const genesisPath = parsed.l1.source.chainSync.genesisConfigPath;
    const genesis = JSON.stringify({
      networkMagic: 42,
      systemStart: "2026-09-30T00:00:00Z",
      slotLength: 1,
      networkId: "Testnet",
    });
    writeFileSync(genesisPath, genesis);
    writeFileSync(
      parsed.l1.source.chainSync.nodeConfigPath,
      JSON.stringify({
        ShelleyGenesisFile: genesisPath,
        TestShelleyHardForkAtEpoch: 0,
        TestConwayHardForkAtEpoch: 0,
      }),
    );
    const raw = {
      ...parsed,
      targetNetwork: "Custom",
      customNetwork: {
        networkMagic: 42,
        slotConfig: {
          zeroTime: Date.parse("2026-09-30T00:00:00Z"),
          zeroSlot: 0,
          slotLength: 1000,
        },
      },
      l1: {
        ...parsed.l1,
        source: {
          ...parsed.l1.source,
          chainSync: {
            ...parsed.l1.source.chainSync,
            genesisIdentitySha256: createHash("sha256")
              .update(genesis)
              .digest("hex"),
          },
        },
        finality: {
          ...parsed.l1.finality,
          depth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
          rollback: {
            beforeFinality: parsed.l1.finality.rollback.beforeFinality,
            afterFinality: parsed.l1.finality.rollback.afterFinality,
            maxDepth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
          },
        },
      },
    };
    const config = JSON.parse(
      readFileSync(layout.watcherProcessConfig, "utf8"),
    );
    config.watcherConfig = raw;
    writeFileSync(layout.watcherProcessConfig, JSON.stringify(config));
    writeFileSync(layout.watcherRuntimeConfig, JSON.stringify(raw));
    mkdirSync(layout.bin, { recursive: true });
    cpSync(f.binaryPath, join(layout.bin, "midgard-l1-node-transport"));
    writeFileSync(
      join(layout.bin, "architecture-g-owner"),
      "synthetic unused owner",
    );
    mkdirSync(join(layout.blueprint, ".."), { recursive: true });

    // Normal software-generated identities in this owned synthetic namespace;
    // no identity material is inspected, logged, fingerprinted or reused.
    loadIdentities(layout);
    const directories = HISTORY_ROLES.map((role) =>
      layout.watcherHistoryArchive(role),
    );
    const policy =
      readFinalizedManifest(layout).contracts.stateQueueMint?.scriptHash;
    if (policy === undefined)
      throw Error("signed fixture state queue policy absent");
    const chain = createHistoryChainFollower({
      directories,
      commitsDirectory: layout.watcherHistoryCommits,
      stateQueuePolicyId: policy,
      admit: admitWatcherNativeRollForwardBlock,
    });
    const tip = f.events[2161];
    if (tip === undefined) throw Error("synthetic raw tip absent");
    await chain.onEvent({
      schemaVersion: tip.schemaVersion,
      kind: "roll_backward",
      point: { kind: "origin" },
      tip: tip.tip,
    });
    await chain.onEvent(tip);
    const [rawDirectory] = f.directories;
    const [firstArchive] = directories;
    if (rawDirectory === undefined || firstArchive === undefined)
      throw Error("synthetic archive fixture absent");
    for (const directory of directories)
      cpSync(join(rawDirectory, "canonical"), join(directory, "canonical"), {
        recursive: true,
      });
    const controller = historyOwnedChild(
      process.execPath,
      [
        join(
          root,
          ".probe-dist/history-custom-role-fixture/history-role-controller-fixture.js",
        ),
        layout.runDir,
        mode,
      ],
      { cwd: root },
    );
    children.push(controller);
    const controllerTimer = setTimeout(() => void controller.close(), 100000);
    try {
      const result = await controller.done;
      console.info(controller.output());
      expect(result.code, controller.output()).toBe(0);
    } finally {
      clearTimeout(controllerTimer);
    }
  } finally {
    await Promise.all(children.map((child) => child.close()));
    await new Promise<void>((resolve) => rejectDatabase.close(() => resolve()));
    await f.close();
  }
};
for (const mode of [
  "registry",
  "supervisor",
  "supervisor-cas-drift",
  "unix",
  "unix-stable",
  "child-refusal",
  "registry-drift",
] as const)
  it.skipIf(!isLocalProfile(SELECTED_DEPLOYMENT_PROFILE.name))(
    `proves actual compiled four-role full2160 readiness through ${mode}`,
    () => exerciseRoles(mode),
    120000,
  );
