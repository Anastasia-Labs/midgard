import { createHash, randomInt } from "node:crypto";
import {
  cpSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { build } from "tsup";
import { beforeAll, expect, it } from "vitest";

import { makeLayout } from "../src/devnet-stack/layout.js";
import { historyOwnedChild } from "./helpers/history-owned-child.js";

const root = fileURLToPath(new URL("../", import.meta.url));
const localProfile = (name: string) => name === "local-devnet-testing";
beforeAll(async () => {
  if (!localProfile(SELECTED_DEPLOYMENT_PROFILE.name)) return;
  await build({
    config: join(root, "tsup.config.ts"),
    entry: [
      "tests/helpers/history-custom-role-fixture.ts",
      "tests/helpers/history-recorder-cancellation-fixture.ts",
    ],
    outDir: ".probe-dist/history-recorder-cancellation-fixture",
    clean: true,
    target: "node22",
    noExternal: [/^midgard-node(\/|$)/, /^@al-ft\/midgard-test-support(\/|$)/],
    esbuildOptions(options) {
      options.conditions = [...(options.conditions ?? []), "midgard-source"];
      options.loader = { ...(options.loader ?? {}), ".sql": "text" };
    },
  });
}, 30000);

it
  .skipIf(!localProfile(SELECTED_DEPLOYMENT_PROFILE.name))
  .each(["pending", "backoff"] as const)(
  "drains actual compiled recorder native %s startup on its own SIGTERM",
  async (mode) => {
    const temporary = mkdtempSync("/var/tmp/midgard-recorder-cancel-");
    const layout = makeLayout(join(temporary, "run"));
    const owned = [];
    try {
      mkdirSync(layout.runDir, { recursive: true, mode: 0o700 });
      const prepared = historyOwnedChild(
        process.execPath,
        [
          join(
            root,
            ".probe-dist/history-recorder-cancellation-fixture/history-custom-role-fixture.js",
          ),
          layout.runDir,
        ],
        { cwd: root },
      );
      owned.push(prepared);
      const preparationTimer = setTimeout(() => void prepared.close(), 20000);
      try {
        expect((await prepared.done).code, prepared.output()).toBe(0);
      } finally {
        clearTimeout(preparationTimer);
      }
      const offset = randomInt(1000, 15000);
      writeFileSync(
        layout.runEnv,
        Object.entries({
          MIDGARD_PHASE4_RUN_DIR: layout.runDir,
          MIDGARD_PHASE4_RUN_ID: "synthetic-history-binding",
          MIDGARD_PHASE4_PORT_OFFSET: offset,
          MIDGARD_PHASE4_COMPOSE_PROJECT: "synthetic-recorder-cancel",
          MIDGARD_PHASE4_NETWORK_MAGIC: 42,
          MIDGARD_PHASE4_OGMIOS_PORT: 2337 + offset,
          MIDGARD_PHASE4_KUPO_PORT: 2442 + offset,
          MIDGARD_PHASE4_POSTGRES_PORT: 1,
          MIDGARD_PHASE4_POSTGRES_USER: "unused",
          MIDGARD_PHASE4_POSTGRES_PASSWORD: "unused-synthetic",
          MIDGARD_PHASE4_POSTGRES_DATABASE: "unused",
          MIDGARD_PHASE4_CARDANO_NODE_IMAGE: "unused",
          MIDGARD_PHASE4_POSTGRES_IMAGE: "unused",
        })
          .map(([k, v]) => `${k}=${v}`)
          .join("\n"),
      );
      const genesisPath = join(temporary, "genesis.json");
      const nodePath = join(temporary, "node.json");
      const genesis = JSON.stringify({
        networkMagic: 42,
        systemStart: "2026-09-30T00:00:00Z",
        slotLength: 1,
        networkId: "Testnet",
      });
      writeFileSync(genesisPath, genesis);
      writeFileSync(
        nodePath,
        JSON.stringify({
          ShelleyGenesisFile: genesisPath,
          TestShelleyHardForkAtEpoch: 0,
          TestConwayHardForkAtEpoch: 0,
        }),
      );
      const config = JSON.parse(
        readFileSync(layout.watcherProcessConfig, "utf8"),
      );
      const raw = config.watcherConfig;
      raw.l1.finality.depth = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;
      raw.l1.finality.rollback.maxDepth =
        DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;
      delete raw.l1.finality.rollback.postFinalityRecoveryMaxDepth;
      raw.targetNetwork = "Custom";
      raw.customNetwork = {
        networkMagic: 42,
        slotConfig: {
          zeroTime: Date.parse("2026-09-30T00:00:00Z"),
          zeroSlot: 0,
          slotLength: 1000,
        },
      };
      Object.assign(raw.l1.source.chainSync, {
        socketPath: join(temporary, "node.socket"),
        nodeConfigPath: nodePath,
        genesisConfigPath: genesisPath,
        genesisIdentitySha256: createHash("sha256")
          .update(genesis)
          .digest("hex"),
      });
      writeFileSync(layout.watcherProcessConfig, JSON.stringify(config));
      writeFileSync(layout.watcherRuntimeConfig, JSON.stringify(raw));
      mkdirSync(layout.bin, { recursive: true });
      cpSync(
        join(layout.transportRoot, "dist/native/midgard-l1-node-transport"),
        join(layout.bin, "midgard-l1-node-transport"),
      );
      writeFileSync(
        join(layout.bin, "architecture-g-owner"),
        "synthetic unused owner",
      );
      const controller = historyOwnedChild(
        process.execPath,
        [
          join(
            root,
            ".probe-dist/history-recorder-cancellation-fixture/history-recorder-cancellation-fixture.js",
          ),
          layout.runDir,
          mode,
        ],
        { cwd: root },
      );
      owned.push(controller);
      const timer = setTimeout(() => void controller.close(), 30000);
      try {
        const result = await controller.done;
        console.info(controller.output());
        expect(result.code, controller.output()).toBe(0);
      } finally {
        clearTimeout(timer);
      }
    } finally {
      await Promise.all(owned.map((child) => child.close()));
      rmSync(temporary, { recursive: true, force: true });
    }
  },
  60000,
);
