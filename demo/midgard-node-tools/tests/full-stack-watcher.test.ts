import { join } from "node:path";

import { afterEach, expect, it } from "vitest";

import { renderWatcherCompose } from "../src/full-stack/watcher-services.js";
import {
  RecordingProcesses,
  removeStackFixtures,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";

afterEach(removeStackFixtures);

const secrets = {
  WATCHER_ROLLBACK_KEY_FILE: "/private/rollback",
  WATCHER_PROVER_KEY_FILE: "/private/prover",
  WATCHER_AVAILABILITY_KEY_FILE: "/private/availability",
};
const mount = (source: string, target: string) => ({
  type: "bind",
  source,
  target,
});
/** `docker compose config --format json` for demo/midgard-watcher/compose.yaml. */
const rendered = (files: Record<string, string>) => ({
  services: {
    watcher: {
      image: "midgard-watcher:local",
      volumes: [
        mount(
          "/checkout/config/watcher-process.json",
          "/etc/midgard/watcher-process.json",
        ),
        mount(
          "/checkout/config/watcher-runtime.json",
          "/etc/midgard/watcher-runtime.json",
        ),
        mount("/checkout/bundles", "/etc/midgard/bundles"),
        mount("/l1/ipc", "/ipc"),
        mount("/l1/config", "/cardano-config"),
        mount(files.WATCHER_ROLLBACK_KEY_FILE!, "/run/secrets/rollback_key"),
        mount(files.WATCHER_PROVER_KEY_FILE!, "/run/secrets/prover_key"),
        mount(
          files.WATCHER_AVAILABILITY_KEY_FILE!,
          "/run/secrets/availability_key",
        ),
      ],
    },
  },
  volumes: {
    "watcher-state": { name: "midgard-watcher_watcher-state" },
  },
});
async function watcher(files = secrets) {
  const { config } = await stackFixture();
  const env = {
    ...stackEnvironment(config),
    // A node environment key the watcher must never see.
    WATCHER_PROVER_KEY_FILE: "/node/prover",
    MIDGARD_WATCHER_IMAGE_TAG: "node-tag",
  };
  const processes = new RecordingProcesses(config, env);
  processes.responses["watcher-compose-configuration"] = rendered(files);
  const render = () =>
    renderWatcherCompose(processes, {
      env: secrets,
      operationsEndpoint: "http://127.0.0.1:17402",
      l1Directory: "/stack/cardano",
    });
  return { config, processes, render };
}

it("renders the watcher from its own env file and the template ports only", async () => {
  const { config, processes, render } = await watcher();
  const source = await render();
  const [call] = processes.calls;
  // Host scope: no node stack variable reaches the watcher's Compose rendering.
  expect(call!.scope).toBe("host");
  expect(call!.overrides).toEqual({
    ...secrets,
    MIDGARD_L1_CONFIG_DIR: "/stack/cardano",
    MIDGARD_L1_IPC_DIR: join(config.nodeRoot, "cardano/ipc"),
    WATCHER_OPERATIONS_PORT: "17402",
  });
  expect(call!.args).toContain(config.watcher.composeEnvFile);
  expect(Object.keys(source.services)).toEqual(["watcher"]);
  expect(source.services.watcher!.image).toBe(
    "midgard-watcher:${COMPOSE_PROJECT_NAME}",
  );
  expect(source.volumes["watcher-state"]).toEqual({});
});
it("refuses a secret mount that differs from the validated env file", async () => {
  const { render } = await watcher({
    ...secrets,
    WATCHER_PROVER_KEY_FILE: "/node/prover",
  });
  await expect(render()).rejects.toThrow("/run/secrets/prover_key");
});
it("refuses watcher ports that are implicit or already taken", async () => {
  const { config, processes } = await watcher();
  const endpoints = { env: secrets, l1Directory: "/stack/cardano" };
  await expect(
    renderWatcherCompose(processes, {
      ...endpoints,
      operationsEndpoint: "http://127.0.0.1",
    }),
  ).rejects.toThrow("explicit port");
  await expect(
    renderWatcherCompose(processes, {
      ...endpoints,
      operationsEndpoint: `http://127.0.0.1:${config.da.ports.committeeApiBase}`,
    }),
  ).rejects.toThrow("must differ");
});
