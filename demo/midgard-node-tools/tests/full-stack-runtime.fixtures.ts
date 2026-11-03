import { mkdir, writeFile } from "node:fs/promises";
import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";
import { join } from "node:path";

import { runtimeInputDigest, runtimeSteps } from "../src/full-stack/runtime.js";
import {
  RecordingProcesses,
  removeStackFixtures,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";

export const manifestId = "a".repeat(64);
export const committeeReady = (signerIndex: number) => ({
  ready: true,
  deployment: {
    configuredFingerprint: manifestId,
    storeMatchesConfigured: true,
  },
  peer: { signerIndex, localPeerId: `peer-${signerIndex}` },
});

const listen = async (server: Server) => {
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  return (server.address() as AddressInfo).port;
};
const servers: Server[] = [];
/** Closes the stacks' HTTP services and removes their fixtures. */
export async function closeRuntimeStacks() {
  await Promise.all(
    servers
      .splice(0)
      .map((server) => new Promise((resolve) => server.close(resolve))),
  );
  await removeStackFixtures();
}

export const DEPLOYED_FOLLOWER_INPUTS = {
  HUB_ORACLE_ONE_SHOT_TX_HASH: "ab".repeat(32),
  HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: "0",
  L1_ORIGIN: `7.${"cd".repeat(32)}`,
};

/**
 * Compose over a DA database the committee has not started on yet: the L1
 * follower store exists only once a committee runs, so any committee command
 * before then fails as it does on a fresh stack.
 */
class FreshDaDatabase extends RecordingProcesses {
  committeeStarted = false;
  override async command(
    id: string,
    command: string,
    args: readonly string[],
    ...rest: [Record<string, string>?, string?, ("stack" | "host")?]
  ) {
    const committee = args.some((arg) => /^da-committee-\d+$/u.test(arg));
    if (committee && args.includes("up")) this.committeeStarted = true;
    else if (
      committee &&
      !this.committeeStarted &&
      (args.includes("run") || args.includes("exec"))
    )
      throw new Error(
        'L1ProviderTransientError: L1 provider store unavailable: relation "l1_follower_cursor" does not exist',
      );
    return super.command(id, command, args, ...rest);
  }
}

/** A generated run with one committee member and live HTTP services on loopback. */
export async function stack() {
  const state: {
    nodeStarted: boolean;
    committee: { status: number; body: unknown };
  } = {
    nodeStarted: false,
    committee: { status: 200, body: committeeReady(0) },
  };
  const committee = createServer((_, response) => {
    response.statusCode = state.committee.status;
    response.end(JSON.stringify(state.committee.body));
  });
  const services = createServer((request, response) => {
    const body: Record<string, unknown> = {
      "/readyz": state.nodeStarted
        ? { ready: true, reasons: [], settlement: { state: "waiting" } }
        : undefined,
      "/v1/status": {
        liveness: "live",
        readiness: "ready",
        readinessReasons: [],
        deploymentFingerprint: manifestId,
        launchScope: { complete: true },
      },
    };
    const value = body[request.url ?? ""];
    response.statusCode = value === undefined ? 503 : 200;
    response.end(JSON.stringify(value ?? { ready: false }));
  });
  servers.push(committee, services);
  const committeePort = await listen(committee);
  const servicesUrl = `http://127.0.0.1:${await listen(services)}`;
  const { directory, config } = await stackFixture((sample) => {
    Object.assign(sample, { endpoint: servicesUrl, timeoutMs: 1 });
    sample.da.members = sample.da.members.slice(0, 1);
  });
  config.da.ports.committeeApiBase = committeePort;
  const env = stackEnvironment(config);
  const processes = new FreshDaDatabase(config, env);
  // What the deployment steps restore before the services step runs.
  Object.assign(processes.env, DEPLOYED_FOLLOWER_INPUTS);
  processes.responses["runtime-start"] = () => {
    state.nodeStarted = true;
  };
  processes.responses["native-owner-pin"] = { sha: "c".repeat(64) };
  await mkdir(join(directory, "run/services"), { recursive: true });
  await mkdir(join(directory, "deploymentInfo"));
  await writeFile(
    join(directory, "deploymentInfo/contract-deployment-info.json"),
    JSON.stringify({ manifestId }),
  );
  await writeFile(
    join(directory, "run/services/committee-0.json"),
    JSON.stringify({
      deployment: { fingerprint: manifestId },
      da_committee: { members: [{ signer_index: 0, peer_id: "peer-0" }] },
    }),
  );
  await writeFile(
    join(directory, "watcher.env"),
    `WATCHER_ROLLBACK_KEY_FILE=${join(directory, "rollback-key")}\n`,
  );
  await writeFile(
    join(directory, "run/runtime.json"),
    JSON.stringify({
      inputDigest: await runtimeInputDigest(processes),
      compose: join(directory, "run/services/compose.json"),
      operationsEndpoint: servicesUrl,
      committeeServices: ["da-committee-0"],
    }),
  );
  const [configuration, , services_] = runtimeSteps(processes);
  return {
    directory,
    processes,
    state,
    configuration: configuration!,
    services: services_!,
  };
}
