import { join } from "node:path";

import { writeTextFileAtomic } from "midgard-node/files/atomic-write";

import { composeYaml, override } from "./compose-yaml.js";
import type { StackProcesses } from "./process.js";

/**
 * The native owner's digest in the node image the services step built: the
 * actual owner, never a hash copied from another checkout. The pin runs
 * before node.env exists, and Compose appends env_file lists across files, so
 * the base file's .env, with every role's secrets, would reach it: its own
 * overlay drops every env_file.
 */
export async function pinNativeOwner(
  processes: StackProcesses,
  servicesDirectory: string,
  compose: string,
) {
  const pinOverlay = join(servicesDirectory, "owner-pin.yaml");
  await writeTextFileAtomic(
    pinOverlay,
    `${composeYaml({
      services: { "midgard-node": { env_file: override([]) } },
    })}\n`,
    { mode: 0o644 },
  );
  const pin = (await processes.compose(
    "native-owner-pin",
    [
      "run",
      "--rm",
      "--no-deps",
      "--entrypoint",
      "node",
      "midgard-node",
      "-e",
      "console.log(JSON.stringify({sha:require('node:fs').readFileSync('/app/native/architecture-g-owner.sha256','utf8').trim()}))",
    ],
    compose,
    pinOverlay,
  )) as { sha: string };
  if (!/^[0-9a-f]{64}$/.test(pin.sha))
    throw new Error("Invalid native owner image pin");
  return pin.sha;
}

/**
 * Checks the producer's DA transport before the node container serves it:
 * binding its host port only before the container first starts. Docker holds
 * its port even while the node restarts or is not ready yet, so ask Compose.
 * `ps --format json` prints one object per container, and nothing when none
 * match.
 */
export async function preflightProducerTransport(
  processes: StackProcesses,
  servicesDirectory: string,
  compose: string,
) {
  const alreadyRunning =
    (await processes.compose(
      "producer-container-state",
      [
        "ps",
        "--format",
        "json",
        "--status",
        "running",
        "--status",
        "restarting",
        "midgard-node",
      ],
      compose,
    )) !== null;
  await processes.node(
    "producer-da-preflight",
    [
      "da-libp2p-preflight",
      "--mode",
      alreadyRunning ? "dial-only" : "bind-listen",
      "--json",
    ],
    {
      MIDGARD_DEPLOYMENT_MANIFEST_PATH: join(
        servicesDirectory,
        "producer-host.json",
      ),
      DA_LIBP2P_PRIVATE_KEY_SOURCE:
        processes.env[processes.config.da.producerTransportEnv]!,
    },
  );
}
