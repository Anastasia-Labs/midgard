/**
 * The one-shot commands that read the local node (the nonce command's origin
 * read and the finalization proof of the recorded origin) against a node
 * transport that never becomes ready: each is refused within its bound with
 * `LocalNodeNotReadyError`, naming the reason and what the operator checks,
 * instead of waiting for the transport forever.
 */
import { mkdtemp, realpath, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";
import { Cause, Effect, Exit, Option, type Scope } from "effect";
import { describe, expect, it, onTestFinished } from "vitest";

import { localNodeManifestL1OriginProver } from "../src/commands/contract-deployment-info.manifest-l1-origin.js";
import {
  type LocalNodeConfig,
  nodeHubOracleNonceOriginSource,
} from "../src/commands/prepare-hub-oracle-nonce.l1-origin.js";

const READY_TIMEOUT_MS = 3_000;

const handlerModule = fileURLToPath(
  new URL("./fixtures/origin-chain-handler.mjs", import.meta.url),
);

/** A Custom-network local node whose transport binary `sidecar` writes. */
const localNode = async (
  sidecar: (directory: string) => Promise<string>,
): Promise<LocalNodeConfig> => {
  const directory = await realpath(
    await mkdtemp(join(tmpdir(), "midgard-local-node-not-ready-")),
  );
  onTestFinished(() => rm(directory, { recursive: true, force: true }));
  await writeFile(
    join(directory, "shelley-genesis.json"),
    JSON.stringify({ networkMagic: 42 }),
  );
  await writeFile(
    join(directory, "config.json"),
    JSON.stringify({ ShelleyGenesisFile: "shelley-genesis.json" }),
  );
  return {
    NETWORK: "Custom",
    L1_NATIVE_LEDGER: {
      socketPath: join(directory, "node.socket"),
      nodeConfigPath: join(directory, "config.json"),
      binaryPath: await sidecar(directory),
    },
  };
};

const missingBinary = async (directory: string) =>
  join(directory, "missing-sidecar");

const unreachableNode = (directory: string) =>
  writeFakeSidecar({
    path: join(directory, "sidecar"),
    handlerModule,
    options: { tip: "origin", settles: [{ events: [] }], unreachable: true },
  });

const ENTRY_POINTS: ReadonlyArray<
  readonly [
    string,
    (config: LocalNodeConfig) => Effect.Effect<unknown, Error, Scope.Scope>,
  ]
> = [
  [
    "the nonce command's origin read",
    (config: LocalNodeConfig) =>
      nodeHubOracleNonceOriginSource(config, READY_TIMEOUT_MS),
  ],
  [
    "the finalization proof of the recorded origin",
    (config: LocalNodeConfig) =>
      localNodeManifestL1OriginProver(config, READY_TIMEOUT_MS),
  ],
];

const CASES = [
  [
    "the transport binary is missing",
    missingBinary,
    "sidecar_unavailable",
    "check that L1_NODE_TRANSPORT_BINARY_PATH names",
  ],
  [
    "the node socket does not accept",
    unreachableNode,
    "node_unreachable",
    "check that the cardano-node is running and that L1_NODE_SOCKET_PATH names its socket",
  ],
] as const;

describe.each(ENTRY_POINTS)(
  "%s on a local node transport that never becomes ready",
  (_name, open) => {
    it.each(CASES)(
      "R-1: is refused within the bound, naming the reason, when %s",
      async (_case, sidecar, reason, hint) => {
        const config = await localNode(sidecar);
        const startedAt = Date.now();
        const exit = await Effect.runPromiseExit(Effect.scoped(open(config)));
        const elapsedMs = Date.now() - startedAt;
        const failure = Exit.isFailure(exit)
          ? Option.getOrUndefined(Cause.failureOption(exit.cause))
          : undefined;
        expect(failure).toMatchObject({
          _tag: "LocalNodeNotReadyError",
          reason,
        });
        expect(failure?.message).toContain(
          `not ready within ${(READY_TIMEOUT_MS / 1000).toString()} s (${reason}`,
        );
        expect(failure?.message).toContain(hint);
        expect(elapsedMs).toBeLessThan(10_000);
      },
      20_000,
    );
  },
);
