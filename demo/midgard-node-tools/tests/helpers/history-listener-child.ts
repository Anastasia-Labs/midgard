import { readFileSync } from "node:fs";
import { createServer as httpServer } from "node:http";
import { createServer as httpsServer } from "node:https";
import { connect, type Socket } from "node:net";

import type { WatcherNativeChainSyncEvent } from "midgard-watcher/native-chain-sync";

import {
  answerChildStatus,
  CHILD_STATUS_ATTEMPT_ENV,
  CHILD_STATUS_CODE_ENV,
  CHILD_STATUS_SPECS_ENV,
} from "../../src/devnet-stack/child-status-channel.js";
import {
  HISTORY_CHILD_SCHEMA,
  historyActorsMatch,
  parseHistoryChildActor,
  parseHistoryChildChallenge,
} from "../../src/devnet-stack/history-child-evidence.js";
import { parseHistoryListenerBinding } from "../../src/devnet-stack/history-listener-evidence.js";
import { createHistoryListenerRequestHandler } from "../../src/devnet-stack/history-listener-request.js";
import { createHistoryWindowSealer } from "../../src/devnet-stack/history-native-window-proof.js";
import {
  type HistoryPinnedListener,
  provePinnedHistoryListener,
} from "../../src/devnet-stack/history-pinned-listener.js";
import {
  historyProofDeadline,
  historyProofRemaining,
} from "../../src/devnet-stack/history-proof-deadline.js";
import { answerHistoryRecorderReadiness } from "../../src/devnet-stack/history-recorder-readiness.js";
import { windowFixture } from "./history-native-window-fixture.js";

const object = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === "object" && !Array.isArray(value);
const run = async () => {
  const path = process.argv[2];
  if (path === undefined)
    throw Error("synthetic listener configuration absent");
  const config: unknown = JSON.parse(readFileSync(path, "utf8"));
  if (
    !object(config) ||
    typeof config.runId !== "string" ||
    !Array.isArray(config.listeners)
  )
    throw Error("synthetic listener configuration malformed");
  const listeners: HistoryPinnedListener[] = [];
  for (const value of config.listeners) {
    if (
      !object(value) ||
      typeof value.hostname !== "string" ||
      typeof value.port !== "number" ||
      typeof value.caPath !== "string"
    )
      throw Error("synthetic listener binding malformed");
    const binding = parseHistoryListenerBinding(value.binding);
    if (binding === null) throw Error("synthetic authority binding absent");
    listeners.push({
      hostname: value.hostname,
      port: value.port,
      ca: readFileSync(value.caPath, "utf8"),
      binding,
    });
  }
  const first = listeners[0];
  if (first === undefined) throw Error("synthetic listener absent");
  const actor = parseHistoryChildActor({
    role: config.role,
    runId: config.runId,
    deploymentFingerprint: first.binding.deploymentIdentityDigest,
    codeStamp: process.env[CHILD_STATUS_CODE_ENV],
    serviceSpecsDigest: process.env[CHILD_STATUS_SPECS_ENV],
    attemptId: process.env[CHILD_STATUS_ATTEMPT_ENV],
    childPid: process.pid,
  });
  if (actor === null) throw Error("synthetic actual child actor malformed");
  if (actor.role === "history-recorder") {
    const fixture = await windowFixture(2161);
    let close: (() => void) | undefined;
    try {
      const windowActor = {
        role: "history-recorder" as const,
        runId: actor.runId,
        deploymentFingerprint: actor.deploymentFingerprint,
        codeStamp: actor.codeStamp,
        serviceSpecsDigest: actor.serviceSpecsDigest,
        attemptId: actor.attemptId,
      };
      const sealer = createHistoryWindowSealer({
        actor: windowActor,
        directories: fixture.directories,
        watcherConfig: fixture.watcherConfig,
        binaryPath: fixture.binaryPath,
      });
      let opening: WatcherNativeChainSyncEvent | undefined;
      let announce: (event: WatcherNativeChainSyncEvent) => void = () => {};
      const acquired = new Promise<WatcherNativeChainSyncEvent>((resolve) => {
        announce = resolve;
      });
      await fixture.startMain(async (event) => {
        if (opening === undefined) {
          opening = event;
          announce(event);
        }
      });
      const event = await acquired;
      const target = fixture.points[2161];
      if (target === undefined) throw Error("synthetic native target absent");
      await sealer.capture(event, target, 20000);
      close = answerHistoryRecorderReadiness({
        actor,
        directories: fixture.directories,
        sealer,
        timeoutMs: 5000,
      });
      console.log(
        JSON.stringify({
          port: 0,
          actor,
          directories: fixture.directories,
          controlPath: `${fixture.root}/synthetic-control.json`,
        }),
      );
      await new Promise<void>((resolve) => {
        for (const signal of ["SIGTERM", "SIGINT"] as const)
          process.once(signal, () => resolve());
      });
    } finally {
      close?.();
      await fixture.close();
    }
    return;
  }
  const stopped = new AbortController();
  const sockets = new Set<Socket>();
  const server =
    actor.role === "history-tunnel"
      ? httpServer((_request, response) => response.writeHead(404).end())
      : (() => {
          if (
            typeof config.directory !== "string" ||
            typeof config.keyPath !== "string" ||
            typeof config.certificatePath !== "string"
          )
            throw Error("synthetic archive files absent");
          const handler = createHistoryListenerRequestHandler({
            directory: config.directory,
            binding: first.binding,
            maximumBudgetMs: 5000,
          });
          return httpsServer(
            {
              key: readFileSync(config.keyPath),
              cert: readFileSync(config.certificatePath),
            },
            (request, response) => {
              void handler(request, response).then((handled) => {
                if (!handled) response.writeHead(404).end();
              });
            },
          );
        })();
  server.on("connection", (socket: Socket) => {
    sockets.add(socket);
    socket.on("error", () => socket.destroy());
    socket.once("close", () => sockets.delete(socket));
  });
  if (actor.role === "history-tunnel") {
    server.on("connect", (request, client: Socket, head: Buffer) => {
      const listener = listeners.find(
        (value) => `${value.hostname}:443` === request.url,
      );
      if (listener === undefined) {
        client.end("HTTP/1.1 403 Forbidden\r\n\r\n");
        return;
      }
      const upstream = connect({ host: "127.0.0.1", port: listener.port });
      sockets.add(upstream);
      upstream.on("error", () => client.destroy());
      upstream.once("close", () => {
        sockets.delete(upstream);
        client.destroy();
      });
      client.once("close", () => upstream.destroy());
      upstream.once("connect", () => {
        client.write("HTTP/1.1 200 Connection Established\r\n\r\n");
        if (head.length > 0) upstream.write(head);
        client.pipe(upstream).pipe(client);
      });
    });
  }
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen(0, "127.0.0.1", resolve);
  });
  const address = server.address();
  if (address === null || typeof address === "string")
    throw Error("synthetic listener did not bind");
  if (actor.role !== "history-tunnel")
    listeners[0] = { ...first, port: address.port };
  const close = answerChildStatus({
    parse: (value) => {
      const request = parseHistoryChildChallenge(value);
      return request !== null &&
        request.operation === "prove" &&
        historyActorsMatch(request.actor, actor)
        ? request
        : null;
    },
    answer: async (request) => {
      const deadline = historyProofDeadline(Math.min(5000, request.budgetMs));
      let ready =
        request.offer !== null && deadline !== null && !stopped.signal.aborted;
      if (request.offer !== null && deadline !== null) {
        for (const listener of listeners) {
          if (
            historyProofRemaining(deadline) === 0 ||
            !(await provePinnedHistoryListener({
              listener,
              offer: request.offer,
              timeoutMs: historyProofRemaining(deadline),
              signal: stopped.signal,
              ...(actor.role === "history-tunnel"
                ? { tunnelPort: address.port }
                : {}),
            }))
          ) {
            ready = false;
            break;
          }
        }
        ready =
          ready &&
          historyProofRemaining(deadline) > 0 &&
          !stopped.signal.aborted;
      }
      return {
        schema: HISTORY_CHILD_SCHEMA,
        challengeId: request.challengeId,
        actor,
        offer: ready ? request.offer : null,
      };
    },
  });
  console.log(JSON.stringify({ port: address.port, actor }));
  await new Promise<void>((resolve) => {
    for (const signal of ["SIGTERM", "SIGINT"] as const)
      process.once(signal, () => resolve());
  });
  stopped.abort();
  close();
  for (const socket of sockets) socket.destroy();
  server.closeAllConnections();
  await new Promise<void>((resolve) => server.close(() => resolve()));
};
void run().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
