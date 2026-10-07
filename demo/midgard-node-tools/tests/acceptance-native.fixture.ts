import { createHash } from "node:crypto";
import {
  appendFileSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
} from "node:fs";
import { createServer } from "node:http";
import { tmpdir } from "node:os";
import { join } from "node:path";
import type { Duplex } from "node:stream";
import { fileURLToPath } from "node:url";

import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";
import * as watcher from "midgard-watcher";
import * as native from "midgard-watcher/native-chain-sync";
import {
  config,
  readIdentityFixture,
} from "midgard-watcher/tests/l1/native-chain-sync.config";
import { afterAll } from "vitest";

import type { AcceptanceNativeReadConfig } from "../src/devnet-stack/acceptance-native-config.js";

export const POINT = { blockHash: "bb".repeat(32), blockNo: "11", slot: "102" };

const handlerModule = fileURLToPath(
  new URL("./acceptance-native.handler.mjs", import.meta.url),
);
// Every fake node transport of a test file lives in one directory. The watcher
// native config closes the shared transports after each test.
const fakeDirectory = realpathSync(
  mkdtempSync(join(tmpdir(), "midgard-acceptance-native-")),
);
let fakeCount = 0;
afterAll(() => {
  rmSync(fakeDirectory, { recursive: true, force: true });
});

export type NativeCommand = "forward" | "rollback" | "exit" | "error";

/**
 * A node transport whose node serves the acceptance native boundary (see
 * acceptance-native.handler.mjs). `mode` "no-current" opens streams without
 * delivering the current block.
 */
export const nativeFixture = async (mode = "current") => {
  fakeCount += 1;
  const base = join(fakeDirectory, `transport-${String(fakeCount)}`);
  const journalPath = `${base}.journal`;
  const commandsPath = `${base}.commands`;
  const binaryPath = await writeFakeSidecar({
    path: base,
    handlerModule,
    options: { mode, journal: journalPath, commands: commandsPath },
  });
  const journal = (): string[] => {
    try {
      return readFileSync(journalPath, "utf8").split("\n").filter(Boolean);
    } catch {
      return [];
    }
  };
  const count = (line: string): number =>
    journal().filter((entry) => entry === line).length;
  return {
    binaryPath,
    /** Whether the node saw a chain-sync stream opened. */
    opened: (): boolean => count("opened") > 0,
    /** Whether every stream the node saw opened has ended. */
    closed: (): boolean =>
      count("opened") > 0 && count("closed") === count("opened"),
    /** Has the node apply `command` to every open stream. */
    send: (command: NativeCommand): void =>
      appendFileSync(commandsPath, `${command}\n`),
    unsafeReadIdentityFileForTest: readIdentityFixture,
  };
};

const frame = (text: string): Buffer => {
  const body = Buffer.from(text);
  const head =
    body.length < 126
      ? Buffer.from([0x81, body.length])
      : Buffer.from([0x81, 126, body.length >> 8, body.length & 255]);
  return Buffer.concat([head, body]);
};
export const ogmiosFixture = async (
  options: {
    wrongAcquire?: boolean;
    wrongSelected?: boolean;
    stallQuery?: boolean;
    wrongEnvelope?: boolean;
    wrongLineageTip?: boolean;
    stallDepth?: boolean;
    stallBlock?: boolean;
  } = {},
) => {
  const sockets = new Set<Duplex>();
  const requestWaiters: { method: string; resolve(): void }[] = [];
  const closeWaiters: { count: number; resolve(): void }[] = [];
  const requests: { method: string; params: Record<string, unknown> }[] = [];
  let closeFrame!: () => void;
  const closeFrameReady = new Promise<void>((resolve) => {
    closeFrame = resolve;
  });
  let closed = 0;
  let selected = {
    id: POINT.blockHash,
    slot: Number(POINT.slot),
    height: Number(POINT.blockNo),
  };
  let beforeReply: (() => void) | undefined;
  const server = createServer();
  server.on("upgrade", (request, socket) => {
    sockets.add(socket);
    socket.once("end", () => socket.end());
    socket.once("close", () => {
      sockets.delete(socket);
      closed += 1;
      for (const waiter of closeWaiters)
        if (closed >= waiter.count) waiter.resolve();
    });
    const accept = createHash("sha1")
      .update(
        request.headers["sec-websocket-key"]! +
          "258EAFA5-E914-47DA-95CA-C5AB0DC85B11",
      )
      .digest("base64");
    socket.write(
      `HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ${accept}\r\n\r\n`,
    );
    let nextBlock = 0;
    let buffer = Buffer.alloc(0);
    socket.on("data", (chunk: Buffer) => {
      buffer = Buffer.concat([buffer, chunk]);
      while (buffer.length >= 6) {
        const length = buffer[1]! & 127;
        const size =
          length === 126
            ? buffer.length >= 8
              ? buffer.readUInt16BE(2)
              : Infinity
            : length;
        const offset = length === 126 ? 4 : 2;
        if (buffer.length < offset + 4 + size) return;
        const opcode = buffer[0]! & 15;
        const mask = buffer.subarray(offset, offset + 4);
        const data = Buffer.from(
          buffer.subarray(offset + 4, offset + 4 + size),
        );
        for (let i = 0; i < data.length; i += 1)
          data[i] = data[i]! ^ mask[i % 4]!;
        buffer = buffer.subarray(offset + 4 + size);
        if (opcode === 8) closeFrame();
        if (opcode !== 1) continue;
        const rpc = JSON.parse(data.toString()) as {
          id: number;
          method: string;
          params: Record<string, unknown>;
        };
        requests.push(rpc);
        for (const waiter of requestWaiters)
          if (waiter.method === rpc.method) waiter.resolve();
        let result: unknown;
        if (rpc.method === "findIntersection") {
          const origin = (rpc.params.points as unknown[])[0] === "origin";
          const requested = (
            rpc.params.points as { slot: number; id: string }[]
          )[0];
          if (
            options.stallDepth &&
            !origin &&
            requests.filter((r) => r.method === "findIntersection").length >= 3
          )
            continue;
          result = {
            intersection: origin
              ? "origin"
              : {
                  id: options.wrongSelected ? "dd".repeat(32) : requested!.id,
                  slot: requested!.slot,
                },
            tip: origin
              ? { id: "aa".repeat(32), slot: 101, height: 10 }
              : {
                  ...selected,
                  ...(options.wrongLineageTip &&
                  requests.filter((r) => r.method === "findIntersection")
                    .length === 3
                    ? { id: "dd".repeat(32) }
                    : {}),
                },
          };
        } else if (rpc.method === "acquireLedgerState") {
          result = {
            acquired: "ledgerState",
            point: {
              id: options.wrongAcquire ? "dd".repeat(32) : POINT.blockHash,
              slot: 102,
            },
          };
        } else if (rpc.method === "nextBlock") {
          if (options.stallBlock) continue;
          nextBlock += 1;
          result =
            nextBlock === 1
              ? {
                  direction: "backward",
                  point: { id: "aa".repeat(32), slot: 101 },
                  tip: selected,
                }
              : {
                  direction: "forward",
                  block: {
                    id: POINT.blockHash,
                    slot: 102,
                    height: 11,
                    transactions: [
                      {
                        id: "ee".repeat(32),
                        inputs: [],
                        references: [],
                        mint: {},
                        redeemers: [],
                        cbor: "84a3008001800200a0f5f6",
                      },
                    ],
                  },
                  tip: selected,
                };
        } else {
          if (options.stallQuery) continue;
          beforeReply?.();
          // Preserve an integer that JSON.parse would round. The adapter must
          // hand the exact frame through to the independently owned decoder.
          socket.write(
            frame(
              `{"jsonrpc":"2.0","id":${rpc.id},"method":"${rpc.method}","result":[{"value":{"ada":{"lovelace":9007199254740993}}}]}`,
            ),
          );
          continue;
        }
        socket.write(
          frame(
            JSON.stringify({
              jsonrpc: "2.0",
              id: options.wrongEnvelope ? rpc.id + 1 : rpc.id,
              method: rpc.method,
              result,
            }),
          ),
        );
      }
    });
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const port = (server.address() as { port: number }).port;
  return {
    endpoint: `ws://127.0.0.1:${port}`,
    requests,
    closeFrameReady,
    waitForRequest: (method: string): Promise<void> =>
      requests.some((r) => r.method === method)
        ? Promise.resolve()
        : new Promise((resolve) => requestWaiters.push({ method, resolve })),
    waitForClosed: (count: number): Promise<void> =>
      closed >= count
        ? Promise.resolve()
        : new Promise((resolve) => closeWaiters.push({ count, resolve })),
    closed: () => closed,
    active: () => sockets.size,
    beforeReply: (callback: () => void) => {
      beforeReply = callback;
    },
    advance: () => {
      selected = { id: "cc".repeat(32), slot: 103, height: 12 };
    },
    close: async () => {
      for (const socket of sockets) socket.destroy();
      await new Promise<void>((resolve) => server.close(() => resolve()));
    },
  };
};

export const readConfig = (
  endpoint: string,
  transport: Readonly<{ binaryPath: string }>,
  assertUnchanged = async () => {},
): AcceptanceNativeReadConfig => {
  const base = config();
  const source = base.l1.source;
  if (source.sourceMode !== "local_node") throw new Error("fixture source");
  return {
    native,
    watcher,
    watcherConfig: {
      ...base,
      l1: {
        ...base.l1,
        source: {
          ...source,
          queryServices: [
            { kind: "ogmios", identity: "test-ogmios", endpoint },
          ],
        },
      },
    },
    binaryPath: transport.binaryPath,
    assertUnchanged,
    binding: {
      runDir: "/owned/run",
      deploymentManifestId: "11".repeat(32),
      releaseAuthoritySha256: "22".repeat(32),
      codeStamp: "33".repeat(32),
      nativeBinarySha256: "44".repeat(32),
      publicConfigurationSha256: {},
    },
  };
};
