import { createHash } from "node:crypto";
import {
  createServer,
  type IncomingMessage,
  type ServerResponse,
} from "node:http";
import type { Duplex } from "node:stream";

import {
  decodeTransaction,
  encodeWebSocketFrame,
  headerHashOf,
  listen,
  readWebSocketFrames,
} from "./local-l1-observation.read-web-socket-frames.js";
import {
  datumHashOf,
  type KupoMatch,
  type KupoResolvedMatch,
  type LocalL1,
  type LocalL1Block,
  type LocalL1Point,
  SLOTS_PER_BLOCK,
  WEBSOCKET_GUID,
} from "./local-l1-observation.websocket-guid.js";

export const startLocalL1Observation = async (): Promise<LocalL1> => {
  const blocks: LocalL1Block[] = [];
  const matches = new Map<string, KupoMatch>();
  const datums = new Map<string, string>();
  let datumRewrite: ((datum: string) => string | null) | null = null;
  let ignoringResolveHashes = false;
  const spent = new Set<string>();
  // A genesis checkpoint, so the first real block always has an ancestor to
  // intersect at — exactly as a synced Kupo always does.
  blocks.push({
    slot: 0,
    headerHash: headerHashOf(0, []),
    height: 0,
    transactions: [],
  });

  const appendBlock = (transactionCbors: readonly string[]): LocalL1Block => {
    const previous = blocks[blocks.length - 1]!;
    const slot = previous.slot + SLOTS_PER_BLOCK;
    const decoded = transactionCbors.map(decodeTransaction);
    const headerHash = headerHashOf(
      slot,
      decoded.map((transaction) => transaction.txHash),
    );
    const block: LocalL1Block = {
      slot,
      headerHash,
      height: previous.height + 1,
      transactions: decoded.map((transaction) => transaction.json),
    };
    blocks.push(block);
    decoded.forEach((transaction, transactionIndex) => {
      // Spends first: a transaction consumes outputs of earlier ones, never its
      // own.
      for (const input of transaction.inputs) {
        spent.add(`${input.transaction.id}#${input.index.toString()}`);
      }
      transaction.outputs.forEach((output, outputIndex) => {
        const datumHash =
          output.datum === null ? null : datumHashOf(output.datum);
        if (datumHash !== null && output.datum !== null) {
          datums.set(datumHash, output.datum);
        }
        matches.set(`${transaction.txHash}#${outputIndex.toString()}`, {
          transaction_index: transactionIndex,
          transaction_id: transaction.txHash,
          output_index: outputIndex,
          address: output.address,
          value: { coins: "0", assets: {} },
          datum_hash: datumHash,
          // Omitted, not nulled, when the output carries no datum.
          ...(datumHash === null ? {} : { datum_type: "inline" as const }),
          script_hash: null,
          created_at: { slot_no: slot, header_hash: headerHash },
          spent_at: null,
        });
      });
    });
    return block;
  };

  /**
   * The `?resolve_hashes` join, as Kupo performs it: `datum` and `script` both
   * become present on the match, each carrying the stored value or `null`. A datum
   * the store cannot produce — which is what {@link LocalL1.rewriteDatums} can
   * turn any datum into — is `null` under a **present** key, never an absent one.
   */
  const resolveHashesOn = (match: KupoMatch): KupoResolvedMatch => {
    const stored =
      match.datum_hash === null ? undefined : datums.get(match.datum_hash);
    const datum =
      stored === undefined
        ? null
        : datumRewrite === null
          ? stored
          : datumRewrite(stored);
    return { ...match, datum, script: null };
  };

  const kupoServer = createServer(
    (request: IncomingMessage, response: ServerResponse) => {
      const url = new URL(request.url ?? "/", "http://127.0.0.1");
      const send = (body: unknown, status = 200): void => {
        response.writeHead(status, {
          "content-type": "application/json;charset=utf-8",
        });
        response.end(JSON.stringify(body));
      };
      const matchPath = /^\/matches\/(\d+)@([0-9a-f]{64})$/u.exec(url.pathname);
      if (matchPath !== null) {
        const key = `${matchPath[2]!}#${Number(matchPath[1]!).toString()}`;
        const match = matches.get(key);
        if (match === undefined) {
          send([]);
          return;
        }
        if (spent.has(key)) {
          send(
            {
              hint:
                "the local L1 harness does not model spent_at; read spends " +
                "against the recorded Kupo in @al-ft/midgard-test-support",
            },
            501,
          );
          return;
        }
        // `?resolve_hashes` is a bare flag (`allowEmptyValue: true`), so its mere
        // presence is the request. Without it — and under the pre-v2.10.0 shape,
        // which ignores it — the match is served with neither join, which is what
        // makes a reader that does not ask, or an index that cannot answer,
        // visibly different from one that got its bytes.
        send([
          url.searchParams.has("resolve_hashes") && !ignoringResolveHashes
            ? resolveHashesOn(match)
            : match,
        ]);
        return;
      }
      const checkpointPath = /^\/checkpoints\/(\d+)$/u.exec(url.pathname);
      if (checkpointPath !== null) {
        const slot = Number(checkpointPath[1]!);
        // Kupo's flexible lookup: the most recent checkpoint at or before the
        // requested slot, `null` when the index has none.
        const ancestor = [...blocks]
          .reverse()
          .find((block) => block.slot <= slot);
        send(
          ancestor === undefined
            ? null
            : { slot_no: ancestor.slot, header_hash: ancestor.headerHash },
        );
        return;
      }
      send({ hint: "unsupported endpoint" }, 404);
    },
  );

  const ogmiosServer = createServer(
    (_request: IncomingMessage, response: ServerResponse) => {
      response.writeHead(400).end();
    },
  );

  const sockets = new Set<Duplex>();
  ogmiosServer.on("upgrade", (request: IncomingMessage, socket: Duplex) => {
    const key = request.headers["sec-websocket-key"];
    if (typeof key !== "string") {
      socket.destroy();
      return;
    }
    sockets.add(socket);
    socket.on("close", () => sockets.delete(socket));
    socket.write(
      [
        "HTTP/1.1 101 Switching Protocols",
        "Upgrade: websocket",
        "Connection: Upgrade",
        `Sec-WebSocket-Accept: ${createHash("sha1")
          .update(`${key}${WEBSOCKET_GUID}`)
          .digest("base64")}`,
        "",
        "",
      ].join("\r\n"),
    );
    let cursor = -1;
    let pendingRollback: LocalL1Point | null = null;
    const tip = (): unknown => {
      const last = blocks[blocks.length - 1]!;
      return { slot: last.slot, id: last.headerHash, height: last.height };
    };
    readWebSocketFrames(socket, (text) => {
      const request_ = JSON.parse(text) as {
        id?: unknown;
        method?: unknown;
        params?: { points?: readonly { slot: number; id: string }[] };
      };
      const reply = (result: unknown): void => {
        socket.write(
          encodeWebSocketFrame(
            JSON.stringify({
              jsonrpc: "2.0",
              method: request_.method,
              result,
              id: request_.id,
            }),
          ),
        );
      };
      if (request_.method === "findIntersection") {
        const points = request_.params?.points ?? [];
        const index = blocks.findIndex((block) =>
          points.some(
            (point) =>
              point.slot === block.slot && point.id === block.headerHash,
          ),
        );
        if (index === -1) {
          socket.write(
            encodeWebSocketFrame(
              JSON.stringify({
                jsonrpc: "2.0",
                method: "findIntersection",
                error: { code: 1000, message: "IntersectionNotFound" },
                id: request_.id,
              }),
            ),
          );
          return;
        }
        cursor = index;
        pendingRollback = {
          slot: blocks[index]!.slot,
          headerHash: blocks[index]!.headerHash,
        };
        reply({
          intersection: {
            slot: blocks[index]!.slot,
            id: blocks[index]!.headerHash,
          },
          tip: tip(),
        });
        return;
      }
      if (request_.method === "nextBlock") {
        if (pendingRollback !== null) {
          const point = pendingRollback;
          pendingRollback = null;
          reply({
            direction: "backward",
            tip: tip(),
            point: { slot: point.slot, id: point.headerHash },
          });
          return;
        }
        const next = blocks[cursor + 1];
        if (next === undefined) {
          // At the tip a real chain-sync simply does not answer until a block
          // arrives. Leaving the request outstanding is that behaviour.
          return;
        }
        cursor += 1;
        reply({
          direction: "forward",
          tip: tip(),
          block: {
            type: "praos",
            era: "conway",
            id: next.headerHash,
            ancestor: blocks[cursor - 1]?.headerHash ?? "genesis",
            height: next.height,
            slot: next.slot,
            size: { bytes: 1 },
            transactions: next.transactions,
            protocol: { version: { major: 10, minor: 0 } },
          },
        });
        return;
      }
      socket.write(
        encodeWebSocketFrame(
          JSON.stringify({
            jsonrpc: "2.0",
            method: request_.method,
            error: { code: 1001, message: "unsupported method" },
            id: request_.id,
          }),
        ),
      );
    });
  });

  const kupoPort = await listen(kupoServer);
  const ogmiosPort = await listen(ogmiosServer);

  return {
    kupoUrl: `http://127.0.0.1:${kupoPort.toString()}`,
    ogmiosUrl: `http://127.0.0.1:${ogmiosPort.toString()}`,
    appendBlock,
    rewriteDatums: (rewrite) => {
      datumRewrite = rewrite;
    },
    ignoreResolveHashes: (ignore) => {
      ignoringResolveHashes = ignore;
    },
    close: async () => {
      for (const socket of sockets) {
        socket.destroy();
      }
      await Promise.all(
        [kupoServer, ogmiosServer].map(
          (server) =>
            new Promise<void>((resolve) => {
              server.close(() => resolve());
            }),
        ),
      );
    },
  };
};
