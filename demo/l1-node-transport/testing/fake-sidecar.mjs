// A scripted stand-in for the `midgard-l1-node-transport` sidecar, for tests
// of the transport's callers. It speaks frame protocol version 1 (README.md)
// on stdin/stdout. A handler module decides what the "node" answers; the
// fake applies the protocol itself: the hello exchange, request ids, stream
// sequence numbers, the credit window, acks and close.
//
// The transport spawns its binary with no arguments and only PATH in the
// environment, so `writeFakeSidecar` writes an executable wrapper that names
// the handler module and its options.

import { chmod, writeFile } from "node:fs/promises";
import { fileURLToPath, pathToFileURL } from "node:url";

import { decodeCbor, encodeCbor } from "./cbor.mjs";

export { decodeCbor, encodeCbor };

/** The handler module of a node ledger holding one script credential. */
export const LEDGER_HANDLER = fileURLToPath(
  new URL("./ledger-handler.mjs", import.meta.url),
);

const FRAME_PROTOCOL_VERSION = 1;

// ------------------------------------------------------------- points

const hexBytes = (hex) => Uint8Array.from(Buffer.from(hex, "hex"));

/** A point as the fake's handlers see it: "origin" or {slot, hash (hex)}. */
const encodePoint = (point) =>
  point === "origin" || point?.kind === "origin"
    ? []
    : [BigInt(point.slot), hexBytes(point.hash ?? point.blockHash)];

const decodePoint = (value) =>
  value.length === 0
    ? "origin"
    : { slot: BigInt(value[0]), hash: Buffer.from(value[1]).toString("hex") };

/** A tip: {point, blockNo}. */
const encodeTip = (tip) => [encodePoint(tip.point), BigInt(tip.blockNo)];

export const samePoint = (a, b) =>
  a === "origin" || b === "origin"
    ? a === b
    : BigInt(a.slot) === BigInt(b.slot) && a.hash === b.hash;

// ------------------------------------------------------------ serving

/**
 * Serves one sidecar session on stdin/stdout with the given handler. The
 * handler may define:
 *
 * - hello({socketPath, networkMagic}): undefined, {nodeToClientVersion}, or
 *   {fatal: {code, message, status}} to refuse the session.
 * - openStream({points, startSeq, window}, stream): {intersection, tip}
 *   or {notFound: tip}; may be async. `stream` delivers events within credit.
 *   As the sidecar does, the fake delivers a rollback to the intersection
 *   before the handler's events whenever the intersection is not the first
 *   requested point (the consumer's position).
 * - acquire(point | undefined), ledgerQuery({query, ...params}):
 *   Uint8Array (the raw answer) or {error: {code, message}}.
 * - submit(tx, era): {accepted: true} | {rejection: Uint8Array} | {error}.
 * - hasTx(txIdHex): boolean; sizes(): {capacity, size, txCount}.
 * - ignoreInputEnd: true keeps the process running after stdin closes.
 */
export const serveFakeSidecar = (handler) => {
  const write = (header, payload = new Uint8Array()) => {
    const encoded = encodeCbor(header);
    const lengths = Buffer.alloc(8);
    lengths.writeUInt32BE(encoded.length, 0);
    lengths.writeUInt32BE(payload.length, 4);
    process.stdout.write(Buffer.concat([lengths, encoded, payload]));
  };
  const fatal = (code, message, status = 1) => {
    write({ type: "fatal", code, message });
    process.stdout.end(() => process.exit(status));
  };
  const streams = new Map();
  let helloDone = false;
  let acquired;

  const makeStream = (id, startSeq, ackedSeq, window) => {
    const state = {
      id,
      lastSeq: BigInt(startSeq),
      acked: BigInt(ackedSeq ?? startSeq),
      window,
      queue: [],
      closed: false,
      opened: false,
      closeListeners: [],
    };
    const flush = () => {
      while (state.opened && !state.closed && state.queue.length > 0) {
        if (state.queue[0].kind === "skip") {
          state.queue.shift();
          state.lastSeq += 1n;
          continue;
        }
        if (state.lastSeq - state.acked >= BigInt(state.window)) {
          // A failure is not an event and takes no credit: as with the
          // sidecar, the events held back by the credit are never sent.
          const failure = state.queue.find((entry) => entry.kind === "fail");
          if (failure === undefined) return;
          state.queue = [failure];
        }
        const next = state.queue.shift();
        if (next.kind === "fail") {
          write({
            type: "cs_failed",
            stream: id,
            code: next.code,
            message: next.message,
          });
          state.closed = true;
          streams.delete(id);
          return;
        }
        state.lastSeq += 1n;
        if (next.kind === "forward")
          write(
            {
              type: "cs_roll_forward",
              stream: id,
              seq: state.lastSeq,
              point: encodePoint(next.point),
              blockNo: BigInt(next.blockNo),
              blockType: next.blockType ?? 7,
              prevHash:
                next.prevHash == null ? undefined : hexBytes(next.prevHash),
              tip: encodeTip(next.tip),
            },
            next.block,
          );
        else
          write({
            type: "cs_roll_backward",
            stream: id,
            seq: state.lastSeq,
            point: encodePoint(next.point),
            tip: encodeTip(next.tip),
          });
      }
    };
    const api = {
      get closed() {
        return state.closed;
      },
      /** Events delivered and not yet acknowledged. */
      get unacked() {
        return Number(state.lastSeq - state.acked);
      },
      get queued() {
        return state.queue.length;
      },
      rollForward(event) {
        state.queue.push({ kind: "forward", ...event });
        flush();
      },
      rollBackward(event) {
        state.queue.push({ kind: "backward", ...event });
        flush();
      },
      fail(code, message = code) {
        state.queue.push({ kind: "fail", code, message });
        flush();
      },
      skipSequence() {
        state.queue.push({ kind: "skip" });
        flush();
      },
      onClose(listener) {
        state.closeListeners.push(listener);
      },
    };
    return { state, api, flush };
  };

  const answer = (id, header, payload) => write({ ...header, id }, payload);
  const refuse = (id, code, message = code) =>
    write({ type: "error", id, code, message });

  const handle = async (header, payload) => {
    if (!helloDone) {
      if (header.type !== "hello")
        return fatal("client_protocol_violation", "hello first", 64);
      if (header.version !== FRAME_PROTOCOL_VERSION)
        return fatal("version_unsupported", `version ${header.version}`, 64);
      const result =
        (await handler.hello?.({
          socketPath: header.socketPath,
          networkMagic: header.networkMagic,
        })) ?? {};
      if (result.fatal !== undefined)
        return fatal(
          result.fatal.code,
          result.fatal.message ?? result.fatal.code,
          result.fatal.status ?? 69,
        );
      helloDone = true;
      write({
        type: "hello_ok",
        version: FRAME_PROTOCOL_VERSION,
        nodeToClientVersion: result.nodeToClientVersion ?? 32784,
      });
      return;
    }
    const id = header.id;
    switch (header.type) {
      case "cs_open": {
        const { state, api, flush } = makeStream(
          header.stream,
          header.startSeq,
          header.ackedSeq,
          header.window,
        );
        streams.set(header.stream, { state, flush });
        const result = await handler.openStream?.(
          {
            points: header.points.map(decodePoint),
            startSeq: BigInt(header.startSeq),
            window: Number(header.window),
          },
          api,
        );
        if (result === undefined) {
          streams.delete(header.stream);
          return refuse(
            id,
            "node_unavailable",
            "the fake serves no chain-sync",
          );
        }
        if (result.notFound !== undefined) {
          streams.delete(header.stream);
          return answer(id, {
            type: "cs_intersect_not_found",
            stream: header.stream,
            tip: encodeTip(result.notFound),
          });
        }
        if (result.error !== undefined) {
          streams.delete(header.stream);
          return refuse(id, result.error.code, result.error.message);
        }
        answer(id, {
          type: "cs_opened",
          stream: header.stream,
          point: encodePoint(result.intersection),
          tip: encodeTip(result.tip),
        });
        const intersection = decodePoint(encodePoint(result.intersection));
        if (!samePoint(decodePoint(header.points[0]), intersection))
          state.queue.unshift({
            kind: "backward",
            point: result.intersection,
            tip: result.tip,
          });
        state.opened = true;
        flush();
        return;
      }
      case "cs_ack": {
        const entry = streams.get(header.stream);
        if (entry === undefined) return;
        const seq = BigInt(header.seq);
        if (seq < entry.state.acked || seq > entry.state.lastSeq)
          return fatal(
            "client_protocol_violation",
            "ack outside the delivered range",
            64,
          );
        entry.state.acked = seq;
        entry.flush();
        return;
      }
      case "cs_window": {
        const entry = streams.get(header.stream);
        if (entry === undefined) return;
        entry.state.window = header.window;
        entry.flush();
        return;
      }
      case "cs_close": {
        const entry = streams.get(header.stream);
        streams.delete(header.stream);
        if (entry !== undefined) {
          entry.state.closed = true;
          for (const listener of entry.state.closeListeners) listener();
        }
        return answer(id, { type: "ok" });
      }
      case "lsq_acquire": {
        const point =
          header.point === undefined ? undefined : decodePoint(header.point);
        const result = await handler.acquire?.(point);
        if (result?.error !== undefined)
          return refuse(id, result.error.code, result.error.message);
        acquired = point ?? "tip";
        return answer(id, { type: "ok" });
      }
      case "lsq_release":
        acquired = undefined;
        return answer(id, { type: "ok" });
      case "lsq_query": {
        if (acquired === undefined) return refuse(id, "not_acquired");
        const { type: _type, id: _id, ...query } = header;
        const result = await handler.ledgerQuery?.(query, acquired);
        if (result === undefined) return refuse(id, "unknown_query");
        if (result instanceof Uint8Array)
          return answer(id, { type: "lsq_result" }, result);
        return refuse(id, result.error.code, result.error.message);
      }
      case "submit": {
        const result = (await handler.submit?.(payload, header.era)) ?? {
          error: {
            code: "tx_undecodable",
            message: "the fake takes no transactions",
          },
        };
        if (result.accepted === true)
          return answer(id, { type: "submit_accepted" });
        if (result.rejection !== undefined)
          return answer(id, { type: "submit_rejected" }, result.rejection);
        return refuse(id, result.error.code, result.error.message);
      }
      case "monitor_has_tx": {
        if (handler.hasTx === undefined)
          return refuse(id, "monitor_unavailable");
        const has = await handler.hasTx(
          Buffer.from(header.txId).toString("hex"),
        );
        return answer(id, { type: "monitor_has_tx_result", has });
      }
      case "monitor_sizes": {
        if (handler.sizes === undefined)
          return refuse(id, "monitor_unavailable");
        const sizes = await handler.sizes();
        return answer(id, {
          type: "monitor_sizes_result",
          capacity: sizes.capacity,
          size: sizes.size,
          txCount: sizes.txCount,
        });
      }
      default:
        return fatal(
          "client_protocol_violation",
          `unknown frame ${header.type}`,
          64,
        );
    }
  };

  let pending = Buffer.alloc(0);
  process.stdin.on("data", (chunk) => {
    pending = Buffer.concat([pending, chunk]);
    for (;;) {
      if (pending.length < 8) return;
      const headerLength = pending.readUInt32BE(0);
      const payloadLength = pending.readUInt32BE(4);
      if (pending.length < 8 + headerLength + payloadLength) return;
      const header = decodeCbor(pending.subarray(8, 8 + headerLength));
      const payload = new Uint8Array(
        pending.subarray(8 + headerLength, 8 + headerLength + payloadLength),
      );
      pending = pending.subarray(8 + headerLength + payloadLength);
      // Frames are taken in arrival order; an answer a handler computes
      // asynchronously may overtake later ones, as the sidecar's do.
      handle(header, payload).catch((error) => {
        process.stderr.write(
          `fake sidecar handler failed: ${error?.stack ?? error}\n`,
        );
        process.exit(1);
      });
    }
  });
  // A handler with ignoreInputEnd stands in for a wedged sidecar.
  process.stdin.on("end", () => {
    if (handler.ignoreInputEnd === true) setInterval(() => undefined, 1_000);
    else process.exit(0);
  });
  return {
    /** Ends the session as the sidecar does on a fault. */
    fatal,
    /** Writes one frame as given, as a broken sidecar might. */
    writeFrame: write,
    /** Ends the process without a frame, as a crash would. */
    exit: (status) => process.exit(status),
  };
};

/**
 * Writes an executable fake sidecar at `path`. The handler module's default
 * export is called with `options` (JSON) and the session controls, and
 * returns the handler.
 */
export const writeFakeSidecar = async ({
  path,
  handlerModule,
  options = {},
}) => {
  const self = import.meta.url;
  const handler = pathToFileURL(handlerModule).href;
  await writeFile(
    path,
    `#!${process.execPath}
import { serveFakeSidecar } from ${JSON.stringify(self)};
import createHandler from ${JSON.stringify(handler)};
let session;
const controls = { fatal: (...args) => session.fatal(...args), writeFrame: (...args) => session.writeFrame(...args), exit: (status) => process.exit(status) };
session = serveFakeSidecar(await createHandler(${JSON.stringify(options)}, controls));
`,
  );
  await chmod(path, 0o755);
  return path;
};
