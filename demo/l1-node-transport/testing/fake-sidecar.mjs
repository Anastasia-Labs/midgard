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

/** The handler module of a node ledger holding one script credential. */
export const LEDGER_HANDLER = fileURLToPath(
  new URL("./ledger-handler.mjs", import.meta.url),
);

const FRAME_PROTOCOL_VERSION = 1;

// ---------------------------------------------------------------- CBOR

const head = (major, value) => {
  const n = BigInt(value);
  if (n < 24n) return Buffer.from([(major << 5) | Number(n)]);
  if (n < 0x100n) return Buffer.from([(major << 5) | 24, Number(n)]);
  if (n < 0x10000n) {
    const b = Buffer.alloc(3);
    b[0] = (major << 5) | 25;
    b.writeUInt16BE(Number(n), 1);
    return b;
  }
  if (n < 0x100000000n) {
    const b = Buffer.alloc(5);
    b[0] = (major << 5) | 26;
    b.writeUInt32BE(Number(n), 1);
    return b;
  }
  const b = Buffer.alloc(9);
  b[0] = (major << 5) | 27;
  b.writeBigUInt64BE(n, 1);
  return b;
};

/** Encodes naturals, text, bytes, booleans, null, arrays and text-keyed objects. */
export const encodeCbor = (value) => {
  if (value === null) return Buffer.from([0xf6]);
  if (value === true) return Buffer.from([0xf5]);
  if (value === false) return Buffer.from([0xf4]);
  if (typeof value === "number" || typeof value === "bigint") {
    if (BigInt(value) < 0n) throw new TypeError("negative CBOR integer");
    return head(0, value);
  }
  if (typeof value === "string") {
    const text = Buffer.from(value, "utf8");
    return Buffer.concat([head(3, text.length), text]);
  }
  if (value instanceof Uint8Array)
    return Buffer.concat([head(2, value.length), value]);
  if (Array.isArray(value))
    return Buffer.concat([head(4, value.length), ...value.map(encodeCbor)]);
  if (value instanceof Map)
    return Buffer.concat([
      head(5, value.size),
      ...[...value].flatMap(([k, v]) => [encodeCbor(k), encodeCbor(v)]),
    ]);
  if (typeof value === "object") {
    const entries = Object.entries(value).filter(([, v]) => v !== undefined);
    return Buffer.concat([
      head(5, entries.length),
      ...entries.flatMap(([k, v]) => [encodeCbor(k), encodeCbor(v)]),
    ]);
  }
  throw new TypeError(`cannot encode ${typeof value} as CBOR`);
};

/** Decodes the subset `encodeCbor` writes; maps decode as objects. */
export const decodeCbor = (bytes) => {
  const buffer = Buffer.from(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  let offset = 0;
  const read = () => {
    const initial = buffer[offset++];
    if (initial === undefined) throw new Error("truncated CBOR");
    const major = initial >> 5;
    const info = initial & 31;
    if (major === 7) {
      if (info === 20) return false;
      if (info === 21) return true;
      if (info === 22) return null;
      throw new Error("unsupported CBOR simple value");
    }
    let n;
    if (info < 24) n = BigInt(info);
    else if (info === 24) (n = BigInt(buffer.readUInt8(offset))), (offset += 1);
    else if (info === 25)
      (n = BigInt(buffer.readUInt16BE(offset))), (offset += 2);
    else if (info === 26)
      (n = BigInt(buffer.readUInt32BE(offset))), (offset += 4);
    else if (info === 27) (n = buffer.readBigUInt64BE(offset)), (offset += 8);
    else throw new Error("unsupported CBOR length");
    const length = Number(n);
    switch (major) {
      case 0:
        return n <= BigInt(Number.MAX_SAFE_INTEGER) ? Number(n) : n;
      case 2: {
        const value = new Uint8Array(buffer.subarray(offset, offset + length));
        offset += length;
        return value;
      }
      case 3: {
        const value = buffer.toString("utf8", offset, offset + length);
        offset += length;
        return value;
      }
      case 4:
        return Array.from({ length }, read);
      case 5: {
        const record = {};
        for (let i = 0; i < length; i++) {
          const key = read();
          if (typeof key !== "string") throw new Error("non-text map key");
          record[key] = read();
        }
        return record;
      }
      default:
        throw new Error(`unsupported CBOR major type ${major}`);
    }
  };
  const value = read();
  if (offset !== buffer.length) throw new Error("trailing CBOR bytes");
  return value;
};

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
 * - openStream({points, startSeq, window, consumerAt}, stream): {intersection, tip}
 *   or {notFound: tip}; may be async. `stream` delivers events within credit.
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
            consumerAt:
              header.consumerAt === undefined
                ? undefined
                : decodePoint(header.consumerAt),
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
const controls = { fatal: (...args) => session.fatal(...args), exit: (status) => process.exit(status) };
session = serveFakeSidecar(await createHandler(${JSON.stringify(options)}, controls));
`,
  );
  await chmod(path, 0o755);
  return path;
};
