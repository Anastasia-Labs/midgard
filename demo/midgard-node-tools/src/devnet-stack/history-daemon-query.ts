import { randomUUID } from "node:crypto";
import {
  chmodSync,
  linkSync,
  lstatSync,
  realpathSync,
  unlinkSync,
} from "node:fs";
import { createConnection, createServer, type Socket } from "node:net";
import { dirname, join, resolve } from "node:path";

export const HISTORY_DAEMON_QUERY_MS = 5000;
const FRAME_BYTES = 4096;
const SCHEMA = "history-daemon-query-v1";
const SERVICES = new Set([
  "history-recorder",
  "history-archive-a",
  "history-archive-b",
  "history-tunnel",
]);
export type HistoryDaemonScope = Readonly<{
  runId: string;
  daemonPid: number;
  codeStamp: string;
  serviceSpecsDigest: string;
  incarnation: string;
}>;
export type HistoryDaemonQueryResult = "ready" | "unknown";
const record = (v: unknown): v is Record<string, unknown> =>
  v !== null && typeof v === "object" && !Array.isArray(v);
const keys = (v: Record<string, unknown>, expected: string[]) =>
  Object.keys(v).sort().join() === expected.sort().join();
const uuid = (v: unknown): v is string =>
  typeof v === "string" &&
  /^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/u.test(v);
const hex = (v: unknown): v is string =>
  typeof v === "string" && /^[0-9a-f]{64}$/u.test(v);
const parseScope = (v: unknown): HistoryDaemonScope | null => {
  if (
    !record(v) ||
    !keys(v, [
      "runId",
      "daemonPid",
      "codeStamp",
      "serviceSpecsDigest",
      "incarnation",
    ]) ||
    typeof v.runId !== "string" ||
    !v.runId.length ||
    v.runId.length > 200 ||
    typeof v.daemonPid !== "number" ||
    !Number.isSafeInteger(v.daemonPid) ||
    v.daemonPid <= 0 ||
    !hex(v.codeStamp) ||
    !hex(v.serviceSpecsDigest) ||
    !uuid(v.incarnation)
  )
    return null;
  return Object.freeze({
    runId: v.runId,
    daemonPid: v.daemonPid,
    codeStamp: v.codeStamp,
    serviceSpecsDigest: v.serviceSpecsDigest,
    incarnation: v.incarnation,
  });
};
const matches = (a: HistoryDaemonScope, b: HistoryDaemonScope) =>
  a.runId === b.runId &&
  a.daemonPid === b.daemonPid &&
  a.codeStamp === b.codeStamp &&
  a.serviceSpecsDigest === b.serviceSpecsDigest &&
  a.incarnation === b.incarnation;
const current = (
  read: () => HistoryDaemonScope | undefined,
  expected: HistoryDaemonScope,
) => {
  try {
    const value = parseScope(read());
    return value !== null && matches(expected, value);
  } catch {
    return false;
  }
};
// Unix peers share this kernel monotonic clock. Passing its absolute deadline
// prevents resetting the five-second budget at connect, decode or registry hops.
const deadline = (ms: number) =>
  Number.isSafeInteger(ms) && ms > 0 && ms <= HISTORY_DAEMON_QUERY_MS
    ? process.hrtime.bigint() + BigInt(ms) * 1_000_000n
    : null;
const remaining = (end: bigint) =>
  Math.max(0, Number((end - process.hrtime.bigint()) / 1_000_000n));
const pathCheck = (path: string) => {
  if (resolve(path) !== path || Buffer.byteLength(path) > 100)
    throw new Error("History query requires a short absolute Unix path");
  const parent = dirname(path);
  const stat = lstatSync(parent);
  if (
    !stat.isDirectory() ||
    realpathSync(parent) !== parent ||
    (stat.mode & 0o777) !== 0o700 ||
    stat.uid !== process.getuid?.()
  )
    throw new Error("History query requires an owned private directory");
};
const sameInode = (path: string, owned: { dev: number; ino: number }) => {
  try {
    const value = lstatSync(path);
    return (
      value.isSocket() && value.dev === owned.dev && value.ino === owned.ino
    );
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "ENOENT") return false;
    throw error;
  }
};

/** Only the actual daemon supplies this live registry callback. No descriptor
 * is readiness proof. The protected parent/private bind namespace must not be
 * mutated by other writers; existing public paths are never adopted or removed.
 * check must settle its owned work after cancellation; close joins it even when
 * it ignores cancellation. A timed-out callback never releases the active slot.
 */
export const startHistoryDaemonQuery = async (input: {
  readonly socketPath: string;
  readonly scope: () => HistoryDaemonScope | undefined;
  readonly check: (
    serviceName: string,
    remainingMs: number,
    signal: AbortSignal,
  ) => Promise<boolean>;
}) => {
  const startup = deadline(HISTORY_DAEMON_QUERY_MS)!;
  const scope = parseScope(input.scope());
  if (scope === null || scope.daemonPid !== process.pid)
    throw new Error("History query requires the actual daemon scope");
  pathCheck(input.socketPath);
  try {
    lstatSync(input.socketPath);
    throw new Error("History query path already exists");
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
  }
  const bindPath = join(dirname(input.socketPath), `.hq-${randomUUID()}`);
  pathCheck(bindPath);
  let owned: { dev: number; ino: number } | undefined;
  let active = false;
  let closing: Promise<void> | undefined;
  const sockets = new Set<Socket>();
  const closed = new Set<Promise<void>>();
  const checks = new Set<Promise<void>>();
  const controllers = new Set<AbortController>();
  const server = createServer((socket) => {
    if (closing) {
      socket.destroy();
      return;
    }
    sockets.add(socket);
    let resolveClosed!: () => void;
    const done = new Promise<void>((resolve) => {
      resolveClosed = resolve;
    });
    closed.add(done);
    const controller = new AbortController();
    controllers.add(controller);
    let timer = setTimeout(() => {
      controller.abort();
      socket.destroy();
    }, HISTORY_DAEMON_QUERY_MS);
    let bytes = Buffer.alloc(0);
    let received = false;
    socket.on("error", () => undefined);
    socket.once("close", () => {
      clearTimeout(timer);
      controller.abort();
      controllers.delete(controller);
      sockets.delete(socket);
      closed.delete(done);
      resolveClosed();
    });
    socket.on("data", (chunk: Buffer) => {
      if (received || bytes.length + chunk.length > FRAME_BYTES) {
        controller.abort();
        socket.destroy();
        return;
      }
      bytes = Buffer.concat([bytes, chunk]);
      const newline = bytes.indexOf(10);
      if (newline < 0) return;
      received = true;
      if (newline !== bytes.length - 1) {
        socket.destroy();
        return;
      }
      let value: unknown;
      try {
        value = JSON.parse(bytes.subarray(0, newline).toString("utf8"));
      } catch {
        socket.destroy();
        return;
      }
      if (
        !record(value) ||
        !keys(value, [
          "schema",
          "nonce",
          "scope",
          "serviceName",
          "deadlineNs",
        ]) ||
        value.schema !== SCHEMA ||
        !uuid(value.nonce) ||
        typeof value.serviceName !== "string" ||
        !SERVICES.has(value.serviceName) ||
        typeof value.deadlineNs !== "string" ||
        !/^[0-9]{1,20}$/u.test(value.deadlineNs)
      ) {
        socket.destroy();
        return;
      }
      const serviceName = value.serviceName;
      const expected = parseScope(value.scope);
      const end = BigInt(value.deadlineNs);
      const left = remaining(end);
      const reply = (status: HistoryDaemonQueryResult) => {
        if (!socket.destroyed && !closing)
          socket.end(
            JSON.stringify({
              schema: SCHEMA,
              nonce: value.nonce,
              scope,
              serviceName: value.serviceName,
              status,
            }) + "\n",
          );
      };
      if (
        expected === null ||
        !matches(scope, expected) ||
        left <= 0 ||
        end - process.hrtime.bigint() >
          BigInt(HISTORY_DAEMON_QUERY_MS) * 1_000_000n ||
        active ||
        !current(input.scope, scope)
      ) {
        reply("unknown");
        return;
      }
      active = true;
      clearTimeout(timer);
      timer = setTimeout(
        () => {
          controller.abort();
          socket.destroy();
        },
        Math.max(1, remaining(end)),
      );
      const task = (async () => {
        try {
          const budget = remaining(end);
          const ready =
            budget > 0 &&
            !controller.signal.aborted &&
            (await input.check(serviceName, budget, controller.signal));
          reply(
            ready === true &&
              current(input.scope, scope) &&
              scope.daemonPid === process.pid &&
              remaining(end) > 0 &&
              !controller.signal.aborted
              ? "ready"
              : "unknown",
          );
        } catch {
          reply("unknown");
        } finally {
          active = false;
        }
      })();
      checks.add(task);
      void task.finally(() => checks.delete(task));
    });
  });
  server.maxConnections = 8;
  const close = (): Promise<void> => {
    if (closing) return closing;
    closing = (async () => {
      controllers.forEach((controller) => controller.abort());
      sockets.forEach((socket) => socket.destroy());
      await Promise.all([
        new Promise<void>((resolve) => server.close(() => resolve())),
        ...closed,
        ...checks,
      ]);
      if (owned && sameInode(input.socketPath, owned))
        unlinkSync(input.socketPath);
    })();
    return closing;
  };
  try {
    await new Promise<void>((resolve, reject) => {
      const timer = setTimeout(
        () => reject(new Error("History query startup expired")),
        Math.max(1, remaining(startup)),
      );
      const fail = (error: Error) => {
        clearTimeout(timer);
        reject(error);
      };
      server.once("error", fail);
      server.listen({ path: bindPath, backlog: 8 }, () => {
        clearTimeout(timer);
        server.removeListener("error", fail);
        resolve();
      });
    });
    chmodSync(bindPath, 0o600);
    const stat = lstatSync(bindPath);
    owned = { dev: stat.dev, ino: stat.ino };
    // Node close unlinks its original bind name. Exclusive publication keeps
    // it from deleting an unrelated replacement at the public query name.
    linkSync(bindPath, input.socketPath);
    if (!current(input.scope, scope) || remaining(startup) === 0)
      throw new Error("History query startup scope expired");
    server.on("error", () => undefined);
    return { close };
  } catch (error) {
    await close();
    throw error;
  }
};

/** The current-scope callback must read actual running-daemon/spec bindings,
 * including the fresh incarnation. Its descriptor never supplies a ready bit.
 * Unknown includes faults, cancellation, missing transport and scope drift.
 */
export const queryHistoryDaemon = async (input: {
  readonly socketPath: string;
  readonly expectedScope: HistoryDaemonScope;
  readonly scope: () => HistoryDaemonScope | undefined;
  readonly serviceName: string;
  readonly timeoutMs: number;
  readonly signal?: AbortSignal;
}): Promise<HistoryDaemonQueryResult> => {
  const end = deadline(input.timeoutMs);
  const scope = parseScope(input.expectedScope);
  if (
    end === null ||
    scope === null ||
    !SERVICES.has(input.serviceName) ||
    input.signal?.aborted
  )
    return "unknown";
  try {
    pathCheck(input.socketPath);
    const stat = lstatSync(input.socketPath);
    if (
      !stat.isSocket() ||
      (stat.mode & 0o777) !== 0o600 ||
      stat.uid !== process.getuid?.() ||
      !current(input.scope, scope) ||
      remaining(end) === 0
    )
      return "unknown";
  } catch {
    return "unknown";
  }
  const nonce = randomUUID();
  const socket = createConnection(input.socketPath);
  let resolveClosed!: () => void;
  const closed = new Promise<void>((resolve) => {
    resolveClosed = resolve;
  });
  socket.once("close", resolveClosed);
  const stop = () => socket.destroy();
  const timer = setTimeout(stop, Math.max(1, remaining(end)));
  input.signal?.addEventListener("abort", stop, { once: true });
  if (input.signal?.aborted) stop();
  let reply: unknown;
  try {
    reply = await new Promise<unknown>((resolve) => {
      let bytes = Buffer.alloc(0);
      socket.once("error", () => resolve(null));
      socket.once("close", () => resolve(null));
      socket.once("connect", () => {
        if (remaining(end) > 0 && current(input.scope, scope))
          socket.write(
            JSON.stringify({
              schema: SCHEMA,
              nonce,
              scope,
              serviceName: input.serviceName,
              deadlineNs: end.toString(),
            }) + "\n",
          );
        else socket.destroy();
      });
      socket.on("data", (chunk: Buffer) => {
        if (bytes.length + chunk.length > FRAME_BYTES) {
          socket.destroy();
          return;
        }
        bytes = Buffer.concat([bytes, chunk]);
        const newline = bytes.indexOf(10);
        if (newline < 0) return;
        if (newline !== bytes.length - 1) {
          socket.destroy();
          return;
        }
        try {
          resolve(JSON.parse(bytes.subarray(0, newline).toString("utf8")));
        } catch {
          resolve(null);
        }
        socket.destroy();
      });
    });
  } finally {
    clearTimeout(timer);
    input.signal?.removeEventListener("abort", stop);
    socket.destroy();
    await closed;
  }
  const actual = record(reply) ? parseScope(reply.scope) : null;
  return actual !== null &&
    record(reply) &&
    keys(reply, ["schema", "nonce", "scope", "serviceName", "status"]) &&
    reply.schema === SCHEMA &&
    reply.nonce === nonce &&
    reply.serviceName === input.serviceName &&
    reply.status === "ready" &&
    matches(scope, actual) &&
    current(input.scope, scope) &&
    remaining(end) > 0 &&
    !input.signal?.aborted
    ? "ready"
    : "unknown";
};
