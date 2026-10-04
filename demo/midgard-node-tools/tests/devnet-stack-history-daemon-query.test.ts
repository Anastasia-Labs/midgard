import { randomUUID } from "node:crypto";
import {
  chmodSync,
  existsSync,
  linkSync,
  lstatSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  unlinkSync,
  writeFileSync,
} from "node:fs";
import { createServer, type Socket } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { expect, it } from "vitest";

import {
  HISTORY_DAEMON_QUERY_MS,
  type HistoryDaemonScope,
  queryHistoryDaemon,
  startHistoryDaemonQuery,
} from "../src/devnet-stack/history-daemon-query.js";

const scope = (): HistoryDaemonScope => ({
  runId: "owned-review-run",
  daemonPid: process.pid,
  codeStamp: "a".repeat(64),
  serviceSpecsDigest: "b".repeat(64),
  incarnation: randomUUID(),
});
const deferred = <T>() => {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((accept) => {
    resolve = accept;
  });
  return { promise, resolve };
};
const fixture = () => {
  const dir = mkdtempSync(join(tmpdir(), "hq-"));
  chmodSync(dir, 0o700);
  return {
    dir,
    socketPath: join(dir, "current.sock"),
    dispose: () => rmSync(dir, { recursive: true, force: true }),
  };
};
const query = (
  socketPath: string,
  expectedScope: HistoryDaemonScope,
  read: () => HistoryDaemonScope | undefined = () => expectedScope,
  timeoutMs = 1000,
  signal?: AbortSignal,
) =>
  queryHistoryDaemon({
    socketPath,
    expectedScope,
    scope: read,
    serviceName: "history-recorder",
    timeoutMs,
    signal,
  });

it("queries the live registry afresh and keeps the owned socket private", async () => {
  const f = fixture();
  const s = scope();
  let ready = true;
  let calls = 0;
  const daemon = await startHistoryDaemonQuery({
    socketPath: f.socketPath,
    scope: () => s,
    check: async (name, left) => {
      expect(name).toBe("history-recorder");
      expect(left).toBeGreaterThan(0);
      expect(left).toBeLessThanOrEqual(5000);
      calls++;
      return ready;
    },
  });
  try {
    expect(lstatSync(f.socketPath).mode & 0o777).toBe(0o600);
    expect(await query(f.socketPath, s, () => s, 5000)).toBe("ready");
    ready = false;
    expect(await query(f.socketPath, s)).toBe("unknown");
    ready = true;
    expect(await query(f.socketPath, s)).toBe("ready");
    expect(calls).toBe(3);
  } finally {
    await daemon.close();
    expect(readdirSync(f.dir)).toEqual([]);
    f.dispose();
  }
});
it.each([
  "runId",
  "daemonPid",
  "codeStamp",
  "serviceSpecsDigest",
  "incarnation",
] as const)(
  "refuses changed server %s after its registry await",
  async (field) => {
    const f = fixture();
    const s = scope();
    let live = s;
    const entered = deferred<void>();
    const finish = deferred<boolean>();
    const daemon = await startHistoryDaemonQuery({
      socketPath: f.socketPath,
      scope: () => live,
      check: async () => {
        entered.resolve();
        return finish.promise;
      },
    });
    try {
      const pending = query(f.socketPath, s);
      await entered.promise;
      live = {
        ...s,
        [field]:
          field === "daemonPid"
            ? process.pid + 1
            : field === "incarnation"
              ? randomUUID()
              : field === "runId"
                ? "another-run"
                : "c".repeat(64),
      };
      finish.resolve(true);
      expect(await pending).toBe("unknown");
    } finally {
      finish.resolve(false);
      await daemon.close();
      f.dispose();
    }
  },
);
it("refuses client scope drift after a successful live check", async () => {
  const f = fixture();
  const s = scope();
  let controller = s;
  const daemon = await startHistoryDaemonQuery({
    socketPath: f.socketPath,
    scope: () => s,
    check: async () => {
      controller = { ...s, incarnation: randomUUID() };
      return true;
    },
  });
  try {
    expect(await query(f.socketPath, s, () => controller)).toBe("unknown");
  } finally {
    await daemon.close();
    f.dispose();
  }
});
it("keeps one proving owner until a timed-out callback actually settles", async () => {
  const f = fixture();
  const s = scope();
  const entered = deferred<void>();
  const finish = deferred<boolean>();
  let calls = 0;
  let aborted = false;
  const daemon = await startHistoryDaemonQuery({
    socketPath: f.socketPath,
    scope: () => s,
    check: async (_name, _left, signal) => {
      calls++;
      signal.addEventListener(
        "abort",
        () => {
          aborted = true;
        },
        { once: true },
      );
      entered.resolve();
      return finish.promise;
    },
  });
  let closing: Promise<void> | undefined;
  try {
    const pending = query(f.socketPath, s, () => s, 60);
    await entered.promise;
    expect(await pending).toBe("unknown");
    expect(aborted).toBe(true);
    expect(await query(f.socketPath, s)).toBe("unknown");
    expect(calls).toBe(1);
    let closed = false;
    closing = daemon.close().then(() => {
      closed = true;
    });
    await new Promise((resolve) => setTimeout(resolve, 30));
    expect(closed).toBe(false);
    finish.resolve(true);
    await closing;
    expect(closed).toBe(true);
    expect(existsSync(f.socketPath)).toBe(false);
  } finally {
    finish.resolve(false);
    await (closing ?? daemon.close());
    f.dispose();
  }
});
it("cancels a query and joins its socket while server close joins the actual callback", async () => {
  const f = fixture();
  const s = scope();
  const entered = deferred<void>();
  const finish = deferred<boolean>();
  const abort = new AbortController();
  let observed = false;
  const daemon = await startHistoryDaemonQuery({
    socketPath: f.socketPath,
    scope: () => s,
    check: async (_name, _left, signal) => {
      signal.addEventListener(
        "abort",
        () => {
          observed = true;
        },
        { once: true },
      );
      entered.resolve();
      return finish.promise;
    },
  });
  try {
    const pending = query(f.socketPath, s, () => s, 1000, abort.signal);
    await entered.promise;
    abort.abort();
    expect(await pending).toBe("unknown");
    const closing = daemon.close();
    await new Promise((resolve) => setTimeout(resolve, 20));
    expect(observed).toBe(true);
    finish.resolve(true);
    await closing;
  } finally {
    finish.resolve(false);
    await daemon.close();
    f.dispose();
  }
});
it("rejects concurrent fresh requests without queuing another registry check", async () => {
  const f = fixture();
  const s = scope();
  const entered = deferred<void>();
  const finish = deferred<boolean>();
  let calls = 0;
  const daemon = await startHistoryDaemonQuery({
    socketPath: f.socketPath,
    scope: () => s,
    check: async () => {
      calls++;
      entered.resolve();
      return finish.promise;
    },
  });
  try {
    const first = query(f.socketPath, s);
    await entered.promise;
    expect(
      await Promise.all(
        Array.from({ length: 4 }, () => query(f.socketPath, s)),
      ),
    ).toEqual(Array(4).fill("unknown"));
    expect(calls).toBe(1);
    finish.resolve(true);
    expect(await first).toBe("ready");
    expect(await query(f.socketPath, s)).toBe("ready");
    expect(calls).toBe(2);
  } finally {
    finish.resolve(false);
    await daemon.close();
    f.dispose();
  }
});
it("does not reset the absolute budget after slow synchronous scope reads", async () => {
  const f = fixture();
  const s = scope();
  let calls = 0;
  const daemon = await startHistoryDaemonQuery({
    socketPath: f.socketPath,
    scope: () => s,
    check: async () => {
      calls++;
      return true;
    },
  });
  try {
    expect(
      await query(
        f.socketPath,
        s,
        () => {
          const end = process.hrtime.bigint() + 40_000_000n;
          while (process.hrtime.bigint() < end) {
            /* deterministic synchronous read delay */
          }
          return s;
        },
        20,
      ),
    ).toBe("unknown");
    expect(calls).toBe(0);
  } finally {
    await daemon.close();
    f.dispose();
  }
});
it("keeps callback faults unknown and rejects absent or oversized budgets", async () => {
  const f = fixture();
  const s = scope();
  let calls = 0;
  const daemon = await startHistoryDaemonQuery({
    socketPath: f.socketPath,
    scope: () => s,
    check: async () => {
      calls++;
      throw Error("registry refuses");
    },
  });
  try {
    expect(await query(f.socketPath, s)).toBe("unknown");
    expect(HISTORY_DAEMON_QUERY_MS).toBe(5000);
    expect(await query(f.socketPath, s, () => s, 5001)).toBe("unknown");
    expect(await query(f.socketPath, s, () => s, 0)).toBe("unknown");
    expect(await query(f.socketPath, s, () => undefined)).toBe("unknown");
    expect(calls).toBe(1);
  } finally {
    await daemon.close();
    f.dispose();
  }
});
it.each(["file", "symlink", "stale-socket"])(
  "refuses an existing %s without deleting or adopting it",
  async (kind) => {
    const f = fixture();
    const s = scope();
    let existing: ReturnType<typeof createServer> | undefined;
    if (kind === "stale-socket") {
      existing = createServer();
      await new Promise<void>((resolve) =>
        existing!.listen(join(f.dir, "previous.sock"), resolve),
      );
      linkSync(join(f.dir, "previous.sock"), f.socketPath);
      await new Promise<void>((resolve) => existing!.close(() => resolve()));
      existing = undefined;
      chmodSync(f.socketPath, 0o600);
    } else if (kind === "file")
      writeFileSync(f.socketPath, "other-owner", { mode: 0o600 });
    else {
      const { symlinkSync } = await import("node:fs");
      symlinkSync("missing-owner", f.socketPath);
    }
    try {
      const before = lstatSync(f.socketPath);
      await expect(
        startHistoryDaemonQuery({
          socketPath: f.socketPath,
          scope: () => s,
          check: async () => true,
        }),
      ).rejects.toThrow("already exists");
      expect(lstatSync(f.socketPath).ino).toBe(before.ino);
      if (kind === "file")
        expect(readFileSync(f.socketPath, "utf8")).toBe("other-owner");
    } finally {
      if (existing)
        await new Promise<void>((resolve) => existing!.close(() => resolve()));
      f.dispose();
    }
  },
);
it("preserves an unrelated public-path replacement during close", async () => {
  const f = fixture();
  const s = scope();
  const daemon = await startHistoryDaemonQuery({
    socketPath: f.socketPath,
    scope: () => s,
    check: async () => true,
  });
  try {
    unlinkSync(f.socketPath);
    writeFileSync(f.socketPath, "unowned replacement", { mode: 0o600 });
    await daemon.close();
    await daemon.close();
    expect(readFileSync(f.socketPath, "utf8")).toBe("unowned replacement");
    expect(readdirSync(f.dir)).toEqual(["current.sock"]);
  } finally {
    await daemon.close();
    f.dispose();
  }
});
it("refuses nonprivate directories and a daemon PID it does not own", async () => {
  const f = fixture();
  const s = scope();
  try {
    chmodSync(f.dir, 0o755);
    await expect(
      startHistoryDaemonQuery({
        socketPath: f.socketPath,
        scope: () => s,
        check: async () => true,
      }),
    ).rejects.toThrow("private directory");
    chmodSync(f.dir, 0o700);
    await expect(
      startHistoryDaemonQuery({
        socketPath: f.socketPath,
        scope: () => ({ ...s, daemonPid: process.pid + 1 }),
        check: async () => true,
      }),
    ).rejects.toThrow("actual daemon scope");
    expect(readdirSync(f.dir)).toEqual([]);
  } finally {
    f.dispose();
  }
});
it.each(["nonce", "incarnation", "oversize"])(
  "refuses a Unix reply with %s contradiction",
  async (fault) => {
    const f = fixture();
    const s = scope();
    const peers = new Set<Socket>();
    const seen: string[] = [];
    const server = createServer((peer) => {
      peers.add(peer);
      peer.once("close", () => peers.delete(peer));
      peer.once("data", (chunk) => {
        const request = JSON.parse(chunk.toString());
        seen.push(request.nonce);
        const response = {
          schema: "history-daemon-query-v1",
          nonce: fault === "nonce" ? randomUUID() : request.nonce,
          scope:
            fault === "incarnation" ? { ...s, incarnation: randomUUID() } : s,
          serviceName: request.serviceName,
          status: "ready",
        };
        peer.end(
          fault === "oversize"
            ? JSON.stringify(response) + " ".repeat(4097) + "\n"
            : JSON.stringify(response) + "\n",
        );
      });
    });
    await new Promise<void>((resolve) => server.listen(f.socketPath, resolve));
    chmodSync(f.socketPath, 0o600);
    try {
      expect(await query(f.socketPath, s)).toBe("unknown");
      expect(await query(f.socketPath, s)).toBe("unknown");
      expect(seen).toHaveLength(2);
      expect(seen[0]).not.toBe(seen[1]);
    } finally {
      peers.forEach((peer) => peer.destroy());
      await new Promise<void>((resolve) => server.close(() => resolve()));
      f.dispose();
    }
  },
);
