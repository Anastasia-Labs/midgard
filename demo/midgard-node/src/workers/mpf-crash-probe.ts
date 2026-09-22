import { spawnSync } from "node:child_process";
import { readFileSync, rmSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import { deleteMpfStore, MidgardMpf } from "../mpf/index.js";

const mode = process.env.MPF_CRASH_PROBE_MODE ?? "orchestrate";
const dbPath =
  process.env.MPF_CRASH_PROBE_DB ?? "/tmp/midgard-mpf-crash-probe-db";
const expectedPath = `${dbPath}.expected.json`;

const key = (index: number): Buffer => {
  const value = Buffer.alloc(32);
  value.writeUInt32BE(index, 28);
  return value;
};

const reset = deleteMpfStore(dbPath, "crash-probe").pipe(
  Effect.andThen(
    Effect.sync(() => {
      rmSync(expectedPath, { force: true });
    }),
  ),
);

const writeExpected = (root: string, lastKey: Buffer): void =>
  writeFileSync(
    expectedPath,
    JSON.stringify({ root, lastKey: lastKey.toString("hex") }),
  );

const crashPreFlush = Effect.gen(function* () {
  const mpf = yield* MidgardMpf.create("crash-probe", dbPath, {
    engine: "overlay",
  });
  yield* mpf.beginBlockOverlay();
  const entryCount = 256;
  const lastKey = key(entryCount);
  const ops = Array.from({ length: entryCount }, (_, index) => ({
    type: "insert" as const,
    key: key(index + 1),
    value: Buffer.alloc(128, index % 251),
  }));
  yield* mpf.primeBlockPathArena(ops, 2, false);
  const root = yield* mpf.applyBatch(ops);
  yield* mpf.checkpointAndCollapseDecodedArena(2, false, false);
  writeExpected(root.toString("hex"), lastKey);
  process.kill(process.pid, "SIGKILL");
});

const crashMidFlush = Effect.gen(function* () {
  const mpf = yield* MidgardMpf.create("crash-probe", dbPath, {
    engine: "overlay",
    spillThresholdBytes: Number.MAX_SAFE_INTEGER,
  });
  yield* mpf.beginBlockOverlay();
  const entryCount = 20_000;
  const ops = Array.from({ length: entryCount }, (_, index) => ({
    type: "insert" as const,
    key: key(index + 1),
    value: Buffer.alloc(256, index % 251),
  }));
  yield* mpf.primeBlockPathArena(ops, 2, false);
  const root = yield* mpf.applyBatch(ops);
  yield* mpf.checkpointAndCollapseDecodedArena(2, false, false);
  const lastKey = key(entryCount);
  writeExpected(root.toString("hex"), lastKey);
  setTimeout(() => process.kill(process.pid, "SIGKILL"), 0);
  yield* mpf.flushBlockOverlay(root);
  yield* Effect.promise(() => new Promise<never>(() => undefined));
});

const verify = (allowNewRoot: boolean) =>
  Effect.gen(function* () {
    const expected = JSON.parse(readFileSync(expectedPath, "utf8")) as {
      readonly root: string;
      readonly lastKey: string;
    };
    const mpf = yield* MidgardMpf.create("crash-probe", dbPath, {
      engine: "overlay",
    });
    const durableRoot = yield* mpf.persistedRootHex();
    if (
      durableRoot !== SDK.EMPTY_MERKLE_TREE_ROOT &&
      (!allowNewRoot || durableRoot !== expected.root)
    ) {
      throw new Error(
        `Crash left an impossible durable root: ${durableRoot} (expected old=${SDK.EMPTY_MERKLE_TREE_ROOT},new=${expected.root})`,
      );
    }
    const retained = yield* mpf.get(Buffer.from(expected.lastKey, "hex"));
    if (durableRoot === SDK.EMPTY_MERKLE_TREE_ROOT && Option.isSome(retained)) {
      throw new Error("Old crash root unexpectedly exposes uncommitted data");
    }
    if (durableRoot === expected.root && Option.isNone(retained)) {
      throw new Error(
        "New crash root does not expose its atomically flushed data",
      );
    }
    yield* mpf.close();
    return durableRoot;
  });

const orchestrate = Effect.sync(() => {
  const self = fileURLToPath(import.meta.url);
  const run = (childMode: string, expectKill = false) => {
    const child = spawnSync(process.execPath, [self], {
      env: { ...process.env, MPF_CRASH_PROBE_MODE: childMode },
      encoding: "utf8",
    });
    if (expectKill ? child.signal !== "SIGKILL" : child.status !== 0) {
      throw new Error(
        `Crash probe mode ${childMode} failed: status=${String(child.status)},signal=${String(child.signal)},stderr=${child.stderr}`,
      );
    }
    return child.stdout.trim();
  };

  run("reset");
  run("crash-pre-flush", true);
  const preFlushRoot = run("verify-pre-flush");
  run("reset");
  run("crash-mid-flush", true);
  const midFlushRoot = run("verify-mid-flush");
  run("reset");
  process.stdout.write(
    `${JSON.stringify({ preFlushRoot, midFlushRoot, result: "atomic-old-or-new" })}\n`,
  );
});

const asProbeProgram = <A, E>(
  effect: Effect.Effect<A, E, never>,
): Effect.Effect<void, unknown, never> => effect.pipe(Effect.asVoid);

const program: Effect.Effect<void, unknown, never> = (() => {
  switch (mode) {
    case "reset":
      return asProbeProgram(reset);
    case "crash-pre-flush":
      return asProbeProgram(crashPreFlush);
    case "crash-mid-flush":
      return asProbeProgram(crashMidFlush);
    case "verify-pre-flush":
      return asProbeProgram(
        verify(false).pipe(
          Effect.tap((root) =>
            Effect.sync(() => process.stdout.write(`${root}\n`)),
          ),
        ),
      );
    case "verify-mid-flush":
      return asProbeProgram(
        verify(true).pipe(
          Effect.tap((root) =>
            Effect.sync(() => process.stdout.write(`${root}\n`)),
          ),
        ),
      );
    default:
      return asProbeProgram(orchestrate);
  }
})();

void Effect.runPromise(program).catch((error: unknown) => {
  process.stderr.write(
    `${error instanceof Error ? error.stack : String(error)}\n`,
  );
  process.exitCode = 1;
});
