import { cp, mkdtemp, rm, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import { basename, join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Clock, Effect, Either } from "effect";
import { Level } from "level";

import { parseStoredRootHex, ROOT_KEY } from "../mpf/store-primitives.js";
import {
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { READINESS_ENDPOINT } from "./readiness.js";
import { evaluateStateReconciliation } from "./state-reconciliation.check-ledger-cache.js";
import {
  collectL1StateView,
  l1Fingerprint,
} from "./state-reconciliation.collect-l1-state-view.js";
import {
  collectSqlStateSnapshot,
  committedTipSelector,
  HEX_32,
} from "./state-reconciliation.collect-sql-state-snapshot.js";
import {
  type L1Observation,
  type NativeRootObservation,
  type ReconciliationReport,
  type SqlStateSnapshot,
} from "./state-reconciliation.compares.js";
import {
  describeError,
  redactSensitive,
} from "./state-reconciliation.walk-merged-chain.js";

/** Reads the Architecture-G owner's durable root from node readiness. */
export const readNativeRootFromReadiness = async (
  nodeUrl: string,
  timeoutMs = 5_000,
): Promise<NativeRootObservation> => {
  let response: Response;
  try {
    response = await fetch(new URL(`/${READINESS_ENDPOINT}`, nodeUrl), {
      signal: AbortSignal.timeout(timeoutMs),
    });
  } catch (error) {
    return {
      kind: "unavailable",
      reason: `node readiness endpoint unreachable (${describeError(error)})`,
    };
  }
  if (response.status !== 200 && response.status !== 503) {
    return {
      kind: "unavailable",
      reason: `node readiness endpoint answered HTTP ${response.status.toString()}`,
    };
  }
  let body: unknown;
  try {
    body = await response.json();
  } catch {
    return {
      kind: "unavailable",
      reason: "node readiness response is not JSON",
    };
  }
  const owner =
    typeof body === "object" && body !== null
      ? (body as { nativeMpfOwner?: unknown }).nativeMpfOwner
      : undefined;
  if (owner === null || owner === undefined || typeof owner !== "object") {
    return {
      kind: "unavailable",
      reason: "node readiness carries no native MPF owner diagnostics",
    };
  }
  const { healthy, durableRoot, error } = owner as {
    healthy?: unknown;
    durableRoot?: unknown;
    error?: unknown;
  };
  if (healthy !== true) {
    return {
      kind: "unhealthy",
      reason: `node reports its native MPF owner unhealthy (${redactSensitive(String(error))})`,
    };
  }
  if (typeof durableRoot !== "string" || !HEX_32.test(durableRoot)) {
    return {
      kind: "unavailable",
      reason: "node readiness durableRoot is malformed",
    };
  }
  return { kind: "observed", root: durableRoot, source: "node-readiness" };
};

/**
 * Reads the persisted `__root__` marker from a private copy of the LevelDB
 * directory. The live store is never opened (LevelDB holds an exclusive lock
 * and opening can write), so this is safe beside a running node; a copy taken
 * mid-write may be stale, which the before/after comparison catches.
 */
export const readNativeRootFromLevelCopy = async (
  levelPath: string,
): Promise<NativeRootObservation> => {
  try {
    const info = await stat(levelPath);
    if (!info.isDirectory()) {
      return {
        kind: "unavailable",
        reason: "LEDGER_MPF_DB_PATH is not a directory",
      };
    }
  } catch {
    return {
      kind: "unavailable",
      reason: "no MPF LevelDB exists at LEDGER_MPF_DB_PATH",
    };
  }
  const copy = await mkdtemp(join(tmpdir(), "midgard-state-reconcile-"));
  try {
    await cp(levelPath, copy, {
      recursive: true,
      filter: (source) => basename(source) !== "LOCK",
    });
    const db = new Level<string, unknown>(copy, {
      valueEncoding: "json",
      createIfMissing: false,
    });
    await db.open();
    try {
      const marker = await db.get(ROOT_KEY);
      return {
        kind: "observed",
        root: parseStoredRootHex(marker).toString("hex"),
        source: "leveldb-copy",
      };
    } finally {
      await db.close();
    }
  } catch (error) {
    return {
      kind: "unavailable",
      reason: `MPF LevelDB copy unreadable (${describeError(error)})`,
    };
  } finally {
    await rm(copy, { recursive: true, force: true });
  }
};

export type NativeRootSourceOptions = {
  readonly nodeUrl?: string;
};

export const readNativeRoot = (
  options: NativeRootSourceOptions,
): Effect.Effect<NativeRootObservation, never, NodeConfig> =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    const url = options.nodeUrl ?? `http://127.0.0.1:${config.PORT.toString()}`;
    const fromReadiness = yield* Effect.promise(() =>
      readNativeRootFromReadiness(url),
    );
    // The copy stands in only for a default-URL node that gave no owner
    // answer: an explicit --node-url, or an owner reported unhealthy, is
    // the result itself.
    if (fromReadiness.kind !== "unavailable" || options.nodeUrl !== undefined)
      return fromReadiness;
    const fromCopy = yield* Effect.promise(() =>
      readNativeRootFromLevelCopy(config.LEDGER_MPF_DB_PATH),
    );
    return fromCopy.kind === "observed"
      ? fromCopy
      : {
          kind: "unavailable",
          reason: `${fromReadiness.reason}; ${fromCopy.reason}`,
        };
  });

// ---------------------------------------------------------------------------
// Orchestration
// ---------------------------------------------------------------------------

export type StateReconciliationOptions = NativeRootSourceOptions & {
  readonly allowInFlight?: boolean;
  readonly maxAttempts?: number;
};

const nativeKey = (native: NativeRootObservation): string =>
  native.kind === "observed" ? `${native.source}:${native.root}` : native.kind;

/**
 * Collects L1, native root and SQL, re-reads L1 and the native root after the
 * SQL snapshot, and retries until both were stable across it. Returns the
 * evaluated report; never writes.
 */
export const stateReconciliationProgram = (
  options: StateReconciliationOptions = {},
): Effect.Effect<
  ReconciliationReport,
  unknown,
  Database | Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const maxAttempts = Math.max(1, Math.floor(options.maxAttempts ?? 3));
    let last:
      | {
          readonly l1: L1Observation;
          readonly native: NativeRootObservation;
          readonly sql: SqlStateSnapshot;
          readonly nowMs: number;
        }
      | undefined;
    let attempts = 0;
    for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
      attempts = attempt;
      const l1Before = yield* Effect.either(collectL1StateView);
      const nativeBefore = yield* readNativeRoot(options);
      const sql = yield* collectSqlStateSnapshot({
        committedTipHeaderHash: committedTipSelector(
          Either.isRight(l1Before) ? l1Before.right : null,
        ),
      });
      // Taken before the L1 read the report uses: a header that read still
      // shows Unattested was Unattested at this instant too.
      const nowMs = yield* Clock.currentTimeMillis;
      const l1After = yield* Effect.either(collectL1StateView);
      const nativeAfter = yield* readNativeRoot(options);
      const nativeStable = nativeKey(nativeBefore) === nativeKey(nativeAfter);
      const native: NativeRootObservation = nativeStable
        ? nativeAfter
        : {
            kind: "unavailable",
            reason: `native root changed while SQL was read, in each of ${attempt.toString()} snapshot attempts`,
          };
      let l1: L1Observation;
      let l1Stable = false;
      if (Either.isLeft(l1Before)) {
        l1 = {
          kind: "unavailable",
          reason: `L1 read failed: ${describeError(l1Before.left)}`,
        };
      } else if (Either.isLeft(l1After)) {
        l1 = {
          kind: "unavailable",
          reason: `L1 read failed: ${describeError(l1After.left)}`,
        };
      } else if (
        l1Fingerprint(l1Before.right) !== l1Fingerprint(l1After.right)
      ) {
        l1 = {
          kind: "unavailable",
          reason: `L1 state changed while SQL was read, in each of ${attempt.toString()} snapshot attempts; L1 comparisons need a quiescent window`,
        };
      } else {
        l1 = { kind: "observed", view: l1After.right };
        l1Stable = true;
      }
      last = { l1, native, sql, nowMs };
      if (l1Stable && nativeStable) break;
    }
    if (last === undefined) {
      return yield* Effect.fail(
        new Error("state reconciliation took no snapshot"),
      );
    }
    const final = last;
    const { l1, native } = final;
    return evaluateStateReconciliation({
      l1,
      sql: final.sql,
      native,
      allowInFlight: options.allowInFlight === true,
      attempts,
      nowMs: final.nowMs,
      daAttestationTimeoutMs: Number(SDK.DA_ATTESTATION_TIMEOUT_MS),
    });
  });
