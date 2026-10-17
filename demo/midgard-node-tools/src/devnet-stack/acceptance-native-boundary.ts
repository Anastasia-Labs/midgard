import {
  createDaAvailabilityReadScope,
  type DaAvailabilityReadScope,
} from "@al-ft/midgard-sdk";
import type {
  WatcherNativeChainSyncEventReceipt,
  WatcherNativeChainSyncRuntime,
} from "midgard-watcher";

import {
  type AcceptanceNativeReadConfig,
  type AcceptanceNativeRunBinding,
  loadAcceptanceNativeReadConfig,
} from "./acceptance-native-config.js";
import { acceptanceNativeLineageReads } from "./acceptance-native-lineage.js";
import { openAcceptanceNativeSession } from "./acceptance-native-session.js";
import type { Layout } from "./layout.js";

export type AcceptanceNativePoint = Readonly<{
  blockHash: string;
  blockNo: string;
  slot: string;
}>;
export type AcceptanceNativePayoutBoundary = AcceptanceNativeRunBinding &
  Readonly<{
    point: AcceptanceNativePoint;
    generation: string;
    authorityNodeId: string;
    socketPath: string;
    genesisIdentitySha256: string;
    startupDigest: string;
    eventDigest: string;
    observedAt: string;
  }>;
export type AcceptanceNativePayoutScope = Pick<
  ReturnType<typeof acceptanceNativeLineageReads>,
  "readExactTransaction" | "canonicalBlockDepth"
> &
  Readonly<{
    point: AcceptanceNativePoint;
    signal: AbortSignal;
    deadlineEpochMs: number;
    assertCurrent(): void;
    /** Original JSON-RPC text. The payout verifier must parse quantities losslessly. */
    queryExactOutRefs(
      refs: readonly { txHash: string; outputIndex: number }[],
    ): Promise<string>;
  }>;

const natural = (value: unknown): string => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0)
    throw new Error("acceptance Ogmios point has an unsafe coordinate");
  return value.toString();
};
const hash = (value: unknown): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value))
    throw new Error("acceptance Ogmios point hash is invalid");
  return value;
};
const same = (a: AcceptanceNativePoint, b: AcceptanceNativePoint): boolean =>
  a.blockHash === b.blockHash && a.slot === b.slot && a.blockNo === b.blockNo;

/** Private implementation seam; tests use the actual public native supervisor. */
export const captureAcceptanceNativeRead = async <T>(
  config: AcceptanceNativeReadConfig,
  scope: DaAvailabilityReadScope,
  use: (scope: AcceptanceNativePayoutScope) => Promise<T>,
  unsafeNativeForTest: Pick<
    Parameters<typeof config.native.startWatcherNativeChainSync>[0],
    "unsafeReadIdentityFileForTest"
  > = {},
): Promise<{ value: T; boundary: AcceptanceNativePayoutBoundary }> => {
  if (scope.deadlineEpochMs === undefined)
    throw new Error(
      "acceptance native read requires an absolute deadline before I/O",
    );
  const controller = new AbortController();
  const retire = (message: string): void =>
    controller.abort(new Error(message));
  const onAbort = (): void =>
    retire("acceptance native read deadline or caller revoked");
  scope.signal.addEventListener("abort", onAbort, { once: true });
  if (scope.signal.aborted) onAbort();
  const operation = createDaAvailabilityReadScope({
    deadlineEpochMs: scope.deadlineEpochMs,
    attemptTimeoutMs: Math.max(1, Math.floor(scope.remainingMs())),
    signal: controller.signal,
  });
  let nativeStartup: Promise<WatcherNativeChainSyncRuntime> | undefined;
  let nativeRuntime: WatcherNativeChainSyncRuntime | undefined;
  let session:
    | Awaited<ReturnType<typeof openAcceptanceNativeSession>>
    | undefined;
  let usePending: Promise<T> | undefined;
  let lineage: ReturnType<typeof acceptanceNativeLineageReads> | undefined;
  let generation = 0n;
  let current:
    | {
        point: AcceptanceNativePoint;
        receipt: WatcherNativeChainSyncEventReceipt;
      }
    | undefined;
  let captured: typeof current;
  let resolveCurrent!: () => void;
  const currentReady = new Promise<void>((resolve) => {
    resolveCurrent = resolve;
  });
  const parse = (text: string): Record<string, unknown> => {
    const frame = config.watcher.parseWatcherStrictJsonValue(text) as {
      result: Record<string, unknown>;
    };
    return frame.result;
  };
  const tip = (value: unknown): AcceptanceNativePoint => {
    const point = value as { id?: unknown; slot?: unknown; height?: unknown };
    return Object.freeze({
      blockHash: hash(point?.id),
      slot: natural(point?.slot),
      blockNo: natural(point?.height),
    });
  };
  const assertCurrent = (): void => {
    scope.assertCurrent();
    operation.assertCurrent();
    if (
      captured === undefined ||
      current !== captured ||
      nativeRuntime === undefined
    )
      throw new Error("acceptance native current boundary changed");
    const observed = config.watcher.readWatcherNativeChainSyncEventReceipt(
      captured.receipt,
    );
    const details = config.watcher.watcherNativeChainSyncAuthorityDetails(
      nativeRuntime.authority,
    );
    const source = config.watcherConfig.l1.source;
    if (
      source.sourceMode !== "local_node" ||
      details === null ||
      observed.authority !== nativeRuntime.authority ||
      details.authorityNodeId !== source.authorityNodeId ||
      details.socketPath !== source.chainSync.socketPath ||
      details.genesisIdentitySha256 !==
        source.chainSync.genesisIdentitySha256 ||
      details.network !== config.watcherConfig.targetNetwork ||
      details.operation.kind !== "stream" ||
      observed.startupDigest !== details.startupDigest ||
      observed.event.kind !== "roll_forward" ||
      !same(captured.point, observed.event)
    )
      throw new Error("acceptance native boundary authority changed");
  };
  let outcome:
    | { value: T; boundary: AcceptanceNativePayoutBoundary }
    | undefined;
  let failure: unknown;
  let failed = false;
  try {
    scope.assertCurrent();
    operation.assertCurrent();
    const source = config.watcherConfig.l1.source;
    if (source.sourceMode !== "local_node")
      throw new Error("acceptance native read requires local node");
    session = await openAcceptanceNativeSession({
      endpoint: config.ogmiosEndpoint,
      scope: operation,
      parseJson: config.watcher.parseWatcherStrictJsonValue,
    });
    const initial = parse(
      await session.request("findIntersection", { points: ["origin"] }),
    );
    const initialTip = tip(initial.tip);
    if (initial.intersection !== "origin")
      throw new Error("acceptance native initial intersection is not origin");
    const startupTimeoutMs = Math.min(
      120_000,
      Math.floor(operation.remainingMs()),
    );
    if (startupTimeoutMs < 100)
      throw new Error("acceptance native startup budget expired");
    nativeStartup = config.native.startWatcherNativeChainSync({
      watcherConfig: config.watcherConfig,
      binaryPath: config.binaryPath,
      intersection: {
        kind: "point",
        blockHash: initialTip.blockHash,
        slot: initialTip.slot,
      },
      startupTimeoutMs,
      signal: scope.signal,
      ...unsafeNativeForTest,
      onAuthorityRevoked: () => retire("acceptance native authority was lost"),
      onEvent: async (event) => {
        operation.assertCurrent();
        generation += 1n;
        if (captured !== undefined) {
          retire("acceptance native current point changed");
          return;
        }
        current = undefined;
        if (
          event.kind !== "roll_forward" ||
          event.tip.kind !== "point" ||
          !same(event, event.tip)
        )
          return;
        const receipt =
          config.watcher.watcherNativeChainSyncEventReceipt(event);
        if (receipt === null)
          throw new Error(
            "acceptance native forward lacks live acquisition provenance",
          );
        current = Object.freeze({
          point: Object.freeze({
            blockHash: event.blockHash,
            slot: event.slot,
            blockNo: event.blockNo,
          }),
          receipt,
        });
        resolveCurrent();
      },
    });
    nativeRuntime = await nativeStartup;
    void nativeRuntime.done.then(
      () => retire("acceptance native helper exited"),
      () => retire("acceptance native helper failed"),
    );
    await operation.read(() => currentReady);
    captured = current;
    const capturedGeneration = generation;
    assertCurrent();
    const selectedPoint = {
      id: captured!.point.blockHash,
      slot: Number(captured!.point.slot),
    };
    natural(selectedPoint.slot);
    const verifySelected = async (): Promise<void> => {
      assertCurrent();
      const selected = parse(
        await session!.request("findIntersection", { points: [selectedPoint] }),
      );
      const intersection = selected.intersection as {
        id?: unknown;
        slot?: unknown;
      };
      if (
        hash(intersection?.id) !== captured!.point.blockHash ||
        natural(intersection?.slot) !== captured!.point.slot ||
        !same(tip(selected.tip), captured!.point)
      )
        throw new Error(
          "acceptance Ogmios selected chain differs from current native point",
        );
      assertCurrent();
    };
    await verifySelected();
    const acquired = parse(
      await session.request("acquireLedgerState", { point: selectedPoint }),
    );
    const acquiredPoint = acquired.point as { id?: unknown; slot?: unknown };
    if (
      acquired.acquired !== "ledgerState" ||
      hash(acquiredPoint?.id) !== selectedPoint.id ||
      natural(acquiredPoint?.slot) !== captured!.point.slot
    )
      throw new Error("acceptance Ogmios acquired a different native point");
    assertCurrent();
    lineage = acceptanceNativeLineageReads({
      endpoint: config.ogmiosEndpoint,
      scope: operation,
      point: captured!.point,
      assertCurrent,
      parseJson: config.watcher.parseWatcherStrictJsonValue,
    });
    usePending = use(
      Object.freeze({
        readExactTransaction: lineage.readExactTransaction,
        canonicalBlockDepth: lineage.canonicalBlockDepth,
        point: captured!.point,
        signal: operation.signal,
        deadlineEpochMs: scope.deadlineEpochMs!,
        assertCurrent,
        queryExactOutRefs: async (refs) => {
          assertCurrent();
          if (
            refs.length === 0 ||
            refs.length > 64 ||
            new Set(refs.map((ref) => `${ref.txHash}#${ref.outputIndex}`))
              .size !== refs.length
          )
            throw new Error(
              "acceptance payout query refs are empty, duplicate or oversized",
            );
          const outputReferences = refs.map((ref) => ({
            transaction: { id: hash(ref.txHash) },
            index: Number(natural(ref.outputIndex)),
          }));
          const text = await session!.request("queryLedgerState/utxo", {
            outputReferences,
          });
          assertCurrent();
          return text;
        },
      }),
    );
    void usePending.catch(() => undefined);
    const value = await operation.read(() => usePending!);
    assertCurrent();
    await verifySelected();
    await config.assertUnchanged(operation);
    assertCurrent();
    const receipt = config.watcher.readWatcherNativeChainSyncEventReceipt(
      captured!.receipt,
    );
    const details = config.watcher.watcherNativeChainSyncAuthorityDetails(
      nativeRuntime.authority,
    )!;
    outcome = {
      value,
      boundary: Object.freeze({
        ...config.binding,
        point: captured!.point,
        generation: capturedGeneration.toString(),
        authorityNodeId: details.authorityNodeId,
        socketPath: details.socketPath,
        genesisIdentitySha256: details.genesisIdentitySha256,
        startupDigest: receipt.startupDigest,
        eventDigest: receipt.eventDigest,
        observedAt: new Date().toISOString(),
      }),
    };
  } catch (error) {
    failure = error;
    failed = true;
  } finally {
    retire("acceptance native read closed");
    scope.signal.removeEventListener("abort", onAbort);
    const owned =
      nativeRuntime ?? (await nativeStartup?.catch(() => undefined));
    const results = await Promise.allSettled([
      session?.close(),
      (async () => {
        if (owned !== undefined) {
          await owned.close();
          await owned.done;
        }
      })(),
    ]);
    await lineage?.drain();
    await usePending?.catch(() => undefined);
    operation.close();
    for (const result of results) {
      if (result.status === "rejected" && !failed) {
        failure = result.reason;
        failed = true;
      }
    }
  }
  if (failed) throw failure;
  if (outcome === undefined)
    throw new Error("acceptance native read omitted its result");
  return outcome;
};

export const captureAcceptanceNativeBoundary = async <T>(
  input: { layout: Layout; timeoutMs: number; signal?: AbortSignal },
  use: (scope: AcceptanceNativePayoutScope) => Promise<T>,
): Promise<{ value: T; boundary: AcceptanceNativePayoutBoundary }> => {
  if (
    !Number.isSafeInteger(input.timeoutMs) ||
    input.timeoutMs < 100 ||
    input.timeoutMs > 120_000
  )
    throw new Error("acceptance native read timeout must be 100..120000ms");
  const scope = createDaAvailabilityReadScope({
    deadlineEpochMs: Date.now() + input.timeoutMs,
    attemptTimeoutMs: input.timeoutMs,
    signal: input.signal,
  });
  try {
    return await captureAcceptanceNativeRead(
      await loadAcceptanceNativeReadConfig(input.layout, scope),
      scope,
      use,
    );
  } finally {
    scope.close();
  }
};
