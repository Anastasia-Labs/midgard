import { randomUUID } from "node:crypto";
import { performance } from "node:perf_hooks";

import {
  parseWatcherConfig,
  readWatcherNativeChainSyncEventReceipt,
  WATCHER_CARDANO_SECURITY_PARAMETER_K,
  watcherNativeChainSyncAuthorityDetails,
  type WatcherNativeChainSyncEvent,
  type WatcherNativeChainSyncEventReceipt,
  watcherNativeChainSyncEventReceipt,
  type WatcherNativeChainSyncRollForward,
  type WatcherNativeChainSyncRuntime,
} from "midgard-watcher";

import {
  digest,
  type HistoryWindowPoint,
  point,
  publication,
  rangeAt,
  rangeDigest,
  refuse,
  type Row,
  rowAt,
  rowFromEvent,
} from "./history-window-canonical.js";
import { startWatcherNativeChainSync } from "./native-chain-sync.js";
export {
  type HistoryWindowPoint,
  HistoryWindowRefusal,
} from "./history-window-canonical.js";
export type HistoryWindowActor = Readonly<{
  role: "history-recorder";
  runId: string;
  deploymentFingerprint: string;
  codeStamp: string;
  serviceSpecsDigest: string;
  attemptId: string;
}>;
export type HistoryWindowSeal = Readonly<{
  actor: HistoryWindowActor;
  sourceEpoch: string;
  authorityDigest: string;
  startupDigest: string;
  sealId: string;
  promotionDigest: string;
  first: HistoryWindowPoint;
  last: HistoryWindowPoint;
  rowCount: number;
  recoveryWindow: number;
  rangeDigest: string;
  generation: string;
}>;
const receiptAt = (event: WatcherNativeChainSyncEvent) => {
  const receipt = watcherNativeChainSyncEventReceipt(event);
  if (receipt === null)
    return refuse("native event has no acquisition receipt");
  return { receipt, value: readWatcherNativeChainSyncEventReceipt(receipt) };
};
/** Read-only source admission and one process-local, revocable full-window pin. */
export const createHistoryWindowSealer = (input: {
  readonly actor: HistoryWindowActor;
  readonly directories: readonly string[];
  readonly watcherConfig: unknown;
  readonly binaryPath: string;
}) => {
  const config = parseWatcherConfig(input.watcherConfig);
  // The automatic recovery window is the Cardano security parameter.
  const recoveryWindow = WATCHER_CARDANO_SECURITY_PARAMETER_K;
  if (input.directories.length !== 2)
    return refuse("exact provider pair is required");
  const actor = Object.freeze({ ...input.actor });
  let anchor: WatcherNativeChainSyncEventReceipt | undefined;
  let rows: readonly Row[] = [];
  let sourceEpoch = randomUUID();
  let promotionDigest = "";
  let busy = false;
  let pending:
    | { event: WatcherNativeChainSyncRollForward; row: Row }
    | undefined;
  let pin:
    | { rows: readonly Row[]; seal: HistoryWindowSeal; expires: number }
    | undefined;
  const revoke = () => {
    anchor = undefined;
    sourceEpoch = randomUUID();
    rows = [];
    pending = undefined;
    pin = undefined;
    promotionDigest = "";
  };
  const live = () => {
    if (anchor === undefined)
      throw new Error("history source epoch is unknown");
    return readWatcherNativeChainSyncEventReceipt(anchor);
  };
  const capture = async (
    opening: WatcherNativeChainSyncEvent,
    target: HistoryWindowPoint,
    timeoutMs: number,
  ): Promise<void> => {
    if (busy) throw new Error("history capture is already active");
    busy = true;
    revoke();
    const epoch = sourceEpoch;
    let native: WatcherNativeChainSyncRuntime | undefined;
    const controller = new AbortController();
    let timer: ReturnType<typeof setTimeout> | undefined;
    let failed = false;
    let failure: unknown;
    try {
      if (
        !Number.isSafeInteger(timeoutMs) ||
        timeoutMs < 100 ||
        timeoutMs > 120000
      )
        throw new Error("history capture deadline is invalid");
      const main = receiptAt(opening);
      const expectedTarget = point(target);
      const matchesMain =
        opening.kind === "roll_forward"
          ? JSON.stringify(rowFromEvent(opening).point) ===
            JSON.stringify(expectedTarget)
          : opening.point.kind === "point" &&
            opening.point.blockHash === expectedTarget.blockHash &&
            opening.point.slot === expectedTarget.slot;
      if (!matchesMain)
        return refuse("capture target is not the actual main receipt point");
      const last = BigInt(expectedTarget.blockNo);
      const first =
        last >= BigInt(recoveryWindow - 1)
          ? last - BigInt(recoveryWindow) + 1n
          : 0n;
      const expected = rangeAt(input.directories, first, last);
      if (
        expected[expected.length - 1]?.bytes !==
        JSON.stringify({
          point: expectedTarget,
          prevHash: expected[expected.length - 1]?.prevHash,
        })
      )
        return refuse("retained target mismatch");
      const firstExpected = expected[0];
      if (firstExpected === undefined)
        return refuse("required window is empty");
      const predecessor =
        first === 0n ? undefined : rowAt(input.directories, first - 1n);
      if (
        predecessor !== undefined &&
        (firstExpected.prevHash !== predecessor.point.blockHash ||
          BigInt(firstExpected.point.slot) <= BigInt(predecessor.point.slot))
      )
        return refuse("required predecessor is unproven");
      const intersection =
        predecessor === undefined
          ? { kind: "origin" as const }
          : {
              kind: "point" as const,
              blockHash: predecessor.point.blockHash,
              slot: predecessor.point.slot,
            };
      let count = 0;
      let acknowledged = false;
      let resolveComplete!: () => void;
      let rejectComplete!: (error: unknown) => void;
      const complete = new Promise<void>((resolve, reject) => {
        resolveComplete = resolve;
        rejectComplete = reject;
      });
      void complete.catch(() => undefined);
      timer = setTimeout(() => {
        controller.abort();
        rejectComplete(new Error("history capture deadline expired"));
      }, timeoutMs);
      native = await startWatcherNativeChainSync({
        binaryPath: input.binaryPath,
        watcherConfig: config,
        intersection,
        startupTimeoutMs: timeoutMs,
        signal: controller.signal,
        onEvent: async (event) => {
          if (count === expected.length) return;
          readWatcherNativeChainSyncEventReceipt(main.receipt);
          const acquired = receiptAt(event);
          const mainIdentity = watcherNativeChainSyncAuthorityDetails(
            main.value.authority,
          );
          const capturedIdentity = watcherNativeChainSyncAuthorityDetails(
            acquired.value.authority,
          );
          if (
            mainIdentity === null ||
            capturedIdentity === null ||
            mainIdentity.network !== capturedIdentity.network ||
            mainIdentity.authorityNodeId !== capturedIdentity.authorityNodeId ||
            mainIdentity.genesisIdentitySha256 !==
              capturedIdentity.genesisIdentitySha256 ||
            mainIdentity.socketPath !== capturedIdentity.socketPath
          )
            return refuse("native capture authority mismatch");
          if (event.kind === "roll_backward") {
            if (
              acknowledged ||
              JSON.stringify(event.point) !== JSON.stringify(intersection)
            )
              return refuse("capture ancestry changed");
            acknowledged = true;
            return;
          }
          if (
            !acknowledged ||
            rowFromEvent(event).bytes !== expected[count]?.bytes
          )
            return refuse(
              "entire source window does not match retained history",
            );
          count++;
          if (count === expected.length) resolveComplete();
        },
      });
      await Promise.race([complete, native.done]);
      if (count !== expected.length)
        throw new Error("native capture ended before complete range");
      readWatcherNativeChainSyncEventReceipt(main.receipt);
      if (
        sourceEpoch !== epoch ||
        rangeDigest(rangeAt(input.directories, first, last)) !==
          rangeDigest(expected)
      )
        return refuse("capture range changed");
      anchor = main.receipt;
      rows = expected;
      promotionDigest = digest(
        JSON.stringify([
          "history-proven-anchor-v1",
          actor,
          epoch,
          rows[0]?.point,
          rangeDigest(rows),
        ]),
      );
    } catch (error) {
      revoke();
      failed = true;
      failure = error;
    } finally {
      clearTimeout(timer);
      controller.abort();
      try {
        await native?.close();
      } catch (error) {
        revoke();
        if (!failed) {
          failed = true;
          failure = error;
        }
      } finally {
        busy = false;
      }
    }
    if (failed) throw failure;
  };
  const guarded = <T>(operation: () => T): T => {
    try {
      return operation();
    } catch (error) {
      revoke();
      throw error;
    }
  };
  const prepareAppend = (event: WatcherNativeChainSyncRollForward) =>
    guarded(() => {
      const main = live();
      const acquired = receiptAt(event);
      if (
        busy ||
        pending !== undefined ||
        acquired.value.authority !== main.authority
      )
        return refuse("append belongs to another source epoch");
      const row = rowFromEvent(event);
      const previous = rows[rows.length - 1];
      if (
        previous === undefined ||
        row.prevHash !== previous.point.blockHash ||
        BigInt(row.point.blockNo) !== BigInt(previous.point.blockNo) + 1n ||
        BigInt(row.point.slot) <= BigInt(previous.point.slot)
      )
        return refuse("append is not a proven direct successor");
      pending = { event, row };
    });
  const completeAppend = () =>
    guarded(() => {
      const main = live();
      const held = pending;
      if (
        held === undefined ||
        receiptAt(held.event).value.authority !== main.authority
      )
        return refuse("append publication has no source receipt");
      const before = publication(input.directories);
      if (
        rowAt(input.directories, BigInt(held.row.point.blockNo)).bytes !==
          held.row.bytes ||
        publication(input.directories) !== before
      )
        return refuse("append publication is incoherent");
      const next = [...rows, held.row];
      if (next.length > recoveryWindow) {
        const prior = next.shift();
        promotionDigest = digest(
          JSON.stringify([
            "history-proven-promotion-v1",
            actor,
            sourceEpoch,
            promotionDigest,
            prior?.point,
            next[0]?.point,
            rangeDigest(rows),
          ]),
        );
      }
      rows = next;
      pending = undefined;
    });
  const seal = (timeoutMs: number): HistoryWindowSeal | null => {
    try {
      const main = live();
      if (
        busy ||
        pending !== undefined ||
        rows.length === 0 ||
        !Number.isSafeInteger(timeoutMs) ||
        timeoutMs <= 0 ||
        timeoutMs > 120000
      )
        return null;
      const first = rows[0];
      const last = rows[rows.length - 1];
      if (first === undefined || last === undefined) return null;
      if (
        rows.length !==
        Number(
          BigInt(last.point.blockNo) < BigInt(recoveryWindow)
            ? BigInt(last.point.blockNo) + 1n
            : BigInt(recoveryWindow),
        )
      )
        return null;
      const range = rangeDigest(rows);
      if (
        rangeDigest(
          rangeAt(
            input.directories,
            BigInt(first.point.blockNo),
            BigInt(last.point.blockNo),
          ),
        ) !== range
      )
        return null;
      live();
      const fields = {
        actor,
        sourceEpoch,
        authorityDigest: main.authority.authorityDigest,
        startupDigest: main.startupDigest,
        sealId: randomUUID(),
        promotionDigest,
        first: first.point,
        last: last.point,
        rowCount: rows.length,
        recoveryWindow,
        rangeDigest: range,
      };
      const proof = Object.freeze({
        ...fields,
        generation: digest(
          JSON.stringify(["history-full-window-seal-v1", fields]),
        ),
      });
      pin = {
        rows: [...rows],
        seal: proof,
        expires: performance.now() + timeoutMs,
      };
      return proof;
    } catch {
      return null;
    }
  };
  const revalidate = (
    sealId: string,
    generation: string,
  ): HistoryWindowSeal | null => {
    try {
      live();
      const held = pin;
      if (
        busy ||
        pending !== undefined ||
        held === undefined ||
        held.seal.sourceEpoch !== sourceEpoch ||
        held.seal.sealId !== sealId ||
        held.seal.generation !== generation ||
        performance.now() >= held.expires
      )
        return null;
      if (
        rangeDigest(
          rangeAt(
            input.directories,
            BigInt(held.seal.first.blockNo),
            BigInt(held.seal.last.blockNo),
          ),
        ) !== rangeDigest(held.rows)
      )
        return null;
      live();
      return held.seal;
    } catch {
      return null;
    }
  };
  return { capture, prepareAppend, completeAppend, seal, revalidate, revoke };
};
