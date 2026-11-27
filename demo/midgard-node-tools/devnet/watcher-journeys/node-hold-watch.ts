import { existsSync } from "node:fs";
import { appendFile, readFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import {
  classifyNodeFaultHold,
  type NodeFaultHold,
} from "./honest-successor.observables.js";

/** One `/readyz` read of our node, bound to the fault it was read for. */
export type NodeHoldObservation = Readonly<{
  faultHeaderHash: string;
  observedAt: string;
  hold?: NodeFaultHold;
  unreachable?: string;
}>;

export const NODE_HOLD_OBSERVATIONS = "node-hold-observations.ndjson";

/**
 * Our node's `/readyz`, watched for its hold on one fault from the moment
 * the drill commits it. Each change of the hold (its state, binding and
 * reasons) is appended to `node-hold-observations.ndjson` with the fault's
 * header hash, so a fault the watcher corrects before staging ends is still
 * seen held, and a rerun that stages another fault never reads this one's
 * entries. A hold on another block, or a failure to record, stops the watch;
 * `stop` returns that failure.
 */
export const createNodeHoldWatch = (
  input: Readonly<{
    directory: string;
    /** Our node's HTTP endpoint. */
    nodeEndpoint: string;
    pollIntervalMs?: number;
    fetchImpl?: typeof fetch;
  }>,
) => {
  const fetchImpl = input.fetchImpl ?? fetch;
  const path = join(input.directory, NODE_HOLD_OBSERVATIONS);
  let watching:
    | {
        faultHeaderHash: string;
        monitor: AbortController;
        done: Promise<void>;
      }
    | undefined;
  let failure: Error | undefined;
  const read = async (
    faultHeaderHash: string,
  ): Promise<NodeHoldObservation> => {
    const observedAt = new Date().toISOString();
    let body: unknown;
    try {
      const response = await fetchImpl(`${input.nodeEndpoint}/readyz`, {
        signal: AbortSignal.timeout(10_000),
      });
      body = await response.json();
    } catch (cause) {
      return { faultHeaderHash, observedAt, unreachable: String(cause) };
    }
    return {
      faultHeaderHash,
      observedAt,
      hold: classifyNodeFaultHold(body, faultHeaderHash),
    };
  };
  const watch = async (faultHeaderHash: string, signal: AbortSignal) => {
    let last: string | undefined;
    try {
      while (!signal.aborted) {
        const observation = await read(faultHeaderHash);
        const summary = JSON.stringify({
          state: observation.hold?.state,
          binding: observation.hold?.binding,
          reasons: observation.hold?.reasons,
          unreachable: observation.unreachable !== undefined,
        });
        if (summary !== last) {
          last = summary;
          await appendFile(path, `${JSON.stringify(observation)}\n`);
        }
        await pause(input.pollIntervalMs ?? 5000, undefined, {
          signal,
        }).catch(() => undefined);
      }
    } catch (cause) {
      failure =
        cause instanceof Error
          ? cause
          : new Error(`Our node's hold watch failed: ${JSON.stringify(cause)}`);
    }
  };
  const stop = async (): Promise<Error | undefined> => {
    const current = watching;
    current?.monitor.abort();
    await current?.done;
    return failure;
  };
  return Object.freeze({
    /** Starts watching for `faultHeaderHash`; again for the same fault is a no-op. */
    start: (faultHeaderHash: string) => {
      if (watching !== undefined) {
        if (watching.faultHeaderHash !== faultHeaderHash)
          throw new Error(
            `Our node's hold watch is on fault ${watching.faultHeaderHash}, not ${faultHeaderHash}`,
          );
        if (!watching.monitor.signal.aborted) return;
      }
      const monitor = new AbortController();
      watching = {
        faultHeaderHash,
        monitor,
        done: watch(faultHeaderHash, monitor.signal),
      };
    },
    /** One read, unrecorded. */
    read,
    /** Every recorded observation of `faultHeaderHash`, oldest first. */
    observations: async (
      faultHeaderHash: string,
    ): Promise<NodeHoldObservation[]> =>
      existsSync(path)
        ? (await readFile(path, "utf8"))
            .split("\n")
            .filter((line) => line !== "")
            .map((line) => JSON.parse(line) as Partial<NodeHoldObservation>)
            .filter(
              (entry): entry is NodeHoldObservation =>
                entry.faultHeaderHash === faultHeaderHash,
            )
        : [],
    stop,
  });
};

export type NodeHoldWatch = ReturnType<typeof createNodeHoldWatch>;
