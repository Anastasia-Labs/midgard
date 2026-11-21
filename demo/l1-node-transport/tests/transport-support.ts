import { expect, inject } from "vitest";

import {
  type ChainSyncEvent,
  type ChainSyncStream,
  L1NodeTransport,
  type TransportReadiness,
} from "../src/index.js";
import { MockNode, within } from "./mock-node.js";

export const sidecarBinary = inject("sidecarBinary");
export const mockNodeBinary = inject("mockNodeBinary");
export const MAGIC = 42;

export const owned: Array<{ close(): Promise<void> }> = [];

export const mockNode = async (): Promise<MockNode> => {
  const node = await MockNode.start(mockNodeBinary, MAGIC);
  owned.push(node);
  return node;
};

export const transportFor = (
  socketPath: string,
  onReadiness?: (readiness: TransportReadiness) => void,
  requestTimeoutMs = 20_000,
  networkMagic = MAGIC,
): L1NodeTransport => {
  const transport = new L1NodeTransport({
    binaryPath: sidecarBinary,
    socketPath,
    networkMagic,
    requestTimeoutMs,
    restartDelayMs: { initial: 50, max: 200 },
    ...(onReadiness === undefined ? {} : { onReadiness }),
  });
  owned.push(transport);
  return transport;
};

export const take = async (
  stream: ChainSyncStream,
  count: number,
): Promise<ChainSyncEvent[]> => {
  const events: ChainSyncEvent[] = [];
  while (events.length < count) {
    const event = await within(stream.next(), 15_000);
    if (event === "timeout" || event === undefined)
      throw new Error(`stream stalled after ${events.length} events`);
    events.push(event);
  }
  return events;
};

/** Each event's seq follows the last, with no gap and no repeat. */
export const expectContiguous = (
  events: ChainSyncEvent[],
  from: bigint,
): void => {
  events.forEach((event, index) =>
    expect(event.seq).toBe(from + BigInt(index)),
  );
};
