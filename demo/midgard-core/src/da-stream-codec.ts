import { DA_TRANSPORT_LIMITS } from "./da-transport.js";

const LENGTH_PREFIX_BYTES = 4;

export type DaStreamFrameOptions = {
  readonly maxFrameBytes?: number;
  readonly allowEmpty?: boolean;
  readonly timing?: DaStreamTimingOptions;
};

export type DaStreamTimingOptions = {
  readonly monotonicNow?: () => number;
  readonly onStageTiming?: (
    stage: "frame_receive" | "frame_write",
    durationMs: number,
  ) => void;
};

export type DaStreamChunk =
  | Uint8Array
  | {
      readonly byteLength: number;
      subarray(start?: number, end?: number): Uint8Array;
    };

export type DaWritableStream = {
  send(data: Uint8Array): boolean;
  onDrain?(): Promise<void>;
  close?(): Promise<void> | void;
};

export const encodeDaStreamFrame = (
  payload: Uint8Array,
  options: DaStreamFrameOptions = {},
): Buffer => {
  const maxFrameBytes = frameLimit(options);
  if (payload.byteLength === 0 && options.allowEmpty !== true) {
    throw new Error("DA libp2p stream frame must not be empty");
  }
  if (payload.byteLength > maxFrameBytes) {
    throw new Error(
      `DA libp2p stream frame exceeds ${maxFrameBytes.toString()} bytes`,
    );
  }
  const frame = Buffer.allocUnsafe(LENGTH_PREFIX_BYTES + payload.byteLength);
  frame.writeUInt32BE(payload.byteLength, 0);
  frame.set(payload, LENGTH_PREFIX_BYTES);
  return frame;
};

export async function* decodeDaStreamFrames(
  chunks: AsyncIterable<DaStreamChunk> | Iterable<DaStreamChunk>,
  options: DaStreamFrameOptions = {},
): AsyncGenerator<Buffer> {
  const maxFrameBytes = frameLimit(options);
  const prefix = Buffer.allocUnsafe(LENGTH_PREFIX_BYTES);
  let prefixOffset = 0;
  let expectedLength: number | undefined;
  let frame: Buffer | undefined;
  let frameOffset = 0;
  let frameStartedAt: number | undefined;
  for await (const chunk of untilRemoteWriteCloses(chunks)) {
    const bytes = chunkView(chunk);
    if (bytes.byteLength === 0) {
      continue;
    }
    let chunkOffset = 0;
    while (chunkOffset < bytes.byteLength) {
      if (expectedLength === undefined) {
        frameStartedAt ??= readMonotonicNow(options);
        const prefixBytes = Math.min(
          LENGTH_PREFIX_BYTES - prefixOffset,
          bytes.byteLength - chunkOffset,
        );
        prefix.set(
          bytes.subarray(chunkOffset, chunkOffset + prefixBytes),
          prefixOffset,
        );
        prefixOffset += prefixBytes;
        chunkOffset += prefixBytes;
        if (prefixOffset < LENGTH_PREFIX_BYTES) {
          continue;
        }
        expectedLength = prefix.readUInt32BE(0);
        prefixOffset = 0;
        if (expectedLength === 0 && options.allowEmpty !== true) {
          throw new Error("DA libp2p stream frame must not be empty");
        }
        if (expectedLength > maxFrameBytes) {
          throw new Error(
            `DA libp2p stream frame exceeds ${maxFrameBytes.toString()} bytes`,
          );
        }
        frame = Buffer.allocUnsafe(expectedLength);
        frameOffset = 0;
        if (expectedLength === 0) {
          recordFrameTiming(options, "frame_receive", frameStartedAt);
          frameStartedAt = undefined;
          expectedLength = undefined;
          yield frame;
          frame = undefined;
          continue;
        }
      }
      if (frame === undefined) {
        throw new Error("DA libp2p stream frame assembly state is invalid");
      }
      const frameBytes = Math.min(
        expectedLength - frameOffset,
        bytes.byteLength - chunkOffset,
      );
      frame.set(
        bytes.subarray(chunkOffset, chunkOffset + frameBytes),
        frameOffset,
      );
      frameOffset += frameBytes;
      chunkOffset += frameBytes;
      if (frameOffset === expectedLength) {
        recordFrameTiming(options, "frame_receive", frameStartedAt);
        frameStartedAt = undefined;
        expectedLength = undefined;
        const completed = frame;
        frame = undefined;
        yield completed;
      }
    }
  }
  if (expectedLength !== undefined || prefixOffset > 0) {
    throw new Error("incomplete DA libp2p stream frame");
  }
}

/** What a libp2p stream says about the remote's writable end. */
type DaRemoteWriteStatus = {
  readonly remoteWriteStatus: string;
  readonly readBufferLength: number;
};

const reportsRemoteWriteStatus = (
  chunks: AsyncIterable<DaStreamChunk> | Iterable<DaStreamChunk>,
): chunks is AsyncIterable<DaStreamChunk> & DaRemoteWriteStatus =>
  Symbol.asyncIterator in chunks &&
  "remoteWriteStatus" in chunks &&
  "readBufferLength" in chunks;

/**
 * A libp2p stream's iterator ends on the remote's end-of-stream event, which
 * reaches only a listener attached when it fires. A reader that starts after
 * the remote closed its writable end, such as a payload submit queued behind
 * admission, gets the buffered bytes and then waits for an event already
 * gone, until the request deadline. The stream's status still says the remote
 * is done: once its unread buffer is empty, the stream has ended.
 */
async function* untilRemoteWriteCloses(
  chunks: AsyncIterable<DaStreamChunk> | Iterable<DaStreamChunk>,
): AsyncGenerator<DaStreamChunk> {
  if (!reportsRemoteWriteStatus(chunks)) {
    yield* chunks;
    return;
  }
  const iterator = chunks[Symbol.asyncIterator]();
  try {
    while (
      chunks.remoteWriteStatus !== "closed" ||
      chunks.readBufferLength > 0
    ) {
      const next = await iterator.next();
      if (next.done === true) {
        return;
      }
      yield next.value;
    }
  } finally {
    await iterator.return?.();
  }
}

export const readSingleDaStreamFrame = async (
  chunks: AsyncIterable<DaStreamChunk> | Iterable<DaStreamChunk>,
  options: DaStreamFrameOptions = {},
): Promise<Buffer> => {
  let frame: Buffer | undefined;
  for await (const next of decodeDaStreamFrames(chunks, options)) {
    if (frame !== undefined) {
      throw new Error("expected exactly one DA libp2p stream frame");
    }
    frame = next;
  }
  if (frame === undefined) {
    throw new Error("missing DA libp2p stream frame");
  }
  return frame;
};

export const writeDaStreamFrame = async (
  stream: DaWritableStream,
  payload: Uint8Array,
  options: DaStreamFrameOptions & { readonly close?: boolean } = {},
): Promise<void> => {
  const startedAt = readMonotonicNow(options);
  try {
    const accepted = stream.send(encodeDaStreamFrame(payload, options));
    if (!accepted && stream.onDrain !== undefined) {
      await stream.onDrain();
    }
    if (options.close === true && stream.close !== undefined) {
      await stream.close();
    }
  } finally {
    recordFrameTiming(options, "frame_write", startedAt);
  }
};

const frameLimit = (options: DaStreamFrameOptions): number => {
  const maxFrameBytes =
    options.maxFrameBytes ?? DA_TRANSPORT_LIMITS.maxPayloadBytes;
  if (
    !Number.isSafeInteger(maxFrameBytes) ||
    maxFrameBytes <= 0 ||
    maxFrameBytes > 0xffffffff
  ) {
    throw new Error("DA libp2p stream frame limit must fit in uint32");
  }
  return maxFrameBytes;
};

const chunkView = (chunk: DaStreamChunk): Uint8Array =>
  chunk.subarray(0, chunk.byteLength);

const readMonotonicNow = (
  options: DaStreamFrameOptions,
): number | undefined => {
  try {
    return (options.timing?.monotonicNow ?? (() => performance.now()))();
  } catch {
    return undefined;
  }
};

const recordFrameTiming = (
  options: DaStreamFrameOptions,
  stage: "frame_receive" | "frame_write",
  startedAt: number | undefined,
): void => {
  if (startedAt === undefined) return;
  const completedAt = readMonotonicNow(options);
  if (completedAt === undefined) return;
  try {
    options.timing?.onStageTiming?.(stage, completedAt - startedAt);
  } catch {
    // Observability must not change stream framing semantics.
  }
};
