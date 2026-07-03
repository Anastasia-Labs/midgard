import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";

const LENGTH_PREFIX_BYTES = 4;

export type DaStreamFrameOptions = {
  readonly maxFrameBytes?: number;
  readonly allowEmpty?: boolean;
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
  Buffer.from(payload).copy(frame, LENGTH_PREFIX_BYTES);
  return frame;
};

export async function* decodeDaStreamFrames(
  chunks: AsyncIterable<DaStreamChunk> | Iterable<DaStreamChunk>,
  options: DaStreamFrameOptions = {},
): AsyncGenerator<Buffer> {
  const maxFrameBytes = frameLimit(options);
  let pending = Buffer.alloc(0);
  let expectedLength: number | undefined;
  for await (const chunk of chunks) {
    const bytes = chunkToBuffer(chunk);
    if (bytes.byteLength === 0) {
      continue;
    }
    pending = Buffer.concat([pending, bytes]);
    while (true) {
      if (expectedLength === undefined) {
        if (pending.byteLength < LENGTH_PREFIX_BYTES) {
          break;
        }
        expectedLength = pending.readUInt32BE(0);
        if (expectedLength === 0 && options.allowEmpty !== true) {
          throw new Error("DA libp2p stream frame must not be empty");
        }
        if (expectedLength > maxFrameBytes) {
          throw new Error(
            `DA libp2p stream frame exceeds ${maxFrameBytes.toString()} bytes`,
          );
        }
        pending = pending.subarray(LENGTH_PREFIX_BYTES);
      }
      if (pending.byteLength < expectedLength) {
        break;
      }
      yield Buffer.from(pending.subarray(0, expectedLength));
      pending = pending.subarray(expectedLength);
      expectedLength = undefined;
    }
  }
  if (expectedLength !== undefined || pending.byteLength > 0) {
    throw new Error("incomplete DA libp2p stream frame");
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
  const accepted = stream.send(encodeDaStreamFrame(payload, options));
  if (!accepted && stream.onDrain !== undefined) {
    await stream.onDrain();
  }
  if (options.close === true && stream.close !== undefined) {
    await stream.close();
  }
};

const frameLimit = (options: DaStreamFrameOptions): number => {
  const maxFrameBytes =
    options.maxFrameBytes ?? DA_TRANSPORT_LIMITS_V1.maxPayloadBytes;
  if (
    !Number.isSafeInteger(maxFrameBytes) ||
    maxFrameBytes <= 0 ||
    maxFrameBytes > 0xffffffff
  ) {
    throw new Error("DA libp2p stream frame limit must fit in uint32");
  }
  return maxFrameBytes;
};

const chunkToBuffer = (chunk: DaStreamChunk): Buffer =>
  Buffer.from(chunk.subarray(0, chunk.byteLength));
