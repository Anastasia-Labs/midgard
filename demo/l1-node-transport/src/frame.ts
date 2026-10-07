import {
  type CborInput,
  CborMap,
  type CborValue,
  decodeCbor,
  encodeCbor,
} from "./cbor.js";

/**
 * Frame layout (README.md, "Frame protocol"):
 *
 *   u32 big-endian header length | u32 big-endian payload length |
 *   header: one CBOR map with text keys | payload: raw bytes
 */
export const FRAME_PROTOCOL_VERSION = 1;
export const MAX_HEADER_BYTES = 64 * 1024;
export const MAX_PAYLOAD_BYTES = 64 * 1024 * 1024;
const LENGTHS = 8;

export type Frame = Readonly<{
  header: Readonly<Record<string, CborValue>>;
  payload: Uint8Array;
}>;

export class FrameError extends Error {
  override readonly name = "FrameError";
}

export const encodeFrame = (
  header: { readonly [key: string]: CborInput | undefined },
  payload: Uint8Array = new Uint8Array(),
): Buffer => {
  const encoded = encodeCbor(header);
  if (encoded.length > MAX_HEADER_BYTES || payload.length > MAX_PAYLOAD_BYTES)
    throw new FrameError("frame exceeds its byte bound");
  const frame = Buffer.allocUnsafe(LENGTHS + encoded.length + payload.length);
  frame.writeUInt32BE(encoded.length, 0);
  frame.writeUInt32BE(payload.length, 4);
  frame.set(encoded, LENGTHS);
  frame.set(payload, LENGTHS + encoded.length);
  return frame;
};

export const decodeFrameHeader = (
  bytes: Uint8Array,
): Readonly<Record<string, CborValue>> => {
  const value = decodeCbor(bytes);
  if (!(value instanceof CborMap))
    throw new FrameError("frame header is not a CBOR map");
  const header = value.textRecord("frame header");
  if (typeof header.type !== "string" || header.type.length === 0)
    throw new FrameError("frame header has no type");
  return header;
};

/** Splits a byte stream into whole frames. */
export class FrameReader {
  #chunks: Buffer[] = [];
  #buffered = 0;

  push(chunk: Buffer): Frame[] {
    this.#chunks.push(chunk);
    this.#buffered += chunk.length;
    const frames: Frame[] = [];
    for (;;) {
      if (this.#buffered < LENGTHS) return frames;
      const lengths = this.#peek(LENGTHS);
      const headerLength = lengths.readUInt32BE(0);
      const payloadLength = lengths.readUInt32BE(4);
      if (
        headerLength === 0 ||
        headerLength > MAX_HEADER_BYTES ||
        payloadLength > MAX_PAYLOAD_BYTES
      )
        throw new FrameError("frame exceeds its byte bound");
      const total = LENGTHS + headerLength + payloadLength;
      if (this.#buffered < total) return frames;
      const bytes = this.#take(total);
      frames.push({
        header: decodeFrameHeader(
          bytes.subarray(LENGTHS, LENGTHS + headerLength),
        ),
        payload: bytes.subarray(LENGTHS + headerLength),
      });
    }
  }

  /** Bytes of an unfinished frame. */
  get pending(): number {
    return this.#buffered;
  }

  #peek(size: number): Buffer {
    if (this.#chunks[0]!.length < size)
      this.#chunks = [Buffer.concat(this.#chunks)];
    return this.#chunks[0]!.subarray(0, size);
  }

  #take(size: number): Buffer {
    if (this.#chunks[0]!.length < size)
      this.#chunks = [Buffer.concat(this.#chunks)];
    const first = this.#chunks[0]!;
    const taken = first.subarray(0, size);
    if (first.length === size) this.#chunks.shift();
    else this.#chunks[0] = first.subarray(size);
    this.#buffered -= size;
    return taken;
  }
}
