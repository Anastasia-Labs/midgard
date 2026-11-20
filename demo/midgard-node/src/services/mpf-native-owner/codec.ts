import { timingSafeEqual } from "node:crypto";

import {
  NATIVE_MPF_OWNER_DEFAULT_CAPS,
  NATIVE_MPF_RPC_DIGEST_DOMAIN,
  NATIVE_MPF_RPC_MAGIC,
  NATIVE_MPF_RPC_SCHEMA,
  NativeMpfRpcKind,
  type NativeMpfRpcFrame,
} from "./protocol.js";

const LENGTH_BYTES = 4;
const HEADER_BYTES = 36;
const DIGEST_BYTES = 32;
const MIN_FRAME_BYTES = HEADER_BYTES + DIGEST_BYTES;

export type NativeMpfRpcDigest = (chunks: readonly Uint8Array[]) => Uint8Array;

const assertDigest = (value: Uint8Array): Buffer => {
  const digest = Buffer.from(value);
  if (digest.byteLength !== DIGEST_BYTES) {
    throw new Error("Native MPF RPC digest must contain exactly 32 bytes");
  }
  return digest;
};

const assertKind: (value: number) => asserts value is NativeMpfRpcKind = (
  value,
) => {
  if (
    !Number.isSafeInteger(value) ||
    value < NativeMpfRpcKind.Hello ||
    value > NativeMpfRpcKind.Error
  ) {
    throw new Error(`Unknown native MPF RPC message kind ${value.toString()}`);
  }
};

const assertEpoch = (value: Uint8Array): Buffer => {
  const epoch = Buffer.from(value);
  if (epoch.byteLength !== 16) {
    throw new Error("Native MPF RPC owner epoch must contain exactly 16 bytes");
  }
  return epoch;
};

export const encodeNativeMpfRpcFrame = (
  frame: NativeMpfRpcFrame,
  digest: NativeMpfRpcDigest,
  maxFrameBytes = NATIVE_MPF_OWNER_DEFAULT_CAPS.maxFrameBytes,
): Buffer => {
  assertKind(frame.kind);
  const epoch = assertEpoch(frame.ownerEpoch);
  const payload = Buffer.from(frame.payload);
  const frameBytes = HEADER_BYTES + payload.byteLength + DIGEST_BYTES;
  if (frameBytes > maxFrameBytes) {
    throw new Error(
      `Native MPF RPC frame exceeds cap: bytes=${frameBytes.toString()},cap=${maxFrameBytes.toString()}`,
    );
  }
  const output = Buffer.allocUnsafe(LENGTH_BYTES + frameBytes);
  output.writeUInt32LE(frameBytes, 0);
  output.write(NATIVE_MPF_RPC_MAGIC, 4, "ascii");
  output.writeUInt16LE(NATIVE_MPF_RPC_SCHEMA, 8);
  output.writeUInt16LE(frame.kind, 10);
  output.writeBigUInt64LE(frame.requestId, 12);
  epoch.copy(output, 20);
  output.writeUInt32LE(payload.byteLength, 36);
  payload.copy(output, 40);
  const bodyEnd = LENGTH_BYTES + HEADER_BYTES + payload.byteLength;
  assertDigest(
    digest([
      Buffer.from(NATIVE_MPF_RPC_DIGEST_DOMAIN),
      output.subarray(LENGTH_BYTES, bodyEnd),
    ]),
  ).copy(output, bodyEnd);
  return output;
};

export class NativeMpfRpcFrameDecoder {
  private buffered = Buffer.alloc(0);

  public constructor(
    private readonly digest: NativeMpfRpcDigest,
    private readonly maxFrameBytes = NATIVE_MPF_OWNER_DEFAULT_CAPS.maxFrameBytes,
  ) {}

  public push(chunk: Uint8Array): readonly NativeMpfRpcFrame[] {
    if (chunk.byteLength === 0) return [];
    this.buffered = Buffer.concat([this.buffered, Buffer.from(chunk)]);
    const frames: NativeMpfRpcFrame[] = [];
    let offset = 0;
    while (this.buffered.byteLength - offset >= LENGTH_BYTES) {
      const frameBytes = this.buffered.readUInt32LE(offset);
      if (frameBytes < MIN_FRAME_BYTES || frameBytes > this.maxFrameBytes) {
        throw new Error(
          `Invalid native MPF RPC frame length ${frameBytes.toString()}`,
        );
      }
      const end = offset + LENGTH_BYTES + frameBytes;
      if (end > this.buffered.byteLength) break;
      const body = this.buffered.subarray(offset + LENGTH_BYTES, end);
      if (body.subarray(0, 4).toString("ascii") !== NATIVE_MPF_RPC_MAGIC) {
        throw new Error("Invalid native MPF RPC frame magic");
      }
      if (body.readUInt16LE(4) !== NATIVE_MPF_RPC_SCHEMA) {
        throw new Error("Unsupported native MPF RPC schema");
      }
      const kind = body.readUInt16LE(6);
      assertKind(kind);
      const payloadBytes = body.readUInt32LE(32);
      if (HEADER_BYTES + payloadBytes + DIGEST_BYTES !== frameBytes) {
        throw new Error("Native MPF RPC payload length does not match frame");
      }
      const digestOffset = HEADER_BYTES + payloadBytes;
      const expected = assertDigest(
        this.digest([
          Buffer.from(NATIVE_MPF_RPC_DIGEST_DOMAIN),
          body.subarray(0, digestOffset),
        ]),
      );
      const actual = body.subarray(digestOffset);
      if (!timingSafeEqual(actual, expected)) {
        throw new Error("Native MPF RPC frame digest mismatch");
      }
      frames.push({
        schema: NATIVE_MPF_RPC_SCHEMA,
        kind,
        requestId: body.readBigUInt64LE(8),
        ownerEpoch: Uint8Array.from(body.subarray(16, 32)),
        payload: Uint8Array.from(body.subarray(HEADER_BYTES, digestOffset)),
      });
      offset = end;
    }
    if (offset > 0) this.buffered = this.buffered.subarray(offset);
    if (this.buffered.byteLength > this.maxFrameBytes + LENGTH_BYTES) {
      throw new Error("Native MPF RPC buffered partial frame exceeds cap");
    }
    return frames;
  }

  public finish(): void {
    if (this.buffered.byteLength !== 0) {
      throw new Error("Native MPF RPC stream ended with a truncated frame");
    }
  }
}
