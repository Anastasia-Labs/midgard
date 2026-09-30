import { type ChildProcessWithoutNullStreams, spawn } from "node:child_process";
import { createHash, timingSafeEqual } from "node:crypto";
import { readFile } from "node:fs/promises";

import { encodeNativeMpfRpcFrame, NativeMpfRpcFrameDecoder } from "./codec.js";
import {
  NATIVE_MPF_OWNER_DEFAULT_CAPS,
  NATIVE_MPF_RPC_SCHEMA,
  type NativeMpfRpcFrame,
  NativeMpfRpcKind,
} from "./protocol.js";
import {
  assertBufferLength,
  digest,
  FULL_INDEX_MAX_RECORDS,
  type PendingPromotion,
  type PendingRequest,
  SELF_TEST_DOMAIN,
  ZERO_EPOCH,
} from "./service.normalize-owner-options.js";

export class NativeChildRpc {
  private readonly child: ChildProcessWithoutNullStreams;
  private readonly decoder: NativeMpfRpcFrameDecoder;
  private readonly pending = new Map<bigint, PendingRequest>();
  private readonly promotions = new Map<bigint, PendingPromotion>();
  private requestId = 0n;
  private ownerEpoch = ZERO_EPOCH;
  private stderr = "";
  private closed = false;
  private failureHandler: ((error: Error) => void) | undefined;

  public constructor(
    binaryPath: string,
    private readonly binarySha256: string,
    private readonly maxFrameBytes: number,
    private readonly timeoutMs: number,
    onChildSpawnForTests?: (pid: number) => void,
  ) {
    this.decoder = new NativeMpfRpcFrameDecoder(
      (chunks) => digest(...chunks),
      maxFrameBytes,
    );
    this.child = spawn(binaryPath, ["--rpc"], {
      stdio: ["pipe", "pipe", "pipe"],
    });
    if (this.child.pid !== undefined) onChildSpawnForTests?.(this.child.pid);
    this.child.stdout.on("data", (chunk: Buffer) => this.onData(chunk));
    this.child.stderr.on("data", (chunk: Buffer) => {
      this.stderr = (this.stderr + chunk.toString("utf8")).slice(-16_384);
    });
    this.child.once("error", (error) => this.failAll(error));
    this.child.once("exit", (code, signal) => {
      this.failAll(
        new Error(
          `Native MPF owner exited: code=${String(code)},signal=${String(signal)},stderr=${this.stderr}`,
        ),
      );
    });
  }

  public async handshake(): Promise<void> {
    const response = await this.request(
      NativeMpfRpcKind.Hello,
      Buffer.from(this.binarySha256, "hex"),
      new Set([NativeMpfRpcKind.HelloAck]),
      true,
      NATIVE_MPF_OWNER_DEFAULT_CAPS.handshakeTimeoutMs,
    );
    this.ownerEpoch = assertBufferLength(response.ownerEpoch, 16, "ownerEpoch");
    if (response.payload.byteLength !== 82) {
      throw new Error("Native MPF HelloAck payload length is invalid");
    }
    const payload = Buffer.from(response.payload);
    if (
      payload.readUInt16LE(0) !== NATIVE_MPF_RPC_SCHEMA ||
      payload.subarray(2, 34).toString("hex") !== this.binarySha256 ||
      payload.readUInt32LE(34) !== FULL_INDEX_MAX_RECORDS ||
      payload.readUInt32LE(38) !== NATIVE_MPF_OWNER_DEFAULT_CAPS.maxEvents ||
      payload.readUInt32LE(42) !== NATIVE_MPF_OWNER_DEFAULT_CAPS.maxOps ||
      payload.readUInt32LE(46) !==
        NATIVE_MPF_OWNER_DEFAULT_CAPS.maxActiveGenerations ||
      !timingSafeEqual(payload.subarray(50), digest(SELF_TEST_DOMAIN))
    ) {
      throw new Error("Native MPF HelloAck capability/self-test mismatch");
    }
  }

  public get epoch(): Buffer {
    return Buffer.from(this.ownerEpoch);
  }

  public get isClosed(): boolean {
    return this.closed;
  }

  public setFailureHandler(handler: (error: Error) => void): void {
    this.failureHandler = handler;
  }

  public request(
    kind: NativeMpfRpcKind,
    payload: Uint8Array,
    expected: ReadonlySet<NativeMpfRpcKind>,
    zeroEpoch = false,
    timeoutMs = this.timeoutMs,
  ): Promise<NativeMpfRpcFrame> {
    if (this.closed)
      return Promise.reject(new Error("Native MPF owner is closed"));
    const requestId = ++this.requestId;
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        const error = new Error(
          `Native MPF RPC request timed out: kind=${kind.toString()}`,
        );
        this.child.kill("SIGKILL");
        this.failAll(error);
      }, timeoutMs);
      this.pending.set(requestId, { expected, resolve, reject, timer });
      try {
        this.child.stdin.write(
          encodeNativeMpfRpcFrame(
            {
              schema: NATIVE_MPF_RPC_SCHEMA,
              kind,
              requestId,
              ownerEpoch: zeroEpoch ? ZERO_EPOCH : this.ownerEpoch,
              payload,
            },
            (chunks) => digest(...chunks),
            this.maxFrameBytes,
          ),
        );
      } catch (error) {
        clearTimeout(timer);
        this.pending.delete(requestId);
        reject(error instanceof Error ? error : new Error(String(error)));
      }
    });
  }

  public promotion(
    generationId: Uint8Array,
    timeoutMs: number,
  ): Promise<{ frame: NativeMpfRpcFrame; bytes: Buffer }> {
    if (this.closed)
      return Promise.reject(new Error("Native MPF owner is closed"));
    const requestId = ++this.requestId;
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        const error = new Error("Native MPF promotion stream timed out");
        this.child.kill("SIGKILL");
        this.failAll(error);
      }, timeoutMs);
      this.promotions.set(requestId, { chunks: [], resolve, reject, timer });
      try {
        this.child.stdin.write(
          encodeNativeMpfRpcFrame(
            {
              schema: NATIVE_MPF_RPC_SCHEMA,
              kind: NativeMpfRpcKind.PreparePromotion,
              requestId,
              ownerEpoch: this.ownerEpoch,
              payload: generationId,
            },
            (chunks) => digest(...chunks),
            this.maxFrameBytes,
          ),
        );
      } catch (error) {
        clearTimeout(timer);
        this.promotions.delete(requestId);
        reject(error instanceof Error ? error : new Error(String(error)));
      }
    });
  }

  private onData(chunk: Buffer): void {
    try {
      for (const frame of this.decoder.push(chunk)) this.dispatch(frame);
    } catch (error) {
      this.child.kill("SIGKILL");
      this.failAll(error instanceof Error ? error : new Error(String(error)));
    }
  }

  private dispatch(frame: NativeMpfRpcFrame): void {
    if (!timingSafeEqual(Buffer.from(frame.ownerEpoch), this.ownerEpoch)) {
      if (
        !(
          this.ownerEpoch.equals(ZERO_EPOCH) &&
          frame.kind === NativeMpfRpcKind.HelloAck
        )
      ) {
        throw new Error("Native MPF RPC response owner epoch mismatch");
      }
    }
    const promotion = this.promotions.get(frame.requestId);
    if (promotion !== undefined) {
      if (frame.kind === NativeMpfRpcKind.PromotionChunk) {
        const total = promotion.chunks.reduce(
          (sum, value) => sum + value.length,
          0,
        );
        if (
          total + frame.payload.byteLength >
          NATIVE_MPF_OWNER_DEFAULT_CAPS.maxGeneratedBytes
        ) {
          throw new Error(
            "Native MPF promotion stream exceeds generated-byte cap",
          );
        }
        promotion.chunks.push(Buffer.from(frame.payload));
        return;
      }
      clearTimeout(promotion.timer);
      this.promotions.delete(frame.requestId);
      if (frame.kind === NativeMpfRpcKind.Error) {
        promotion.reject(
          new Error(Buffer.from(frame.payload).toString("utf8")),
        );
      } else if (frame.kind !== NativeMpfRpcKind.PromotionEnd) {
        promotion.reject(
          new Error(
            `Unexpected native MPF promotion response ${frame.kind.toString()}`,
          ),
        );
      } else {
        promotion.resolve({ frame, bytes: Buffer.concat(promotion.chunks) });
      }
      return;
    }
    const pending = this.pending.get(frame.requestId);
    if (pending === undefined) {
      throw new Error(
        `Native MPF RPC unsolicited response id=${frame.requestId.toString()}`,
      );
    }
    clearTimeout(pending.timer);
    this.pending.delete(frame.requestId);
    if (frame.kind === NativeMpfRpcKind.Error) {
      pending.reject(new Error(Buffer.from(frame.payload).toString("utf8")));
    } else if (!pending.expected.has(frame.kind)) {
      pending.reject(
        new Error(
          `Unexpected native MPF RPC response kind ${frame.kind.toString()}`,
        ),
      );
    } else {
      pending.resolve(frame);
    }
  }

  private failAll(error: Error): void {
    if (this.closed) return;
    this.closed = true;
    for (const pending of this.pending.values()) {
      clearTimeout(pending.timer);
      pending.reject(error);
    }
    this.pending.clear();
    for (const pending of this.promotions.values()) {
      clearTimeout(pending.timer);
      pending.reject(error);
    }
    this.promotions.clear();
    this.failureHandler?.(error);
  }

  public async close(): Promise<void> {
    if (this.closed) return;
    await this.request(
      NativeMpfRpcKind.Shutdown,
      Buffer.alloc(0),
      new Set([NativeMpfRpcKind.ShutdownAck]),
      false,
      NATIVE_MPF_OWNER_DEFAULT_CAPS.shutdownTimeoutMs,
    );
    this.closed = true;
    this.child.stdin.end();
  }
}

export const assertPinnedOwnerBinary = async (
  binaryPath: string,
  binarySha256: string,
): Promise<void> => {
  const actualSha = createHash("sha256")
    .update(await readFile(binaryPath))
    .digest("hex");
  if (actualSha !== binarySha256) {
    throw new Error(
      `Native MPF owner binary SHA-256 mismatch: expected=${binarySha256},actual=${actualSha}`,
    );
  }
};
