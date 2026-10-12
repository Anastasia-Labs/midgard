/**
 * The DA stream decoder over a real `@libp2p/utils` stream: the muxer's side
 * of the stream is driven directly (`onData`, `onRemoteCloseWrite`), as yamux
 * drives it from the frames of one TCP read.
 */
import type { Logger } from "@libp2p/interface";
import { AbstractStream, type SendResult } from "@libp2p/utils";
import { describe, expect, it } from "vitest";

import {
  encodeDaStreamFrame,
  readSingleDaStreamFrame,
} from "../src/da-stream-codec.js";

const silent: Logger = Object.assign(() => undefined, {
  error: () => undefined,
  trace: () => undefined,
  enabled: false,
  newScope: () => silent,
});

/** An inbound stream whose remote side the test plays. */
class InboundStream extends AbstractStream {
  constructor() {
    super({ id: "da-test", log: silent, direction: "inbound" });
  }
  sendData(data: { readonly byteLength: number }): SendResult {
    return { sentBytes: data.byteLength, canSendMore: true };
  }
  sendReset(): void {}
  sendPause(): void {}
  sendResume(): void {}
  async sendCloseWrite(): Promise<void> {}
  async sendCloseRead(): Promise<void> {}
}

const READ_DEADLINE_MS = 2_000;

/** `reading`, or a rejection naming a reader that never ended. */
const withinDeadline = <A>(reading: Promise<A>): Promise<A> => {
  let timer: ReturnType<typeof setTimeout> | undefined;
  return Promise.race([
    reading,
    new Promise<never>((_, reject) => {
      timer = setTimeout(
        () => reject(new Error("the reader did not end")),
        READ_DEADLINE_MS,
      );
    }),
  ]).finally(() => clearTimeout(timer));
};

/** Lets a started reader attach its listeners before the remote writes. */
const nextMacrotask = () =>
  new Promise<void>((resolve) => setTimeout(resolve, 0));

/** A 300 kB payload, framed and cut into three chunks. */
const threeChunkFrame = () => {
  const payload = Buffer.alloc(300_000);
  for (let index = 0; index < payload.length; index += 1)
    payload[index] = (index * 31) & 0xff;
  const frame = encodeDaStreamFrame(payload);
  return {
    payload,
    chunks: [
      frame.subarray(0, 100_000),
      frame.subarray(100_000, 200_000),
      frame.subarray(200_000),
    ],
  };
};

describe("DA stream decoder on a libp2p stream", () => {
  it("decodes a frame whose last chunks arrive in the same tick as the remote's end", async () => {
    const { payload, chunks } = threeChunkFrame();
    const stream = new InboundStream();
    const reading = withinDeadline(readSingleDaStreamFrame(stream));
    await nextMacrotask();
    for (const chunk of chunks) stream.onData(chunk);
    stream.onRemoteCloseWrite();

    expect((await reading).equals(payload)).toBe(true);
  });

  it("decodes a frame whose chunks and end arrive in separate ticks", async () => {
    const { payload, chunks } = threeChunkFrame();
    const stream = new InboundStream();
    const reading = withinDeadline(readSingleDaStreamFrame(stream));
    for (const chunk of chunks) {
      await nextMacrotask();
      stream.onData(chunk);
    }
    await nextMacrotask();
    stream.onRemoteCloseWrite();

    expect((await reading).equals(payload)).toBe(true);
  });

  it("ends a reader that starts after the remote closed its writable end", async () => {
    const { payload, chunks } = threeChunkFrame();
    const stream = new InboundStream();
    for (const chunk of chunks) stream.onData(chunk);
    stream.onRemoteCloseWrite();
    await nextMacrotask();

    expect(
      (await withinDeadline(readSingleDaStreamFrame(stream))).equals(payload),
    ).toBe(true);
  });

  it("ends a reader that starts after the remote closed without writing", async () => {
    const stream = new InboundStream();
    stream.onRemoteCloseWrite();

    await expect(
      withinDeadline(readSingleDaStreamFrame(stream)),
    ).rejects.toThrow("missing DA libp2p stream frame");
  });

  it("refuses a frame cut short by the remote's end in the same tick", async () => {
    const { chunks } = threeChunkFrame();
    const stream = new InboundStream();
    const reading = withinDeadline(readSingleDaStreamFrame(stream));
    await nextMacrotask();
    stream.onData(chunks[0]!);
    stream.onData(chunks[1]!);
    stream.onRemoteCloseWrite();

    await expect(reading).rejects.toThrow("incomplete DA libp2p stream frame");
  });
});
