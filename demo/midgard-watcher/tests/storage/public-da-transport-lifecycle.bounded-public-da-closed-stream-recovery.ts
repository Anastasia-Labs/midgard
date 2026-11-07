import {
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import { encodeWatcherPublicDaFrame } from "../../src/storage/public-da-libp2p-transport.js";
import {
  closure,
  type FailurePhase,
  requestFixture,
  scriptedTransport,
} from "./public-da-transport-lifecycle.scripted-transport.js";

describe("bounded public DA closed-stream recovery", () => {
  it.each(
    ["submission", "mismatched protocol", "foreign namespace"].flatMap(
      (scenario) =>
        ["StreamStateError", "StreamResetError"].map((errorName) => ({
          scenario,
          errorName,
        })),
    ),
  )(
    "does not replay $scenario after $errorName",
    async ({ scenario, errorName }) => {
      const request = requestFixture();
      if (scenario === "submission") {
        Object.assign(request, {
          protocol: DaRequestResponseProtocol.payloadSubmit,
          protocolId: daRequestResponseProtocolId(
            "22".repeat(32),
            DaRequestResponseProtocol.payloadSubmit,
          ),
        });
      } else if (scenario === "mismatched protocol") {
        Object.assign(request, {
          protocolId: daRequestResponseProtocolId(
            "22".repeat(32),
            DaRequestResponseProtocol.payloadSubmit,
          ),
        });
      } else {
        Object.assign(request, {
          protocolId: request.protocolId.replace("/midgard/", "/foreign/"),
        });
      }
      const { transport, dials } = scriptedTransport({
        phase: "read",
        error: closure(errorName),
      });
      await transport.start();
      try {
        await expect(transport.request(request)).rejects.toThrow(
          "read failed (attempt 1",
        );
        expect(dials).toHaveLength(1);
      } finally {
        await transport.stop();
      }
    },
  );
  it.each<FailurePhase>(["dial", "send", "drain", "close", "read"])(
    "retries closure during %s once, discarding any partial response",
    async (phase) => {
      const { transport, dials, sent, abort } = scriptedTransport({ phase });
      await transport.start();
      try {
        const request = requestFixture();
        await expect(transport.request(request)).resolves.toEqual(
          Buffer.from("response"),
        );
        expect(dials).toHaveLength(2);
        expect(dials[0]).toEqual(dials[1]);
        expect(dials[0]?.signal).toBe(request.signal);
        expect(
          sent.every((frame) =>
            frame.equals(encodeWatcherPublicDaFrame(request.requestCbor)),
          ),
        ).toBe(true);
        expect(abort).toHaveBeenCalledTimes(phase === "dial" ? 0 : 1);
      } finally {
        await transport.stop();
      }
    },
  );

  it.each([
    "StreamClosedError",
    "StreamResetError",
    "ConnectionClosedError",
    "ConnectionClosingError",
    "MuxerClosedError",
  ])(
    "recognizes pinned %s closure without retrying generic errors",
    async (name) => {
      const { transport, dials } = scriptedTransport({
        phase: "read",
        error: closure(name),
      });
      await transport.start();
      try {
        await expect(transport.request(requestFixture())).resolves.toEqual(
          Buffer.from("response"),
        );
        expect(dials).toHaveLength(2);
      } finally {
        await transport.stop();
      }
    },
  );

  it("bounds permanent closure at two attempts and retains stage, type, state and cause", async () => {
    const error = closure();
    const { transport, dials, abort } = scriptedTransport({
      phase: "read",
      permanent: true,
      error,
    });
    await transport.start();
    try {
      await expect(transport.request(requestFixture())).rejects.toMatchObject({
        cause: error,
        message: expect.stringContaining(
          "read failed (attempt 2, StreamStateError, writeStatus=writable)",
        ),
      });
      expect(dials).toHaveLength(2);
      expect(abort).toHaveBeenCalledTimes(2);
    } finally {
      await transport.stop();
    }
  });

  it.each([
    {
      phase: "read" as const,
      error: new Error("Cannot write to a stream that is closed"),
    },
    {
      phase: "send" as const,
      error: Object.assign(new Error("invalid stream operation"), {
        name: "StreamStateError",
      }),
    },
    {
      phase: "read" as const,
      error: Object.assign(new Error("aborted"), {
        name: "StreamAbortedError",
      }),
    },
    { malformed: true },
    { foreignPeer: true },
  ])(
    "does not retry non-closure, identity or malformed-frame failure %j",
    async (input) => {
      const { transport, dials, abort } = scriptedTransport(input);
      await transport.start();
      try {
        await expect(
          transport.request(requestFixture()),
        ).rejects.toBeInstanceOf(Error);
        expect(dials).toHaveLength(1);
        expect(abort).toHaveBeenCalledOnce();
      } finally {
        await transport.stop();
      }
    },
  );

  it("snapshots exact bytes, peer, protocol and signal across caller mutation", async () => {
    const request = requestFixture();
    const originalSignal = request.signal;
    const originalProtocol = request.protocolId;
    const { transport, sent, dials } = scriptedTransport({
      phase: "read",
      onFailure: () => {
        request.requestCbor.fill(0xff);
        Object.assign(request, {
          protocolId: "changed",
          peerId: "foreign",
          signal: new AbortController().signal,
        });
      },
    });
    await transport.start();
    try {
      await expect(transport.request(request)).resolves.toEqual(
        Buffer.from("response"),
      );
      expect(sent).toEqual([
        encodeWatcherPublicDaFrame(Buffer.from([0xa0])),
        encodeWatcherPublicDaFrame(Buffer.from([0xa0])),
      ]);
      expect(
        dials.map(({ signal, protocol }) => ({ signal, protocol })),
      ).toEqual([
        { signal: originalSignal, protocol: originalProtocol },
        { signal: originalSignal, protocol: originalProtocol },
      ]);
    } finally {
      await transport.stop();
    }
  });

  it.each(["StreamStateError", "StreamResetError"])(
    "does not redial after cancellation during a partial response with %s",
    async (errorName) => {
      const controller = new AbortController();
      const reason = new Error("lease revoked");
      const { transport, dials } = scriptedTransport({
        phase: "read",
        error: closure(errorName),
        onFailure: () => controller.abort(reason),
      });
      await transport.start();
      try {
        await expect(
          transport.request(requestFixture(controller.signal)),
        ).rejects.toBe(reason);
        expect(dials).toHaveLength(1);
      } finally {
        await transport.stop();
      }
    },
  );

  it("does not extend the original deadline to redial", async () => {
    const signal = AbortSignal.timeout(20);
    const { transport, dials } = scriptedTransport({
      phase: "dial",
      onFailure: () => new Promise((resolve) => setTimeout(resolve, 30)),
    });
    await transport.start();
    try {
      const failure: unknown = await transport
        .request(requestFixture(signal))
        .catch((cause: unknown) => cause);
      expect(signal.aborted).toBe(true);
      expect(failure).toBe(signal.reason);
      expect(dials).toHaveLength(1);
    } finally {
      await transport.stop();
    }
  });

  it("does not redial after owner shutdown during a partial response", async () => {
    const { transport, dials } = scriptedTransport({
      phase: "read",
      onFailure: () => transport.stop(),
    });
    await transport.start();
    await expect(transport.request(requestFixture())).rejects.toThrow(
      "read failed (attempt 1",
    );
    expect(dials).toHaveLength(1);
  });
});
