import "./workflow-kupmios-source.registration.js";

import { describe, expect, it, vi } from "vitest";

import {
  type FraudProofRawL1Fetch,
  LocalKupmiosTransportUnavailableError,
} from "../src/workflow/index.js";
import { sourceFixture } from "./workflow-kupmios-source.source-fixture.js";

describe("typed raw-source transport failures", () => {
  it.each([429, 500, 502, 503, 504])(
    "marks HTTP %i temporary without accepting response data",
    async (status) => {
      const fixture = sourceFixture({
        fetchOverride: async () =>
          new Response("temporarily unavailable", { status }),
      });
      await expect(fixture.source.readBoundary()).rejects.toBeInstanceOf(
        LocalKupmiosTransportUnavailableError,
      );
    },
  );

  it.each([400, 401, 403, 404])(
    "keeps HTTP %i refusal hard",
    async (status) => {
      const fixture = sourceFixture({
        fetchOverride: async () => new Response("refused", { status }),
      });
      const error = await fixture.source
        .readBoundary()
        .catch((cause: unknown) => cause);
      expect(error).toBeInstanceOf(Error);
      expect(error).not.toBeInstanceOf(LocalKupmiosTransportUnavailableError);
    },
  );

  it("classifies structured network failure but never a message substring", async () => {
    const network = Object.assign(new TypeError("fetch failed"), {
      cause: Object.assign(new Error("socket ended"), { code: "ECONNRESET" }),
    });
    const transport = sourceFixture({
      fetchOverride: async () => {
        throw network;
      },
    });
    await expect(transport.source.readBoundary()).rejects.toMatchObject({
      name: "LocalKupmiosTransportUnavailableError",
      cause: network,
    });
    const ordinary = new Error("ECONNRESET malformed checkpoint");
    const malformed = sourceFixture({
      fetchOverride: async () => {
        throw ordinary;
      },
    });
    await expect(malformed.source.readBoundary()).rejects.toBe(ordinary);
  });

  it("distinguishes internal HTTP timeout from caller cancellation", async () => {
    const fetchOverride: FraudProofRawL1Fetch = async (_url, init) =>
      new Promise((_resolve, reject) => {
        init!.signal!.addEventListener(
          "abort",
          () => reject(new DOMException("request aborted", "AbortError")),
          { once: true },
        );
      });
    await expect(
      sourceFixture({ fetchOverride, timeoutMs: 25 }).source.readBoundary(),
    ).rejects.toBeInstanceOf(LocalKupmiosTransportUnavailableError);
    const controller = new AbortController();
    const fixture = sourceFixture({
      fetchOverride,
      signal: controller.signal,
      timeoutMs: 500,
    });
    const outcome = fixture.source
      .readBoundary()
      .catch((cause: unknown) => cause);
    await vi.waitFor(() => expect(fixture.requests.length).toBeGreaterThan(0));
    controller.abort();
    expect(await outcome).toMatchObject({ name: "AbortError" });
    expect(await outcome).not.toBeInstanceOf(
      LocalKupmiosTransportUnavailableError,
    );
  });

  it.each([
    { label: "JSON", fetchOverride: async () => new Response("{broken") },
    {
      label: "checkpoint headers",
      fetchOverride: async () =>
        new Response("{}", {
          headers: { "x-most-recent-checkpoint": "no", etag: "bad" },
        }),
    },
    {
      label: "byte budget",
      fetchOverride: async () =>
        new Response("failure", {
          status: 503,
          headers: { "content-length": "67108865" },
        }),
    },
  ])(
    "keeps $label failure hard even when transport is available",
    async ({ fetchOverride }) => {
      const error = await sourceFixture({ fetchOverride })
        .source.readBoundary()
        .catch((cause: unknown) => cause);
      expect(error).toBeInstanceOf(Error);
      expect(error).not.toBeInstanceOf(LocalKupmiosTransportUnavailableError);
    },
  );

  it.each(["error", "close"])("types a WebSocket %s event", async (event) => {
    const fixture = sourceFixture({
      timeoutMs: 100,
      socketBehavior: { open: false },
    });
    const outcome = fixture.source
      .readBoundary()
      .catch((cause: unknown) => cause);
    const socket = await fixture.socketCreated;
    socket.emit(event, {
      code: 1006,
      reason: "connection lost",
      wasClean: false,
    });
    expect(await outcome).toBeInstanceOf(LocalKupmiosTransportUnavailableError);
    expect([...socket.listeners.values()].flat()).toHaveLength(0);
  });

  it.each([1002, 1008, 1009])(
    "keeps explicit WebSocket protocol/policy refusal %i hard",
    async (code) => {
      const fixture = sourceFixture({
        timeoutMs: 100,
        socketBehavior: { open: false },
      });
      const outcome = fixture.source
        .readBoundary()
        .catch((cause: unknown) => cause);
      const socket = await fixture.socketCreated;
      socket.emit("close", { code, reason: "peer refusal", wasClean: true });
      expect(await outcome).toBeInstanceOf(Error);
      expect(await outcome).not.toBeInstanceOf(
        LocalKupmiosTransportUnavailableError,
      );
    },
  );

  it.each(["opening", "request"])(
    "types internal WebSocket %s timeout",
    async (phase) => {
      const fixture = sourceFixture({
        timeoutMs: 25,
        socketBehavior:
          phase === "opening" ? { open: false } : { respond: false },
      });
      await expect(fixture.source.readBoundary()).rejects.toBeInstanceOf(
        LocalKupmiosTransportUnavailableError,
      );
      expect(fixture.sockets[0]?.closeCount).toBe(1);
    },
  );

  it.each([
    "{broken",
    JSON.stringify({
      id: 0,
      error: { code: 1000, message: "invalid request" },
    }),
  ])("keeps malformed/RPC error frames hard: %s", async (responseText) => {
    const fixture = sourceFixture({ socketBehavior: { responseText } });
    const error = await fixture.source
      .readBoundary()
      .catch((cause: unknown) => cause);
    expect(error).toBeInstanceOf(Error);
    expect(error).not.toBeInstanceOf(LocalKupmiosTransportUnavailableError);
  });

  it("does not conceal malformed data with a later socket-close timeout", async () => {
    const fixture = sourceFixture({
      timeoutMs: 25,
      socketBehavior: { responseText: "{broken", close: false },
    });
    try {
      const error = await fixture.source
        .readBoundary()
        .catch((cause: unknown) => cause);
      expect(error).toMatchObject({
        message: expect.stringContaining("malformed JSON"),
      });
      expect(error).not.toBeInstanceOf(LocalKupmiosTransportUnavailableError);
    } finally {
      for (const socket of fixture.sockets)
        socket.emit("close", { code: 1000, wasClean: true });
    }
  });
});
