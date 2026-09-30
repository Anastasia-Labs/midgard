import "./workflow-kupmios-source.admitted-historical-kupmios-page-contexts.js";

import { describe, expect, it, vi } from "vitest";

import {
  readAdmittedLocalKupmiosBoundary,
  readAdmittedLocalKupmiosPredecessorPoint,
  readAdmittedLocalKupmiosRawBlockAtPoint,
} from "../src/workflow/index.js";
import {
  chainPoint,
  TARGET,
} from "./workflow-kupmios-source.ogmios-boundary-socket.js";
import { sourceFixture } from "./workflow-kupmios-source.source-fixture.js";

describe("concrete Kupmios transport cancellation and response bounds", () => {
  it("rejects invalid bounds and a non-platform signal before acquisition", () => {
    for (const maxResponseBytes of [0, -1, 1.5, Number.NaN, 67_108_865]) {
      expect(() => sourceFixture({ maxResponseBytes })).toThrow(
        "maxResponseBytes",
      );
    }
    expect(() =>
      sourceFixture({
        signal: Object.create(AbortSignal.prototype) as AbortSignal,
      }),
    ).toThrow("platform AbortSignal");
    const controller = new AbortController();
    controller.abort();
    expect(() => sourceFixture({ signal: controller.signal })).toThrow(
      "aborted",
    );
  });

  it("cancels a pending HTTP fetch and removes the owner listener", async () => {
    const controller = new AbortController();
    const add = vi.spyOn(controller.signal, "addEventListener");
    const remove = vi.spyOn(controller.signal, "removeEventListener");
    let started!: () => void;
    const ready = new Promise<void>((resolve) => {
      started = resolve;
    });
    let requestSignal: AbortSignal | null | undefined;
    const fixture = sourceFixture({
      signal: controller.signal,
      timeoutMs: 500,
      fetchOverride: async (_url, init) => {
        requestSignal = init?.signal;
        started();
        return await new Promise<Response>((_resolve, reject) => {
          requestSignal!.addEventListener(
            "abort",
            () => reject(requestSignal!.reason),
            { once: true },
          );
        });
      },
    });
    const outcome = fixture.source
      .readBoundary()
      .catch((error: unknown) => error);
    await ready;
    controller.abort();
    expect(await outcome).toMatchObject({ name: "AbortError" });
    expect(requestSignal?.aborted).toBe(true);
    expect(fixture.sockets).toHaveLength(1);
    expect(fixture.sockets[0]!.closeCount).toBe(1);
    expect(remove).toHaveBeenCalledWith("abort", add.mock.calls[0]![1]);
    await expect(fixture.source.readBoundary()).rejects.toThrow("aborted");
    expect(fixture.requests).toHaveLength(1);
  });

  it("cancels and releases an active HTTP body reader", async () => {
    const controller = new AbortController();
    let pulled!: () => void;
    const ready = new Promise<void>((resolve) => {
      pulled = resolve;
    });
    const cancel = vi.fn();
    const body = new ReadableStream<Uint8Array>(
      {
        pull: () => {
          pulled();
        },
        cancel,
      },
      { highWaterMark: 0 },
    );
    const fixture = sourceFixture({
      signal: controller.signal,
      timeoutMs: 500,
      fetchOverride: async () => new Response(body),
    });
    const outcome = fixture.source
      .readBoundary()
      .catch((error: unknown) => error);
    await ready;
    controller.abort();
    expect(await outcome).toMatchObject({ name: "AbortError" });
    expect(cancel).toHaveBeenCalledOnce();
    expect(body.locked).toBe(false);
  });

  it("enforces the supplied HTTP cap for declared and streamed bytes", async () => {
    for (const declared of [false, true]) {
      const cancel = vi.fn();
      const body = new ReadableStream<Uint8Array>({
        start: (stream) =>
          stream.enqueue(new TextEncoder().encode(" ".repeat(1025))),
        cancel,
      });
      const fixture = sourceFixture({
        maxResponseBytes: 1024,
        fetchOverride: async () =>
          new Response(body, {
            headers: declared ? { "content-length": "1025" } : {},
          }),
      });
      await expect(fixture.source.readBoundary()).rejects.toThrow(
        "exceeds the raw-source byte bound",
      );
      expect(cancel).toHaveBeenCalledOnce();
      expect(body.locked).toBe(false);
    }
    const exact = sourceFixture({
      maxResponseBytes: 1024,
      fetchOverride: async () => new Response("{}" + " ".repeat(1022)),
    });
    await expect(exact.source.readBoundary()).rejects.toThrow(
      "Kupo response omitted",
    );
  });

  it("bounds physical Ogmios sessions across simultaneous source instances", async () => {
    const controllers = Array.from({ length: 9 }, () => new AbortController());
    const fixtures = controllers.map((controller) =>
      sourceFixture({
        signal: controller.signal,
        socketBehavior: { respond: false },
      }),
    );
    const outcomes = fixtures.map((fixture) =>
      fixture.source.readBoundary().catch((error: unknown) => error),
    );
    try {
      await vi.waitFor(() =>
        expect(
          fixtures.flatMap(({ sockets }) => sockets).length,
        ).toBeGreaterThanOrEqual(4),
      );
      expect(fixtures.flatMap(({ sockets }) => sockets)).toHaveLength(4);
    } finally {
      controllers.forEach((controller) => controller.abort());
      await Promise.all(outcomes);
    }
  });

  it("holds capacity through delayed physical close and cancels queued acquisition", async () => {
    const controllers = Array.from({ length: 6 }, () => new AbortController());
    const fixtures = controllers.map((controller) =>
      sourceFixture({
        signal: controller.signal,
        socketBehavior: { respond: false, close: false },
      }),
    );
    const outcomes = fixtures.map((fixture) =>
      fixture.source.readBoundary().catch((error: unknown) => error),
    );
    try {
      await vi.waitFor(() =>
        expect(fixtures.flatMap(({ sockets }) => sockets)).toHaveLength(4),
      );
      controllers[5]!.abort();
      expect(await outcomes[5]).toMatchObject({ name: "AbortError" });
      controllers[0]!.abort();
      await vi.waitFor(() =>
        expect(fixtures[0]!.sockets[0]!.closeCount).toBe(1),
      );
      expect(fixtures.flatMap(({ sockets }) => sockets)).toHaveLength(4);
      fixtures[0]!.sockets[0]!.emit("close", { code: 1000 });
      await vi.waitFor(() => expect(fixtures[4]!.sockets).toHaveLength(1));
      expect(fixtures[5]!.sockets).toHaveLength(0);
    } finally {
      controllers.forEach((controller) => controller.abort());
      fixtures
        .flatMap(({ sockets }) => sockets)
        .forEach((socket) => socket.emit("close", { code: 1000 }));
      await Promise.all(outcomes);
    }
    expect(
      fixtures
        .flatMap(({ sockets }) => sockets)
        .every((socket) => [...socket.listeners.values()].flat().length === 0),
    ).toBe(true);
  });

  it("fails boundedly without releasing physically unclosed capacity", async () => {
    vi.useFakeTimers();
    const fixtures = Array.from({ length: 5 }, () =>
      sourceFixture({ timeoutMs: 25, socketBehavior: { close: false } }),
    );
    const outcomes = fixtures.map((fixture) =>
      fixture.source.readBoundary().catch((error: unknown) => error),
    );
    try {
      await vi.advanceTimersByTimeAsync(25);
      const failures = await Promise.all(outcomes);
      expect(
        failures.slice(0, 4).map((error) => (error as Error).message),
      ).toEqual(Array(4).fill("Ogmios physical socket close timed out"));
      expect(failures[4]).toMatchObject({
        message: "Ogmios session capacity wait timed out",
      });
      expect(fixtures.flatMap(({ sockets }) => sockets)).toHaveLength(4);
      expect(vi.getTimerCount()).toBe(0);
    } finally {
      fixtures
        .flatMap(({ sockets }) => sockets)
        .forEach((socket) => socket.emit("close", { code: 1000 }));
      await Promise.all(outcomes);
      vi.useRealTimers();
    }
    await expect(sourceFixture().source.readBoundary()).resolves.toBeDefined();
  });

  it("retains close diagnostics and rejects the failed RPC without retrying", async () => {
    const fixture = sourceFixture({ socketBehavior: { respond: false } });
    const outcome = fixture.source
      .readBoundary()
      .catch((error: unknown) => error);
    await fixture.socketCreated;
    await new Promise<void>((resolve) => setTimeout(resolve, 0));
    fixture.sockets[0]!.emit("close", {
      code: 1011,
      reason: "node connection resource exhausted",
      wasClean: true,
    });
    const error = await outcome;
    expect(error).toBeInstanceOf(Error);
    expect((error as Error).message).toContain(
      '"pendingMethods":["findIntersection"]',
    );
    expect((error as Error).message).toContain('"code":1011');
    expect((error as Error).message).toContain(
      "node connection resource exhausted",
    );
    expect(fixture.sockets).toHaveLength(1);
    expect([...fixture.sockets[0]!.listeners.values()].flat()).toHaveLength(0);
  });

  it("deduplicates concurrent reads of one exact block before taking session capacity", async () => {
    const fixture = sourceFixture();
    const blocks = await Promise.all(
      Array.from({ length: 9 }, () =>
        readAdmittedLocalKupmiosRawBlockAtPoint({
          source: fixture.source,
          point: chainPoint(),
        }),
      ),
    );
    expect(blocks.every((block) => block.point.blockHash === TARGET)).toBe(
      true,
    );
    expect(fixture.sockets).toHaveLength(1);
    expect(fixture.sockets[0]!.closeCount).toBe(1);
  });

  it.each(["opening", "request"] as const)(
    "aborts an Ogmios %s and disposes its timers and listeners",
    async (phase) => {
      vi.useFakeTimers();
      try {
        const controller = new AbortController();
        const add = vi.spyOn(controller.signal, "addEventListener");
        const remove = vi.spyOn(controller.signal, "removeEventListener");
        const fixture = sourceFixture({
          signal: controller.signal,
          timeoutMs: 500,
          socketBehavior:
            phase === "opening" ? { open: false } : { respond: false },
        });
        const outcome = fixture.source
          .readBoundary()
          .catch((error: unknown) => error);
        const socket = await fixture.socketCreated;
        // Allow the existing open microtask and request continuation to run.
        await Promise.resolve();
        await Promise.resolve();
        controller.abort();
        expect(await outcome).toMatchObject({ name: "AbortError" });
        expect(socket.closeCount).toBe(1);
        expect([...socket.listeners.values()].flat()).toHaveLength(0);
        expect(remove.mock.calls).toHaveLength(add.mock.calls.length);
        expect(vi.getTimerCount()).toBe(0);
        await expect(fixture.source.readBoundary()).rejects.toThrow("aborted");
        expect(fixture.sockets).toHaveLength(1);
      } finally {
        vi.useRealTimers();
      }
    },
  );

  it.each(["opening", "request"] as const)(
    "closes an Ogmios %s timeout without retained timers",
    async (phase) => {
      vi.useFakeTimers();
      try {
        const fixture = sourceFixture({
          timeoutMs: 25,
          socketBehavior:
            phase === "opening" ? { open: false } : { respond: false },
        });
        const outcome = fixture.source
          .readBoundary()
          .catch((error: unknown) => error);
        const socket = await fixture.socketCreated;
        await vi.advanceTimersByTimeAsync(25);
        expect(await outcome).toBeInstanceOf(Error);
        expect(socket.closeCount).toBe(1);
        expect([...socket.listeners.values()].flat()).toHaveLength(0);
        expect(vi.getTimerCount()).toBe(0);
      } finally {
        vi.useRealTimers();
      }
    },
  );

  it("closes opening error and active send failures through the same cleanup", async () => {
    for (const openingFailure of [true, false]) {
      const fixture = sourceFixture({
        timeoutMs: 500,
        socketBehavior: openingFailure ? { open: false } : { sendError: true },
      });
      const outcome = fixture.source
        .readBoundary()
        .catch((error: unknown) => error);
      const socket = await fixture.socketCreated;
      if (openingFailure) socket.emit("error", {});
      expect(await outcome).toBeInstanceOf(Error);
      expect(socket.closeCount).toBe(1);
      expect([...socket.listeners.values()].flat()).toHaveLength(0);
    }
  });

  it("checks UTF-8 WebSocket response bytes before JSON parsing and closes non-object responses", async () => {
    const overCap = "é".repeat(129);
    const parse = vi.spyOn(JSON, "parse");
    try {
      const fixture = sourceFixture({
        maxResponseBytes: 256,
        socketBehavior: { responseText: overCap },
      });
      await expect(fixture.source.readBoundary()).rejects.toThrow(
        "exceeds the raw-source byte bound",
      );
      expect(parse).not.toHaveBeenCalledWith(overCap);
      expect(fixture.sockets[0]?.closeCount).toBe(1);
    } finally {
      parse.mockRestore();
    }
    const scalar = sourceFixture({ socketBehavior: { responseText: "null" } });
    await expect(scalar.source.readBoundary()).rejects.toThrow(
      "non-object JSON response",
    );
    expect(scalar.sockets[0]?.closeCount).toBe(1);
  });

  it("retains omitted defaults, accepts an exact response cap, and refuses cached reads after cancellation", async () => {
    const original = sourceFixture();
    await original.source.readBoundary();
    const maximum = Math.max(
      ...original.sockets.flatMap((socket) =>
        socket.frames.map((frame) => Buffer.byteLength(frame, "utf8")),
      ),
    );
    const controller = new AbortController();
    const fixture = sourceFixture({
      signal: controller.signal,
      maxResponseBytes: maximum,
    });
    const boundary = await readAdmittedLocalKupmiosBoundary({
      source: fixture.source,
    });
    await fixture.source.scanAddressPage({
      address: "ordinary-address",
      throughPoint: boundary.kupoCheckpoint,
      after: null,
    });
    const unit = "11".repeat(28);
    await fixture.source.scanUnitHistoryPage({
      unit,
      fromGenesis: true,
      throughPoint: boundary.kupoCheckpoint,
      after: null,
    });
    const requests = fixture.requests.length;
    const sockets = fixture.sockets.length;
    controller.abort();
    await expect(
      fixture.source.scanAddressPage({
        address: "ordinary-address",
        throughPoint: boundary.kupoCheckpoint,
        after: null,
      }),
    ).rejects.toThrow("aborted");
    await expect(
      fixture.source.scanUnitHistoryPage({
        unit,
        fromGenesis: true,
        throughPoint: boundary.kupoCheckpoint,
        after: null,
      }),
    ).rejects.toThrow("aborted");
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint({
        source: fixture.source,
        point: boundary.kupoCheckpoint,
      }),
    ).rejects.toThrow("aborted");
    await expect(
      readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: boundary.kupoCheckpoint,
      }),
    ).rejects.toThrow("aborted");
    expect(fixture.requests).toHaveLength(requests);
    expect(fixture.sockets).toHaveLength(sockets);
    expect(
      fixture.sockets.every(
        (socket) =>
          socket.closeCount === 1 &&
          [...socket.listeners.values()].flat().length === 0,
      ),
    ).toBe(true);
  });
});
