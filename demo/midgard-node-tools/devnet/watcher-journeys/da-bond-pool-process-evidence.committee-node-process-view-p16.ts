import "./da-bond-pool-process-evidence.da-bond-cli-submit-evidence-p18.js";

import { describe, expect, it } from "vitest";

import {
  createDaBondPoolStderrCursor,
  parseDaBondPoolReadyz,
} from "./da-bond-pool-process-evidence.js";

describe("committee node process view (P16)", () => {
  it("reads the pool reasons from /readyz", () => {
    const short =
      "da_bond_pool_backing_short: backing=20000000, required=500000000, checkedAt=2026-09-28T00:00:00.000Z";
    const other = "last committee node tick completed with errors";
    expect(
      parseDaBondPoolReadyz(
        503,
        JSON.stringify({
          ready: false,
          reasons: [other, short],
          scanner: { lastStartedAt: "2026-09-28T00:00:01.000Z" },
        }),
      ),
    ).toEqual({
      httpStatus: 503,
      ready: false,
      reasons: [other, short],
      poolReasons: [short],
      scannerLastStartedAt: "2026-09-28T00:00:01.000Z",
    });
    expect(
      parseDaBondPoolReadyz(200, JSON.stringify({ ready: true, reasons: [] })),
    ).toEqual({ httpStatus: 200, ready: true, reasons: [], poolReasons: [] });
  });

  it("reads the L1 source status and its quarantine reason from /readyz", () => {
    const reason = "committee replay cannot advance its durable queue";
    expect(
      parseDaBondPoolReadyz(
        503,
        JSON.stringify({
          ready: false,
          reasons: [`L1 source is quarantined: ${reason}`],
          l1Source: {
            sourceMode: "local_node",
            status: "quarantined",
            observedAt: "2026-09-29T05:52:05.000Z",
            quarantineReason: reason,
          },
        }),
      ).l1Source,
    ).toEqual({ status: "quarantined", quarantineReason: reason });
    expect(
      parseDaBondPoolReadyz(
        200,
        JSON.stringify({
          ready: true,
          reasons: [],
          l1Source: { status: "healthy" },
        }),
      ).l1Source,
    ).toEqual({ status: "healthy" });
  });

  it("refuses a /readyz answer whose status and body disagree, or another shape", () => {
    expect(() =>
      parseDaBondPoolReadyz(200, JSON.stringify({ ready: false, reasons: [] })),
    ).toThrow(/answered 200/u);
    expect(() =>
      parseDaBondPoolReadyz(500, JSON.stringify({ ready: false, reasons: [] })),
    ).toThrow(/answered 500/u);
    expect(() =>
      parseDaBondPoolReadyz(503, JSON.stringify({ ready: false })),
    ).toThrow(/not \{ ready, reasons\[\] \}/u);
    expect(() =>
      parseDaBondPoolReadyz(
        503,
        JSON.stringify({ ready: "false", reasons: [] }),
      ),
    ).toThrow(/not \{ ready, reasons\[\] \}/u);
    expect(() => parseDaBondPoolReadyz(503, "not json")).toThrow();
  });

  it("collects the pool-monitor events from stderr by byte offset, once each, whole lines only, tied to the pid", () => {
    const cursor = createDaBondPoolStderrCursor(4242);
    const names = (events: readonly { pid: number; event: string }[]) =>
      events.map(({ pid, event }) => `${pid}:${event}`);
    // A multi-byte character before the events: offsets are bytes.
    let stderr =
      'DA committee node listening on 127.0.0.1:7001 \u2713\n{"event":"da_bond_pool_backing_short","backing":"20000000"}\n{"event":"l1_submitter_funding","ok":"true"}\n{"event":"da_bond_pool_withd';
    const bytes = () => new Uint8Array(Buffer.from(stderr, "utf8"));
    expect(names(cursor.take(bytes()))).toEqual([
      "4242:da_bond_pool_backing_short",
    ]);
    expect(cursor.offset()).toBe(
      Buffer.byteLength(stderr.slice(0, stderr.lastIndexOf("\n") + 1)),
    );
    expect(cursor.take(bytes())).toEqual([]);
    stderr += 'rawing","unlockAt":"1"}\nnot json {\n';
    expect(names(cursor.take(bytes()))).toEqual([
      "4242:da_bond_pool_withdrawing",
    ]);
    stderr +=
      '{"event":"da_bond_pool_backing_restored"}\n{"event":"da_bond_pool_bonded"}\n';
    const taken = cursor.take(bytes());
    expect(names(taken)).toEqual([
      "4242:da_bond_pool_backing_restored",
      "4242:da_bond_pool_bonded",
    ]);
    expect(taken[1]!.line).toBe('{"event":"da_bond_pool_bonded"}');
    expect(() => cursor.take(new Uint8Array(0))).toThrow(/pid 4242 shrank/u);
  });

  it("collects only the events its predicate admits", () => {
    const stderr = new Uint8Array(
      Buffer.from(
        '{"event":"availability_responder","headerHash":"b1","status":"unavailable"}\n{"event":"da_bond_pool_bonded"}\n{"event":"availability_responder_extra"}\n',
        "utf8",
      ),
    );
    const responder = createDaBondPoolStderrCursor(
      7,
      (event) => event === "availability_responder",
    );
    expect(responder.take(stderr)).toEqual([
      {
        pid: 7,
        event: "availability_responder",
        line: '{"event":"availability_responder","headerHash":"b1","status":"unavailable"}',
      },
    ]);
    expect(
      createDaBondPoolStderrCursor(7)
        .take(stderr)
        .map(({ event }) => event),
    ).toEqual(["da_bond_pool_bonded"]);
  });

  it("collects only pool transitions by default, never read failures or backoffs", () => {
    const stderr = new Uint8Array(
      Buffer.from(
        '{"event":"da_bond_pool_read_failed","error":"fetch failed","failedAt":"t"}\n' +
          '{"event":"da_bond_pool_apply_backoff","headerHash":"b2","reason":"pool-under-backed"}\n' +
          '{"event":"da_bond_pool_init_backoff","headerHash":"b2","reason":"pool-unavailable"}\n' +
          '{"event":"da_bond_pool_backing_short","backing":"1"}\n' +
          '{"event":"da_bond_pool_backing_restored"}\n' +
          '{"event":"da_bond_pool_withdrawing","unlockAt":"1"}\n' +
          '{"event":"da_bond_pool_bonded"}\n',
        "utf8",
      ),
    );
    expect(
      createDaBondPoolStderrCursor(9)
        .take(stderr)
        .map(({ event }) => event),
    ).toEqual([
      "da_bond_pool_backing_short",
      "da_bond_pool_backing_restored",
      "da_bond_pool_withdrawing",
      "da_bond_pool_bonded",
    ]);
  });
});
