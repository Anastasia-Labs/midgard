import { describe, expect, it } from "vitest";

import { createHttpIngress } from "../src/commands/listen-http-ingress.js";

/** A clock the test moves, and an ingress with the public defaults on it. */
const ingressAt = (start = 0) => {
  let now = start;
  const ingress = createHttpIngress({ now: () => now });
  /** Sends `count` one-transaction submissions; the accepted ones release at once. */
  const submit = (client: string, count = 1, cost = 1) => {
    let accepted = 0;
    let retryAfter: number | undefined;
    for (let index = 0; index < count; index += 1) {
      const result = ingress.reserve(client, "write", 0, cost);
      if (result.accepted) {
        accepted += 1;
        result.release();
      } else retryAfter = result.retryAfterSeconds;
    }
    return { accepted, retryAfter };
  };
  return {
    ingress,
    submit,
    set: (ms: number) => {
      now = ms;
    },
    advance: (ms: number) => {
      now += ms;
    },
  };
};

const ipv4 = (first: number, index: number) =>
  `${first}.${(index >> 16) & 255}.${(index >> 8) & 255}.${index & 255}`;

describe("HTTP submission rate buckets", () => {
  it("keeps memory fixed and forgets no client across 1,000,000 distinct clients", () => {
    const node = ingressAt();
    expect(node.submit("192.0.2.1", 101)).toEqual({
      accepted: 100,
      retryAfter: 1,
    });
    const heapBefore = process.memoryUsage().heapUsed;
    let refused = 0;
    for (let index = 0; index < 1_000_000; index += 1)
      refused += 1 - node.submit(ipv4(10, index)).accepted;
    const heapGrowth = process.memoryUsage().heapUsed - heapBefore;
    // No table fills: only the few clients whose cell the exhausted client
    // shares are refused (about 8 expected out of 1,000,000).
    expect(refused).toBeLessThan(64);
    // A table of 1,000,000 client entries alone would take well over 100 MiB.
    expect(heapGrowth).toBeLessThan(64 * 2 ** 20);
    // Nothing was evicted: the exhausted client is still exhausted...
    expect(node.submit("192.0.2.1").accepted).toBe(0);
    // ...and gets exactly one second of refill a second later.
    node.advance(1_000);
    expect(node.submit("192.0.2.1", 30).accepted).toBe(20);
  }, 120_000);

  it("limits one client to its burst and refill, sharing it across one IPv6 /56", () => {
    const node = ingressAt();
    expect(node.submit("2001:db8:7:4200::1", 60).accepted).toBe(60);
    expect(node.submit("2001:db8:7:42ff:9::2", 60).accepted).toBe(40);
    expect(node.submit("2001:db8:7:4300::1", 60).accepted).toBe(60);
    node.advance(1_000);
    expect(node.submit("2001:db8:7:4255::3", 60).accepted).toBe(20);
  });

  it("caps a whole IPv6 /48 or IPv4 /24 at the prefix rate however it spreads over its clients", () => {
    const node = ingressAt();
    const spread = (client: (index: number) => string) => {
      let accepted = 0;
      for (let round = 0; round < 10; round += 1)
        for (let index = 0; index < 256; index += 1)
          accepted += node.submit(client(index)).accepted;
      return accepted;
    };
    const slash56 = (index: number) =>
      `2001:db8:5:${(index << 8).toString(16)}::1`;
    // 256 /56s with 100 each would pass 2,560; the /48 bucket holds 1,000.
    expect(spread(slash56)).toBe(1_000);
    expect(spread((index) => `203.0.113.${index}`)).toBe(1_000);
    // Another /48 keeps its own quota.
    expect(node.submit("2001:db8:6::1", 100).accepted).toBe(100);
    node.advance(1_000);
    expect(spread(slash56)).toBe(200);
  });

  it("refills correctly across 50 days of uptime, past where a millisecond Uint32 clock wraps", () => {
    const wrap = 2 ** 32;
    const node = ingressAt();
    node.set(wrap - 2_000);
    expect(node.submit("198.51.100.9", 101).accepted).toBe(100);
    // Three whole seconds later, across the 49.7-day mark.
    node.set(wrap + 1_000);
    expect(node.submit("198.51.100.9", 101).accepted).toBe(60);
    node.set(50 * 86_400_000);
    expect(node.submit("198.51.100.9", 101).accepted).toBe(100);
    expect(node.submit("198.51.100.10", 101).accepted).toBe(100);
    node.advance(1_000);
    expect(node.submit("198.51.100.9", 101).accepted).toBe(20);
  });

  it("charges a request one token per transaction it carries", () => {
    const node = ingressAt();
    expect(node.submit("198.51.100.20", 4, 30)).toEqual({
      accepted: 3,
      retryAfter: 1,
    });
    expect(node.submit("198.51.100.20", 1, 10).accepted).toBe(1);
  });

  it("holds each client to four requests in flight and frees a slot on release", () => {
    const node = ingressAt();
    const held = Array.from({ length: 4 }, () =>
      node.ingress.reserve("198.51.100.30", "write", 0),
    );
    expect(held.every((result) => result.accepted)).toBe(true);
    expect(node.ingress.reserve("198.51.100.30", "read", 0)).toMatchObject({
      accepted: false,
      status: 503,
    });
    expect(node.submit("198.51.100.31").accepted).toBe(1);
    const [first] = held;
    if (first?.accepted) first.release();
    expect(node.submit("198.51.100.30").accepted).toBe(1);
  });
});
