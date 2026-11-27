import { expect, it } from "vitest";

import { assertLibp2pDaRetentionDays } from "../src/config.parse-libp2p-da-committee-peers.js";

it("binds the public committee's advertised 120-day retention and refuses an above-floor shorter window", () => {
  expect(
    assertLibp2pDaRetentionDays({
      runtimeRetentionDays: 120,
      manifestRetentionDays: 120,
    }),
  ).toBe(120);
  for (const runtimeRetentionDays of [15, 119, 121])
    expect(() =>
      assertLibp2pDaRetentionDays({
        runtimeRetentionDays,
        manifestRetentionDays: 120,
      }),
    ).toThrow();
});
