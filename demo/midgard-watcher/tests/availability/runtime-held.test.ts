import { describe, expect, it } from "vitest";

import { utxo } from "../support/availability-challenge-fixture.js";
import {
  ADA,
  io,
  observation,
  OPENING,
  runtime,
  TIMEOUT_COLLATERAL,
  withheld,
} from "./concurrent-challenges.fixture.js";

describe("an existing availability promise held before signing", () => {
  it("reports an operation the run holds as blocked with its reason", async () => {
    const header = withheld("45");
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL, "d1"),
      utxo(1, OPENING, "d2"),
      utxo(2, 100n * ADA, "d2"),
    ];
    const held = { txHash: "aa".repeat(32), detail: "why" };
    io.run.mockResolvedValueOnce({ ...held, status: "held" });
    const watcher = await runtime();
    try {
      await watcher.reconcile(observation([header.attested]), true);
      expect(watcher.status()).toMatchObject({ ...held, phase: "blocked" });
    } finally {
      await watcher.close();
    }
  });
});
