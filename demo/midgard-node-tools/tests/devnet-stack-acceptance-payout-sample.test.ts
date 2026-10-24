import { expect, it, vi } from "vitest";

import { collectAcceptancePayoutLineages } from "../src/devnet-stack/acceptance-payout-collect.js";
import { collectorFixture } from "./devnet-stack-acceptance-payout-collector.fixtures.js";

it("reads four distinct exact canonical graphs, including moving Order predecessors", async () => {
  const fixture = collectorFixture({ orderMoves: 2 });
  const proofs = await collectAcceptancePayoutLineages(fixture);
  expect(new Set(proofs.map((proof) => proof.eventId)).size).toBe(4);
  expect(new Set(proofs.map((proof) => proof.beneficiary.txHash)).size).toBe(4);
  expect(
    proofs.every((proof) => proof.currentOrder.txHash !== proof.order.txHash),
  ).toBe(true);
  expect(
    proofs.every(
      (proof) =>
        proof.lineage.filter((row) => row.phase === "order-update").length ===
        2,
    ),
  ).toBe(true);
  expect(fixture.scope.readExactTransaction).toHaveBeenCalledTimes(28);
});
it("refuses stale selected-chain depth and locator/native point mismatch", async () => {
  const shallow = collectorFixture();
  vi.mocked(shallow.scope.canonicalBlockDepth).mockResolvedValue(2159n);
  await expect(collectAcceptancePayoutLineages(shallow)).rejects.toThrow(
    /selected-chain depth/,
  );
  const stale = collectorFixture();
  vi.mocked(stale.scope.canonicalBlockDepth).mockResolvedValue(null);
  await expect(collectAcceptancePayoutLineages(stale)).rejects.toThrow(
    /not on current selected chain/,
  );
  const foreign = collectorFixture();
  const original = foreign.scope.readExactTransaction;
  foreign.scope.readExactTransaction = async (args) => ({
    ...(await original(args)),
    blockPoint: { headerHash: "00".repeat(32), slot: 100, blockNo: 100 },
  });
  await expect(collectAcceptancePayoutLineages(foreign)).rejects.toThrow(
    /point differs/,
  );
});
it("refuses loss after an awaited canonical read before any next read", async () => {
  const fixture = collectorFixture();
  const original = fixture.scope.readExactTransaction;
  fixture.scope.readExactTransaction = async (args) => {
    const result = await original(args);
    fixture.controller.abort();
    return result;
  };
  await expect(collectAcceptancePayoutLineages(fixture)).rejects.toThrow(
    /native revoked/,
  );
  expect(fixture.scope.canonicalBlockDepth).not.toHaveBeenCalled();
});
it("bounds Order traversal before reading an extra transaction", async () => {
  const fixture = collectorFixture({ orderMoves: 4 });
  await expect(collectAcceptancePayoutLineages(fixture)).rejects.toThrow(
    /exceeds explicit bound/,
  );
  // original +three movements; fourth would exceed admission+4 settlements+8 total.
  expect(fixture.scope.readExactTransaction).toHaveBeenCalledTimes(4);
});
it("refuses duplicate initializer receipts and missing completed events", async () => {
  const fixture = collectorFixture();
  const initial = fixture.snapshot.attempts.find(
    (row) => row.phase === "initialize",
  )!;
  fixture.snapshot.attempts = [...fixture.snapshot.attempts, initial];
  await expect(collectAcceptancePayoutLineages(fixture)).rejects.toThrow(
    /initializer receipt.*ambiguous/,
  );
  await expect(
    collectAcceptancePayoutLineages({ ...collectorFixture(), records: [] }),
  ).rejects.toThrow(/four distinct/);
});
it("loads hash-bound external payloads through original canonical reference locators", async () => {
  const fixture = collectorFixture({ external: true });
  const proofs = await collectAcceptancePayoutLineages(fixture);
  expect(proofs).toHaveLength(4);
  const bounded = collectorFixture({ external: true });
  bounded.maxReferenceInputs = 0;
  await expect(collectAcceptancePayoutLineages(bounded)).rejects.toThrow(
    /source work bound/,
  );
  const unavailable = collectorFixture({ external: true });
  const original = unavailable.fetchImpl;
  const unavailableFetch = async (url: string) => {
    const response = await original(url);
    if (
      unavailable.inputs.some((input) =>
        url.includes(input.externalPublication!.observed.txHash),
      )
    ) {
      const rows = (await response.json()) as Record<string, unknown>[];
      rows[0]!.datum = "d87980";
      return Response.json(rows);
    }
    return response;
  };
  await expect(
    collectAcceptancePayoutLineages({
      ...unavailable,
      fetchImpl: unavailableFetch,
    }),
  ).rejects.toThrow(/external.*unavailable/);
});
