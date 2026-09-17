import { expect, it, vi } from "vitest";

import { readKupoUnitConsumptions } from "./kupo-consumption.js";

const unit = "ab".repeat(28) + "4e6f6465" + "cd".repeat(32);

const kupo = (body: unknown, status = 200) =>
  vi.fn(
    async () =>
      new Response(JSON.stringify(body), {
        status,
        headers: { "content-type": "application/json" },
      }),
  ) as unknown as typeof fetch;

const match = (
  txHash: string,
  outputIndex: number,
  spentAt: Record<string, unknown> | null,
) => ({
  transaction_index: 0,
  transaction_id: txHash,
  output_index: outputIndex,
  address: "addr_test1",
  value: { coins: 5_000_000, assets: {} },
  datum_hash: null,
  datum_type: "inline",
  script_hash: null,
  created_at: { slot_no: 10, header_hash: "00".repeat(32) },
  spent_at: spentAt,
});

it("maps spent matches of the unit to oldest-first consumptions", async () => {
  const fetchImpl = kupo([
    match("22".repeat(32), 1, {
      slot_no: 400,
      header_hash: "01".repeat(32),
      transaction_id: "33".repeat(32),
      input_index: 0,
      redeemer: null,
    }),
    match("11".repeat(32), 0, {
      slot_no: 200,
      header_hash: "02".repeat(32),
      transaction_id: "22".repeat(32),
      input_index: 2,
      redeemer: "d87980",
    }),
  ]);
  await expect(
    readKupoUnitConsumptions("http://kupo", unit, fetchImpl),
  ).resolves.toEqual([
    {
      txHash: "11".repeat(32),
      outputIndex: 0,
      spentByTxHash: "22".repeat(32),
      spentAtSlot: 200,
    },
    {
      txHash: "22".repeat(32),
      outputIndex: 1,
      spentByTxHash: "33".repeat(32),
      spentAtSlot: 400,
    },
  ]);
  expect(fetchImpl).toHaveBeenCalledWith(
    `http://kupo/matches/${"ab".repeat(28)}.${"4e6f6465" + "cd".repeat(32)}?spent`,
  );
});

it("rejects a spent match whose spender the index did not record", async () => {
  const fetchImpl = kupo([
    match("11".repeat(32), 0, {
      slot_no: 200,
      header_hash: "02".repeat(32),
      transaction_id: null,
      input_index: null,
      redeemer: null,
    }),
  ]);
  await expect(
    readKupoUnitConsumptions("http://kupo", unit, fetchImpl),
  ).rejects.toThrow("lacks an authenticated spend reference");
});

it("fails closed on a non-list body or an HTTP error", async () => {
  await expect(
    readKupoUnitConsumptions("http://kupo", unit, kupo({ hint: "nope" })),
  ).rejects.toThrow("are not a list");
  await expect(
    readKupoUnitConsumptions("http://kupo", unit, kupo([], 503)),
  ).rejects.toThrow("failed with 503");
  await expect(
    readKupoUnitConsumptions("http://kupo", "zz", kupo([])),
  ).rejects.toThrow("Malformed unit");
});
