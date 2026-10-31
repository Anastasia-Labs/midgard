import { vi } from "vitest";

import type { AcceptanceNativePayoutScope } from "../src/devnet-stack/acceptance-native-boundary.js";
import type { AcceptanceSettlementSnapshot } from "../src/devnet-stack/acceptance-payout-sql.js";
import { decodeAcceptanceCanonicalTransaction } from "../src/devnet-stack/acceptance-payout-transaction.js";
import {
  config,
  hash,
  payoutFixture,
} from "./devnet-stack-acceptance-payout.fixtures.js";

export const collectorFixture = (
  options: { orderMoves?: number; external?: boolean } = {},
) => {
  const inputs = ["11", "12", "13", "14"].map((nonceByte) =>
    payoutFixture({ nonceByte, ...options }),
  );
  const transactions = inputs.flatMap((input) => [
    input.order,
    ...(input.externalPublication === undefined
      ? []
      : [input.externalPublication]),
    ...(input.orderSuccessors ?? []),
    ...input.settlements,
  ]);
  const snapshot: AcceptanceSettlementSnapshot = {
    generation: "9",
    attempts: inputs
      .flatMap((input) =>
        input.settlements.map((row) => ({
          event_id: input.record.withdrawalEventId,
          tx_hash: row.observed.txHash,
          phase: row.phase,
          signed_cbor: row.signedCbor,
          required_outputs: [...row.requiredOutputs],
        })),
      )
      .sort((a, b) =>
        a.tx_hash < b.tx_hash ? -1 : a.tx_hash > b.tx_hash ? 1 : 0,
      ),
  };
  const controller = new AbortController();
  const scope: AcceptanceNativePayoutScope = {
    signal: controller.signal,
    point: { blockHash: hash("78"), slot: "5000", blockNo: "2259" },
    deadlineEpochMs: Date.now() + 10_000,
    assertCurrent() {
      if (controller.signal.aborted) throw new Error("native revoked");
    },
    readExactTransaction: vi.fn(async ({ txHash }) => {
      const row = transactions.find((row) => row.observed.txHash === txHash);
      if (row === undefined) throw new Error("transaction missing");
      return row.observed;
    }),
    canonicalBlockDepth: vi.fn(async () => 2160n),
    queryExactOutRefs: vi.fn(async () => {
      throw new Error("not a lineage query");
    }),
  };
  const fetchImpl = vi.fn(async (url: string) => {
    const path = new URL(url).pathname;
    if (path.startsWith("/checkpoints/"))
      return Response.json({ slot_no: 99, header_hash: hash("76") });
    const ref = /^\/matches\/(\d+)@([0-9a-f]{64})$/u.exec(path);
    if (ref === null) throw new Error(`unexpected locator ${path}`);
    const outputIndex = Number(ref[1]);
    const txHash = ref[2]!;
    const tx = transactions.find((row) => row.observed.txHash === txHash);
    if (tx === undefined) throw new Error("locator transaction missing");
    const output = decodeAcceptanceCanonicalTransaction(tx, config).outputs[
      outputIndex
    ];
    if (output === undefined) throw new Error("locator output missing");
    const spending = transactions.filter((row) =>
      row.observed.spentInputs!.some(
        (input) => input.txHash === txHash && input.outputIndex === outputIndex,
      ),
    );
    if (spending.length > 1) throw new Error("fixture spend ambiguous");
    return Response.json([
      {
        transaction_id: txHash,
        output_index: outputIndex,
        created_at: { slot_no: 100, header_hash: hash("77") },
        datum: output.datum ?? null,
        datum_type: output.datum === undefined ? null : "inline",
        datum_hash: null,
        spent_at:
          spending.length === 0
            ? null
            : {
                transaction_id: spending[0]!.observed.txHash,
                input_index: 0,
                redeemer: null,
                slot_no: 100,
                header_hash: hash("77"),
              },
      },
    ]);
  });
  return {
    inputs,
    transactions,
    records: inputs.map((input) => input.record),
    snapshot,
    scope,
    fetchImpl,
    config,
    kupoUrl: "http://127.0.0.1:12345",
    blockScanLimit: 1,
    maxReferenceInputs: 4,
    controller,
  };
};
