/**
 * A fake Midgard node for the transfer CLI's HTTP client: `/utxos`,
 * `/tx-status` and `/submit` each answer from a queue of handlers.
 */
import { decodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { walletFromSeed } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import type { NodeUtxo } from "../src/commands/command-utils.js";
import {
  mkNodeUtxo,
  OTHER_TEST_SEED,
  TEST_SEED,
} from "./submit-l2-transfer.submit-l2-transfer-config-helpers.js";

export type Route = "utxos" | "tx-status" | "submit";

/** The transaction a request names: the submitted one or the queried one. */
export type Handler = (txId: string) => Promise<Response>;

/** One fake node: each route answers from its own queue of handlers. */
export const fakeNode = (handlers: {
  readonly [route in Route]?: Handler[];
}) => {
  const calls: { readonly route: Route; readonly body?: string }[] = [];
  vi.stubGlobal(
    "fetch",
    vi.fn<typeof fetch>(async (input, init) => {
      const url = new URL(String(input));
      const route = url.pathname.slice(1) as Route;
      let txId = url.searchParams.get("tx_hash") ?? "";
      let body: string | undefined;
      if (route === "submit") {
        const txCbor = decodeMidgardProofSubmission(
          Buffer.from(init?.body as Uint8Array),
        ).transactionCbor;
        body = txCbor.toString("hex");
        txId = computeMidgardNativeTxId(
          decodeMidgardNativeTxFullFromCanonicalCbor(txCbor),
        ).toString("hex");
      }
      calls.push({ route, body });
      const handler = handlers[route]?.shift();
      if (handler === undefined) throw new Error(`unexpected /${route}`);
      return handler(txId);
    }),
  );
  return {
    routes: () => calls.map((call) => call.route),
    submittedBodies: () =>
      calls.flatMap((call) => (call.body === undefined ? [] : [call.body])),
  };
};

export const sender = walletFromSeed(TEST_SEED, { network: "Preprod" });
export const destination = walletFromSeed(OTHER_TEST_SEED, {
  network: "Preprod",
});
export const senderUtxo = mkNodeUtxo({
  txHash: "44".repeat(32),
  outputIndex: 0,
  address: sender.address,
  assets: { lovelace: 8_000_000n },
});

/** A `/utxos` answer listing exactly `listed`. */
export const utxosOf =
  (...listed: readonly NodeUtxo[]): Handler =>
  async () =>
    new Response(
      JSON.stringify({
        utxos: listed.map((utxo) => ({
          outref: utxo.outrefCbor.toString("hex"),
          outputCbor: utxo.outputCbor.toString("hex"),
        })),
      }),
      { status: 200 },
    );

export const utxos: Handler = utxosOf(senderUtxo);

/** 202 admits a new transaction; 200 answers a byte-identical duplicate. */
export const admitted =
  (status: 200 | 202): Handler =>
  async (txId) =>
    new Response(
      JSON.stringify({ txId, status: "queued", duplicate: status === 200 }),
      { status },
    );

export const txStatus =
  (status: string): Handler =>
  async (txId) =>
    new Response(JSON.stringify({ txId, status }), {
      status: status === "not_found" ? 404 : 200,
    });

export const connectionRefused: Handler = async () => {
  throw new TypeError("fetch failed", {
    cause: Object.assign(new Error("connect ECONNREFUSED 127.0.0.1:3000"), {
      code: "ECONNREFUSED",
    }),
  });
};
