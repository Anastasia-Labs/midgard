import { once } from "node:events";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { fetchNodeUtxosByOutRefs } from "../src/commands/command-utils.js";
import { UtxoReadDataLimitError } from "../src/commands/command-utils.utxo-http-request.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";

const ADDRESS =
  "addr_test1qzyem8ex0v9v76q0u52x3t2xmj5rkhjd9rsd44kx3klsut4qga2669x30zsng46mhfrrk4ngylfnnlda7rkfvxq5fywqvurkrs";
const requested = [4, 1, 3, 2].map(
  (index) => `${Buffer.alloc(32, index).toString("hex")}#0`,
);
type Reply = { status: number; errorCode?: string; retryAfter?: string };

const withUtxoServer = async (
  reply: (refs: readonly string[], call: number) => Reply,
  run: (endpoint: string, calls: string[][]) => Promise<void>,
) => {
  const calls: string[][] = [];
  const output = makeMidgardTxOutput(
    CML.Address.from_bech32(ADDRESS),
    CML.Value.from_coin(1_500_000n),
  )
    .to_cbor_bytes()
    .toString("hex");
  const server = createServer((request, response) => {
    void (async () => {
      const chunks: Buffer[] = [];
      for await (const chunk of request) chunks.push(Buffer.from(chunk));
      const refs = JSON.parse(
        Buffer.concat(chunks).toString("utf8"),
      ) as string[];
      calls.push(refs);
      const next = reply(refs, calls.length);
      response.statusCode = next.status;
      response.setHeader("content-type", "application/json");
      if (next.retryAfter !== undefined)
        response.setHeader("retry-after", next.retryAfter);
      response.end(
        JSON.stringify(
          next.status === 200
            ? {
                utxos: refs.map((ref) => ({
                  outref: makeOutRefCbor(ref.split("#")[0]!, 0).toString("hex"),
                  outputCbor: output,
                })),
              }
            : { error_code: next.errorCode },
        ),
      );
    })().catch(() => {
      response.statusCode = 500;
      response.end();
    });
  });
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  try {
    await run(
      `http://127.0.0.1:${(server.address() as AddressInfo).port}`,
      calls,
    );
  } finally {
    server.closeAllConnections();
    await new Promise<void>((resolve) => server.close(() => resolve()));
  }
};

// Real HTTP framing and canonical decoding require a distinct setup from the
// existing pure utxos-command parser tests. The SQL budget is tested separately.
describe("bounded UTxO HTTP client", () => {
  it("splits only an explicit aggregate data-limit refusal and keeps caller order", async () => {
    await withUtxoServer(
      (refs) =>
        refs.length > 2
          ? {
              status: 503,
              errorCode: "E_HTTP_READ_DATA_LIMIT",
              retryAfter: "5",
            }
          : { status: 200 },
      async (endpoint, calls) => {
        const result = await fetchNodeUtxosByOutRefs(endpoint, requested);
        expect(
          result.map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`),
        ).toEqual(requested);
        expect(calls).toEqual([
          requested,
          requested.slice(0, 2),
          requested.slice(2),
        ]);
      },
    );
  });

  it("retries a transient capacity refusal as the same batch without splitting", async () => {
    await withUtxoServer(
      (_refs, call) =>
        call === 1
          ? {
              status: 503,
              errorCode: "E_HTTP_READ_UNAVAILABLE",
              retryAfter: "1",
            }
          : { status: 200 },
      async (endpoint, calls) => {
        const result = await fetchNodeUtxosByOutRefs(endpoint, requested);
        expect(result).toHaveLength(4);
        expect(calls).toEqual([requested, requested]);
      },
    );
  });

  it("refuses an oversized single row promptly without retries or an empty split", async () => {
    await withUtxoServer(
      () => ({
        status: 503,
        errorCode: "E_HTTP_READ_DATA_LIMIT",
        retryAfter: "5",
      }),
      async (endpoint, calls) => {
        const started = performance.now();
        await expect(
          fetchNodeUtxosByOutRefs(endpoint, requested.slice(0, 1)),
        ).rejects.toBeInstanceOf(UtxoReadDataLimitError);
        expect(calls).toEqual([requested.slice(0, 1)]);
        expect(performance.now() - started).toBeLessThan(2_000);
      },
    );
  });
});
