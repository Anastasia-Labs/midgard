import { decodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import { describe, expect, it } from "vitest";

import { ProviderHttpError, ProviderPayloadError } from "../src/index.js";
import {
  jsonResponse,
  makeProvider,
  submitTx,
} from "./provider.protocol-info.js";

const requestBody = async (init?: RequestInit): Promise<Buffer> =>
  init?.body instanceof Uint8Array
    ? Buffer.from(init.body)
    : Buffer.from(await new Response(init?.body).arrayBuffer());

describe("MidgardNodeProvider.evaluateTx", () => {
  it("posts the draft envelope and parses the budgets", async () => {
    const provider = await makeProvider(async (input, init) => {
      expect(new URL(String(input)).pathname).toBe("/tx/evaluate");
      expect(init?.method).toBe("POST");
      expect(init?.headers).toMatchObject({
        "content-type": "application/vnd.midgard.v1+cbor",
      });
      const submission = decodeMidgardProofSubmission(await requestBody(init));
      expect(submission.transactionCbor.toString("hex")).toBe(submitTx.txHex);
      expect(Object.keys(submission)).toEqual(["transactionCbor"]);
      return jsonResponse({
        redeemers: [
          { tag: 0, index: "1", memory: "1200", steps: "345678" },
          { tag: 1, index: "0", memory: "0", steps: "18446744073709551616" },
        ],
      });
    });

    await expect(provider.evaluateTx(submitTx.txHex)).resolves.toEqual([
      { tag: 0, index: 1n, memory: 1200n, steps: 345678n },
      { tag: 1, index: 0n, memory: 0n, steps: 18446744073709551616n },
    ]);
  });

  it("rejects malformed budget payloads", async () => {
    for (const payload of [
      {},
      { redeemers: [{ tag: 0, index: 0, memory: "1", steps: "1" }] },
      { redeemers: [{ tag: -1, index: "0", memory: "1", steps: "1" }] },
      { redeemers: [{ tag: 0, index: "0", memory: "01", steps: "1" }] },
      { redeemers: [{ tag: 0, index: "0", memory: "1" }] },
    ]) {
      const provider = await makeProvider(async () => jsonResponse(payload));
      await expect(provider.evaluateTx(submitTx.txHex)).rejects.toBeInstanceOf(
        ProviderPayloadError,
      );
    }
  });

  it("surfaces evaluation failures and marks capacity refusals retryable", async () => {
    const failing = await makeProvider(async () =>
      jsonResponse(
        { error: "E_TX_EVALUATION_FAILED", detail: "script failed" },
        422,
      ),
    );
    const failure = await failing
      .evaluateTx(submitTx.txHex)
      .catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(ProviderHttpError);
    expect(failure).toMatchObject({
      statusCode: 422,
      retryable: false,
      message: "E_TX_EVALUATION_FAILED: script failed",
    });

    const busy = await makeProvider(async () =>
      jsonResponse({ error: "Transaction evaluation capacity is full" }, 503),
    );
    await expect(busy.evaluateTx(submitTx.txHex)).rejects.toMatchObject({
      statusCode: 503,
      retryable: true,
    });
  });
});
