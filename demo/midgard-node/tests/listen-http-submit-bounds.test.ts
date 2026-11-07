import { encodeMidgardProofSubmission } from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { afterEach, describe, expect, it } from "vitest";

import { SUBMIT_HTTP_BODY_MAX_BYTES } from "../src/commands/http/submission-capacity.js";
import {
  closeServers,
  policyWith,
  proxied,
  serve,
} from "./listen-http-ingress.serve-fixture.js";

afterEach(closeServers);

describe("HTTP submission body bounds and kept connections", () => {
  const client = "198.51.100.1";

  it("caps a submission at one envelope around the largest transaction", async () => {
    const largest = encodeMidgardProofSubmission({
      transactionCbor: Buffer.alloc(
        MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes,
        1,
      ),
    });
    expect(largest.length).toBe(SUBMIT_HTTP_BODY_MAX_BYTES);
    const node = await serve(proxied);
    expect(
      (await node.call("/submit", { method: "POST", client, body: largest }))
        .status,
    ).toBe(200);
    expect(node.invoked()).toBe(1);
  });

  it("refuses a larger submission or evaluation body before reading it", async () => {
    const node = await serve(proxied);
    const oversized = {
      method: "POST",
      client,
      headers: { "content-length": String(SUBMIT_HTTP_BODY_MAX_BYTES + 1) },
    };
    // No body is sent: the answer comes from the declared length alone.
    expect((await node.call("/submit", oversized)).status).toBe(413);
    expect((await node.call("/tx/evaluate", oversized)).status).toBe(413);
    expect(
      (
        await node.call("/submit", {
          ...oversized,
          headers: {
            "content-length": String(
              MIDGARD_CONSENSUS_LIMITS.maxDaPayloadBytes,
            ),
          },
        })
      ).status,
    ).toBe(413);
    expect(node.invoked()).toBe(0);
  });

  it("answers 411 to a submission without Content-Length", async () => {
    const node = await serve(proxied);
    const reply = await node.call("/submit", {
      method: "POST",
      client,
      chunked: true,
      body: Buffer.from("8200", "hex"),
    });
    expect(reply).toMatchObject({
      status: 411,
      body: { error: "Content-Length is required" },
    });
    expect(node.invoked()).toBe(0);
  });

  it("keeps the connection on rate and capacity refusals and closes it on malformed requests", async () => {
    const node = await serve({
      ...proxied,
      policy: policyWith({
        write: {
          peer: { burst: 1, perSecond: 1 },
          global: { burst: 1_000, perSecond: 1_000 },
        },
      }),
    });
    const submit = () => node.call("/submit", { method: "POST", client });
    expect((await submit()).status).toBe(200);
    expect((await submit()).status).toBe(429);
    node.advance(1_000);
    // The client retries on the connection that carried the refusal.
    expect((await submit()).status).toBe(200);
    expect(node.connections()).toBe(1);

    // Every public slot held: the capacity refusal keeps the connection too.
    for (let holder = 1; holder <= 16; holder += 1)
      for (let slot = 0; slot < 4; slot += 1)
        node.hold("/utxos", { client: `203.0.113.${holder}` });
    await node.waitFor(() => node.ingress.snapshot().active === 64);
    const before = node.connections();
    node.advance(1_000);
    const full = await submit();
    expect([full.status, full.body.error_code]).toEqual([
      503,
      "E_HTTP_INGRESS_CAPACITY",
    ]);
    node.releaseHeld();
    await node.waitFor(() => node.ingress.snapshot().active === 0);
    node.advance(1_000);
    expect((await submit()).status).toBe(200);
    expect(node.connections()).toBe(before);

    // A malformed request closes it.
    expect(
      (
        await node.call("/submit", {
          method: "POST",
          client,
          headers: { "content-length": "01" },
        })
      ).status,
    ).toBe(400);
    node.advance(1_000);
    expect((await submit()).status).toBe(200);
    expect(node.connections()).toBe(before + 1);
  });
});
