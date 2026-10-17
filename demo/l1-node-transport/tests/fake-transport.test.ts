import { describe, expect, it } from "vitest";

import { fakeL1NodeTransport } from "../src/testing/index.js";

describe("fakeL1NodeTransport", () => {
  it("names the member a test used without stubbing it", async () => {
    const transport = fakeL1NodeTransport({ hasTx: async () => true });
    expect(await transport.hasTx("00")).toBe(true);
    expect(() => transport.submit(new Uint8Array())).toThrow(
      "not stubbed: submit",
    );
    expect(() => transport.readiness).toThrow("not stubbed: readiness");
  });

  it("keeps a supplied getter live", () => {
    let ready = false;
    const transport = fakeL1NodeTransport({
      get readiness() {
        return ready
          ? { ready: true as const, nodeToClientVersion: 16 }
          : {
              ready: false as const,
              reason: "sidecar_starting" as const,
              detail: "starting",
            };
      },
    });
    expect(transport.readiness.ready).toBe(false);
    ready = true;
    expect(transport.readiness.ready).toBe(true);
  });
});
