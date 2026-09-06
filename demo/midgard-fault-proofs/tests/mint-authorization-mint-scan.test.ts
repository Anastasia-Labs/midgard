import { encodeCbor, encodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  advanceMintScan,
  initialMintScan,
  mintScanComplete,
} from "../src/mint-authorization/mint-scan.js";
import { mintAuthorizationMaximumMintField } from "./support/mint-authorization-maxima.js";

const run = (bytes: Buffer, index: bigint) => {
  let state = initialMintScan(bytes, index);
  let batches = 0;
  while (!mintScanComplete(state, bytes.length)) {
    const previous = state;
    state = advanceMintScan(state, bytes, index);
    expect(state).not.toEqual(previous);
    batches++;
    if (batches > 1024) throw new Error("mint scan failed to progress");
  }
  return { state, batches };
};

describe("bounded mint claim scan", () => {
  it("authenticates the final policy of the maximum field over bounded batches", () => {
    const maximum = mintAuthorizationMaximumMintField();
    const bytes = encodeMidgardFieldPreimage(
      maximum.mintItemCbors.map((item) => Buffer.from(item, "hex")),
    );
    const { state, batches } = run(bytes, BigInt(maximum.targetPolicyIndex));
    expect(state.policy_id).toBe("ff".repeat(28));
    expect(batches).toBe(Math.ceil((maximum.mintItemCbors.length + 1) / 32));
    expect(batches).toBeGreaterThan(1);
  });
  it("bounds selected-policy asset parsing independently of field cardinality", () => {
    const assets = new Map<Buffer, bigint>();
    for (let index = 0; index < 8100; index++) {
      const name = Buffer.alloc(2);
      name.writeUInt16BE(index);
      assets.set(name, index % 2 === 0 ? 1n : -1n);
    }
    const bytes = encodeMidgardFieldPreimage([
      encodeCbor([Buffer.alloc(28, 1), assets]),
    ]);
    expect(bytes.length).toBeGreaterThan(32000);
    const { state, batches } = run(bytes, 0n);
    expect(state.policy_id).toBe("01".repeat(28));
    expect(batches).toBe(Math.ceil(8101 / 32));
  });
  it("refuses zero quantities and trailing or truncated fields", () => {
    const policy = (quantity: bigint) =>
      encodeCbor([Buffer.alloc(28, 1), new Map([[Buffer.alloc(0), quantity]])]);
    expect(() => run(encodeMidgardFieldPreimage([policy(0n)]), 0n)).toThrow();
    const valid = encodeMidgardFieldPreimage([policy(-1n)]);
    expect(() => run(Buffer.concat([valid, Buffer.from([0])]), 0n)).toThrow();
    expect(() => run(valid.subarray(0, valid.length - 1), 0n)).toThrow();
    expect(() => initialMintScan(valid, 1n)).toThrow();
  });
});
