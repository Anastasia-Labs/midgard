import { describe, expect, it } from "vitest";

import {
  commitLeaseOwner,
  parseCommitLeaseOwner,
} from "../src/commit-lease-owner.js";

const id = "12345678-1234-4123-8123-123456789abc";
describe("commit parent/worker owner contract", () => {
  it("round trips the exact serialized owner", () => {
    expect(parseCommitLeaseOwner(commitLeaseOwner(id))).toEqual({
      kind: "commit",
      id,
    });
  });
  it.each([
    `node-commit:${id}`,
    "commit:test",
    `commit:${id}suffix`,
    undefined,
    42,
  ])("rejects independently invented owner %s", (value) => {
    expect(parseCommitLeaseOwner(value)).toBeUndefined();
  });
  it("refuses constructing an unparseable parent owner", () => {
    expect(() => commitLeaseOwner("test")).toThrow("UUID v4");
  });
});
