import { describe, expect, it } from "vitest";

import {
  keyValuePhasMembershipProofs,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";

describe("batched exact PHAS membership", () => {
  it("matches individual openings in requested order, including duplicates", async () => {
    const entries = Array.from({ length: 17 }, (_, index) => ({
      key: Buffer.from([index]),
      value: Buffer.from([index + 32]),
    }));
    const root = await keyValuePhasRootWithCount(entries);
    const requested = [entries[16]!, entries[0]!, entries[8]!, entries[16]!];
    expect(await keyValuePhasMembershipProofs(root, requested)).toEqual(
      await Promise.all(
        requested.map(({ key, value }) => keyValuePhasProof(root, key, value)),
      ),
    );
    await expect(
      keyValuePhasMembershipProofs(root, [
        { ...entries[0]!, value: Buffer.from([255]) },
      ]),
    ).rejects.toThrow();
    await expect(
      keyValuePhasMembershipProofs(root, [
        { key: Buffer.from([255]), value: entries[0]!.value },
      ]),
    ).rejects.toThrow();
    await expect(
      keyValuePhasMembershipProofs(
        { ...root, root: "ff".repeat(32) },
        requested,
      ),
    ).rejects.toThrow();
    expect(await keyValuePhasMembershipProofs(root, [])).toEqual([]);
  });
});
