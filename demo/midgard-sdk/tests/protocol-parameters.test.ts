import {
  getProtocolParameters,
  resolveEventInclusionTime,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

describe("protocol parameters", () => {
  it("keeps the offchain event wait duration aligned with the onchain environment", () => {
    expect(getProtocolParameters("Preprod").event_wait_duration).toEqual(
      300_000,
    );
    expect(() => getProtocolParameters("Mainnet")).toThrow(
      /compiled deployment profile/u,
    );
    expect(resolveEventInclusionTime(1_000, "Preprod")).toEqual(300_999);
  });
});
