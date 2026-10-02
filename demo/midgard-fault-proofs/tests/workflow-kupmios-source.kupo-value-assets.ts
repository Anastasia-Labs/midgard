import {
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { parseKupoMatch } from "../src/workflow/local-kupmios-http-ogmios-source.fetch-json.js";

const POLICY = "e5ae40ed24f832ffedad05b8741167d07609bd426024397128d4fa27";

const matchWithAssets = (assets: Record<string, unknown>) => ({
  transaction_index: 0,
  transaction_id: "3d".repeat(32),
  output_index: 9,
  address: credentialToAddress(
    "Preprod",
    scriptHashToCredential("31".repeat(28)),
  ),
  value: { coins: 1_564_530, assets },
  datum_hash: null,
  script_hash: null,
  created_at: { slot_no: 400, header_hash: "44".repeat(32) },
  spent_at: null,
  datum: null,
  script: null,
});

describe("Kupo value asset keys", () => {
  it("admits Kupo's bare policy id for an empty asset name", () => {
    expect(
      parseKupoMatch(matchWithAssets({ [POLICY]: 1 }), "match").assets,
    ).toEqual({ lovelace: 1_564_530n, [POLICY]: 1n });
  });

  it.each([["abcd"], ["ab".repeat(32)]])(
    "admits <policy>.%s and normalizes it to the unit",
    (name) => {
      expect(
        parseKupoMatch(matchWithAssets({ [`${POLICY}.${name}`]: "7" }), "match")
          .assets,
      ).toEqual({ lovelace: 1_564_530n, [`${POLICY}${name}`]: 7n });
    },
  );

  it.each([
    [`${POLICY}.`],
    [`${POLICY}.abc`],
    [`${POLICY}.AB`],
    [POLICY.toUpperCase()],
    [`${POLICY}.${"ab".repeat(33)}`],
    [POLICY.slice(2)],
  ])("refuses non-canonical asset key %s", (key) => {
    expect(() =>
      parseKupoMatch(matchWithAssets({ [key]: 1 }), "match"),
    ).toThrow("match.value.assets is not canonical Kupo value JSON");
  });
});
