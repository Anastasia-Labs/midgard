import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import {
  buildScriptLanguageViews,
  computeHash32,
  computeScriptIntegrityHash,
  computeScriptIntegrityHashForLanguages,
  EMPTY_NULL_ROOT,
  encodeCbor,
  MIDGARD_CANONICAL_COST_MODEL_VIEW,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
  ScriptLanguageTags,
} from "../src/index.js";

const REDEEMER_TX_WITS_HASH = Buffer.from(
  "509a422cbd3d2fdca7c6521277d3117b305aa7578bdcf1627df36382429743d1",
  "hex",
);
const AIKEN_REDEEMER_TX_WITS_HASH = Buffer.alloc(32, 0x11);

describe("script language views", () => {
  it("freezes the initial protocol cost model views", () => {
    // The 350-parameter PlutusV3 model of the latest mainnet protocol
    // version. Library defaults move, so the view is pinned by its own digest.
    expect(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW).toHaveLength(350);
    expect(
      createHash("sha256")
        .update(JSON.stringify(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW))
        .digest("hex"),
    ).toBe("6b74a19b00bcca178a4c723c0ce871d41fc5c5a417d738798faea60930961b4e");
    expect(MIDGARD_CANONICAL_COST_MODEL_VIEW).toEqual(
      PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
    );
  });

  it("uses EMPTY_NULL_ROOT for an empty required language set", () => {
    expect(
      computeScriptIntegrityHashForLanguages(REDEEMER_TX_WITS_HASH, []),
    ).toEqual(EMPTY_NULL_ROOT);
  });

  it("matches canonical fixture hashes for initial language sets", () => {
    expect(
      computeScriptIntegrityHashForLanguages(REDEEMER_TX_WITS_HASH, [
        "PlutusV3",
      ]).toString("hex"),
    ).toBe("2c8dd895d15d14f0197d323f15b153811874ca2d6bc8fcda1fb84e1bfed925f3");
    expect(
      computeScriptIntegrityHashForLanguages(REDEEMER_TX_WITS_HASH, [
        "MidgardV1",
      ]).toString("hex"),
    ).toBe("057035c930f43e00f71020693b3004b01eb6632d7c8eb6a6128eb3ddf147cf24");
    expect(
      computeScriptIntegrityHashForLanguages(REDEEMER_TX_WITS_HASH, [
        "MidgardV1",
        "PlutusV3",
      ]).toString("hex"),
    ).toBe("a994ca9d22a562c1f057d8a60c0ebafc046f47e4a083ea3c050e51d437c524c9");
  });

  it("matches the exact Aiken vectors for every supported language bitmap", () => {
    expect(
      computeScriptIntegrityHashForLanguages(
        AIKEN_REDEEMER_TX_WITS_HASH,
        [],
      ).toString("hex"),
    ).toBe("01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53");
    expect(
      computeScriptIntegrityHashForLanguages(AIKEN_REDEEMER_TX_WITS_HASH, [
        "PlutusV3",
      ]).toString("hex"),
    ).toBe("b37aff1034628d20a78418d2c2cf20796ac08c83467b6f8558e6cea542de4c26");
    expect(
      computeScriptIntegrityHashForLanguages(AIKEN_REDEEMER_TX_WITS_HASH, [
        "MidgardV1",
      ]).toString("hex"),
    ).toBe("74c606847a40bed5f147cd6cca208fa1b0db6a130f7d86d7c478fd5838d6d878");
    expect(
      computeScriptIntegrityHashForLanguages(AIKEN_REDEEMER_TX_WITS_HASH, [
        "MidgardV1",
        "PlutusV3",
      ]).toString("hex"),
    ).toBe("ceab70e09654e1f35ddf1566b15861055566fda429e056107de070c58c8b3991");
  });

  it("changes when redeemer witness hash changes", () => {
    const changedHash = computeHash32(Buffer.from("changed-redeemer-hash"));

    expect(
      computeScriptIntegrityHashForLanguages(changedHash, ["PlutusV3"]),
    ).not.toEqual(
      computeScriptIntegrityHashForLanguages(REDEEMER_TX_WITS_HASH, [
        "PlutusV3",
      ]),
    );
  });

  it("changes when redeemer data changes", () => {
    const firstHash = computeHash32(encodeCbor([[0n, 0n, 1n, [0n, 0n]]]));
    const secondHash = computeHash32(encodeCbor([[0n, 0n, 2n, [0n, 0n]]]));

    expect(
      computeScriptIntegrityHashForLanguages(firstHash, ["PlutusV3"]),
    ).not.toEqual(
      computeScriptIntegrityHashForLanguages(secondHash, ["PlutusV3"]),
    );
  });

  it("changes when redeemer ex-units change", () => {
    const firstHash = computeHash32(encodeCbor([[0n, 0n, 1n, [0n, 0n]]]));
    const secondHash = computeHash32(encodeCbor([[0n, 0n, 1n, [1n, 0n]]]));

    expect(
      computeScriptIntegrityHashForLanguages(firstHash, ["PlutusV3"]),
    ).not.toEqual(
      computeScriptIntegrityHashForLanguages(secondHash, ["PlutusV3"]),
    );
  });

  it("changes when the required language view changes", () => {
    const changedViews = buildScriptLanguageViews(["PlutusV3"]);
    changedViews.set(ScriptLanguageTags.PlutusV3, [
      ...PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.slice(0, -1),
      PLUTUS_V3_CANONICAL_COST_MODEL_VIEW[
        PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.length - 1
      ] + 1,
    ]);

    expect(
      computeScriptIntegrityHash(REDEEMER_TX_WITS_HASH, changedViews).toString(
        "hex",
      ),
    ).toBe("20da1dd363bd01b6d1561f41a75d44f55071687820d853ca663d11aa3e4b2a7a");
  });
});
