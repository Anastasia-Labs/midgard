import {
  defaultV3Costs,
  toCostModelArrV3,
} from "@harmoniclabs/cardano-costmodels-ts";
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
  it("freezes the initial protocol cost model views to Harmonic PlutusV3 today", () => {
    const harmonicV3 = toCostModelArrV3(defaultV3Costs);

    expect(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW).toEqual(harmonicV3);
    expect(MIDGARD_CANONICAL_COST_MODEL_VIEW).toEqual(harmonicV3);
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
    ).toBe("e2ebd40127c1f2fc48fc46388895edf309bdda534dfc1b1a1c0fceb94a43c60e");
    expect(
      computeScriptIntegrityHashForLanguages(REDEEMER_TX_WITS_HASH, [
        "MidgardV1",
      ]).toString("hex"),
    ).toBe("6439e4fbfe80ed56da131bceafc2bcbdff24b59f69e84ed96507fe3725131442");
    expect(
      computeScriptIntegrityHashForLanguages(REDEEMER_TX_WITS_HASH, [
        "MidgardV1",
        "PlutusV3",
      ]).toString("hex"),
    ).toBe("907fec08d0de18a7da3449fa2b3a14d9898b8485e55f6112fcd184cffc9f93ff");
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
    ).toBe("d7239eb1bd8b7376dedfbf7e6201815b225c023d11c975cd99d25d5236b199a1");
    expect(
      computeScriptIntegrityHashForLanguages(AIKEN_REDEEMER_TX_WITS_HASH, [
        "MidgardV1",
      ]).toString("hex"),
    ).toBe("71201d25ea11e4104eda108782a7d67b37b4ae97df6dc3258b06d9c98e58bbcb");
    expect(
      computeScriptIntegrityHashForLanguages(AIKEN_REDEEMER_TX_WITS_HASH, [
        "MidgardV1",
        "PlutusV3",
      ]).toString("hex"),
    ).toBe("6d49b4f24c60bec1cb34a2538278252059ec0601b7f675ef73fe2b48e24317d8");
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
    ).toBe("e14cfabbe5ac419383fa63b095712f433a5e318d8b18e0e820d1361041a5fc39");
  });
});
