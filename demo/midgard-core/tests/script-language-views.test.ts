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
  EMPTY_SCRIPT_INTEGRITY_HASH,
  encodeCbor,
  MIDGARD_V1_CANONICAL_COST_MODEL_VIEW,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
  ScriptLanguageTags,
} from "../src/index.js";

const REDEEMER_TX_WITS_ROOT = Buffer.from(
  "509a422cbd3d2fdca7c6521277d3117b305aa7578bdcf1627df36382429743d1",
  "hex",
);

describe("script language views", () => {
  it("freezes the initial protocol cost model views to Harmonic PlutusV3 today", () => {
    const harmonicV3 = toCostModelArrV3(defaultV3Costs);

    expect(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW).toEqual(harmonicV3);
    expect(MIDGARD_V1_CANONICAL_COST_MODEL_VIEW).toEqual(harmonicV3);
  });

  it("uses EMPTY_NULL_ROOT for an empty required language set", () => {
    expect(
      computeScriptIntegrityHashForLanguages(REDEEMER_TX_WITS_ROOT, []),
    ).toEqual(EMPTY_SCRIPT_INTEGRITY_HASH);
  });

  it("matches canonical fixture hashes for initial language sets", () => {
    expect(
      computeScriptIntegrityHashForLanguages(REDEEMER_TX_WITS_ROOT, [
        "PlutusV3",
      ]).toString("hex"),
    ).toBe(
      "e2ebd40127c1f2fc48fc46388895edf309bdda534dfc1b1a1c0fceb94a43c60e",
    );
    expect(
      computeScriptIntegrityHashForLanguages(REDEEMER_TX_WITS_ROOT, [
        "MidgardV1",
      ]).toString("hex"),
    ).toBe(
      "6439e4fbfe80ed56da131bceafc2bcbdff24b59f69e84ed96507fe3725131442",
    );
    expect(
      computeScriptIntegrityHashForLanguages(REDEEMER_TX_WITS_ROOT, [
        "MidgardV1",
        "PlutusV3",
      ]).toString("hex"),
    ).toBe(
      "907fec08d0de18a7da3449fa2b3a14d9898b8485e55f6112fcd184cffc9f93ff",
    );
  });

  it("changes when redeemer witness root changes", () => {
    const changedRoot = computeHash32(Buffer.from("changed-redeemer-root"));

    expect(
      computeScriptIntegrityHashForLanguages(changedRoot, ["PlutusV3"]),
    ).not.toEqual(
      computeScriptIntegrityHashForLanguages(REDEEMER_TX_WITS_ROOT, [
        "PlutusV3",
      ]),
    );
  });

  it("changes when redeemer data changes", () => {
    const firstRoot = computeHash32(encodeCbor([[0n, 0n, 1n, [0n, 0n]]]));
    const secondRoot = computeHash32(encodeCbor([[0n, 0n, 2n, [0n, 0n]]]));

    expect(
      computeScriptIntegrityHashForLanguages(firstRoot, ["PlutusV3"]),
    ).not.toEqual(
      computeScriptIntegrityHashForLanguages(secondRoot, ["PlutusV3"]),
    );
  });

  it("changes when redeemer ex-units change", () => {
    const firstRoot = computeHash32(encodeCbor([[0n, 0n, 1n, [0n, 0n]]]));
    const secondRoot = computeHash32(encodeCbor([[0n, 0n, 1n, [1n, 0n]]]));

    expect(
      computeScriptIntegrityHashForLanguages(firstRoot, ["PlutusV3"]),
    ).not.toEqual(
      computeScriptIntegrityHashForLanguages(secondRoot, ["PlutusV3"]),
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
      computeScriptIntegrityHash(
        REDEEMER_TX_WITS_ROOT,
        changedViews,
      ).toString("hex"),
    ).toBe(
      "e14cfabbe5ac419383fa63b095712f433a5e318d8b18e0e820d1361041a5fc39",
    );
  });
});
