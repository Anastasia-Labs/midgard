import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

import type * as Lucid from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { encodeCborArrayRaw, readCborBytes } from "../src/codec/index.js";

const workspace = fileURLToPath(new URL("../../", import.meta.url));
const routes = [
  "midgard-core",
  "da-committee-node",
  "midgard-node",
  "midgard-sdk",
  "midgard-fault-proofs",
  "midgard-validation",
];
const sdkRequire = createRequire(join(workspace, "midgard-sdk/package.json"));
const validationRequire = createRequire(
  join(workspace, "midgard-validation/package.json"),
);
const wasm = sdkRequire("@lucid-evolution/uplc") as {
  apply_params_to_script: (
    params: Uint8Array,
    script: Uint8Array,
  ) => Uint8Array;
};
const scripts = JSON.parse(
  readFileSync(
    join(workspace, "midgard-node/blueprints/always-succeeds/plutus.json"),
    "utf8",
  ),
) as { validators: { title: string; compiledCode: string }[] };
const compiledCode = scripts.validators[0]!.compiledCode;
const versionOf = (entry: string): string => {
  const metadata = JSON.parse(
    readFileSync(join(dirname(entry), "../package.json"), "utf8"),
  ) as { version: string };
  return metadata.version;
};

// The independent oracle uses Aiken's Rust UPLC compiler, the same primitive
// the SDK's guarded blueprint parameterizer uses, rather than Harmonic UPLC.
const expectedApplication = (
  lucid: typeof Lucid,
  params: Lucid.Data[],
): string => {
  const script = readCborBytes(
    lucid.fromHex(lucid.applyDoubleCborEncoding(compiledCode)),
    0,
    "compiled blueprint script",
  ).value;
  return lucid.applyDoubleCborEncoding(
    lucid.toHex(
      // eslint-disable-next-line midgard/apply-params-through-blueprint -- Independent test oracle for Lucid dependency modernization; no script is deployed.
      wasm.apply_params_to_script(
        encodeCborArrayRaw(
          params.map((param) => lucid.fromHex(lucid.Data.to(param))),
        ),
        script,
      ),
    ),
  );
};

describe.each(routes)("installed Lucid parameterization from %s", (route) => {
  it("shares modern Harmonic data, crypto, and CBOR instances with validation", () => {
    const packageRequire = createRequire(
      join(workspace, route, "package.json"),
    );
    const lucidRequire = createRequire(
      packageRequire.resolve("@lucid-evolution/lucid"),
    );
    const utilsRequire = createRequire(
      lucidRequire.resolve("@lucid-evolution/utils"),
    );
    const uplcEntry = utilsRequire.resolve("@harmoniclabs/uplc");
    const dataEntry = utilsRequire.resolve("@harmoniclabs/plutus-data");
    const uplcRequire = createRequire(uplcEntry);
    const dataRequire = createRequire(dataEntry);
    expect(versionOf(uplcEntry)).toMatch(/^2\./);
    expect(versionOf(dataEntry)).toMatch(/^2\./);
    expect(versionOf(uplcRequire.resolve("@harmoniclabs/cbor"))).toMatch(
      /^2\./,
    );
    expect(uplcEntry).toBe(validationRequire.resolve("@harmoniclabs/uplc"));
    expect(dataEntry).toBe(
      validationRequire.resolve("@harmoniclabs/plutus-data"),
    );
    expect(uplcRequire.resolve("@harmoniclabs/plutus-data")).toBe(dataEntry);
    expect(uplcRequire.resolve("@harmoniclabs/cbor")).toBe(
      dataRequire.resolve("@harmoniclabs/cbor"),
    );
    expect(uplcRequire.resolve("@harmoniclabs/cbor")).toBe(
      sdkRequire.resolve("@harmoniclabs/cbor"),
    );
    expect(uplcRequire.resolve("@harmoniclabs/crypto")).toBe(
      sdkRequire.resolve("@harmoniclabs/crypto"),
    );
  });

  it.each(["cjs", "esm"])(
    "applies real blueprint parameters with Aiken-identical bytes and hashes (%s)",
    async (format) => {
      const packageRequire = createRequire(
        join(workspace, route, "package.json"),
      );
      const entry = packageRequire.resolve("@lucid-evolution/lucid");
      const lucid = (
        format === "cjs"
          ? packageRequire("@lucid-evolution/lucid")
          : await import(pathToFileURL(join(dirname(entry), "index.js")).href)
      ) as typeof Lucid;
      const nested: Lucid.Data = new lucid.Constr(2, [
        -((1n << 80n) + 7n),
        "ab".repeat(70),
        new Map<Lucid.Data, Lucid.Data>([
          [1n, new lucid.Constr(0, ["ff", [2n, 3n]])],
          ["aabb", [4n, new lucid.Constr(7, [])]],
        ]),
      ]);
      for (const params of [
        ["11".repeat(28)],
        [nested, 42n, "22".repeat(32)],
      ]) {
        const expected = expectedApplication(lucid, params);
        // eslint-disable-next-line midgard/apply-params-through-blueprint -- This regression must exercise Lucid itself, the broken #647 dependency path.
        const actual = lucid.applyParamsToScript(compiledCode, params);
        expect(actual).toBe(expected);
        expect(actual).not.toBe(lucid.applyDoubleCborEncoding(compiledCode));
        expect(
          lucid.validatorToScriptHash({ type: "PlutusV3", script: actual }),
        ).toBe(
          lucid.validatorToScriptHash({ type: "PlutusV3", script: expected }),
        );
      }
    },
  );
});
