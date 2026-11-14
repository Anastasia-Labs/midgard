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
const manifestOf = (entry: string) =>
  JSON.parse(readFileSync(join(dirname(entry), "../package.json"), "utf8")) as {
    dependencies?: Record<string, string>;
  };

// Every @harmoniclabs package the workspace or Lucid has depended on.
const HARMONIC_PACKAGES = [
  "bigint-utils",
  "bitstream",
  "bytestring",
  "cardano-costmodels-ts",
  "cbor",
  "crypto",
  "obj-utils",
  "pair",
  "plutus-data",
  "plutus-machine",
  "uint8array-utils",
  "uplc",
].map((name) => `@harmoniclabs/${name}`);

// Where `name` resolves from `context`, or undefined when it is not installed
// there. A package that is installed but refuses the bare specifier still
// counts as resolving.
const resolution = (
  context: NodeJS.Require,
  name: string,
): string | undefined => {
  try {
    return context.resolve(name);
  } catch (error) {
    const code = (error as { code?: unknown }).code;
    return code === "MODULE_NOT_FOUND"
      ? undefined
      : `unresolvable (${String(code)})`;
  }
};

// The independent oracle uses Aiken's Rust UPLC compiler, the same primitive
// the SDK's guarded blueprint parameterizer uses.
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
  it("resolves no @harmoniclabs package from the package, Lucid or Lucid's own packages", () => {
    const packageRequire = createRequire(
      join(workspace, route, "package.json"),
    );
    const lucidEntry = packageRequire.resolve("@lucid-evolution/lucid");
    const lucidRequire = createRequire(lucidEntry);
    const lucidDependencies = Object.keys(
      manifestOf(lucidEntry).dependencies ?? {},
    ).filter((name) => name.startsWith("@lucid-evolution/"));
    expect(lucidDependencies).toContain("@lucid-evolution/utils");
    const contexts = [
      packageRequire,
      lucidRequire,
      ...lucidDependencies.map((name) =>
        createRequire(lucidRequire.resolve(name)),
      ),
    ];
    const resolved = contexts.flatMap((context) =>
      HARMONIC_PACKAGES.flatMap((name) => {
        const found = resolution(context, name);
        return found === undefined ? [] : [`${name} -> ${found}`];
      }),
    );
    expect(resolved).toEqual([]);
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
