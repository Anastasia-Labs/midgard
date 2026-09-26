import { writeFile } from "node:fs/promises";
import { join } from "node:path";
import { inspect } from "node:util";

import { createTrackedTempDirFactory } from "@al-ft/midgard-test-support/temp-files";
import { describe, expect, it } from "vitest";

import { loadRuntimeConfig } from "../src/runtime-config.js";

const makeTempDir = createTrackedTempDirFactory("midgard-runtime-config-");
const fakeSecret = "fake-sensitive-value-for-config-tests";
const renderedFailure = (run: () => void): string => {
  let failure: unknown;
  try {
    run();
  } catch (error) {
    failure = error;
  }
  expect(failure).toBeInstanceOf(Error);
  return inspect(failure, { depth: 10 });
};

describe("component YAML runtime configuration", () => {
  it("loads literal strings without replacing explicit values, including empty ones", async () => {
    const cwd = await makeTempDir();
    await writeFile(
      join(cwd, "config.yaml"),
      'YAML_ONLY: "false"\nEXPLICIT: yaml\nEMPTY: yaml\nPORT: "3000"\n',
    );
    const env: NodeJS.ProcessEnv = { EXPLICIT: "process", EMPTY: "" };
    loadRuntimeConfig({ env, cwd });
    expect(env).toEqual({
      EXPLICIT: "process",
      EMPTY: "",
      YAML_ONLY: "false",
      PORT: "3000",
    });
  });

  it("treats absent optional config as a no-op", async () => {
    const env = { KEEP: "original" };
    loadRuntimeConfig({ env, cwd: await makeTempDir() });
    expect(env).toEqual({ KEEP: "original" });
  });

  it("fails on an explicitly missing file without exposing its name", async () => {
    const cwd = await makeTempDir();
    const env = { MIDGARD_CONFIG_FILE: `${fakeSecret}.yaml`, KEEP: "original" };
    const before = { ...env };
    const error = renderedFailure(() => loadRuntimeConfig({ env, cwd }));
    expect(error).toContain("Cannot read component config.yaml");
    expect(error).not.toContain(fakeSecret);
    expect(error).not.toContain(cwd);
    expect(env).toEqual(before);
  });

  it("resolves an explicit file relative to the supplied directory", async () => {
    const cwd = await makeTempDir();
    await writeFile(join(cwd, "config.yaml"), "BAD: [unclosed");
    await writeFile(join(cwd, "selected.yaml"), 'SELECTED: "fake-value"\n');
    const env: NodeJS.ProcessEnv = { MIDGARD_CONFIG_FILE: "selected.yaml" };
    loadRuntimeConfig({ env, cwd });
    expect(env.SELECTED).toBe("fake-value");
  });

  it.each([
    ["syntax", `SECRET: "${fakeSecret}\n`],
    ["duplicate keys", `SECRET: ${fakeSecret}\nSECRET: duplicate\n`],
    ["aliases", `SECRET: &private ${fakeSecret}\nCOPY: *private\n`],
  ])(
    "redacts %s errors and changes no environment entries",
    async (_name, source) => {
      const cwd = await makeTempDir();
      await writeFile(join(cwd, "config.yaml"), source);
      const env = { KEEP: "original" };
      const error = renderedFailure(() => loadRuntimeConfig({ env, cwd }));
      expect(error).toContain(
        "Invalid component config.yaml syntax (contents redacted)",
      );
      expect(error).not.toContain(fakeSecret);
      expect(error).not.toContain(source);
      expect(env).toEqual({ KEEP: "original" });
    },
  );

  it.each([
    ["number", "INVALID: 42"],
    ["boolean", "INVALID: false"],
    ["null", "INVALID: null"],
    ["nested mapping", "INVALID: { NESTED: value }"],
    ["array", "INVALID: [value]"],
    ["empty string", 'INVALID: ""'],
    ["blank string", 'INVALID: "  "'],
    ["invalid setting name", `bad_name: ${fakeSecret}`],
    ["config path control", `MIDGARD_CONFIG_FILE: ${fakeSecret}`],
    ["config mode control", "MIDGARD_CONFIG_MODE: disabled"],
    ["dotenv mode control", "MIDGARD_DOTENV_MODE: disabled"],
  ])("rejects %s atomically after a valid entry", async (_name, invalid) => {
    const cwd = await makeTempDir();
    await writeFile(
      join(cwd, "config.yaml"),
      `VALID_FIRST: ${fakeSecret}\n${invalid}\n`,
    );
    const env = { KEEP: "original" };
    const error = renderedFailure(() => loadRuntimeConfig({ env, cwd }));
    expect(error).toContain(
      "uppercase setting names and nonempty string values",
    );
    expect(error).not.toContain(fakeSecret);
    expect(env).toEqual({ KEEP: "original" });
  });

  it.each(["", "null", "[value]", '"value"'])(
    "rejects a non-mapping document %j",
    async (source) => {
      const cwd = await makeTempDir();
      await writeFile(join(cwd, "config.yaml"), source);
      const env = { KEEP: "original" };
      expect(() => loadRuntimeConfig({ env, cwd })).toThrow(
        "must be a mapping",
      );
      expect(env).toEqual({ KEEP: "original" });
    },
  );

  it("disabled config ignores even an explicit missing file", async () => {
    const env = {
      MIDGARD_CONFIG_MODE: "disabled",
      MIDGARD_CONFIG_FILE: "missing.yaml",
    };
    loadRuntimeConfig({ env, cwd: await makeTempDir() });
    expect(env).toEqual({
      MIDGARD_CONFIG_MODE: "disabled",
      MIDGARD_CONFIG_FILE: "missing.yaml",
    });
  });

  it("rejects invalid loader controls without echoing their values", async () => {
    const cwd = await makeTempDir();
    expect(
      renderedFailure(() =>
        loadRuntimeConfig({ cwd, env: { MIDGARD_CONFIG_MODE: fakeSecret } }),
      ),
    ).not.toContain(fakeSecret);
    expect(() =>
      loadRuntimeConfig({ cwd, env: { MIDGARD_CONFIG_FILE: "  " } }),
    ).toThrow("MIDGARD_CONFIG_FILE must name a file");
  });
});
