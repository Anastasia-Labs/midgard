import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import { createTrackedTempDirFactory } from "@al-ft/midgard-test-support/temp-files";
import { describe, expect, it } from "vitest";

import { loadRuntimeDotenv } from "../src/runtime-env.js";

const makeTempDir = createTrackedTempDirFactory("midgard-runtime-env-");

const configFiles = async (yaml: string, dotenv: string): Promise<string> => {
  const cwd = await makeTempDir();
  await writeFile(join(cwd, "config.yaml"), yaml);
  await writeFile(join(cwd, ".env"), dotenv);
  return cwd;
};

describe("runtime environment loading", () => {
  it("applies process environment over YAML over dotenv without mutating process.env", async () => {
    const before = process.env.MIDGARD_RUNTIME_TEST_ONLY;
    const cwd = await configFiles(
      "EXPLICIT: yaml\nEMPTY: yaml\nYAML_WINS: yaml\nMIDGARD_RUNTIME_TEST_ONLY: fake-yaml\n",
      'EXPLICIT=dotenv\nEMPTY=dotenv\nYAML_WINS=dotenv\nDOTENV_ONLY="fake dotenv value"\n',
    );
    const env: NodeJS.ProcessEnv = { EXPLICIT: "process", EMPTY: "" };
    loadRuntimeDotenv({ env, cwd });
    expect(env).toEqual({
      EXPLICIT: "process",
      EMPTY: "",
      YAML_WINS: "yaml",
      MIDGARD_RUNTIME_TEST_ONLY: "fake-yaml",
      DOTENV_ONLY: "fake dotenv value",
    });
    expect(process.env.MIDGARD_RUNTIME_TEST_ONLY).toBe(before);
  });

  it("isolated dotenv mode does not read checkout YAML or dotenv", async () => {
    const cwd = await configFiles(
      "SECRET: [invalid yaml",
      "DOTENV_ONLY=fake-hidden\n",
    );
    const env = { MIDGARD_DOTENV_MODE: "disabled", KEEP: "original" };
    loadRuntimeDotenv({ env, cwd });
    expect(env).toEqual({ MIDGARD_DOTENV_MODE: "disabled", KEEP: "original" });
  });

  it("isolated mode allows explicit YAML while still refusing dotenv backfill", async () => {
    const cwd = await configFiles(
      "SECRET: [invalid yaml",
      "DOTENV_ONLY=fake-hidden\nEXPLICIT=dotenv\n",
    );
    await writeFile(
      join(cwd, "selected.yaml"),
      "EXPLICIT: yaml\nYAML_ONLY: fake-selected\n",
    );
    const env: NodeJS.ProcessEnv = {
      MIDGARD_DOTENV_MODE: "disabled",
      MIDGARD_CONFIG_FILE: "selected.yaml",
      EXPLICIT: "process",
    };
    loadRuntimeDotenv({ env, cwd });
    expect(env).toEqual({
      MIDGARD_DOTENV_MODE: "disabled",
      MIDGARD_CONFIG_FILE: "selected.yaml",
      EXPLICIT: "process",
      YAML_ONLY: "fake-selected",
    });
  });

  it("disabled YAML config preserves ordinary dotenv loading", async () => {
    const cwd = await configFiles(
      "SECRET: [invalid yaml",
      "DOTENV_ONLY=fake-dotenv\n",
    );
    const env: NodeJS.ProcessEnv = { MIDGARD_CONFIG_MODE: "disabled" };
    loadRuntimeDotenv({ env, cwd });
    expect(env).toEqual({
      MIDGARD_CONFIG_MODE: "disabled",
      DOTENV_ONLY: "fake-dotenv",
    });
  });

  it("both disabled modes leave the supplied environment unchanged", async () => {
    const cwd = await configFiles(
      "SECRET: [invalid yaml",
      "DOTENV_ONLY=fake-dotenv\n",
    );
    const env = {
      MIDGARD_CONFIG_MODE: "disabled",
      MIDGARD_DOTENV_MODE: "disabled",
      MIDGARD_CONFIG_FILE: "missing.yaml",
    };
    const before = { ...env };
    loadRuntimeDotenv({ env, cwd });
    expect(env).toEqual(before);
  });

  it("loads dotenv when optional YAML is absent", async () => {
    const cwd = await makeTempDir();
    await writeFile(join(cwd, ".env"), "DOTENV_ONLY=fake-dotenv\n");
    const env: NodeJS.ProcessEnv = {};
    loadRuntimeDotenv({ env, cwd });
    expect(env).toEqual({ DOTENV_ONLY: "fake-dotenv" });
  });

  it("does not fall back to dotenv when explicitly requested YAML is absent", async () => {
    const cwd = await makeTempDir();
    await writeFile(join(cwd, ".env"), "DOTENV_ONLY=fake-dotenv\n");
    const env = { MIDGARD_CONFIG_FILE: "missing.yaml", KEEP: "original" };
    expect(() => loadRuntimeDotenv({ env, cwd })).toThrow(
      "Cannot read component config.yaml",
    );
    expect(env).toEqual({
      MIDGARD_CONFIG_FILE: "missing.yaml",
      KEEP: "original",
    });
  });

  it("invalid YAML is atomic and prevents dotenv fallback", async () => {
    const cwd = await configFiles(
      "VALID_FIRST: fake-value\nINVALID: 42\n",
      "DOTENV_ONLY=fake-dotenv\n",
    );
    const env = { KEEP: "original" };
    expect(() => loadRuntimeDotenv({ env, cwd })).toThrow(
      "uppercase setting names and nonempty string values",
    );
    expect(env).toEqual({ KEEP: "original" });
  });

  it("rejects an invalid dotenv mode before loading either file", async () => {
    const cwd = await configFiles(
      "YAML_ONLY: fake-yaml\n",
      "DOTENV_ONLY=fake-dotenv\n",
    );
    const env = { MIDGARD_DOTENV_MODE: "fake-invalid-mode", KEEP: "original" };
    expect(() => loadRuntimeDotenv({ env, cwd })).toThrow(
      "MIDGARD_DOTENV_MODE must be enabled or disabled",
    );
    expect(env).toEqual({
      MIDGARD_DOTENV_MODE: "fake-invalid-mode",
      KEEP: "original",
    });
  });
});
