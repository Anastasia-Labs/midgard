import { appendFile, readFile } from "node:fs/promises";
import { join } from "node:path";

import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { entropyToMnemonic } from "bip39";
import { PUBLIC_NODE_SECRET_NAMES } from "midgard-node/commands/listen-public-http";
import { afterAll, afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  loadAttachedStackConfig,
  parseAttachedStackConfig,
} from "../src/full-stack/attached-config.js";
import { parseStackConfig } from "../src/full-stack/config.js";
import {
  attachedFixture,
  NODE_WALLETS,
  removeAttachedFixtures,
  SHARED_RELEASE,
} from "./full-stack-attached.fixtures.js";
import {
  createPublicTls,
  removePublicTls,
} from "./full-stack-public-config.fixtures.js";

// Runs in the `preprod-rehearsal-stack` project: this build compiles the
// preprod-rehearsal profile, with operator-2's operator and merge wallets on
// its allow-list. Loading needs only the compiled profile, not a deployment.
beforeAll(createPublicTls);
afterAll(removePublicTls);
afterEach(removeAttachedFixtures);

describe("a second instance of a public stack", () => {
  it("loads operator-2's node on the stack's release, images and TLS, never its ingress", async () => {
    const fixture = await attachedFixture({ public: {} });
    fixture.primary.config.public!.ingressHostPort = 18443;
    await fixture.primary.writeConfig();
    const loaded = await loadAttachedStackConfig(fixture.path);
    expect(loaded.config.public).toEqual({
      ...fixture.primary.config.public,
      nodeConfigFile: fixture.nodeConfigFile,
      ingressHostPort: null,
    });
    for (const name of SHARED_RELEASE)
      expect(loaded.env[name]).toBe(fixture.primary.env[name]);
    // Its node secrets come from its own private node configuration.
    for (const name of PUBLIC_NODE_SECRET_NAMES)
      expect(loaded.env[name] === fixture.env[name]).toBe(true);
  });
  it.each(["operator", "merge"] as const)(
    "refuses an operator-2 %s wallet the compiled profile does not allow-list",
    async (role) => {
      const fixture = await attachedFixture({ public: {} });
      fixture.env[NODE_WALLETS[role]] = entropyToMnemonic("30".repeat(16));
      await fixture.writeEnv();
      await expect(loadAttachedStackConfig(fixture.path)).rejects.toThrow(
        "operator wallet must match the compiled public allow-list",
      );
    },
  );
  it.each(SHARED_RELEASE)("refuses %s other than the stack's", async (name) => {
    const fixture = await attachedFixture({ public: {} });
    fixture.env[name] = name.endsWith("_IMAGE")
      ? `example.invalid/other@sha256:${"d".repeat(64)}`
      : name.endsWith("ORIGIN")
        ? "https://other.example.test"
        : "b".repeat(40);
    await fixture.writeEnv();
    await expect(loadAttachedStackConfig(fixture.path)).rejects.toThrow(
      `${name} in ${fixture.attached.envFile as string} differs from the stack's: a second instance runs the stack's release`,
    );
  });
  it("refuses a node secret in its .env", async () => {
    const fixture = await attachedFixture({ public: {} });
    await appendFile(
      fixture.attached.envFile as string,
      `\nADMIN_API_KEY=${fixture.env.ADMIN_API_KEY}`,
    );
    await expect(loadAttachedStackConfig(fixture.path)).rejects.toThrow(
      `Keep ADMIN_API_KEY in nodeConfigFile, not in ${fixture.attached.envFile as string}`,
    );
  });
  it.each([
    ["runDirectory", "a run directory in it"],
    ["nodeConfigFile", "a node configuration in it"],
    ["releaseDirectory", "a release in operator-2's run directory"],
  ])(
    "refuses %s that would put %s in the stack's release directory",
    async (field) => {
      const fixture = await attachedFixture({ public: {} });
      const release = fixture.primary.config.public!.releaseDirectory;
      if (field === "releaseDirectory") {
        fixture.primary.config.public!.releaseDirectory = join(
          fixture.attached.runDirectory as string,
          "release",
        );
        await fixture.primary.writeConfig();
      } else {
        fixture.attached[field] = join(release, "operator2");
        await fixture.writeAttached();
      }
      await expect(loadAttachedStackConfig(fixture.path)).rejects.toThrow(
        "the stack's release directory, which a second instance only reads",
      );
    },
  );
  it("reads the operator-2 rehearsal example as a second instance of the rehearsal stack", async () => {
    const example = async (name: string) =>
      JSON.parse(
        await readFile(new URL(`../config/${name}`, import.meta.url), "utf8"),
      ) as Record<string, unknown>;
    const rehearsal = parseStackConfig(
      await example("preprod-rehearsal-stack.example.json"),
    );
    const config = parseAttachedStackConfig(
      await example("preprod-rehearsal-operator2.example.json"),
      rehearsal,
    );
    expect(config.attachedTo).toBe(rehearsal);
    expect(config.endpoint).not.toBe(rehearsal.endpoint);
    expect(config.public!.nodeConfigFile).not.toBe(
      rehearsal.public!.nodeConfigFile,
    );
    expect(config.public!.ingressHostPort).toBeNull();
  });
  it("refuses a plain HTTP endpoint or another profile than the stack's", async () => {
    const fixture = await attachedFixture({ public: {} });
    fixture.attached.endpoint = `http://127.0.0.1:${fixture.api.port}`;
    await fixture.writeAttached();
    await expect(loadAttachedStackConfig(fixture.path)).rejects.toThrow(
      "A public stack's second instance serves HTTPS; use an https:// loopback endpoint",
    );
    fixture.attached.endpoint = `https://127.0.0.1:${fixture.api.port}`;
    await fixture.writeAttached();
    fixture.env.MIDGARD_DEPLOYMENT_PROFILE = "preprod-testing";
    await fixture.writeEnv();
    await expect(loadAttachedStackConfig(fixture.path)).rejects.toThrow(
      `A second instance of a public stack requires Preprod, MIDGARD_DEPLOYMENT_PROFILE=${SELECTED_DEPLOYMENT_PROFILE.name}`,
    );
  });
});
