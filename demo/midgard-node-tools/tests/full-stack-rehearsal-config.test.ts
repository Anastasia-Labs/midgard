import { writeFile } from "node:fs/promises";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { entropyToMnemonic } from "bip39";
import { afterAll, afterEach, beforeAll, describe, expect, it } from "vitest";

import { loadStackConfig } from "../src/full-stack/config.js";
import { removeConfigFixtures } from "./full-stack-config.fixtures.js";
import {
  createPublicTls,
  publicFixture,
  removePublicTls,
} from "./full-stack-public-config.fixtures.js";

// Runs in the `preprod-rehearsal-stack` project: this build compiles the
// preprod-rehearsal profile, with the fixture's wallets on its allow-list
// and not on preprod-public's.
afterEach(removeConfigFixtures);
beforeAll(createPublicTls);
afterAll(removePublicTls);

/** The fixture, written as a run reads it. */
async function rehearsalFixture(example?: string) {
  const value = await publicFixture(example);
  await writeFile(value.config.envFile, value.envText());
  await value.writeConfig();
  return value;
}

describe("a preprod-rehearsal stack takes the public route", () => {
  it("loads with every public input checked before any build or spend", async () => {
    const value = await rehearsalFixture();
    expect(value.env.MIDGARD_DEPLOYMENT_PROFILE).toBe("preprod-rehearsal");
    const loaded = await loadStackConfig(value.path);
    // Only the public checks move the node secrets out of the private file.
    expect(loaded.env.ADMIN_API_KEY).toBe(value.secrets.ADMIN_API_KEY);
  });
  it("refuses it without the public section", async () => {
    const value = await rehearsalFixture();
    delete value.config.public;
    await value.writeConfig();
    await expect(loadStackConfig(value.path)).rejects.toThrow(
      "needs the public section",
    );
  });
  it("checks the operator against the rehearsal allow-list", async () => {
    const value = await rehearsalFixture();
    await value.writeSecrets({
      ...value.secrets,
      L1_OPERATOR_SEED_PHRASE: entropyToMnemonic("30".repeat(16)),
    });
    await expect(loadStackConfig(value.path)).rejects.toThrow(
      "operator wallet must match the compiled public allow-list",
    );
  });
  it("loads the rehearsal example once operator-2's producer address replaces its placeholder", async () => {
    const value = await rehearsalFixture(
      "preprod-rehearsal-stack.example.json",
    );
    // The example names operator-2's producer by a placeholder: --check
    // refuses it until the operator writes the real address.
    await expect(loadStackConfig(value.path)).rejects.toThrow(
      "is not a multiaddr",
    );
    const { peerId } = await loadDaLibp2pIdentity(`seed:${"0b".repeat(32)}`);
    const operator2 = `/ip4/127.0.0.1/tcp/39012/p2p/${peerId}`;
    value.config.da.admittedProducers = [operator2];
    await value.writeConfig();
    const loaded = await loadStackConfig(value.path);
    expect(loaded.config.da.admittedProducers).toEqual([operator2]);
  });
});
