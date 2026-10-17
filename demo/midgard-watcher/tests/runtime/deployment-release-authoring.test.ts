import { createHash, generateKeyPairSync, type KeyObject } from "node:crypto";
import { mkdtemp, readdir, readFile, rm } from "node:fs/promises";
import { join } from "node:path";

import {
  writeTextFileAtomic,
  writeTextFileAtomicNoReplace,
} from "midgard-node/files/atomic-write";
import { afterEach, describe, expect, it } from "vitest";

import {
  authorWatcherDeploymentRelease,
  type WatcherExistingAuthorityPolicy,
} from "../../src/runtime/deployment-release-authoring.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

const BLUEPRINT_JSON = '{"preamble":{"title":"release-authoring-test"}}';
const { manifest } = makeWatcherDeploymentAuthorityFixture({
  blueprintHash: createHash("sha256").update(BLUEPRINT_JSON).digest("hex"),
}).signedIdentity;
const PROGRAM_COMMITMENTS = { "computation-thread-policy-v1": "cd".repeat(32) };

const directories: string[] = [];
afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});

const release = async () => {
  const directory = await mkdtemp("/var/tmp/midgard-release-authoring-");
  directories.push(directory);
  const writes: string[] = [];
  const paths = {
    authority: join(directory, "deployment-authority.json"),
    rules: join(directory, "rules.json"),
    funding: join(directory, "funding-profiles.json"),
    manifest: join(directory, "deployment-manifest.json"),
    blueprint: join(directory, "plutus.json"),
    deploymentInfo: join(directory, "contract-deployment-info.json"),
  };
  const author = (
    signingKey: KeyObject,
    existingAuthority: WatcherExistingAuthorityPolicy,
    programCommitments: Readonly<Record<string, string>> = PROGRAM_COMMITMENTS,
  ) =>
    authorWatcherDeploymentRelease({
      manifest,
      blueprintJson: BLUEPRINT_JSON,
      programCommitments,
      fundingProfiles: [],
      fundingPaymentKeyHash: "ee".repeat(28),
      signingKey,
      paths,
      existingAuthority,
      writer: {
        replace: async (path, contents) => {
          writes.push(`replace ${path.slice(directory.length + 1)}`);
          await writeTextFileAtomic(path, contents);
        },
        create: async (path, contents) => {
          writes.push(`create ${path.slice(directory.length + 1)}`);
          await writeTextFileAtomicNoReplace(path, contents);
        },
      },
    });
  return { directory, paths, writes, author };
};

const key = () => generateKeyPairSync("ed25519").privateKey;

describe("watcher deployment release authoring", () => {
  it("writes every bound artifact before creating the authority, and reuses it unchanged", async () => {
    const { paths, writes, author } = await release();
    const signingKey = key();
    const first = await author(signingKey, "refuse");
    expect(writes).toEqual([
      "replace rules.json",
      "replace deployment-manifest.json",
      "replace contract-deployment-info.json",
      "replace plutus.json",
      "replace funding-profiles.json",
      "create deployment-authority.json",
    ]);
    expect(first.deploymentAuthority.deploymentIdentity.manifestId).toBe(
      manifest.manifestId,
    );
    // The node's contract deployment info is the finalized manifest itself.
    expect(JSON.parse(await readFile(paths.deploymentInfo, "utf8"))).toEqual(
      manifest,
    );
    const saved = await readFile(paths.authority, "utf8");

    writes.length = 0;
    const reopened = await author(signingKey, "refuse");
    expect(writes).toEqual([]);
    expect(reopened.authority).toEqual(first.authority);
    expect(await readFile(paths.authority, "utf8")).toBe(saved);

    await expect(
      author(signingKey, "replace", {
        "computation-thread-policy-v1": "ff".repeat(32),
      }),
    ).rejects.toThrow("Saved watcher authority differs");
    expect(writes).toEqual([]);
    expect(await readFile(paths.authority, "utf8")).toBe(saved);
  });

  it("refuses an authority under another trust root, or sets it aside and re-signs", async () => {
    const { directory, paths, writes, author } = await release();
    const first = await author(key(), "refuse");
    const saved = await readFile(paths.authority, "utf8");
    const firstRoot = first.authority.trustRoots[0]!.trustRootId;

    writes.length = 0;
    await expect(author(key(), "refuse")).rejects.toThrow(
      `signed by trust root ${firstRoot}, not the configured signing key`,
    );
    expect(writes).toEqual([]);
    expect(await readFile(paths.authority, "utf8")).toBe(saved);

    const replaced = await author(key(), "replace");
    expect(replaced.authority.trustRoots[0]!.trustRootId).not.toBe(firstRoot);
    expect(writes.at(-1)).toBe("create deployment-authority.json");
    expect(
      await readFile(
        join(directory, `deployment-authority.superseded-${firstRoot}.json`),
        "utf8",
      ),
    ).toBe(saved);
    expect((await readdir(directory)).sort()).toEqual([
      "contract-deployment-info.json",
      "deployment-authority.json",
      `deployment-authority.superseded-${firstRoot}.json`,
      "deployment-manifest.json",
      "funding-profiles.json",
      "plutus.json",
      "rules.json",
    ]);
  });

  it("refuses a blueprint the manifest does not name", async () => {
    const { paths } = await release();
    await expect(
      authorWatcherDeploymentRelease({
        manifest,
        blueprintJson: `${BLUEPRINT_JSON} `,
        programCommitments: PROGRAM_COMMITMENTS,
        fundingProfiles: [],
        fundingPaymentKeyHash: "ee".repeat(28),
        signingKey: key(),
        paths,
        existingAuthority: "refuse",
        writer: {
          replace: () => Promise.reject(new Error("no write expected")),
          create: () => Promise.reject(new Error("no write expected")),
        },
      }),
    ).rejects.toThrow("Blueprint differs from the deployed release");
  });
});
