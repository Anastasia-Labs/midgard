import { mkdtemp, rm, symlink, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { afterEach, describe, expect, it } from "vitest";

import {
  assertWatcherVerifiedDeploymentAuthority,
  loadWatcherVerifiedDeploymentAuthority,
} from "../../src/runtime/deployment-authority.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

const directories: string[] = [];

// This is separately supplied release material. The production loader must not
// substitute the deployment manifest's protocol-parameter snapshot for it.
const TARGET_PARAMETERS = {
  coinsPerUtxoByte: "4310",
  maxTxExUnits: { memory: "16500000", steps: "10000000000" },
  maxTxSize: 16_384,
  maxValueSize: 5_000,
  minFeeA: 44,
  minFeeB: 155_381,
  prices: { memory: 0.0577, steps: 0.000_072_1 },
};

// Reuse the existing signed unit-authority fixture; no production release
// evidence is generated or replaced by these filesystem acquisition tests.
const construction = makeWatcherDeploymentAuthorityFixture();
const ruleBundle = makeWatcherCanonicalRuleBundle({
  constructionIdentity: {
    manifestId: construction.result.manifestId,
    network: construction.result.network,
    blueprintHash: construction.result.blueprintHash,
    programCommitments: construction.result.programCommitments,
  },
  targetParameterSnapshot: TARGET_PARAMETERS,
});
const fixture = makeWatcherDeploymentAuthorityFixture({
  ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
});

const writeReleaseFiles = async () => {
  const directory = await mkdtemp("/var/tmp/midgard-rule-bundle-authority-");
  directories.push(directory);
  const authority = structuredClone({
    signedIdentity: fixture.signedIdentity,
    policy: fixture.policy,
    trustRoots: fixture.trustRoots,
    durableMarker: fixture.marker,
  });
  const paths = {
    path: join(directory, "deployment-authority.json"),
    ruleBundlePath: join(directory, "rule-bundle.json"),
  };
  await Promise.all([
    writeFile(paths.path, JSON.stringify(authority)),
    writeFile(paths.ruleBundlePath, JSON.stringify(ruleBundle)),
  ]);
  return { authority, paths, directory };
};

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});

describe("signed deployment and release rule-bundle acquisition", () => {
  it("loads both artifacts into one admitted authority and freshly reopens them", async () => {
    const { paths } = await writeReleaseFiles();
    const loaded = await loadWatcherVerifiedDeploymentAuthority(paths);
    expect(() =>
      assertWatcherVerifiedDeploymentAuthority(loaded),
    ).not.toThrow();
    expect(loaded.deploymentIdentity.manifestId).toBe(
      fixture.result.manifestId,
    );
    expect(loaded.ruleBundle.ruleBundleCommitment).toBe(
      loaded.deploymentIdentity.ruleBundleCommitment,
    );
    expect(loaded.ruleBundle.ruleBundle.deploymentManifestId).toBe(
      loaded.deploymentIdentity.manifestId,
    );
    expect(loaded.ruleBundle.ruleBundle.programCommitments).toEqual(
      loaded.deploymentIdentity.programCommitments,
    );
    expect(loaded.ruleBundle.ruleBundle.targetParameters.snapshot).toEqual(
      TARGET_PARAMETERS,
    );
    expect(Object.isFrozen(loaded)).toBe(true);
    expect(Object.isFrozen(loaded.ruleBundle.ruleBundle)).toBe(true);
    expect(
      Object.isFrozen(loaded.ruleBundle.ruleBundle.targetParameters.snapshot),
    ).toBe(true);

    const reopened = await loadWatcherVerifiedDeploymentAuthority(paths);
    expect(reopened).not.toBe(loaded);
    expect(reopened.ruleBundle).toEqual(loaded.ruleBundle);
    expect(() =>
      assertWatcherVerifiedDeploymentAuthority(reopened),
    ).not.toThrow();
    expect(() =>
      assertWatcherVerifiedDeploymentAuthority({ ...loaded }),
    ).toThrow("authority is not admitted");
  });

  it("rejects a rule bundle for another deployment", async () => {
    const { paths } = await writeReleaseFiles();
    await writeFile(
      paths.ruleBundlePath,
      JSON.stringify({
        ...ruleBundle,
        deploymentManifestId: computeDeploymentManifestJsonDigest({
          deployment: "another-deployment",
        }),
      }),
    );
    await expect(
      loadWatcherVerifiedDeploymentAuthority(paths),
    ).rejects.toMatchObject({ code: "deployment_identity_mismatch" });
  });

  it("rejects a changed but internally consistent bundle on reopening", async () => {
    const { paths } = await writeReleaseFiles();
    await loadWatcherVerifiedDeploymentAuthority(paths);
    const snapshot = { ...TARGET_PARAMETERS, minFeeA: 45 };
    await writeFile(
      paths.ruleBundlePath,
      JSON.stringify({
        ...ruleBundle,
        targetParameters: {
          snapshot,
          digest: computeDeploymentManifestJsonDigest(snapshot),
        },
      }),
    );
    await expect(
      loadWatcherVerifiedDeploymentAuthority(paths),
    ).rejects.toMatchObject({
      code: "rule_bundle_commitment_mismatch",
    });
  });

  it("requires the existing trust roots even when the bundle commitment matches", async () => {
    const { authority, paths } = await writeReleaseFiles();
    await writeFile(
      paths.path,
      JSON.stringify({ ...authority, trustRoots: [] }),
    );
    await expect(
      loadWatcherVerifiedDeploymentAuthority(paths),
    ).rejects.toMatchObject({
      code: "invalid_trust_root",
    });
  });

  it("requires the durable deployment marker before admitting the bundle", async () => {
    const { authority, paths } = await writeReleaseFiles();
    await writeFile(
      paths.path,
      JSON.stringify({ ...authority, durableMarker: null }),
    );
    await expect(
      loadWatcherVerifiedDeploymentAuthority(paths),
    ).rejects.toMatchObject({
      code: "missing_durable_marker",
    });
  });

  it("rejects duplicate keys in the supplied rule-bundle artifact", async () => {
    const { paths } = await writeReleaseFiles();
    const encoded = JSON.stringify(ruleBundle);
    await writeFile(
      paths.ruleBundlePath,
      `{"schemaVersion":${JSON.stringify(ruleBundle.schemaVersion)},${encoded.slice(1)}`,
    );
    await expect(
      loadWatcherVerifiedDeploymentAuthority(paths),
    ).rejects.toMatchObject({
      code: "duplicate_field",
    });
  });

  it("rejects a rule-bundle path that traverses a symlink", async () => {
    const { paths, directory } = await writeReleaseFiles();
    const linked = join(directory, "linked-rule-bundle.json");
    await symlink(paths.ruleBundlePath, linked);
    await expect(
      loadWatcherVerifiedDeploymentAuthority({
        ...paths,
        ruleBundlePath: linked,
      }),
    ).rejects.toThrow("path traverses a symlink");
  });
});
