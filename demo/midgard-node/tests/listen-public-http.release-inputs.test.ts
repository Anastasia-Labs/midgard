import {
  chmodSync,
  mkdirSync,
  mkdtempSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  authenticatePublicHttpRelease,
  PUBLIC_RELEASE_JSON_MAX_BYTES,
  PublicHttpConfigError,
  readMountedHttpFile,
  verifyMountedPublicRegistry,
} from "../src/commands/listen-public-http.js";

// These controls cover mounted-byte/digest framing, not compiled public contracts,
// SDK parameter reapplication, a genuine signed release or running image identity.
const registryFixture = () => {
  const registry = {
    schemaVersion: "midgard-deployment-parameter-registry-v1",
    manifestId: "ab".repeat(32),
    blueprintHash: "cd".repeat(32),
    deployment: {
      validators: [
        {
          title: "fixture.deployed",
          disposition: "deployed",
          contracts: ["fixture"],
        },
        {
          title: "fixture.unused",
          disposition: "not_deployed",
          reason: "explicit fixture exclusion",
        },
      ],
      contracts: [{ application: { parameters: [{ cborHex: "d87980" }] } }],
    },
    referenceScripts: {},
  };
  return {
    registry,
    classification: { "fixture.unused": "explicit fixture exclusion" },
    expected: {
      manifestId: registry.manifestId,
      blueprintHash: registry.blueprintHash,
      parameterRegistryDigest: computeDeploymentManifestJsonDigest(registry),
    },
  };
};

describe("mounted public release inputs", () => {
  it("accepts the exact authenticated registry and refuses a one-byte parameter substitution", () => {
    const { registry, classification, expected } = registryFixture();
    expect(() =>
      verifyMountedPublicRegistry(registry, classification, expected),
    ).not.toThrow();
    registry.deployment.contracts[0].application.parameters[0].cborHex =
      "d87981";
    expect(() =>
      verifyMountedPublicRegistry(registry, classification, expected),
    ).toThrow("signed public release provenance");
  });
  it.each(["manifestId", "blueprintHash"] as const)(
    "refuses %s independently of the whole registry digest",
    (field) => {
      const { registry, classification, expected } = registryFixture();
      registry[field] = "ef".repeat(32);
      expected.parameterRegistryDigest =
        computeDeploymentManifestJsonDigest(registry);
      expect(() =>
        verifyMountedPublicRegistry(registry, classification, expected),
      ).toThrow("signed public release provenance");
    },
  );
  it("refuses missing, substituted or additional unused-validator classifications", () => {
    const { registry, classification, expected } = registryFixture();
    for (const invalid of [
      {},
      { "fixture.unused": "different reason" },
      { ...classification, "unknown.raw.title": "default bucket" },
    ])
      expect(() =>
        verifyMountedPublicRegistry(registry, invalid, expected),
      ).toThrow("signed parameter registry");
    expect(() =>
      verifyMountedPublicRegistry(
        registry,
        { "fixture.unused": " " },
        expected,
      ),
    ).toThrow("explicit mounted blueprint classification");
  });
  it("refuses duplicate unused-validator titles even when their bytes have a matching digest", () => {
    const { registry, classification, expected } = registryFixture();
    registry.deployment.validators.push({
      ...registry.deployment.validators[1],
    });
    expected.parameterRegistryDigest =
      computeDeploymentManifestJsonDigest(registry);
    expect(() =>
      verifyMountedPublicRegistry(registry, classification, expected),
    ).toThrow("validator classification is invalid");
  });
  it("tells the operator why a mounted file is refused, never its contents", () => {
    const directory = mkdtempSync(join(tmpdir(), "midgard-mounted-refusal-"));
    const path = join(directory, "mounted-secret");
    const contents = "mounted-secret-contents";
    try {
      writeFileSync(path, contents, { mode: 0o644 });
      chmodSync(path, 0o644);
      const refusal = (read: () => unknown) => {
        try {
          read();
        } catch (error) {
          expect(error).toBeInstanceOf(PublicHttpConfigError);
          expect((error as Error).message).not.toContain(contents);
          return (error as PublicHttpConfigError).reason;
        }
        throw new Error("the mounted file was accepted");
      };
      expect(refusal(() => readMountedHttpFile(path, true))).toBe(
        "mounted secret file grants group or other access; make it mode 0600",
      );
      chmodSync(path, 0o640);
      expect(refusal(() => readMountedHttpFile(path, true))).toBe(
        "mounted secret file grants group or other access; make it mode 0600",
      );
      chmodSync(path, 0o600);
      expect(readMountedHttpFile(path, true).toString("utf8")).toBe(contents);
      const bounds =
        "mounted file must be a nonempty regular file within its size limit";
      expect(refusal(() => readMountedHttpFile(path, true, 4))).toBe(bounds);
      const empty = join(directory, "empty");
      writeFileSync(empty, "", { mode: 0o600 });
      expect(refusal(() => readMountedHttpFile(empty, true))).toBe(bounds);
      const folder = join(directory, "folder");
      mkdirSync(folder, { mode: 0o700 });
      expect(refusal(() => readMountedHttpFile(folder, false))).toBe(bounds);
      expect(
        refusal(() => readMountedHttpFile(join(directory, "absent"), true)),
      ).toBe(
        "mounted file is missing, unreadable or invalid (contents redacted)",
      );
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });
  it("names why a mounted release file is refused instead of calling it invalid JSON", () => {
    const directory = mkdtempSync(join(tmpdir(), "midgard-mounted-release-"));
    const reason = (env: NodeJS.ProcessEnv) => {
      try {
        authenticatePublicHttpRelease(env);
      } catch (error) {
        expect(error).toBeInstanceOf(PublicHttpConfigError);
        return (error as PublicHttpConfigError).reason;
      }
      throw new Error("the mounted release was accepted");
    };
    try {
      const authority = join(directory, "deployment-authority.json");
      expect(reason({ MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE: authority })).toBe(
        "mounted file is missing, unreadable or invalid (contents redacted)",
      );
      writeFileSync(authority, "", { mode: 0o644 });
      expect(reason({ MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE: authority })).toBe(
        "mounted file must be a nonempty regular file within its size limit",
      );
      writeFileSync(authority, "{not json", { mode: 0o644 });
      expect(reason({ MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE: authority })).toBe(
        "mounted public release JSON is invalid (contents redacted)",
      );
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });
  it("keeps the separate release and private-file byte bounds at the accepted and adjacent refused boundaries", () => {
    const directory = mkdtempSync(join(tmpdir(), "midgard-r7-mounted-bound-"));
    const path = join(directory, "mounted-input");
    try {
      const release = Buffer.alloc(PUBLIC_RELEASE_JSON_MAX_BYTES, 0x61);
      release[release.length - 1] = 0x62;
      writeFileSync(path, release, { mode: 0o600 });
      const accepted = readMountedHttpFile(
        path,
        false,
        PUBLIC_RELEASE_JSON_MAX_BYTES,
      );
      expect(accepted.byteLength).toBe(release.byteLength);
      expect(accepted[accepted.length - 1]).toBe(0x62);
      expect(() => readMountedHttpFile(path, true)).toThrow(
        "E_PUBLIC_HTTP_CONFIG",
      );
      writeFileSync(path, Buffer.alloc(PUBLIC_RELEASE_JSON_MAX_BYTES + 1));
      expect(() =>
        readMountedHttpFile(path, false, PUBLIC_RELEASE_JSON_MAX_BYTES),
      ).toThrow("E_PUBLIC_HTTP_CONFIG");
      writeFileSync(path, Buffer.alloc(65_536));
      expect(readMountedHttpFile(path, true).byteLength).toBe(65_536);
      writeFileSync(path, Buffer.alloc(65_537));
      expect(() => readMountedHttpFile(path, true)).toThrow(
        "E_PUBLIC_HTTP_CONFIG",
      );
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });
});
