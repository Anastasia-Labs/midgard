import { createHash } from "node:crypto";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Schema } from "effect";
import { describe, expect, it } from "vitest";

import {
  PUBLIC_RELEASE_INPUT_LIMITS,
  readMountedHttpFile,
  verifyMountedPublicRegistry,
} from "../src/commands/listen-public-http.js";

const outputSchema = Schema.Struct({
  retainedPath: Schema.String,
  serializedBytes: Schema.Number,
  serializedSha256: Schema.String,
});
const measurementSchema = Schema.Struct({
  profile: Schema.Literal("preprod-public"),
  retentionDays: Schema.Literal(120),
  contracts: Schema.Literal(525),
  rawValidators: Schema.Literal(1131),
  unusedValidators: Schema.Literal(620),
  outputs: Schema.Struct({
    manifest: outputSchema,
    parameters: outputSchema,
    classification: outputSchema,
  }),
});
const input = process.env.MIDGARD_PUBLIC_FRAMING_MEASUREMENT_FILE;
if (input === undefined)
  throw new Error(
    "genuine public constructor framing measurement is required for this explicit manual gate",
  );
const measurement = Schema.decodeUnknownSync(measurementSchema)(
  JSON.parse(readFileSync(input, "utf8")),
);
const artifact = (key: keyof typeof measurement.outputs) => {
  const declared = measurement.outputs[key];
  const bytes = readFileSync(declared.retainedPath);
  expect(bytes.length).toBe(declared.serializedBytes);
  expect(createHash("sha256").update(bytes).digest("hex")).toBe(
    declared.serializedSha256,
  );
  return bytes;
};
const limit = (key: keyof typeof measurement.outputs) =>
  key === "manifest"
    ? PUBLIC_RELEASE_INPUT_LIMITS.finalizedManifest
    : key === "parameters"
      ? PUBLIC_RELEASE_INPUT_LIMITS.parameterRegistry
      : PUBLIC_RELEASE_INPUT_LIMITS.classification;

// This gate authenticates retained genuine525-constructor bytes and mounted framing.
// Fixture publication outpoints/unused reasons are explicit. No signature, running
// public image, actual reference publication, funding or live acceptance is claimed.
describe("genuine public constructor mounted framing", () => {
  it.each(["manifest", "parameters", "classification"] as const)(
    "accepts actual %s and its exact envelope, refusing the adjacent byte before JSON parsing",
    (key) => {
      const bytes = artifact(key);
      const maximum = limit(key);
      expect(bytes.length).toBeLessThan(maximum);
      const genuine = readMountedHttpFile(
        measurement.outputs[key].retainedPath,
        false,
        maximum,
      );
      expect(genuine.equals(bytes)).toBe(true);
      const directory = mkdtempSync(
        join(tmpdir(), "midgard-r7-genuine-framing-"),
      );
      const path = join(directory, "padded.json");
      try {
        const padded = Buffer.alloc(maximum, 0x20);
        bytes.copy(padded);
        writeFileSync(path, padded, { mode: 0o600 });
        const paddedRead = readMountedHttpFile(path, false, maximum);
        expect(paddedRead.byteLength).toBe(maximum);
        expect(paddedRead.subarray(0, bytes.length).equals(bytes)).toBe(true);
        expect(
          computeDeploymentManifestJsonDigest(
            JSON.parse(paddedRead.toString("utf8")),
          ),
        ).toBe(
          computeDeploymentManifestJsonDigest(
            JSON.parse(genuine.toString("utf8")),
          ),
        );
        writeFileSync(path, Buffer.concat([padded, Buffer.from(" ")]));
        expect(() => readMountedHttpFile(path, false, maximum)).toThrow(
          "E_PUBLIC_HTTP_CONFIG",
        );
        expect(() =>
          readMountedHttpFile(measurement.outputs[key].retainedPath, true),
        ).toThrow("E_PUBLIC_HTTP_CONFIG");
        console.info("R7 genuine mounted framing", {
          key,
          bytes: bytes.length,
          maximum,
          sha256: measurement.outputs[key].serializedSha256,
        });
      } finally {
        rmSync(directory, { recursive: true, force: true });
      }
    },
  );
  it("keeps the actual genuine registry/classification correspondence and refuses a substituted unused reason", () => {
    const parameters: unknown = JSON.parse(
      artifact("parameters").toString("utf8"),
    );
    const classification: unknown = JSON.parse(
      artifact("classification").toString("utf8"),
    );
    const identity = Schema.decodeUnknownSync(
      Schema.Struct({
        manifestId: Schema.String,
        blueprintHash: Schema.String,
      }),
    )(parameters);
    const expected = {
      ...identity,
      parameterRegistryDigest: computeDeploymentManifestJsonDigest(parameters),
    };
    expect(() =>
      verifyMountedPublicRegistry(parameters, classification, expected),
    ).not.toThrow();
    const reasons = Schema.decodeUnknownSync(
      Schema.Record({ key: Schema.String, value: Schema.String }),
    )(classification);
    const [first] = Object.keys(reasons);
    expect(first).toBeDefined();
    expect(() =>
      verifyMountedPublicRegistry(
        parameters,
        { ...reasons, [first!]: "substituted reason" },
        expected,
      ),
    ).toThrow("signed parameter registry");
  });
});
