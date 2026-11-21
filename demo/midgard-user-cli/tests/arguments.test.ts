/**
 * Arguments, the seed source and the manifest are refused by name before
 * anything reads the chain or writes a record. No L1 is reachable here.
 */
import { chmodSync, existsSync, symlinkSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import { generateSeedPhrase } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { type UserCliDeps } from "../src/deps.js";
import { runUserCli } from "../src/run.js";
import {
  manifestIdentityInput,
  tempDirectory,
  withManifestId,
  writeSeedFile,
} from "./support/user-cli.js";

const SEED = generateSeedPhrase();
const POLICY = "ab".repeat(28);

const run = async (argv: readonly string[], env = { USER_SEED: SEED }) => {
  const out: string[] = [];
  const err: string[] = [];
  const home = tempDirectory("args-home");
  const unreachable = async (): Promise<never> => {
    throw new Error("unreachable in argument tests");
  };
  const deps: UserCliDeps = {
    env,
    stdout: (text) => out.push(text),
    stderr: (text) => err.push(text),
    homeDir: home,
    now: Date.now,
    sleep: unreachable,
    fetch: unreachable,
    openL1View: unreachable,
    connectNode: unreachable,
    connectL2: unreachable,
  };
  const code = await runUserCli(argv, deps);
  return {
    code,
    stdout: out.join(""),
    stderr: err.join(""),
    home,
  };
};

const deposit = (...extra: string[]) => [
  "deposit",
  "--submission-id",
  "args",
  "--l2-address",
  "addr_test1vqz4js6k2c6un3h8y8sh2nmkg7u9s8w7up0psd4w6zv6r9usknpxf",
  "--lovelace",
  "5000000",
  "--wallet-seed-phrase-env",
  "USER_SEED",
  ...extra,
];

const replaceFlag = (argv: string[], flag: string, value: string) => {
  const copy = [...argv];
  copy[copy.indexOf(flag) + 1] = value;
  return copy;
};

const refused = async (argv: readonly string[], message: string) => {
  const result = await run(argv);
  expect(result.code).toBe(1);
  expect(result.stderr).toContain(message);
  expect(result.stderr).not.toContain(SEED);
  return result;
};

describe("midgard-user arguments", () => {
  it("prints help and exits 0", async () => {
    const result = await run(["--help"]);
    expect(result.code).toBe(0);
    for (const command of ["deposit", "withdraw", "status"])
      expect(result.stdout).toContain(command);
  });

  it("requires --submission-id and bounds its length", async () => {
    const argv = deposit();
    const missing = argv.filter((_, index) => index !== 1 && index !== 2);
    expect((await run(missing)).code).toBe(1);
    await refused(
      replaceFlag(argv, "--submission-id", "x".repeat(129)),
      "--submission-id must be 1 to 128 characters long.",
    );
  });

  it("refuses zero and malformed lovelace", async () => {
    await refused(
      replaceFlag(deposit(), "--lovelace", "0"),
      "--lovelace must be greater than zero.",
    );
    await refused(
      replaceFlag(deposit(), "--lovelace", "1.5"),
      'Invalid lovelace amount "1.5"',
    );
    await refused(
      deposit("--structural-lovelace", "0"),
      "--structural-lovelace must be greater than zero.",
    );
  });

  it("refuses malformed and duplicate assets", async () => {
    await refused(
      deposit("--asset", `${POLICY}.00`),
      "Expected policyId.assetName:amount.",
    );
    await refused(
      deposit("--asset", `${POLICY}.00:1`, "--asset", `${POLICY}.00:2`),
      `Duplicate additional asset "${POLICY}00"`,
    );
    await refused(
      deposit("--asset", `${POLICY}.00:0`),
      "Quantity must be greater than zero.",
    );
  });

  it("refuses datum hex that is odd-length or not Plutus data", async () => {
    await refused(
      deposit("--l2-datum", "d8798"),
      "--l2-datum must be an even-length hex string",
    );
    await refused(
      deposit("--l2-datum", "ff"),
      "--l2-datum must decode as Plutus data CBOR",
    );
  });

  it("refuses a malformed --l2-out-ref", async () => {
    await refused(
      [
        "withdraw",
        "--submission-id",
        "args",
        "--l2-out-ref",
        "not-an-outref",
        "--l1-address",
        "addr_test1vqz4js6k2c6un3h8y8sh2nmkg7u9s8w7up0psd4w6zv6r9usknpxf",
        "--node-endpoint",
        "http://127.0.0.1:9",
        "--wallet-seed-phrase-env",
        "USER_SEED",
      ],
      "--l2-out-ref must be <txHash>#<outputIndex>",
    );
  });

  it("refuses an L1 access other than kupmios or blockfrost", async () => {
    const result = await run(deposit("--l1", "node"));
    expect(result.code).toBe(1);
    expect(result.stderr).toContain("--l1");
  });
});

describe("midgard-user wallet seed", () => {
  const withoutSeedFlag = () => deposit().slice(0, -2);

  it("requires exactly one seed source", async () => {
    await refused(
      withoutSeedFlag(),
      "Give exactly one of --wallet-seed-phrase-env NAME or --wallet-seed-phrase-file PATH.",
    );
    const directory = tempDirectory("seed-both");
    await refused(
      deposit("--wallet-seed-phrase-file", writeSeedFile(directory, SEED)),
      "Give exactly one of",
    );
  });

  it("refuses an empty environment variable", async () => {
    const result = await run(deposit(), { USER_SEED: "  " });
    expect(result.code).toBe(1);
    expect(result.stderr).toContain(
      'Environment variable "USER_SEED" does not hold a wallet seed phrase.',
    );
  });

  it("refuses a relative, readable, symlinked or empty seed file without printing it", async () => {
    const directory = tempDirectory("seed-file");
    const path = writeSeedFile(directory, SEED);
    await refused(
      [...withoutSeedFlag(), "--wallet-seed-phrase-file", "wallet-seed"],
      "--wallet-seed-phrase-file must be an absolute path.",
    );
    const link = join(directory, "link");
    symlinkSync(path, link);
    await refused(
      [...withoutSeedFlag(), "--wallet-seed-phrase-file", link],
      "--wallet-seed-phrase-file is missing, unreadable or a symbolic link.",
    );
    const empty = join(directory, "empty");
    writeFileSync(empty, "", { mode: 0o600 });
    await refused(
      [...withoutSeedFlag(), "--wallet-seed-phrase-file", empty],
      "--wallet-seed-phrase-file must be a nonempty regular file",
    );
    chmodSync(path, 0o644);
    await refused(
      [...withoutSeedFlag(), "--wallet-seed-phrase-file", path],
      "make it mode 0600",
    );
  });

  it("reads a mode-0600 seed file and goes on to the manifest", async () => {
    const directory = tempDirectory("seed-ok");
    await refused(
      [
        ...withoutSeedFlag(),
        "--wallet-seed-phrase-file",
        writeSeedFile(directory, SEED),
      ],
      "--manifest (or MIDGARD_DEPLOYMENT_MANIFEST) is required.",
    );
  });
});

describe("midgard-user deployment manifest", () => {
  it("refuses a manifest whose id does not match its content, writing nothing", async () => {
    const directory = tempDirectory("manifest-id");
    const path = join(directory, "manifest.json");
    writeFileSync(
      path,
      JSON.stringify({
        ...withManifestId(manifestIdentityInput()),
        manifestId: "00".repeat(32),
      }),
    );
    const result = await refused(
      deposit("--manifest", path),
      "Deployment manifest id mismatch",
    );
    expect(existsSync(join(result.home, ".midgard"))).toBe(false);
  });

  it("refuses an identity-valid manifest without user-history contracts", async () => {
    const directory = tempDirectory("manifest-empty");
    const path = join(directory, "manifest.json");
    writeFileSync(
      path,
      JSON.stringify(withManifestId(manifestIdentityInput())),
    );
    await refused(deposit("--manifest", path), "is not usable");
  });
});
