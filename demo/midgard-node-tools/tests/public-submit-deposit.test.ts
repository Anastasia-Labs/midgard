import "midgard-node/index.registration-9";

import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  credentialToAddress,
  generateSeedPhrase,
} from "@lucid-evolution/lucid";
import { program } from "midgard-node/index.registration";
import { afterAll, afterEach, beforeAll, expect, it, vi } from "vitest";

// Runs in the `preprod-public-release` project: the node command line as a
// preprod-public build compiles it, which the public journey drives with a
// mounted seed file for its deposit wallet.

let directory: string;
beforeAll(async () => {
  directory = await mkdtemp(join(tmpdir(), "midgard-public-deposit-"));
  program.exitOverride();
});
afterAll(() => rm(directory, { recursive: true, force: true }));
afterEach(() => {
  process.exitCode = undefined;
  vi.restoreAllMocks();
  vi.unstubAllEnvs();
});

/** Runs submit-deposit with a wallet option and returns what it printed to stderr. */
async function submitDeposit(...wallet: string[]) {
  const errors = vi.spyOn(console, "error").mockImplementation(() => {});
  await program.parseAsync([
    "node",
    "midgard-node",
    "submit-deposit",
    "--submission-id",
    "public-journey-deposit",
    "--l2-address",
    credentialToAddress("Preprod", { type: "Key", hash: "00".repeat(28) }),
    "--lovelace",
    "5000000",
    ...wallet,
  ]);
  return errors.mock.calls.flat().join("\n");
}

it("takes the deposit wallet from a mounted seed file and refuses one other users can read", async () => {
  const seedFile = join(directory, "user-seed");
  await writeFile(seedFile, "a seed phrase\n", { mode: 0o644 });
  const output = await submitDeposit("--wallet-seed-phrase-file", seedFile);
  expect(process.exitCode).toBe(1);
  expect(output).toContain(
    'submit-deposit: {"error_code":"E_PUBLIC_HTTP_CONFIG","reason":"mounted file is missing, unreadable or invalid (contents redacted)"}',
  );
});

it("refuses a deposit wallet from the environment, redacting why", async () => {
  // A seed a preprod-testing build would sign with.
  vi.stubEnv("MIDGARD_PUBLIC_DEPOSIT_TEST_SEED", generateSeedPhrase());
  const output = await submitDeposit(
    "--wallet-seed-phrase-env",
    "MIDGARD_PUBLIC_DEPOSIT_TEST_SEED",
  );
  expect(process.exitCode).toBe(1);
  expect(output).toBe('submit-deposit: "[redacted]"');
});
