/**
 * The node-runtime deploy resolves its reference-script auth policy before
 * every publication: a recorded policy that has not expired is reused
 * however little time it has left; an expired one is replaced only while
 * nothing was published under it, otherwise the deploy refuses; and no new
 * policy is recorded unless the reference-script wallet can fund its whole
 * publication.
 */
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { createTrackedTempDirFactory } from "@al-ft/midgard-test-support/temp-files";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import { resolvePublishableReferenceScriptAuthPolicyProgram } from "../src/commands/deployment-run-state.js";
import { loadDeploymentRunState } from "../src/e2e/run-state.js";
import {
  ensureReferenceScriptTargetsProgram,
  nodeRuntimeReferenceScriptTargets,
} from "../src/transactions/reference-scripts.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";

const makeTempDir = createTrackedTempDirFactory("midgard-auth-rotation-");

const HUB_ORACLE_ONE_SHOT = { txHash: "aa".repeat(32), outputIndex: 0 };
const TIMELOCK_MS = SDK.REFERENCE_SCRIPT_AUTH_TIMELOCK_MS;
/** Emulator blocks are 20 one-second slots apart. */
const BLOCK_MS = 20_000;

type Deployment = {
  readonly emulator: Emulator;
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly runStatePath: string;
  readonly resolve: (
    timelockDurationMs?: number,
  ) => Promise<
    Either.Either<
      { readonly authPolicy: SDK.ReferenceScriptAuthPolicy },
      unknown
    >
  >;
  readonly recordedPolicyId: () => Promise<string | undefined>;
  readonly policyStepMessage: () => Promise<string | undefined>;
};

const deployment = async (lovelace: bigint): Promise<Deployment> => {
  const account = generateEmulatorAccount({ lovelace });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const runStatePath = join(await makeTempDir(), "run-state.json");
  const contractsFor = (policy: SDK.ReferenceScriptAuthPolicy) =>
    Effect.promise(() =>
      loadRealMidgardContractsForTest(HUB_ORACLE_ONE_SHOT, policy),
    );
  return {
    emulator,
    lucid,
    address: account.address,
    runStatePath,
    resolve: (timelockDurationMs = TIMELOCK_MS) =>
      runWithoutFollower(
        Effect.either(
          resolvePublishableReferenceScriptAuthPolicyProgram({
            deployment: {
              options: { runStatePath, freshRedeploy: false },
              network: "Preprod",
              hubOracleOneShotTxHash: HUB_ORACLE_ONE_SHOT.txHash,
              hubOracleOneShotOutputIndex: HUB_ORACLE_ONE_SHOT.outputIndex,
              manifestOutputPath: join(
                runStatePath,
                "..",
                "contract-deployment-info.json",
              ),
            },
            lucid,
            referenceScriptsAddress: account.address,
            commandName: "node-runtime",
            timelockDurationMs,
            contractsFor,
          }),
        ),
      ),
    recordedPolicyId: async () =>
      (await loadDeploymentRunState(runStatePath))?.identity
        .referenceScriptAuthPolicyId ?? undefined,
    policyStepMessage: async () =>
      (await loadDeploymentRunState(runStatePath))?.steps
        .referenceScriptAuthPolicy?.message ?? undefined,
  };
};

const policyOf = (
  result: Either.Either<
    { readonly authPolicy: SDK.ReferenceScriptAuthPolicy },
    unknown
  >,
): SDK.ReferenceScriptAuthPolicy => {
  if (Either.isLeft(result)) throw result.left;
  return result.right.authPolicy;
};

const failureMessage = (result: Either.Either<unknown, unknown>): string => {
  expect(Either.isLeft(result)).toBe(true);
  return Either.isLeft(result) ? String((result.left as Error).message) : "";
};

/** Lets the emulator chain run until `policy` has at most `leftMs` left. */
const runUntilLeft = (
  emulator: Emulator,
  policy: SDK.ReferenceScriptAuthPolicy,
  leftMs: number,
) => {
  const target = policy.expiresAtUnixTime - leftMs;
  emulator.awaitBlock(Math.ceil((target - emulator.now()) / BLOCK_MS));
};

describe("node-runtime reference-script auth policy", () => {
  it("reuses a recorded policy with 10 minutes left", async () => {
    const { emulator, resolve, recordedPolicyId, policyStepMessage } =
      await deployment(500_000_000_000n);
    const first = policyOf(await resolve());

    runUntilLeft(emulator, first, 10 * 60_000);
    expect(first.expiresAtUnixTime - emulator.now()).toBeGreaterThan(0);
    const second = policyOf(await resolve());

    expect(second.policyId).toBe(first.policyId);
    expect(await recordedPolicyId()).toBe(first.policyId);
    expect(await policyStepMessage()).not.toContain("fresh_redeploy_reason");
  }, 300_000);

  it("replaces an expired policy nothing was published under and records why", async () => {
    const { emulator, resolve, recordedPolicyId, policyStepMessage } =
      await deployment(500_000_000_000n);
    const first = policyOf(await resolve());
    expect(await recordedPolicyId()).toBe(first.policyId);

    runUntilLeft(emulator, first, 0);
    const second = policyOf(await resolve());

    expect(second.policyId).not.toBe(first.policyId);
    expect(second.expiresAtUnixTime - emulator.now()).toBeGreaterThan(
      TIMELOCK_MS - BLOCK_MS,
    );
    expect(await recordedPolicyId()).toBe(second.policyId);
    expect(await policyStepMessage()).toContain(
      `fresh_redeploy_reason=auth policy ${first.policyId}`,
    );
    expect(await policyStepMessage()).toContain("expired");
    expect(await policyStepMessage()).toContain("nothing was published");
  }, 300_000);

  it("refuses to replace an expired policy something was published under", async () => {
    const { emulator, lucid, resolve, recordedPolicyId } =
      await deployment(500_000_000_000n);
    const policy = policyOf(await resolve());
    const roster = nodeRuntimeReferenceScriptTargets(
      await loadRealMidgardContractsForTest(HUB_ORACLE_ONE_SHOT, policy),
    );
    const smallest = [...roster].sort(
      (left, right) => left.script.script.length - right.script.script.length,
    )[0]!;
    await runWithoutFollower(
      ensureReferenceScriptTargetsProgram(
        lucid,
        "test",
        [smallest],
        policy,
        lucid,
        undefined,
        new Set(),
      ),
    );

    runUntilLeft(emulator, policy, 0);
    const message = failureMessage(await resolve());

    expect(message).toContain(`auth policy ${policy.policyId} expired`);
    expect(message).toContain(
      "1 reference scripts are already published under it",
    );
    expect(message).toContain(
      `republishing all ${roster.length.toString()} reference scripts`,
    );
    expect(message).toMatch(/locks \d+\.\d{6} ADA/);
    expect(message).toContain("--fresh-redeploy --fresh-redeploy-reason");
    expect(await recordedPolicyId()).toBe(policy.policyId);
  }, 300_000);

  it("creates no policy when the reference-script wallet cannot fund the publication", async () => {
    const { resolve, runStatePath } = await deployment(100_000_000n);

    const message = failureMessage(await resolve());

    expect(message).toContain(
      "the reference-script wallet holds 100.000000 ADA",
    );
    expect(message).toContain("short by");
    expect(message).toContain("nothing was created");
    expect(await loadDeploymentRunState(runStatePath)).toBeNull();
  }, 300_000);
});
