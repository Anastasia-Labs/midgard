/**
 * A reference-script publication pass interrupted by a transient provider
 * failure resumes in place: it re-reads what landed, crosses the previous
 * validity window and publishes only what is still missing, so every target
 * ends with exactly one authenticated reference. An auth policy too close to
 * expiry is still refused without a retry.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { Cause, Runtime } from "effect";
import { expect, it } from "vitest";

import type { ProviderRetryOptions } from "../src/provider-retry.js";
import { ensureReferenceScriptTargetsProgram } from "../src/transactions/reference-scripts.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";

const IMMEDIATE_RETRY: ProviderRetryOptions = {
  maxAttempts: 3,
  baseDelayMs: 0,
  maxDelayMs: 0,
};

// What an undici read of a restarting Kupo/Ogmios rejects with.
const connectionRefused = (): Error =>
  new TypeError("fetch failed", {
    cause: Object.assign(new Error("connect ECONNREFUSED 127.0.0.1:1442"), {
      code: "ECONNREFUSED",
    }),
  });

// The shape that ended the lc1 reference-script step: the provider's
// transport KupmiosError inside the FiberFailure of an `Effect.runPromise`.
const kupoTransportFailure = (): Error =>
  Runtime.makeFiberFailure(
    Cause.fail(
      Object.assign(new Error("kupo getUtxos failed"), {
        name: "KupmiosError",
        _tag: "KupmiosError",
        kind: "transport",
        retryable: true,
      }),
    ),
  );

const fixture = async (count: number) => {
  const account = generateEmulatorAccount({ lovelace: 10_000_000_000n });
  const provider = new Emulator([account]);
  const lucid = await Lucid(provider, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const authPolicy = await SDK.createReferenceScriptAuthPolicy(lucid);
  const targets = Object.keys(SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)
    .slice(0, count)
    .map((name) => ({ name, script: authPolicy.mintingScript }));
  const options = {
    mode: "chained" as const,
    synchronize: async () => provider.slot,
    wait: async () => {
      provider.awaitBlock(1);
    },
  };
  provider.awaitTx = async () => {
    await options.wait();
    return true;
  };
  let submitted = 0;
  const submit = provider.submitTx.bind(provider);
  provider.submitTx = async (cbor) => {
    submitted += 1;
    return submit(cbor);
  };
  const run = (roster = targets, minAuthPolicyRemainingMs = 1) =>
    runWithoutFollower(
      ensureReferenceScriptTargetsProgram(
        lucid,
        "test",
        roster,
        authPolicy,
        lucid,
        undefined,
        minAuthPolicyRemainingMs,
        new Set(),
        options,
        IMMEDIATE_RETRY,
      ),
    );
  const referencesPerTarget = async () => {
    const outputs = await lucid.utxosAt(await lucid.wallet().address());
    return targets.map(
      (target) =>
        outputs.filter((utxo) =>
          SDK.hasReferenceScriptAuthRole(utxo, target, authPolicy),
        ).length,
    );
  };
  /** Fails the first indexer read after `afterSubmissions` submissions. */
  const injectOneIndexerFailure = (
    afterSubmissions: number,
    failure: () => Error = connectionRefused,
  ) => {
    let injected = false;
    const synchronize = options.synchronize;
    options.synchronize = async () => {
      if (!injected && submitted >= afterSubmissions) {
        injected = true;
        throw failure();
      }
      return synchronize();
    };
    return () => injected;
  };
  return {
    provider,
    lucid,
    targets,
    run,
    referencesPerTarget,
    injectOneIndexerFailure,
    submitted: () => submitted,
  };
};

it("resumes after a provider failure mid-publication and publishes each target exactly once", async () => {
  const f = await fixture(8);
  const injected = f.injectOneIndexerFailure(1, kupoTransportFailure);

  const result = await f.run();

  expect(injected()).toBe(true);
  expect(result.map(({ name }) => name)).toEqual(
    f.targets.map(({ name }) => name),
  );
  expect(await f.referencesPerTarget()).toEqual(f.targets.map(() => 1));
});

it("still publishes a genuinely absent target when resuming", async () => {
  const f = await fixture(8);
  await f.run(f.targets.slice(0, 4));
  expect(await f.referencesPerTarget()).toEqual([1, 1, 1, 1, 0, 0, 0, 0]);
  const before = f.submitted();
  const injected = f.injectOneIndexerFailure(before + 1);

  const result = await f.run();

  expect(injected()).toBe(true);
  expect(f.submitted()).toBeGreaterThan(before);
  expect(result).toHaveLength(8);
  expect(await f.referencesPerTarget()).toEqual(f.targets.map(() => 1));
});

it("refuses an auth policy too close to expiry without retrying", async () => {
  const f = await fixture(4);
  let reads = 0;
  const utxosAt = f.lucid.utxosAt.bind(f.lucid);
  f.lucid.utxosAt = async (...args: Parameters<typeof utxosAt>) => {
    reads += 1;
    return utxosAt(...args);
  };
  // The policy lives four hours; demanding five left can never be met.
  await expect(f.run(f.targets, 5 * 60 * 60 * 1_000)).rejects.toThrow(
    /cannot publish missing references/u,
  );
  expect(f.submitted()).toBe(0);
  // One discovery read: the refusal was not retried.
  expect(reads).toBe(1);
  expect(await f.referencesPerTarget()).toEqual([0, 0, 0, 0]);
});

it("does not resume a reconciliation that ran out its own progress deadline", async () => {
  const f = await fixture(4);
  const injected = f.injectOneIndexerFailure(
    1,
    () =>
      new Error(
        "Reference publication reconciliation timed out; restart must resolve previous validity windows",
      ),
  );

  await expect(f.run()).rejects.toThrow(/reconciliation timed out/u);
  expect(injected()).toBe(true);
});
