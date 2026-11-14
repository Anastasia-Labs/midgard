/**
 * Reference-script publication runs for as long as the auth policy lets a
 * transaction be valid: it publishes with little time left, and stops with
 * a typed expired error, submitting nothing, once no transaction can be
 * valid under the policy any more.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import { ensureReferenceScriptTargetsProgram } from "../src/transactions/reference-scripts.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";

/** Emulator blocks are 20 one-second slots apart. */
const BLOCK_MS = 20_000;

const fixture = async () => {
  const account = generateEmulatorAccount({ lovelace: 10_000_000_000n });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const authPolicy = await SDK.createReferenceScriptAuthPolicy(
    lucid,
    lucid.slotToUnixTime(lucid.currentSlot()),
    SDK.REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
  );
  const targets = Object.keys(SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)
    .slice(0, 4)
    .map((name) => ({ name, script: authPolicy.mintingScript }));
  let submitted = 0;
  const submit = emulator.submitTx.bind(emulator);
  emulator.submitTx = async (cbor) => {
    submitted += 1;
    return submit(cbor);
  };
  return {
    emulator,
    lucid,
    authPolicy,
    targets,
    submitted: () => submitted,
  };
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

const publish = (
  lucid: LucidEvolution,
  targets: readonly SDK.ReferenceScriptTarget[],
  authPolicy: SDK.ReferenceScriptAuthPolicy,
) =>
  runWithoutFollower(
    Effect.either(
      ensureReferenceScriptTargetsProgram(
        lucid,
        "test",
        targets,
        authPolicy,
        lucid,
        undefined,
        new Set(),
      ),
    ),
  );

const causes = (error: unknown): unknown[] => {
  const chain: unknown[] = [];
  let current: unknown = error;
  while (current !== undefined && chain.length < 16) {
    chain.push(current);
    current =
      typeof current === "object" && current !== null && "cause" in current
        ? (current as { readonly cause?: unknown }).cause
        : undefined;
  }
  return chain;
};

describe("reference-script auth policy expiry", () => {
  it("publishes a batch under a policy with 30 minutes left", async () => {
    const { emulator, lucid, authPolicy, targets } = await fixture();
    runUntilLeft(emulator, authPolicy, 30 * 60_000);
    expect(authPolicy.expiresAtUnixTime - emulator.now()).toBeLessThan(
      31 * 60_000,
    );

    const result = await publish(lucid, targets, authPolicy);

    if (Either.isLeft(result)) throw result.left;
    expect(result.right.map(({ name }) => name)).toEqual(
      targets.map(({ name }) => name),
    );
  }, 300_000);

  it("refuses with the expired error and submits nothing once the policy expired", async () => {
    const { emulator, lucid, authPolicy, targets, submitted } = await fixture();
    runUntilLeft(emulator, authPolicy, 0);

    const result = await publish(lucid, targets, authPolicy);

    expect(Either.isLeft(result)).toBe(true);
    const expired = causes(
      Either.isLeft(result) ? result.left : undefined,
    ).find((cause) => cause instanceof SDK.ReferenceScriptAuthExpiredError);
    expect(expired).toBeInstanceOf(SDK.ReferenceScriptAuthExpiredError);
    expect(String(expired)).toContain(
      `auth policy ${authPolicy.policyId} expired`,
    );
    expect(submitted()).toBe(0);
  }, 300_000);

  it("treats the policy as expired from the last slot its transactions are clamped to", async () => {
    const { emulator, lucid, authPolicy } = await fixture();
    const check = () =>
      SDK.assertReferenceScriptAuthUnexpired({
        lucid,
        policy: authPolicy,
        scopeName: "test",
        targetNames: ["hub-oracle minting"],
      });
    emulator.awaitSlot(authPolicy.expiresAtSlot - 2 - emulator.slot);
    expect(check).not.toThrow();
    emulator.awaitSlot(1);
    expect(check).toThrow(SDK.ReferenceScriptAuthExpiredError);
  });
});
