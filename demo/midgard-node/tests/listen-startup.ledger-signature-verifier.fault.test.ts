/**
 * The node's startup waits for the ledger signature verifier before any
 * durable work or the L1 follower
 * (`listen-startup.await-ledger-signature-verifier.ts`). Here libsodium's
 * initialisation fails in the node's process: the node's real startup
 * (`runNode` under its startup HTTP listener) fails by step and reason in
 * its local preflight, and the process exits non-zero for its supervisor to
 * restart, instead of running and holding. The instance lock, the step after
 * it, is never reached.
 *
 * The node is configured with distinct fresh wallets and a pinned native
 * owner, so every local preflight check before the verifier passes, and with
 * an L1 database no one listens on, so a startup that went past the verifier
 * would wait at the instance lock. Its validation workers are threads with
 * their own libsodium and start; their runtime services use the shared test
 * Postgres. The one seam is libsodium's initialisation in this process
 * (`helpers/ledger-signature-verifier-fails.ts`, imported first).
 */
import "./helpers/ledger-signature-verifier-fails.js";

import {
  findLedgerSignatureVerifierUnavailable,
  LEDGER_ED25519_READY_TIMEOUT_MS,
} from "@al-ft/midgard-core";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpServer } from "@effect/platform";
import { PgClient } from "@effect/sql-pg";
import { generateSeedPhrase } from "@lucid-evolution/lucid";
import { Deferred, Effect, Exit, Fiber, Logger, Redacted } from "effect";
import { describe, expect, it } from "vitest";

import { runNode } from "../src/commands/listen.run-node.js";
import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import {
  LEDGER_SIGNATURE_VERIFIER_STARTUP_BOUNDS,
  LEDGER_SIGNATURE_VERIFIER_STEP,
  LEDGER_SIGNATURE_VERIFIER_UNAVAILABLE,
} from "../src/commands/listen-startup.await-ledger-signature-verifier.js";
import { DEFAULT_IMPORT_CHECK_CONFIG } from "../src/services/config.import-check.js";
import { NodeConfig } from "../src/services/config.js";
import { Globals } from "../src/services/globals.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import { findStartupStepFailure } from "../src/services/startup-waiting.js";
import {
  INJECTED_LIBSODIUM_FAILURE,
  libsodiumReadyReads,
} from "./helpers/ledger-signature-verifier-fails.js";
import { makeTestNodeConfig } from "./helpers/test-node-config.js";

// Every check in the local preflight before the verifier passes: one
// validation worker that answers its readiness job within the verifier's own
// bound, four distinct wallets and a pinned native owner. The node's own
// database is a port no one listens on, so the instance lock, which connects
// to it, waits.
const startupConfig = makeTestNodeConfig({
  PORT: 0,
  VALIDATION_WORKER_POOL_SIZE: 1,
  VALIDATION_WORKER_JOB_TIMEOUT_MS: LEDGER_ED25519_READY_TIMEOUT_MS,
  NETWORK: "Preprod",
  MIN_FEE_A: 0n,
  MIN_FEE_B: 0n,
  VALIDATION_STRICTNESS_PROFILE: "phase1_midgard",
  IMPORT_CHECK: DEFAULT_IMPORT_CHECK_CONFIG,
  L1_OPERATOR_SEED_PHRASE: generateSeedPhrase(),
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: generateSeedPhrase(),
  L1_REFERENCE_SCRIPT_SEED_PHRASE: generateSeedPhrase(),
  L1_SETTLEMENT_SEED_PHRASE: generateSeedPhrase(),
  MPF_NATIVE_OWNER_BINARY_PATH: "/unused/midgard-native-owner",
  MPF_NATIVE_OWNER_BINARY_SHA256: "0".repeat(64),
  MPF_NATIVE_OWNER_SIDECAR_PATH: "/unused/midgard-native-owner.sidecar",
  POSTGRES_HOST: "127.0.0.1",
  POSTGRES_PORT: 1,
  POSTGRES_USER: "midgard",
  POSTGRES_PASSWORD: "midgard",
  POSTGRES_DB: "midgard",
});

/** The shared test Postgres the node's runtime services connect to. */
const testPostgres = PgClient.layer({
  host: process.env.POSTGRES_HOST ?? "127.0.0.1",
  port: Number(process.env.POSTGRES_PORT ?? "5433"),
  username: process.env.POSTGRES_USER ?? "postgres",
  password: Redacted.make(process.env.POSTGRES_PASSWORD ?? "postgres"),
  database: "postgres",
  maxConnections: 1,
});

/** The startup listener's address, from inside its server's scope. */
const listenerUrl = HttpServer.addressWith((address) =>
  address._tag === "TcpAddress"
    ? Effect.succeed(`http://127.0.0.1:${address.port}`)
    : Effect.die("Expected a TCP listener"),
);

// The verifier's attempts and the delays between them, with room for the
// validation workers to start first.
const EXIT_WITHIN_MS = 30_000;

describe("node startup when the ledger signature verifier fails to initialise", () => {
  it("fails the startup by step and reason in its local preflight and exits non-zero, before the instance lock", async () => {
    const messages: unknown[] = [];
    const listening = Effect.runSync(Deferred.make<string>());
    const fiber = Effect.runFork(
      (
        withStartupHttpServer(0, (startup) =>
          listenerUrl.pipe(
            Effect.flatMap((url) => Deferred.succeed(listening, url)),
            Effect.zipRight(runNode(startup)),
            Effect.provideService(NodeConfig, startupConfig),
            Effect.provideService(
              ContractDeploymentIdentity,
              ContractDeploymentIdentity.make({
                kind: "derived",
                consensusProfile: MIDGARD_CONSENSUS_PROFILE,
              }),
            ),
            Effect.provide(Globals.Default),
            Effect.provide(testPostgres),
          ),
        ) as Effect.Effect<void, unknown>
      ).pipe(
        Effect.provide(
          Logger.replace(
            Logger.defaultLogger,
            Logger.make(({ message }) => {
              messages.push(...[message].flat());
            }),
          ),
        ),
      ),
    );
    try {
      const url = await Effect.runPromise(
        Deferred.await(listening).pipe(Effect.timeout("10 seconds")),
      );
      const exit = await Effect.runPromise(
        Fiber.await(fiber).pipe(
          Effect.timeoutTo({
            duration: `${EXIT_WITHIN_MS} millis`,
            onSuccess: (done) => done,
            onTimeout: () => undefined,
          }),
        ),
      );
      if (exit === undefined) {
        const readyz: unknown = await (await fetch(`${url}/readyz`)).json();
        throw new Error(
          `Startup was still running after ${EXIT_WITHIN_MS.toString()} ms: /readyz=${JSON.stringify(readyz)}`,
        );
      }
      // The failure reached the verifier in this process.
      expect(libsodiumReadyReads()).toBeGreaterThan(0);
      expect(Exit.isFailure(exit)).toBe(true);
      const step = Exit.isFailure(exit)
        ? findStartupStepFailure(exit.cause)
        : undefined;
      expect(step).toMatchObject({
        step: LEDGER_SIGNATURE_VERIFIER_STEP,
        reason: LEDGER_SIGNATURE_VERIFIER_UNAVAILABLE,
        exhausted: true,
        attempts: LEDGER_SIGNATURE_VERIFIER_STARTUP_BOUNDS.maxAttempts,
      });
      // The verifier's typed fault, libsodium's own failure kept.
      const fault = findLedgerSignatureVerifierUnavailable(step?.cause);
      expect(String((fault?.cause as Error | undefined)?.message)).toContain(
        INJECTED_LIBSODIUM_FAILURE,
      );
      expect(messages).toContainEqual(
        `node_startup_failed stage=local_preflight step=${LEDGER_SIGNATURE_VERIFIER_STEP} reason=${LEDGER_SIGNATURE_VERIFIER_UNAVAILABLE}; a transient failure outlived its bound; the node exits non-zero`,
      );
      expect(
        messages.filter(
          (message) =>
            typeof message === "string" && message.includes("instance_lock"),
        ),
      ).toEqual([]);
    } finally {
      await Effect.runPromise(Fiber.interrupt(fiber));
    }
  }, 60_000);
});
