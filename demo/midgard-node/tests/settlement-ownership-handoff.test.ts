/**
 * How the settlement worker program takes the settlement ownership lease:
 * the node hands every replacement worker the same owner token, a live lease
 * of another token is waited out as startup, and a refusal that expiry cannot
 * resolve fails the run at once with its cause.
 */
import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { generateSeedPhrase } from "@lucid-evolution/lucid";
import { Effect, Exit, Fiber } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/settlement.js";
import { NodeConfig } from "../src/services/config.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import {
  type SettlementHealth,
  settlementProgram,
  settlementWalletAddress,
} from "../src/services/settlement.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const run = <A, E>(
  effect: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig>,
) => Effect.runPromise(provideDatabaseLayers(effect));
const deploymentId = "a1".repeat(32);
const REFUSAL =
  "Settlement wallet identity changed or another node owns settlement";

beforeEach(() =>
  run(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      const token = yield* Authority.acquire({
        deploymentIdentity: deploymentId,
        ownerToken: randomUUID(),
        leaseDurationMs: 60_000,
      });
      yield* Authority.publishReady(token, {
        point: { slot: 10, id: "b1".repeat(32) },
        snapshotDigest: "c1".repeat(32),
      });
    }),
  ),
);

/** The worker program with no work queued; it never reaches L1. */
const worker = (token: string, seed: string) =>
  Effect.gen(function* () {
    const config = {
      ...(yield* NodeConfig),
      L1_SETTLEMENT_SEED_PHRASE: seed,
    };
    const reports: SettlementHealth[] = [];
    const fiber = yield* Effect.fork(
      settlementProgram((health) => reports.push(health), token).pipe(
        Effect.provideService(NodeConfig, config),
        Effect.provideService(ContractDeploymentIdentity, {
          kind: "manifest",
          manifestId: deploymentId,
        } as unknown as ContractDeploymentIdentity),
        // Only the payout commands of a job read the contracts.
        Effect.provideService(
          MidgardContracts,
          {} as unknown as MidgardContracts,
        ),
        Effect.provideService(
          Lucid,
          new Lucid({
            api: { selectWallet: { fromSeed: () => undefined } },
          } as unknown as Lucid),
        ),
      ),
    );
    const until = (state: SettlementHealth["state"]) =>
      Effect.gen(function* () {
        for (let polls = 0; polls < 300; polls++) {
          if (reports.some((report) => report.state === state)) return;
          yield* Effect.sleep("50 millis");
        }
        throw new Error(`no ${state} report: ${JSON.stringify(reports)}`);
      });
    return { fiber, reports, until };
  });

describe("settlement ownership handoff", () => {
  it("resumes a handed-over token at once and holds a foreign live owner off", async () => {
    await run(
      withoutFollowerJournal(
        Effect.gen(function* () {
          const seed = generateSeedPhrase();
          const handedOver = randomUUID();
          const foreign = {
            deploymentId,
            walletAddress: settlementWalletAddress({
              ...(yield* NodeConfig),
              L1_SETTLEMENT_SEED_PHRASE: seed,
            }),
            token: randomUUID(),
          };
          // Another node process holds a live lease: stand by, as startup.
          yield* Journal.renew(foreign);
          const waiting = yield* worker(handedOver, seed);
          yield* waiting.until("starting");
          expect(waiting.reports.map((report) => report.detail)).toContain(
            `waiting for the previous settlement ownership lease: ${REFUSAL}`,
          );
          expect(
            waiting.reports.some((report) => report.state === "running"),
          ).toBe(false);
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE settlement_owners SET lease_until = clock_timestamp() - interval '1 second'`;
          yield* waiting.until("running");
          // The node stops that worker; its replacement carries the same token
          // and resumes the still-live lease without waiting for it to expire.
          yield* Fiber.interrupt(waiting.fiber);
          const started = Date.now();
          const replacement = yield* worker(handedOver, seed);
          yield* replacement.until("running");
          expect(Date.now() - started).toBeLessThan(5_000);
          expect(
            replacement.reports.some((report) => report.state === "starting"),
          ).toBe(false);
          // The foreign process stays fenced while the replacement holds it.
          expect((yield* Effect.either(Journal.renew(foreign)))._tag).toBe(
            "Left",
          );
          expect(
            (yield* Effect.either(Journal.assertOwner(foreign)))._tag,
          ).toBe("Left");
          yield* Fiber.interrupt(replacement.fiber);
        }),
      ),
    );
  }, 60_000);

  it("fails at once, without a lease wait, when settlement is bound to another wallet", async () => {
    await run(
      withoutFollowerJournal(
        Effect.gen(function* () {
          // An expired lease: only the changed wallet refuses this renew.
          yield* Journal.renew({
            deploymentId,
            walletAddress: "previous-settlement-wallet",
            token: randomUUID(),
          });
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE settlement_owners SET lease_until = clock_timestamp() - interval '1 second'`;
          const started = Date.now();
          const rebound = yield* worker(randomUUID(), generateSeedPhrase());
          const exit = yield* Fiber.await(rebound.fiber).pipe(
            Effect.timeout("5 seconds"),
          );
          expect(Date.now() - started).toBeLessThan(5_000);
          expect(Exit.isFailure(exit)).toBe(true);
          if (Exit.isFailure(exit))
            expect(String(exit.cause)).toContain(REFUSAL);
          expect(rebound.reports).toEqual([]);
        }),
      ),
    );
  }, 30_000);
});
