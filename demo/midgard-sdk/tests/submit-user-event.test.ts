/**
 * A user's deposit or withdrawal, run against genuine applied event-history
 * lists on the Lucid emulator with an in-memory attempt store. Every
 * assertion reads what the chain or the store shows: exact transaction
 * statuses, the Order nodes on the list, and the recorded attempts.
 */
import {
  OutputReference,
  type UserSubmissionAttempt,
  UserSubmissionPending,
  type UserSubmissionStore,
} from "@al-ft/midgard-sdk";
import {
  loadUserHistoryBlueprint,
  setupUserHistoryScene,
  type UserHistoryScene,
} from "@al-ft/midgard-sdk/test-support/user-history";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  CASE_TIMEOUT_MS,
  crashOnce,
  landed,
  listFor,
  memoryStore,
  newUser,
  ordersFor,
  outRefLabels,
  refusal,
  type RunResult,
  SCENE_TIMEOUT_MS,
  statusOf,
  submit,
  type User,
} from "./submit-user-event.harness.js";

const blueprint = loadUserHistoryBlueprint();

describe("a user submission against inline-payload lists", () => {
  let scene: UserHistoryScene;
  beforeAll(async () => {
    scene = await setupUserHistoryScene({ blueprint });
  }, SCENE_TIMEOUT_MS);

  describe.each(["Deposit", "Withdrawal"] as const)("%s", (kind) => {
    // First in each list: only the root exists, so both admissions below
    // spend it.
    it(
      "outlives a rival admission that spends its predecessor: the recorded attempt dies unseen, the next one waits out the rival's protection and lands",
      async () => {
        const user = await newUser(scene);
        const rival = {
          account: scene.deployer,
          view: {
            lucid: scene.operator,
            indexedSlot: async () => scene.emulator.slot,
          },
        };
        const rivalStore = memoryStore();
        let rivalRun: RunResult | undefined;
        const { attempts, store } = memoryStore();
        const raced: UserSubmissionStore = {
          latest: store.latest,
          append: async (attempt) => {
            const created = await store.append(attempt);
            if (rivalRun === undefined)
              rivalRun = await submit(scene, kind, rival, rivalStore.store);
            return created;
          },
        };

        const run = await submit(scene, kind, user, raced);

        const rivalOutcome = landed(rivalRun!);
        const outcome = landed(run);
        expect(attempts.map((a) => a.phase)).toEqual([
          "Admission",
          "Admission",
        ]);
        expect(await statusOf(scene, attempts[0]!.txHash)).toBe("not_found");
        expect(outcome.admissionTxHash).toBe(attempts[1]!.txHash);
        expect(outcome.attempts).toBe(2);
        expect(
          run.lines.some((line) => /is protected; waiting/.test(line)),
        ).toBe(true);
        expect(await ordersFor(scene, kind, outcome.nonce)).toBe(1);
        expect(await ordersFor(scene, kind, rivalOutcome.nonce)).toBe(1);
      },
      CASE_TIMEOUT_MS,
    );

    it(
      "lands once with one recorded attempt, and a rerun returns the same transaction without submitting anything",
      async () => {
        const user = await newUser(scene);
        const { attempts, store } = memoryStore();

        const first = landed(await submit(scene, kind, user, store));

        expect(attempts).toHaveLength(1);
        expect(first.kind).toBe(kind);
        expect(first.admissionTxHash).toBe(attempts[0]!.txHash);
        expect(first.attempts).toBe(1);
        expect(first.publicationTxHash).toBeUndefined();
        expect(Data.from(first.eventId, OutputReference)).toEqual({
          transactionId: first.nonce.txHash,
          outputIndex: BigInt(first.nonce.outputIndex),
        });
        expect(await statusOf(scene, first.admissionTxHash)).toBe("confirmed");
        expect(await ordersFor(scene, kind, first.nonce)).toBe(1);

        const list = listFor(scene, kind).spendingScriptAddress;
        const before = {
          wallet: outRefLabels(await user.lucid.wallet().getUtxos()),
          list: outRefLabels(await scene.operator.utxosAt(list)),
        };
        const rerun = await submit(scene, kind, user, store);

        expect(landed(rerun)).toEqual(first);
        expect(rerun.lines.filter((line) => /^broadcast/.test(line))).toEqual(
          [],
        );
        expect(attempts).toHaveLength(1);
        expect({
          wallet: outRefLabels(await user.lucid.wallet().getUtxos()),
          list: outRefLabels(await scene.operator.utxosAt(list)),
        }).toEqual(before);
      },
      CASE_TIMEOUT_MS,
    );

    it(
      "after a crash between recording and broadcasting, a rerun broadcasts the identical recorded bytes",
      async () => {
        const user = await newUser(scene);
        const { attempts, store } = memoryStore();

        const crashed = await submit(
          scene,
          kind,
          user,
          crashOnce(store, "after-create"),
        );
        expect(refusal(crashed).message).toMatch(/simulated crash/);
        expect(attempts).toHaveLength(1);
        expect(await statusOf(scene, attempts[0]!.txHash)).toBe("not_found");

        const outcome = landed(await submit(scene, kind, user, store));

        expect(attempts).toHaveLength(1);
        expect(outcome.admissionTxHash).toBe(attempts[0]!.txHash);
        expect(await ordersFor(scene, kind, outcome.nonce)).toBe(1);
      },
      CASE_TIMEOUT_MS,
    );

    it(
      "after a crash once the attempt is broadcast, a rerun finds it confirmed and does not submit again",
      async () => {
        const user = await newUser(scene);
        const { attempts, store } = memoryStore();
        const lines: string[] = [];

        const crashed = await submit(scene, kind, user, store, {
          options: {
            log: (line) => {
              lines.push(line);
              if (/^broadcast/.test(line)) throw new Error("simulated crash");
            },
          },
        });
        expect(refusal(crashed).message).toMatch(/simulated crash/);
        expect(lines.at(-1)).toMatch(/^broadcast \w+ admission attempt 1 /);
        scene.emulator.awaitBlock();
        expect(await statusOf(scene, attempts[0]!.txHash)).toBe("confirmed");

        const rerun = await submit(scene, kind, user, store);

        expect(landed(rerun).admissionTxHash).toBe(attempts[0]!.txHash);
        expect(rerun.lines.filter((line) => /^broadcast/.test(line))).toEqual(
          [],
        );
        expect(attempts).toHaveLength(1);
      },
      CASE_TIMEOUT_MS,
    );

    it(
      "builds attempt 2 only once attempt 1 is indexed past its TTL unseen; exactly one Order lands and attempt 1 can no longer be submitted",
      async () => {
        const user = await newUser(scene);
        const { attempts, store } = memoryStore();
        await submit(scene, kind, user, crashOnce(store, "after-create"));
        expect(attempts).toHaveLength(1);
        scene.emulator.awaitSlot(
          attempts[0]!.ttlSlot - scene.emulator.slot + 1,
        );

        const limited = await submit(scene, kind, user, store, {
          options: { maxAttemptsPerRun: 1 },
        });
        expect(refusal(limited).message).toMatch(
          /1 \w+ attempts expired unseen in this run/,
        );
        expect(attempts).toHaveLength(1);

        const outcome = landed(await submit(scene, kind, user, store));

        expect(attempts).toHaveLength(2);
        expect(outcome.attempts).toBe(2);
        expect(outcome.admissionTxHash).toBe(attempts[1]!.txHash);
        expect(await statusOf(scene, attempts[0]!.txHash)).toBe("not_found");
        expect(await ordersFor(scene, kind, outcome.nonce)).toBe(1);
        await expect(
          Promise.resolve().then(() =>
            scene.emulator.submitTx(attempts[0]!.signedTxCbor),
          ),
        ).rejects.toThrow();
      },
      CASE_TIMEOUT_MS,
    );

    it(
      "two concurrent runs of one submission id sharing a store land one transaction and both report it",
      async () => {
        const user = await newUser(scene);
        const { attempts, store } = memoryStore();

        const [a, b] = await Promise.all([
          submit(scene, kind, user, store),
          submit(scene, kind, user, store),
        ]);

        expect(attempts).toHaveLength(1);
        expect(landed(a).admissionTxHash).toBe(attempts[0]!.txHash);
        expect(landed(b).admissionTxHash).toBe(attempts[0]!.txHash);
        expect(await ordersFor(scene, kind, landed(a).nonce)).toBe(1);
      },
      CASE_TIMEOUT_MS,
    );

    it(
      "two submission ids from one wallet, run concurrently, both land on distinct nonces and neither lands twice",
      async () => {
        const user = await newUser(scene);
        const first = memoryStore();
        const second = memoryStore();

        const [a, b] = await Promise.all([
          submit(scene, kind, user, first.store, { amount: 10_000_000n }),
          submit(scene, kind, user, second.store, { amount: 11_000_000n }),
        ]);

        const outcomes = [landed(a), landed(b)];
        expect(outcomes[0]!.eventId).not.toBe(outcomes[1]!.eventId);
        for (const outcome of outcomes)
          expect(await ordersFor(scene, kind, outcome.nonce)).toBe(1);
        // Every attempt that is not the landed one never reached the chain.
        for (const [store, outcome] of [
          [first, outcomes[0]!],
          [second, outcomes[1]!],
        ] as const)
          for (const attempt of store.attempts)
            if (attempt.txHash !== outcome.admissionTxHash)
              expect(await statusOf(scene, attempt.txHash)).toBe("not_found");
      },
      CASE_TIMEOUT_MS,
    );

    it(
      "stops at its deadline with the live attempt named, and a rerun resumes that exact transaction",
      async () => {
        // Clear every predecessor's protection, so the build waits for nothing.
        scene.emulator.awaitSlot(600);
        const user = await newUser(scene);
        const { attempts, store } = memoryStore();
        // A clock that moves while the chain does not: no block confirms the
        // broadcast attempt before the deadline passes.
        let offset = 0;

        const stopped = await submit(scene, kind, user, store, {
          options: {
            deadlineMs: scene.emulator.now() + 1_000,
            now: () => scene.emulator.now() + offset,
            sleep: async (ms) => {
              offset += ms;
            },
          },
        });

        if (stopped.result._tag === "Right")
          throw new Error("expected the run to stop at its deadline");
        const pending = stopped.result.left;
        expect(pending).toBeInstanceOf(UserSubmissionPending);
        if (!(pending instanceof UserSubmissionPending)) return;
        expect(pending.attempt?.txHash).toBe(attempts[0]!.txHash);
        expect(await statusOf(scene, attempts[0]!.txHash)).toBe("pending");

        const outcome = landed(await submit(scene, kind, user, store));

        expect(outcome.admissionTxHash).toBe(attempts[0]!.txHash);
        expect(attempts).toHaveLength(1);
      },
      CASE_TIMEOUT_MS,
    );
  });

  describe("refusals", () => {
    let user: User;
    let spentNonce: UTxO;
    let recorded: UserSubmissionAttempt;
    beforeAll(async () => {
      user = await newUser(scene);
      const walletBefore = await user.lucid.wallet().getUtxos();
      const { attempts, store } = memoryStore();
      const outcome = landed(await submit(scene, "Deposit", user, store));
      spentNonce = walletBefore.find(
        (utxo) =>
          utxo.txHash === outcome.nonce.txHash &&
          utxo.outputIndex === outcome.nonce.outputIndex,
      )!;
      recorded = attempts[0]!;
    }, CASE_TIMEOUT_MS);

    it(
      "refuses an event id that is already admitted, even when a stale wallet view still offers its nonce, and records nothing",
      async () => {
        const stale = await scene.lucidFor(user.account.seedPhrase);
        const { attempts, store } = memoryStore();
        stale.overrideUTxOs([
          ...(await user.lucid.wallet().getUtxos()),
          spentNonce,
        ]);
        try {
          const run = await submit(
            scene,
            "Deposit",
            {
              account: user.account,
              view: {
                lucid: stale,
                indexedSlot: async () => scene.emulator.slot,
              },
            },
            store,
            { config: { nonceInput: spentNonce } },
          );

          expect(refusal(run).message).toMatch(/already admitted on L1/);
          expect(attempts).toEqual([]);
        } finally {
          stale.clearUTxOOverride();
        }
      },
      CASE_TIMEOUT_MS,
    );

    it.each([
      {
        name: "a hash that is not the hash of the recorded bytes",
        tamper: (a: UserSubmissionAttempt) => ({
          ...a,
          txHash: `${a.txHash.slice(0, -1)}${a.txHash.endsWith("0") ? "1" : "0"}`,
        }),
        problem: /txHash is not the hash of signedTxCbor/,
      },
      {
        name: "a TTL that is not the recorded body's",
        tamper: (a: UserSubmissionAttempt) => ({
          ...a,
          ttlSlot: a.ttlSlot + 1,
        }),
        problem: /ttlSlot is not the TTL of signedTxCbor/,
      },
      {
        name: "an admission whose nonce it does not spend",
        tamper: (a: UserSubmissionAttempt) => ({
          ...a,
          nonce: { txHash: "cd".repeat(32), outputIndex: 0 },
        }),
        problem: /the admission does not spend its nonce/,
      },
    ])(
      "refuses a recorded attempt with $name, without building or broadcasting",
      async ({ tamper, problem }) => {
        const tampered = tamper(recorded);
        const run = await submit(scene, "Deposit", user, {
          latest: async () => tampered,
          append: async () => {
            throw new Error("nothing may be appended");
          },
        });

        const refused = refusal(run);
        expect(refused.message).toMatch(/Recorded attempt 1 is inconsistent/);
        expect(refused.message).toMatch(problem);
        expect(run.lines).toEqual([]);
      },
      CASE_TIMEOUT_MS,
    );
  });
});
