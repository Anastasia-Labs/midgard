/**
 * A user's deposit whose payload is over the list's inline limit: the run
 * publishes the payload first, then admits the event reading that output.
 */
import {
  loadUserHistoryBlueprint,
  setupUserHistoryScene,
  type UserHistoryScene,
} from "@al-ft/midgard-sdk/test-support/user-history";
import { beforeAll, describe, expect, it } from "vitest";

import {
  CASE_TIMEOUT_MS,
  crashOnce,
  landed,
  memoryStore,
  newUser,
  ordersFor,
  refusal,
  SCENE_TIMEOUT_MS,
  statusOf,
  submit,
} from "./submit-user-event.harness.js";

const blueprint = loadUserHistoryBlueprint();

describe("a user submission whose payload is published before admission", () => {
  let scene: UserHistoryScene;
  beforeAll(async () => {
    scene = await setupUserHistoryScene({
      blueprint,
      bounds: {
        inlineLimitBytes: 1n,
        maxPayloadBytes: 5000n,
        maxPayloadNodes: 512n,
      },
    });
  }, SCENE_TIMEOUT_MS);

  it(
    "publishes the data, then admits the event reading that exact output",
    async () => {
      const user = await newUser(scene);
      const { attempts, store } = memoryStore();

      const outcome = landed(await submit(scene, "Deposit", user, store));

      expect(attempts.map((a) => a.phase)).toEqual([
        "Publication",
        "Admission",
      ]);
      expect(attempts[1]!.publication).toEqual({
        txHash: attempts[0]!.txHash,
        outputIndex: 0,
      });
      expect(outcome.publicationTxHash).toBe(attempts[0]!.txHash);
      expect(outcome.admissionTxHash).toBe(attempts[1]!.txHash);
      expect(outcome.attempts).toBe(2);
      expect(await statusOf(scene, attempts[0]!.txHash)).toBe("confirmed");
      expect(await ordersFor(scene, "Deposit", outcome.nonce)).toBe(1);
    },
    CASE_TIMEOUT_MS,
  );

  it(
    "republishes when the recorded publication expires unseen",
    async () => {
      const user = await newUser(scene);
      const { attempts, store } = memoryStore();
      await submit(scene, "Deposit", user, crashOnce(store, "after-create"));
      expect(attempts.map((a) => a.phase)).toEqual(["Publication"]);
      scene.emulator.awaitSlot(attempts[0]!.ttlSlot - scene.emulator.slot + 1);

      const outcome = landed(await submit(scene, "Deposit", user, store));

      expect(attempts.map((a) => a.phase)).toEqual([
        "Publication",
        "Publication",
        "Admission",
      ]);
      expect(await statusOf(scene, attempts[0]!.txHash)).toBe("not_found");
      expect(outcome.publicationTxHash).toBe(attempts[1]!.txHash);
      expect(attempts[2]!.nonce).toEqual(attempts[0]!.nonce);
      expect(await ordersFor(scene, "Deposit", outcome.nonce)).toBe(1);
    },
    CASE_TIMEOUT_MS,
  );

  it(
    "after a crash between the publication's confirmation and the admission's record, the rerun admits against the confirmed publication",
    async () => {
      const user = await newUser(scene);
      const { attempts, store } = memoryStore();
      const crashed = await submit(
        scene,
        "Deposit",
        user,
        crashOnce(store, "before-create", (a) => a.phase === "Admission"),
      );
      expect(refusal(crashed).message).toMatch(/simulated crash/);
      expect(attempts.map((a) => a.phase)).toEqual(["Publication"]);
      expect(await statusOf(scene, attempts[0]!.txHash)).toBe("confirmed");

      const outcome = landed(await submit(scene, "Deposit", user, store));

      expect(attempts.map((a) => a.phase)).toEqual([
        "Publication",
        "Admission",
      ]);
      expect(outcome.publicationTxHash).toBe(attempts[0]!.txHash);
      expect(attempts[1]!.publication).toEqual({
        txHash: attempts[0]!.txHash,
        outputIndex: 0,
      });
      expect(await ordersFor(scene, "Deposit", outcome.nonce)).toBe(1);
    },
    CASE_TIMEOUT_MS,
  );
});
