import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import { pendingHistoryLedgerDisposition } from "../src/database/eventHistoryLedgerRepair.js";
import {
  applyHistoryRecoveryPlan,
  type HistoryRecoveryPlan,
  prepareHistoryRecoveryPlan,
  SIGNED_HEADER_RECOVERY_DOMAIN,
} from "../src/database/eventHistoryRecoveryPlans.js";
import {
  append,
  document,
  hash,
  intent,
  probes,
  read,
  refusal,
  repair,
  rows,
  run,
  sha,
  start,
} from "./event-history-recovery-plans.registration.js";

describe("durable history recovery plans (modeled source, real SQL)", () => {
  it("accepts the actual initial authority generation zero", async () => {
    const { token, checkpoint } = await start(true);
    expect(token.generation).toBe("0");
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          intent(),
          hash(30),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    expect((await rows())[0]!.row.owner_generation).toBe(0);
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
    );
    expect(await probes()).toEqual([{ label: "repaired" }]);
  });
  it("persists exact immutable signed intent and native operation before repair", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    expect(plan).toEqual({
      domain: SIGNED_HEADER_RECOVERY_DOMAIN,
      recoveryId: sha(document(value)),
      intent: value,
      evidenceDigest: hash(30),
      checkpointRevision: checkpoint.revision,
      state: "prepared",
      native: {
        recoveryId: sha(document(value)),
        expectedRoot: value.expectedRoot,
        targetRoot: value.targetRoot,
      },
    });
    expect(Object.isFrozen(plan)).toBe(true);
    expect(Object.isFrozen(plan.intent)).toBe(true);
    expect(Object.isFrozen(plan.native)).toBe(true);
    const stored = await rows();
    expect(stored).toHaveLength(1);
    expect(stored[0]!.row).toMatchObject({
      intent: document(value),
      state: "prepared",
      owner_generation: Number(token.generation),
      checkpoint_revision: Number(checkpoint.revision),
      recovery_id: `\\x${plan.recoveryId}`,
      evidence_digest: `\\x${hash(30)}`,
    });
    expect(await probes()).toEqual([]);
  });
  it("retains one recovery ID across actual journal append and refreshed canonical evidence", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const first = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    const next = await append(token, checkpoint);
    const refreshed = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          next,
          value,
          hash(31),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    expect(refreshed.recoveryId).toBe(first.recoveryId);
    expect(refreshed.native).toEqual(first.native);
    expect(refreshed.checkpointRevision).not.toBe(first.checkpointRevision);
    expect((await rows())[0]!.row).toMatchObject({
      head_hash: `\\x${next.head.id}`,
      snapshot_digest: `\\x${next.capture.snapshotDigest}`,
      evidence_digest: `\\x${hash(31)}`,
    });
    expect(await rows()).toHaveLength(1);
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(next, refreshed, repair()),
      ),
    );
    expect(await probes()).toEqual([{ label: "repaired" }]);
  });
  it.each([
    "headerHash",
    "expectedRoot",
    "targetRoot",
    "journalDigest",
    "signedTransactionHash",
    "signedTransactionCborSha256",
  ] as const)(
    "rejects conflicting outstanding %s without replacing intent",
    async (field) => {
      const { token, checkpoint } = await start();
      const value = intent();
      await run(
        Authority.withRecovery(
          token,
          prepareHistoryRecoveryPlan(
            checkpoint,
            value,
            hash(30),
            SIGNED_HEADER_RECOVERY_DOMAIN,
          ),
        ),
      );
      const before = await rows();
      const changed = {
        ...value,
        [field]: field === "headerHash" ? "cd".repeat(28) : hash(999),
      };
      await refusal(
        Authority.withRecovery(
          token,
          prepareHistoryRecoveryPlan(
            checkpoint,
            changed,
            hash(30),
            SIGNED_HEADER_RECOVERY_DOMAIN,
          ),
        ),
        "A different durable native recovery must be resolved first",
      );
      expect(await rows()).toEqual(before);
      expect(await probes()).toEqual([]);
    },
  );
  it("keeps a prepared recovery pending even when unchanged origins are canonical", async () => {
    const { token, checkpoint } = await start();
    const change = {
      kind: "resume" as const,
      before: checkpoint,
      after: checkpoint,
    };
    expect(
      await run(
        Authority.withRecovery(token, pendingHistoryLedgerDisposition(change)),
      ),
    ).toBeUndefined();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          intent(),
          hash(30),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    expect(
      await run(
        Authority.withRecovery(token, pendingHistoryLedgerDisposition(change)),
      ),
    ).toEqual({
      status: "pending",
      reason:
        "A durable native/SQL recovery operation requires current-branch disposition",
    });
    expect(await read()).toEqual(checkpoint);
    expect(await probes()).toEqual([]);
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
    );
    expect(
      await run(
        Authority.withRecovery(token, pendingHistoryLedgerDisposition(change)),
      ),
    ).toBeUndefined();
    expect(await probes()).toEqual([{ label: "repaired" }]);
    expect(await read()).toEqual(checkpoint);
  });

  it("does not execute an applied repair twice, including after evidence refresh", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
    );
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair("duplicate")),
      ),
    );
    const next = await append(token, checkpoint);
    const refreshed = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          next,
          value,
          hash(31),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    expect(refreshed.state).toBe("applied");
    expect(refreshed.recoveryId).toBe(plan.recoveryId);
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(
          next,
          refreshed,
          repair("duplicate-after-refresh"),
        ),
      ),
    );
    expect(await probes()).toEqual([{ label: "repaired" }]);
    expect(await rows()).toHaveLength(1);
  });
  it("rejects replaced evidence at the same checkpoint", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const stale = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(31),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, stale, repair()),
      ),
      "Recovery application evidence changed",
    );
    expect(await rows()).toEqual(before);
    expect(await probes()).toEqual([]);
  });
  it("rejects stale checkpoints for prepare and apply after an actual append", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    await append(token, checkpoint);
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(31),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
      "Recovery plan checkpoint changed",
    );
    await refusal(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
      "Recovery plan checkpoint changed",
    );
    expect(await rows()).toEqual(before);
    expect(await probes()).toEqual([]);
  });
  it("requires newly validated evidence after owner generation revocation", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    const fresh = await run(
      Authority.beginRecovery(token, "Restart modeled owner"),
    );
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
      "History authority generation or owner changed",
    );
    await refusal(
      Authority.withRecovery(
        fresh,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
      "Recovery application evidence changed",
    );
    expect(await rows()).toEqual(before);
    expect(await probes()).toEqual([]);
    const renewed = await run(
      Authority.withRecovery(
        fresh,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(31),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    expect(renewed.recoveryId).toBe(plan.recoveryId);
    await run(
      Authority.withRecovery(
        fresh,
        applyHistoryRecoveryPlan(checkpoint, renewed, repair()),
      ),
    );
    expect(await probes()).toEqual([{ label: "repaired" }]);
  });
  it("refuses no authority, ordinary SQL transactions and Ready-only authority", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    const before = await rows();
    const message = "History journal requires an owned recovery transaction";
    await refusal(
      prepareHistoryRecoveryPlan(
        checkpoint,
        value,
        hash(31),
        SIGNED_HEADER_RECOVERY_DOMAIN,
      ),
      message,
    );
    await refusal(
      applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      message,
    );
    await refusal(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql.withTransaction(
          prepareHistoryRecoveryPlan(
            checkpoint,
            value,
            hash(31),
            SIGNED_HEADER_RECOVERY_DOMAIN,
          ),
        );
      }),
      message,
    );
    await run(
      Authority.publishReady(token, {
        point: checkpoint.head,
        snapshotDigest: checkpoint.capture.snapshotDigest,
      }),
    );
    await refusal(
      Authority.withReady(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(31),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
      message,
    );
    await refusal(
      Authority.withReady(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
      message,
    );
    expect(await rows()).toEqual(before);
    expect(await probes()).toEqual([]);
  });
  it("rolls back repair writes and retains prepared intent on failure, then retries exactly once", async () => {
    const { token, checkpoint } = await start();
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          intent(),
          hash(30),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(
          checkpoint,
          plan,
          repair().pipe(
            Effect.andThen(Effect.fail(new Error("modeled repair failed"))),
          ),
        ),
      ),
      "modeled repair failed",
    );
    expect(await probes()).toEqual([]);
    expect(await rows()).toEqual(before);
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, plan, repair()),
      ),
    );
    expect(await probes()).toEqual([{ label: "repaired" }]);
    expect((await rows())[0]!.row.state).toBe("applied");
  });
  it("snapshots caller intent before the first asynchronous authority read", async () => {
    const { token, checkpoint } = await start();
    const original = intent();
    const mutable = { ...original };
    Object.defineProperty(mutable, "bindingDigest", {
      enumerable: true,
      get() {
        queueMicrotask(() => {
          mutable.targetRoot = hash(999);
        });
        return original.bindingDigest;
      },
    });
    const plan = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          mutable,
          hash(30),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    expect(mutable.targetRoot).toBe(hash(999));
    expect(plan.intent).toEqual(original);
    expect(plan.recoveryId).toBe(sha(document(original)));
    expect((await rows())[0]!.row.intent).toBe(document(original));
  });
  it.each(["intent", "native", "checkpointRevision"] as const)(
    "refuses altered returned plan %s",
    async (field) => {
      const { token, checkpoint } = await start();
      const plan = await run(
        Authority.withRecovery(
          token,
          prepareHistoryRecoveryPlan(
            checkpoint,
            intent(),
            hash(30),
            SIGNED_HEADER_RECOVERY_DOMAIN,
          ),
        ),
      );
      const before = await rows();
      const altered: HistoryRecoveryPlan =
        field === "intent"
          ? { ...plan, intent: { ...plan.intent, targetRoot: hash(999) } }
          : field === "native"
            ? { ...plan, native: { ...plan.native, targetRoot: hash(999) } }
            : { ...plan, checkpointRevision: "999" };
      await refusal(
        Authority.withRecovery(
          token,
          applyHistoryRecoveryPlan(checkpoint, altered, repair()),
        ),
        "Recovery application immutable identity changed",
      );
      expect(await rows()).toEqual(before);
      expect(await probes()).toEqual([]);
    },
  );
});
