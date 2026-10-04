import "./event-history-recovery-plans.durable-history-recovery-plans-modeled-source-real-sql.js";

import { describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import {
  applyHistoryRecoveryPlan,
  discardPreparedHistoryRecoveryPlan,
  type HistoryRecoveryDomain,
  prepareHistoryRecoveryPlan,
  prepareRetainedNativeHistoryRecoveryPlan,
  retainedPreparedRecoveryPlan,
  SIGNED_HEADER_RECOVERY_DOMAIN,
  SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
} from "../src/database/eventHistoryRecoveryPlans.js";
import {
  append,
  binding,
  document,
  hash,
  intent,
  probes,
  refusal,
  repair,
  rows,
  run,
  sha,
  start,
} from "./event-history-recovery-plans.registration.js";
import { retainedIntent } from "./event-history-recovery-plans.retained-intent.js";
describe("retained native recovery root selection (real SQL component)", () => {
  it.each(["unpromoted", "promoted"] as const)(
    "records %s baseline selection before repair",
    async (state) => {
      const { token, checkpoint } = await start();
      const { value, candidateRoot } = retainedIntent();
      const durableRoot =
        state === "unpromoted" ? value.targetRoot : candidateRoot;
      const plan = await run(
        Authority.withRecovery(
          token,
          prepareRetainedNativeHistoryRecoveryPlan(
            checkpoint,
            value,
            hash(30),
            { durableRoot, candidateRoot },
            SIGNED_HEADER_RECOVERY_DOMAIN,
          ),
        ),
      );
      const expectedIntent = { ...value, expectedRoot: durableRoot };
      expect(plan.intent).toEqual(expectedIntent);
      expect(plan.recoveryId).toBe(sha(document(expectedIntent)));
      expect(plan.native).toEqual({
        recoveryId: plan.recoveryId,
        expectedRoot: durableRoot,
        targetRoot: value.targetRoot,
      });
      const stored = await rows();
      expect(stored).toHaveLength(1);
      expect(stored[0]!.row.intent).toBe(document(expectedIntent));
      expect(stored[0]!.row.state).toBe("prepared");
      expect(await probes()).toEqual([]);
      await run(
        Authority.withRecovery(
          token,
          applyHistoryRecoveryPlan(checkpoint, plan, repair()),
        ),
      );
      expect(await probes()).toEqual([{ label: "repaired" }]);
    },
  );
  it("preserves the original operation after modeled native CAS, checkpoint append and actual owner restart", async () => {
    const { token, checkpoint } = await start();
    const { value, candidateRoot } = retainedIntent();
    const first = await run(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          {
            durableRoot: candidateRoot,
            candidateRoot,
          },
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    const afterNative = await run(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(31),
          {
            durableRoot: value.targetRoot,
            candidateRoot,
          },
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    expect(afterNative.recoveryId).toBe(first.recoveryId);
    expect(afterNative.native).toEqual(first.native);
    expect(afterNative.intent.expectedRoot).toBe(candidateRoot);
    const next = await append(token, checkpoint);
    const replacement = await run(
      Authority.acquire({
        deploymentIdentity: binding.manifestId,
        ownerToken: token.ownerToken,
        leaseDurationMs: 60_000,
      }),
    );
    expect(BigInt(replacement.generation)).toBeGreaterThan(
      BigInt(token.generation),
    );
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(
          next,
          value,
          hash(32),
          {
            durableRoot: value.targetRoot,
            candidateRoot,
          },
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
      "History authority generation or owner changed",
    );
    expect(await rows()).toEqual(before);
    const resumed = await run(
      Authority.withRecovery(
        replacement,
        prepareRetainedNativeHistoryRecoveryPlan(
          next,
          value,
          hash(32),
          {
            durableRoot: value.targetRoot,
            candidateRoot,
          },
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    expect(resumed.recoveryId).toBe(first.recoveryId);
    expect(resumed.intent).toEqual(first.intent);
    expect(resumed.native).toEqual(first.native);
    expect(resumed.checkpointRevision).toBe(next.revision);
    expect(resumed.evidenceDigest).toBe(hash(32));
    expect(resumed.state).toBe("prepared");
    const refreshed = await rows();
    expect(refreshed).toHaveLength(1);
    expect(refreshed[0]!.row).toMatchObject({
      intent: document(first.intent),
      owner_generation: Number(replacement.generation),
      checkpoint_revision: Number(next.revision),
      head_hash: `\\x${next.head.id}`,
      snapshot_digest: `\\x${next.capture.snapshotDigest}`,
      evidence_digest: `\\x${hash(32)}`,
    });
    expect(await probes()).toEqual([]);
    await run(
      Authority.withRecovery(
        replacement,
        applyHistoryRecoveryPlan(next, resumed, repair()),
      ),
    );
    await run(
      Authority.withRecovery(
        replacement,
        applyHistoryRecoveryPlan(next, resumed, repair("duplicate")),
      ),
    );
    expect(await probes()).toEqual([{ label: "repaired" }]);
  });

  it.each([false, true])(
    "refuses a third durable root (prepared=%s) without SQL changes",
    async (prepared) => {
      const { token, checkpoint } = await start();
      const { value, candidateRoot } = retainedIntent();
      if (prepared)
        await run(
          Authority.withRecovery(
            token,
            prepareRetainedNativeHistoryRecoveryPlan(
              checkpoint,
              value,
              hash(30),
              { durableRoot: candidateRoot, candidateRoot },
              SIGNED_HEADER_RECOVERY_DOMAIN,
            ),
          ),
        );
      const before = await rows();
      await refusal(
        Authority.withRecovery(
          token,
          prepareRetainedNativeHistoryRecoveryPlan(
            checkpoint,
            value,
            hash(31),
            { durableRoot: hash(999), candidateRoot },
            SIGNED_HEADER_RECOVERY_DOMAIN,
          ),
        ),
        "Native recovery root is outside the authenticated journal",
      );
      expect(await rows()).toEqual(before);
      expect(await probes()).toEqual([]);
    },
  );

  it.each([
    "headerHash",
    "signedTransactionHash",
    "signedTransactionCborSha256",
    "journalDigest",
    "targetRoot",
  ] as const)(
    "refuses changed retained %s after native restoration",
    async (field) => {
      const { token, checkpoint } = await start();
      const { value, candidateRoot } = retainedIntent();
      await run(
        Authority.withRecovery(
          token,
          prepareRetainedNativeHistoryRecoveryPlan(
            checkpoint,
            value,
            hash(30),
            { durableRoot: candidateRoot, candidateRoot },
            SIGNED_HEADER_RECOVERY_DOMAIN,
          ),
        ),
      );
      const before = await rows();
      const changed = {
        ...value,
        [field]: field === "headerHash" ? "cd".repeat(28) : hash(999),
      };
      // The root is individually allowed by the newly supplied target/candidate;
      // only the retained immutable operation identity should reject this retry.
      await refusal(
        Authority.withRecovery(
          token,
          prepareRetainedNativeHistoryRecoveryPlan(
            checkpoint,
            changed,
            hash(31),
            { durableRoot: changed.targetRoot, candidateRoot },
            SIGNED_HEADER_RECOVERY_DOMAIN,
          ),
        ),
        "Retained native recovery requires a different disposition",
      );
      expect(await rows()).toEqual(before);
      expect(await probes()).toEqual([]);
    },
  );

  it("refuses a different replay candidate after the original CAS reached its target", async () => {
    const { token, checkpoint } = await start();
    const { value, candidateRoot } = retainedIntent();
    await run(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          {
            durableRoot: candidateRoot,
            candidateRoot,
          },
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(31),
          {
            durableRoot: value.targetRoot,
            candidateRoot: hash(999),
          },
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
      "Retained native recovery requires a different disposition",
    );
    expect(await rows()).toEqual(before);
    expect(await probes()).toEqual([]);
  });

  it("does not turn a prepared no-op restoration into a candidate-root undo", async () => {
    const { token, checkpoint } = await start();
    const { value, candidateRoot } = retainedIntent();
    const first = await run(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          {
            durableRoot: value.targetRoot,
            candidateRoot,
          },
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
    );
    expect(first.native.expectedRoot).toBe(value.targetRoot);
    expect(first.native.targetRoot).toBe(value.targetRoot);
    const before = await rows();
    await refusal(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(31),
          {
            durableRoot: candidateRoot,
            candidateRoot,
          },
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
      "Retained native recovery requires a different disposition",
    );
    expect(await rows()).toEqual(before);
    expect(await probes()).toEqual([]);
  });

  it("requires owned recovery and an exact current checkpoint before selecting roots", async () => {
    const { token, checkpoint } = await start();
    const { value, candidateRoot } = retainedIntent();
    const observed = { durableRoot: candidateRoot, candidateRoot };
    await refusal(
      prepareRetainedNativeHistoryRecoveryPlan(
        checkpoint,
        value,
        hash(30),
        observed,
        SIGNED_HEADER_RECOVERY_DOMAIN,
      ),
      "History journal requires an owned recovery transaction",
    );
    await append(token, checkpoint);
    await refusal(
      Authority.withRecovery(
        token,
        prepareRetainedNativeHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          observed,
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
      "Recovery plan checkpoint changed",
    );
    expect(await rows()).toEqual([]);
    expect(await probes()).toEqual([]);
  });
  it("keeps a signed-intent release and a signed-header recovery of one header as distinct operations", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const header = await run(
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
    expect(await run(retainedPreparedRecoveryPlan(binding.digest))).toEqual({
      kind: "signed_header",
      headerHash: value.headerHash,
      expectedRoot: value.expectedRoot,
      journalDigest: value.journalDigest,
    });
    // The identical intent under the release's domain is another operation:
    // it never adopts the retained signed-header plan as its own.
    await refusal(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
        ),
      ),
      "A different durable native recovery must be resolved first",
    );
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, header, repair("header")),
      ),
    );
    const release = await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
        ),
      ),
    );
    expect(release.recoveryId).not.toBe(header.recoveryId);
    expect(release.state).toBe("prepared");
    expect(await run(retainedPreparedRecoveryPlan(binding.digest))).toEqual({
      kind: "signed_intent_release",
      headerHash: value.headerHash,
      expectedRoot: value.expectedRoot,
      journalDigest: value.journalDigest,
    });
    await run(
      Authority.withRecovery(
        token,
        applyHistoryRecoveryPlan(checkpoint, release, repair("release")),
      ),
    );
    expect(await probes()).toEqual([{ label: "header" }, { label: "release" }]);
  });
  it("discards only the single prepared release plan of its own header, which meanwhile blocks a signed-header recovery", async () => {
    const { token, checkpoint } = await start();
    const value = intent();
    const discard = (domain: HistoryRecoveryDomain, headerHash: string) =>
      Authority.withRecovery(
        token,
        discardPreparedHistoryRecoveryPlan(checkpoint, domain, headerHash),
      );
    // Nothing is prepared. (Kills "drop the single-plan refusal": the
    // discard then refuses for another reason.)
    await refusal(
      discard(SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN, value.headerHash),
      "No single prepared native recovery to discard",
    );
    await run(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN,
        ),
      ),
    );
    // The retained release blocks a signed-header recovery of the same
    // header. (Kills "drop the conflicting-plan refusal".)
    await refusal(
      Authority.withRecovery(
        token,
        prepareHistoryRecoveryPlan(
          checkpoint,
          value,
          hash(30),
          SIGNED_HEADER_RECOVERY_DOMAIN,
        ),
      ),
      "A different durable native recovery must be resolved first",
    );
    // Neither a signed-header discard of that header nor a release discard of
    // Another header cannot delete it: its native CAS may have run.
    await refusal(
      discard(SIGNED_HEADER_RECOVERY_DOMAIN, value.headerHash),
      "The prepared native recovery is not the operation to discard",
    );
    await refusal(
      discard(SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN, "cd".repeat(28)),
      "The prepared native recovery is not the operation to discard",
    );
    expect(await run(retainedPreparedRecoveryPlan(binding.digest))).toEqual({
      kind: "signed_intent_release",
      headerHash: value.headerHash,
      expectedRoot: value.expectedRoot,
      journalDigest: value.journalDigest,
    });
    await run(discard(SIGNED_INTENT_RELEASE_RECOVERY_DOMAIN, value.headerHash));
    expect(await rows()).toEqual([]);
    expect(await probes()).toEqual([]);
  });
});
