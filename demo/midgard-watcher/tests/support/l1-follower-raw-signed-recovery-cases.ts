import {
  FraudProofL1UnavailableError,
  type FraudProofRawL1Point,
  fraudProofSignedTransactionRecovery,
} from "@al-ft/midgard-fault-proofs";
import type { SimTx } from "@al-ft/midgard-l1-follower/testing";
import { describe, expect, it } from "vitest";

import {
  C,
  expected,
  harness,
  K,
  SEED,
  T,
  X,
} from "./l1-follower-raw-reads-fixture.js";
import {
  FOREIGN_WITNESS,
  nodeDouble,
  RELEASE,
  signedOf,
  sourceOver,
} from "./l1-follower-raw-source-fixture.js";

/**
 * Signed-intent recovery over the follower (ticket W1), registered by
 * `fault-proof-l1-source.test.ts`. Funding transaction A (b1) pays T#0,
 * T#1, X#2 (untracked) and C#3; each case signs an intent over A's outputs
 * and reads it back through `fraudProofSignedTransactionRecovery`, whose
 * signed-byte check runs first. The recovery depth is k + 2 = 5.
 */

const REASON = {
  included: "Exact recorded transaction body is on the canonical chain",
  otherWitnesses:
    "A transaction with the recorded id but other witness bytes is on the canonical chain",
  expired:
    "Recorded TTL passed beyond the canonical recovery horizon and the exact transaction is absent",
  expiredAtTip:
    "Recorded TTL passed at the canonical tip and the exact transaction is absent",
  rebroadcast:
    "Canonical transaction absent and every recorded input remains unspent",
  unknown: "A recorded input lacks exact canonical creation history",
  unspentUnknown:
    "A recorded input is outside the followed outputs and has no recorded spend",
  invalidated:
    "A recorded input is stably spent by another canonical transaction",
  invalidatedAtTip:
    "A recorded input is spent at the canonical tip by another transaction",
  validFrom: "Recorded lower validity bound has not reached the canonical tip",
  mempool: "Recorded transaction remains in the node mempool",
} as const;

/** A in b1, then `above` empty blocks (default: b1 is recovery-deep). */
const setup = async (above = 4) => {
  const h = await harness();
  const A: SimTx = {
    inputs: [h.chain.outsideInput()],
    outputs: [
      { address: T, lovelace: 2_000_000n },
      { address: T, lovelace: 3_000_000n },
      { address: X, lovelace: 1_000_000n },
      { address: C, lovelace: 4_000_000n },
    ],
    nonce: h.chain.nonce(),
  };
  const { hashes } = await h.forward([A]);
  const a = hashes[0]!;
  for (let i = 0; i < above; i += 1) await h.forward([]);
  const at = (index: number) => ({ txHash: Buffer.from(a, "hex"), index });
  const input = (index: number) => ({
    outRef: `${a}#${index.toString()}`,
    outputCbor: expected(A, a, index).outputCbor,
  });
  const double = nodeDouble();
  const recovery = fraudProofSignedTransactionRecovery(
    sourceOver(h, h.reads(), double.node),
    RELEASE,
  );
  const intent = (extra: Partial<SimTx> = {}): SimTx => ({
    inputs: [at(0)],
    outputs: [{ address: T, lovelace: 1_500_000n }],
    nonce: h.chain.nonce(),
    ...extra,
  });
  const observe = (tx: SimTx, witnessSet?: Buffer) =>
    recovery.observeSignedTransaction(signedOf(tx, witnessSet));
  const empty = async (count: number): Promise<FraudProofRawL1Point> => {
    for (let i = 0; i < count; i += 1) await h.forward([]);
    return h.tipPoint();
  };
  return { ...h, a, at, input, double, recovery, intent, observe, empty };
};

export const describeSignedRecovery = (): void => {
  describe("signed-intent recovery over the follower", () => {
    it("rebroadcast: absent, every input live; the mempool holds it pending", async () => {
      const s = await setup();
      const z = s.intent({ referenceInputs: [s.at(3)] });
      const id = signedOf(z).transactionHash;
      const observed = await s.observe(z);
      expect(observed).toEqual({
        ...signedOf(z),
        status: "rebroadcast",
        reason: REASON.rebroadcast,
        canonicalPoint: s.tipPoint(),
        releaseFinalPoint: s.tipPoint(),
        inputs: [s.input(0), s.input(3)],
      });
      expect(s.double.calls).toEqual([`hasTx:${id}`]);
      s.double.state.mempool.add(id);
      expect(await s.observe(z)).toMatchObject({
        status: "pending",
        reason: REASON.mempool,
      });
      s.double.state.hasTx = async () => {
        throw new Error("socket closed");
      };
      await expect(s.observe(z)).rejects.toBeInstanceOf(
        FraudProofL1UnavailableError,
      );
    });

    it("included only with the exact signed bytes; other witnesses are unknown", async () => {
      const s = await setup();
      const z = s.intent();
      const { point } = await s.forward([z]);
      expect(await s.observe(z)).toEqual({
        ...signedOf(z),
        status: "included",
        reason: REASON.included,
        inclusionPoint: point,
        canonicalPoint: point,
        releaseFinalPoint: point,
        inputs: [],
      });
      const other = await s.observe(z, FOREIGN_WITNESS);
      expect(other).toMatchObject({
        status: "unknown",
        reason: REASON.otherWitnesses,
      });
      expect(other.inclusionPoint).toBeUndefined();
    });

    it("a phase-2-invalid landing is not an inclusion: its collateral spend invalidates", async () => {
      const s = await setup();
      const z = s.intent({ collaterals: [s.at(1)] });
      await s.forward([{ ...z, isValid: false }]);
      const observed = await s.observe(z);
      expect(observed).toMatchObject({
        status: "invalidated_at_tip",
        reason: REASON.invalidatedAtTip,
        inputs: [s.input(0), s.input(1)],
      });
      expect(observed.inclusionPoint).toBeUndefined();
    });

    it("a conflicting spend is invalidated_at_tip until recovery-deep, then invalidated", async () => {
      const s = await setup();
      const { point: spentAt } = await s.forward([s.intent()]);
      const z = s.intent();
      for (let depth = 1; depth < 5; depth += 1) {
        expect(await s.observe(z)).toMatchObject({
          status: "invalidated_at_tip",
          reason: REASON.invalidatedAtTip,
          inputs: [s.input(0)],
        });
        await s.empty(1);
      }
      expect(await s.observe(z)).toMatchObject({
        status: "invalidated",
        reason: REASON.invalidated,
        releaseFinalPoint: spentAt,
        inputs: [s.input(0)],
      });
    });

    it("TTL: rebroadcast before it, expired_at_tip past it, expired once recovery-deep", async () => {
      const s = await setup();
      const z = s.intent({ invalidAfter: Number(s.tipPoint().slot) + 1 });
      expect(await s.observe(z)).toMatchObject({ status: "rebroadcast" });
      const firstPast = await s.empty(1);
      for (let blocks = 0; blocks < 4; blocks += 1) {
        expect(await s.observe(z)).toMatchObject({
          status: "expired_at_tip",
          reason: REASON.expiredAtTip,
          inputs: [],
        });
        await s.empty(1);
      }
      expect(await s.observe(z)).toMatchObject({
        status: "expired",
        reason: REASON.expired,
        releaseFinalPoint: firstPast,
      });
    });

    it("a passed TTL on a chain not yet recovery-deep is unavailable", async () => {
      const s = await setup(0);
      const z = s.intent({ invalidAfter: Number(s.tipPoint().slot) });
      await expect(s.observe(z)).rejects.toBeInstanceOf(
        FraudProofL1UnavailableError,
      );
    });

    it("a spend the pruning removed is never stable; unpruned and deep, it is invalidated", async () => {
      const s = await setup();
      const { point: spentAt } = await s.forward([
        s.intent({ outputs: [{ address: X, lovelace: 1_500_000n }] }),
      ]);
      await s.empty(K + 2);
      const z = s.intent();
      expect(await s.observe(z)).toMatchObject({
        status: "invalidated",
        inputs: [s.input(0)],
      });
      await s.pruneAll();
      const pruned = await s.observe(z);
      expect(pruned).toMatchObject({
        status: "invalidated_at_tip",
        reason: REASON.invalidatedAtTip,
        inputs: [s.input(0)],
      });
      // The exact recovery-deep block was pruned: the boundary is a kept block below it.
      expect(Number(pruned.releaseFinalPoint.blockNo)).toBeLessThan(
        Number(s.tipPoint().blockNo) - 4,
      );
      expect(Number(pruned.releaseFinalPoint.blockNo)).toBeLessThanOrEqual(
        Number(spentAt.blockNo),
      );
    });

    it("unknown: no stored creation, a seed output, an untracked output with no recorded spend", async () => {
      const s = await setup();
      for (const [inputs, reason] of [
        [[s.chain.outsideInput()], REASON.unknown],
        [[SEED.outRef], REASON.unknown],
        [[s.at(2)], REASON.unspentUnknown],
      ] as const) {
        const observed = await s.observe(s.intent({ inputs: [...inputs] }));
        expect(observed).toMatchObject({ status: "unknown", reason });
        expect(observed.inputs).toEqual([]);
      }
      expect(s.double.calls).toEqual([]);
    });

    it("an untracked output a stored transaction spends is invalidated; unknown dominates a spend", async () => {
      const s = await setup();
      await s.forward([s.intent({ inputs: [s.at(2)] })]);
      expect(await s.observe(s.intent({ inputs: [s.at(2)] }))).toMatchObject({
        status: "invalidated_at_tip",
        inputs: [s.input(2)],
      });
      expect(
        await s.observe(
          s.intent({ inputs: [s.at(2), s.chain.outsideInput()] }),
        ),
      ).toMatchObject({ status: "unknown", reason: REASON.unknown });
    });

    it("validFrom: pending without asking the mempool until the tip reaches it", async () => {
      const s = await setup();
      const z = s.intent({ invalidBefore: Number(s.tipPoint().slot) + 3 });
      expect(await s.observe(z)).toMatchObject({
        status: "pending",
        reason: REASON.validFrom,
        inputs: [s.input(0)],
      });
      expect(s.double.calls).toEqual([]);
      await s.empty(3);
      expect(await s.observe(z)).toMatchObject({ status: "rebroadcast" });
    });

    it("a rollback during the mempool read restarts the observation at the new tip", async () => {
      const s = await setup();
      let rolled = false;
      s.double.state.hasTx = async () => {
        if (!rolled) {
          rolled = true;
          await s.backward(1);
          await s.empty(2);
        }
        return false;
      };
      const observed = await s.observe(s.intent());
      expect(observed).toMatchObject({
        status: "rebroadcast",
        canonicalPoint: s.tipPoint(),
      });
      expect(
        s.double.calls.filter((call) => call.startsWith("hasTx")),
      ).toHaveLength(2);
    });

    it("rebroadcast submits the exact bytes after the live authorization", async () => {
      const s = await setup();
      const signed = signedOf(s.intent());
      const authorize = async () => {
        s.double.calls.push("authorize");
      };
      expect(
        await s.recovery.rebroadcastSignedTransaction({
          ...signed,
          authorizeResubmission: authorize,
        }),
      ).toBe(signed.transactionHash);
      expect(s.double.calls).toEqual([
        "authorize",
        `submit:${signed.signedTransactionCborHex}`,
      ]);
      s.double.state.submit = async () => ({
        accepted: false,
        rejection: Uint8Array.of(0xde, 0xad),
      });
      await expect(
        s.recovery.rebroadcastSignedTransaction({
          ...signed,
          authorizeResubmission: authorize,
        }),
      ).rejects.toThrow(/rejected [0-9a-f]{64}: dead$/u);
      s.double.state.submit = async () => {
        throw new Error("socket closed");
      };
      await expect(
        s.recovery.rebroadcastSignedTransaction({
          ...signed,
          authorizeResubmission: authorize,
        }),
      ).rejects.toBeInstanceOf(FraudProofL1UnavailableError);
      s.double.calls.length = 0;
      await expect(
        s.recovery.rebroadcastSignedTransaction({
          ...signed,
          authorizeResubmission: async () => {
            throw new Error("authorization withdrawn");
          },
        }),
      ).rejects.toThrow("authorization withdrawn");
      expect(s.double.calls).toEqual([]);
    });
  });
};
