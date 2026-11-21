/** Another prover convicts a withdrawal-mistag header and removes it while
 * a computation thread of ours still stands on it. Our run, over the real
 * emulator chain, cancels that thread back to our wallet (the thread's
 * lovelace less the cancel's fee), and once the cancel has landed ends
 * resolved by the other record: no conviction of ours, no terminal. A
 * restart between the cancel's submission and its confirmation finds the
 * cancel in the mempool: exactly one lands and the thread's lovelace comes
 * back once. */
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { observeEmulatorSubmissions } from "./support/cross-block-raw-emulator.js";
import {
  expectResolvedWithReclaim,
  walletLovelace,
} from "./support/own-thread-reclaim-expectations.js";
import {
  FINAL_DEPTH,
  openOurThread,
  scene,
  theyConvictAndRemove,
} from "./support/withdrawal-mistag-own-thread-reclaim-scene.js";

describe("withdrawal-mistag: our thread on a header another prover's record removed", () => {
  it.each([{ restart: false }, { restart: true }])(
    "cancels our thread to our wallet, then resolves by the other record (restart between submit and confirm: $restart)",
    async ({ restart }) => {
      const s = await scene();
      const submissions: { txHash: string; cbor: string }[] = [];
      const observer = observeEmulatorSubmissions((txHash, cbor) =>
        submissions.push({ txHash, cbor }),
      );
      try {
        let ours = await s.runner({
          lucid: s.h.proverLucid,
          signer: s.h.proverSigner,
          reclaiming: true,
        });
        const theirs = await s.runner({
          lucid: s.theirLucid,
          signer: s.theirSigner,
          reclaiming: false,
        });
        const thread = await openOurThread(s, ours);
        await theyConvictAndRemove(s, theirs);
        if (restart) {
          // A spare plain-ADA coin, so a fresh process can fund a cancel of
          // its own while the first one still waits to land.
          const spare = await s.h.funderLucid
            .newTx()
            .pay.ToAddress(s.h.proverSigner.address, {
              lovelace: 50_000_000n,
            })
            .complete({ localUPLCEval: true });
          await (await spare.sign.withWallet().complete()).submit();
        }
        const submitted = submissions.length;

        // The removal is still within rollback recovery: our run follows
        // what it left in flight (one action per run), then holds our thread
        // where it stands and sends nothing.
        const awaitingRemoval = `header removal awaits release finality before our thread ${thread.threadOutRef} is reclaimed`;
        let result = await ours.run();
        for (
          let pass = 0;
          pass < 4 && !JSON.stringify(result).includes(awaitingRemoval);
          pass += 1
        )
          result = await ours.run();
        expect(result).toMatchObject({
          kind: "pending",
          reason: expect.stringContaining(awaitingRemoval),
        });
        expect(submissions.slice(submitted)).toEqual([]);

        // Past rollback recovery, the run builds our thread's cancel.
        s.h.emulator.awaitBlock(FINAL_DEPTH);
        const before = await walletLovelace(s.h.proverLucid);
        result = await ours.run();
        for (
          let pass = 0;
          pass < 4 &&
          submissions.length === submitted &&
          result.kind === "pending";
          pass += 1
        ) {
          s.h.emulator.awaitBlock();
          result = await ours.run();
        }
        expect(result.kind).toBe("pending");
        expect(submissions.slice(submitted)).toHaveLength(1);
        if (restart) {
          // A fresh process before the cancel lands finds it in the
          // mempool and builds nothing of its own.
          ours = await s.runner({
            lucid: s.h.proverLucid,
            signer: s.h.proverSigner,
            reclaiming: true,
          });
          expect((await ours.run()).kind).toBe("pending");
        }
        s.h.emulator.awaitBlock();
        // Exactly one transaction spending our thread is on chain.
        const spendsThread = ({ cbor }: { readonly cbor: string }) => {
          const inputs = CML.Transaction.from_cbor_hex(cbor).body().inputs();
          return Array.from(
            { length: inputs.len() },
            (_, index) =>
              `${inputs.get(index).transaction_id().to_hex()}#${inputs.get(index).index().toString()}`,
          ).includes(thread.threadOutRef);
        };
        const landed: { txHash: string; cbor: string }[] = [];
        for (const cancel of submissions.slice(submitted).filter(spendsThread))
          if (
            (await s.h.emulator.getTransactionStatus(cancel.txHash)).status ===
            "confirmed"
          )
            landed.push(cancel);
        expect(landed).toHaveLength(1);
        const reclaim = landed[0]!;
        const fee = CML.Transaction.from_cbor_hex(reclaim.cbor).body().fee();
        // Landed: our wallet holds the thread's lovelace, once, less the fee.
        expect(await walletLovelace(s.h.proverLucid)).toBe(
          before + BigInt(thread.lovelace) - fee,
        );
        expect(
          (await ours.l1.observeOwnThreads!({ headerHash: s.headerHash }))
            .standing,
        ).toBeUndefined();
        // Landed: the run resolves, restarted or not (a rollback makes the
        // thread live again for the own-thread sweep).
        result = await ours.run();
        for (let pass = 0; pass < 2 && result.kind === "pending"; pass += 1)
          result = await ours.run();
        expectResolvedWithReclaim(result, thread.threadOutRef, reclaim.txHash);
        if (restart) return;
        expect(submissions.slice(submitted)).toHaveLength(1);
      } finally {
        observer.restore();
        s.restore();
      }
    },
    900_000,
  );
});
