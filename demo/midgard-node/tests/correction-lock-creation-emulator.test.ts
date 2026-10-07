import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { assertAvailabilityRefusal } from "./helpers/availability-challenge-emulator.create-fixture.js";
import { lastAvailabilityEvaluationFailure } from "./helpers/availability-challenge-emulator.measure-availability-transaction.js";
import { correctionLockCreationFixture } from "./initialization-emulator.correction-lock.js";

describe("correction-lock creation emulator", () => {
  it.each(["standalone", "atomic"] as const)(
    "%s creator funds an actual Idle -> Locked correction with exact value conservation",
    async (path) => {
      const f = await correctionLockCreationFixture(path);
      const completed = await (
        await f.prune()
      ).complete({ localUPLCEval: true });
      const signed = await completed.sign.withWallet().complete();
      const hash = await signed.submit();
      await f.lucid.awaitTx(hash);
      const next = await Effect.runPromise(
        SDK.fetchCorrectionLockUTxOProgram(f.lucid, f.lockConfig),
      );
      expect(next.utxo.txHash).toBe(hash);
      expect(next.datum).toEqual(f.nextDatum);
      expect(next.utxo.assets).toEqual(f.lock.utxo.assets);
      expect(f.lock.utxo.assets.lovelace).toBeGreaterThan(f.idleMinimum);
      expect(f.lock.utxo.assets.lovelace).toBeGreaterThanOrEqual(
        f.lockedMinimum,
      );
      expect(await f.lucid.utxosByOutRef([f.lock.utxo])).toEqual([]);
    },
    120_000,
  );

  it.each(["standalone", "atomic"] as const)(
    "%s creator's Idle-only funding is refused by the real conservation check",
    async (path) => {
      const f = await correctionLockCreationFixture(path, true);
      expect(f.lock.utxo.assets.lovelace).toBe(f.idleMinimum);
      expect(f.lock.utxo.assets.lovelace).toBeLessThan(f.lockedMinimum);
      await assertAvailabilityRefusal(await f.prune(), {
        purpose: "spend",
        script: f.contracts.correctionLock.spendingScriptHash,
      });
      const failed = lastAvailabilityEvaluationFailure();
      expect(failed).toBeDefined();
      const outputs = CML.Transaction.from_cbor_hex(failed!.tx)
        .body()
        .outputs();
      const continuedLock = Array.from({ length: outputs.len() }, (_, index) =>
        outputs.get(index),
      ).find((output) => output.address().to_bech32() === f.lock.utxo.address);
      expect(continuedLock).toBeDefined();
      expect(continuedLock!.amount().coin()).toBe(f.lockedMinimum);
      expect(continuedLock!.amount().coin()).toBeGreaterThan(
        f.lock.utxo.assets.lovelace,
      );
      expect(await f.lucid.utxosByOutRef([f.lock.utxo])).toEqual([f.lock.utxo]);
    },
    120_000,
  );
});
