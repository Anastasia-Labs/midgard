import {
  Constr,
  Data,
  type UTxO,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { AuthenticatedValidator } from "../src/common.js";
import { DaParamsDatum } from "../src/da-attestation.js";
import { encodeDaBondPoolDatum } from "../src/da-bond-pool.js";
import {
  assertDaBondPoolOwnerQuorum,
  buildBeginDaBondPoolWithdrawTxProgram,
  buildCancelDaBondPoolWithdrawTxProgram,
  buildCompleteDaBondPoolWithdrawTxProgram,
  buildInitDaBondPoolTxProgram,
  buildTopUpDaBondPoolTxProgram,
  DaBondPoolBuildError,
} from "../src/da-bond-pool-transactions.js";
import * as Sdk from "../src/index.js";
import {
  ADA,
  daParamsDatum,
  expectBuilds,
  expectRefusal,
  FLOOR,
  MIN_TOP_UP,
  NETWORK,
  parameters,
  parkingAddress,
  setupScene,
  syntheticPool,
  WITHDRAW_DELAY_MS,
} from "./da-bond-pool-transactions.setup-scene.js";

describe("DA bond pool builders: refusals before assembly", () => {
  it("counts each owner once and refuses a quorum the validator would refuse", () => {
    const owners = ["0a".repeat(28), "0b".repeat(28), "0c".repeat(28)];
    const params = daParamsDatum(owners, 2n);
    expect(() =>
      assertDaBondPoolOwnerQuorum(params, [owners[0]!, owners[2]!]),
    ).not.toThrow();
    const reasonOf = (signers: readonly string[]) => {
      try {
        assertDaBondPoolOwnerQuorum(params, signers);
      } catch (error) {
        expect(error).toBeInstanceOf(DaBondPoolBuildError);
        return (error as DaBondPoolBuildError).reason;
      }
      return "accepted";
    };
    expect(reasonOf([owners[1]!])).toBe("insufficient_signers");
    expect(reasonOf([])).toBe("insufficient_signers");
    expect(reasonOf([owners[1]!, owners[1]!])).toBe("duplicate_signer");
    expect(reasonOf([owners[0]!, "04".repeat(28)])).toBe("signer_not_owner");
    expect(reasonOf([owners[0]!, owners[1]!.toUpperCase()])).toBe(
      "signer_not_owner",
    );
  });

  it("refuses withdrawals signed below update_threshold, by a non-owner, or twice unless the emulator negative asks", async () => {
    const scene = await setupScene();
    const bonded = syntheticPool(scene, "Bonded", FLOOR + 600n * ADA);
    const withdrawing = syntheticPool(
      scene,
      { Withdrawing: { unlock_at: 0n } },
      FLOOR + 600n * ADA,
    );
    const now = BigInt(scene.emulator.now());
    const twoOfTwo: UTxO = {
      ...scene.daParamsUtxo,
      datum: Data.to(
        daParamsDatum([scene.walletKeyHash, scene.otherOwner], 2n),
        DaParamsDatum,
      ),
    };
    const base = {
      poolValidator: scene.pool,
      parameters,
      daParamsUtxo: twoOfTwo,
    } as const;
    const begin = (
      signerKeyHashes: readonly string[],
      skipQuorumPrecheck?: true,
    ) =>
      buildBeginDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...base,
        pool: { utxo: bonded },
        signerKeyHashes,
        withdrawDelayMs: WITHDRAW_DELAY_MS,
        validity: { validFrom: now, validTo: now + 300_000n },
        ...(skipQuorumPrecheck === undefined ? {} : { skipQuorumPrecheck }),
      });
    await expectRefusal(begin([scene.walletKeyHash]), "insufficient_signers");
    await expectRefusal(
      begin([scene.walletKeyHash, scene.walletKeyHash]),
      "duplicate_signer",
    );
    await expectRefusal(
      begin([scene.walletKeyHash, "ef".repeat(28)]),
      "signer_not_owner",
    );
    await expectBuilds(begin([scene.walletKeyHash, scene.otherOwner]));
    // The emulator negatives: below the quorum, for the validator to refuse.
    await expectBuilds(begin([scene.walletKeyHash], true));
    await expectBuilds(begin([scene.walletKeyHash, "ef".repeat(28)], true));
    const cancel = (skipQuorumPrecheck?: true) =>
      buildCancelDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...base,
        pool: { utxo: withdrawing },
        signerKeyHashes: [scene.otherOwner],
        ...(skipQuorumPrecheck === undefined ? {} : { skipQuorumPrecheck }),
      });
    await expectRefusal(cancel(), "insufficient_signers");
    await expectBuilds(cancel(true));
    await expectRefusal(
      buildCompleteDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...base,
        pool: { utxo: withdrawing },
        signerKeyHashes: [scene.walletKeyHash],
        amount: ADA,
        destination: parkingAddress(),
        validity: { validFrom: now },
      }),
      "insufficient_signers",
    );
    await expectRefusal(
      buildBeginDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...base,
        daParamsUtxo: { ...twoOfTwo, datum: Data.void() },
        pool: { utxo: bonded },
        signerKeyHashes: [scene.walletKeyHash, scene.otherOwner],
        withdrawDelayMs: WITHDRAW_DELAY_MS,
        validity: { validFrom: now, validTo: now + 300_000n },
      }),
      "invalid_da_params",
    );
  });

  it("refuses a top-up below da_bond_min_top_up_lovelace unless the emulator negative asks", async () => {
    const scene = await setupScene();
    const topUp = (amount: bigint, skipMinimumPrecheck?: true) =>
      buildTopUpDaBondPoolTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: { utxo: syntheticPool(scene, "Bonded", FLOOR) },
        amount,
        ...(skipMinimumPrecheck === undefined ? {} : { skipMinimumPrecheck }),
      });
    expect(MIN_TOP_UP).toBeGreaterThan(1n);
    await expectRefusal(topUp(MIN_TOP_UP - 1n), "below_min_top_up");
    await expectRefusal(topUp(0n), "below_min_top_up");
    await expectBuilds(topUp(MIN_TOP_UP));
    await expectBuilds(topUp(MIN_TOP_UP - 1n, true));
    await expectRefusal(topUp(-1n, true), "invalid_amount");
  });

  it("tops up a pool whose datum another spend stored under a different encoding", async () => {
    const scene = await setupScene();
    // TopUp pins the datum by Data value only, so the chain admits these:
    // indefinite and definite field lists, the tag-102 constructor form and
    // non-minimal integers.
    for (const datum of [
      "d8799fff",
      "d866820080",
      "d87a811b000001ba60d33800",
      "d86682018105",
      "d87a9f1b0000000000000005ff",
    ]) {
      await expectBuilds(
        buildTopUpDaBondPoolTxProgram(scene.lucid, {
          poolValidator: scene.pool,
          parameters,
          pool: { utxo: { ...syntheticPool(scene, "Bonded", FLOOR), datum } },
          amount: MIN_TOP_UP,
        }),
      );
    }
    // Still one well-formed pool datum with a canonical value.
    for (const datum of [
      `${encodeDaBondPoolDatum("Bonded")}00`,
      "d87a81",
      Data.to(new Constr(2, [])),
      Data.to(new Constr(1, [-1n])),
    ]) {
      await expectRefusal(
        buildTopUpDaBondPoolTxProgram(scene.lucid, {
          poolValidator: scene.pool,
          parameters,
          pool: { utxo: { ...syntheticPool(scene, "Bonded", FLOOR), datum } },
          amount: MIN_TOP_UP,
        }),
        "invalid_pool",
      );
    }
  });

  it("bounds CompleteWithdraw to 0 < amount <= backing, at or after unlock_at", async () => {
    const scene = await setupScene();
    const now = BigInt(scene.emulator.now());
    const lovelace = FLOOR + 600n * ADA;
    const backing = lovelace - FLOOR;
    const unlockAt = now + 10_000n;
    const complete = (
      amount: bigint,
      validFrom: bigint,
      skipUnlockPrecheck?: true,
    ) =>
      buildCompleteDaBondPoolWithdrawTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: {
          utxo: syntheticPool(
            scene,
            { Withdrawing: { unlock_at: unlockAt } },
            lovelace,
          ),
        },
        daParamsUtxo: scene.daParamsUtxo,
        signerKeyHashes: [scene.walletKeyHash],
        amount,
        destination: parkingAddress(),
        validity: { validFrom },
        ...(skipUnlockPrecheck === undefined ? {} : { skipUnlockPrecheck }),
      });
    await expectBuilds(complete(backing, unlockAt));
    await expectBuilds(complete(1n, unlockAt));
    await expectRefusal(
      complete(backing + 1n, unlockAt),
      "amount_exceeds_backing",
    );
    await expectRefusal(complete(0n, unlockAt), "invalid_amount");
    await expectRefusal(complete(-1n, unlockAt), "invalid_amount");
    // One slot early: the validator reads the inclusive lower bound.
    await expectRefusal(complete(ADA, unlockAt - 1_000n), "before_unlock");
    await expectBuilds(complete(ADA, unlockAt - 1_000n, true));
    // A pool at or below its floor backs nothing.
    const atFloor = buildCompleteDaBondPoolWithdrawTxProgram(scene.lucid, {
      poolValidator: scene.pool,
      parameters,
      pool: {
        utxo: syntheticPool(
          scene,
          { Withdrawing: { unlock_at: unlockAt } },
          FLOOR,
        ),
      },
      daParamsUtxo: scene.daParamsUtxo,
      signerKeyHashes: [scene.walletKeyHash],
      amount: 1n,
      destination: parkingAddress(),
      validity: { validFrom: unlockAt },
    });
    await expectRefusal(atFloor, "amount_exceeds_backing");
  });

  it("refuses the wrong pool state, a pool below its floor, and a pool that is not one", async () => {
    const scene = await setupScene();
    const now = BigInt(scene.emulator.now());
    const withdrawing = syntheticPool(
      scene,
      { Withdrawing: { unlock_at: now } },
      FLOOR + ADA,
    );
    const bonded = syntheticPool(scene, "Bonded", FLOOR + ADA);
    const quorum = {
      poolValidator: scene.pool,
      parameters,
      daParamsUtxo: scene.daParamsUtxo,
      signerKeyHashes: [scene.walletKeyHash],
    } as const;
    await expectRefusal(
      buildBeginDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...quorum,
        pool: { utxo: withdrawing },
        withdrawDelayMs: WITHDRAW_DELAY_MS,
        validity: { validFrom: now, validTo: now + 300_000n },
      }),
      "pool_not_bonded",
    );
    await expectRefusal(
      buildCancelDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...quorum,
        pool: { utxo: bonded },
      }),
      "pool_not_withdrawing",
    );
    await expectRefusal(
      buildCompleteDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...quorum,
        pool: { utxo: bonded },
        amount: ADA,
        destination: parkingAddress(),
        validity: { validFrom: now },
      }),
      "pool_not_withdrawing",
    );
    await expectRefusal(
      buildBeginDaBondPoolWithdrawTxProgram(scene.lucid, {
        ...quorum,
        pool: { utxo: bonded },
        withdrawDelayMs: WITHDRAW_DELAY_MS,
        validity: {
          validFrom: now,
          validTo: now + Sdk.MAX_VALIDITY_RANGE_LENGTH_MS + 2_000n,
        },
      }),
      "invalid_validity_range",
    );
    const noNft = { ...bonded, assets: { lovelace: FLOOR + ADA } };
    await expectRefusal(
      buildTopUpDaBondPoolTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: { utxo: noNft },
        amount: MIN_TOP_UP,
      }),
      "invalid_pool",
    );
    const [walletUtxo] = await scene.lucid.wallet().getUtxos();
    await expectRefusal(
      buildInitDaBondPoolTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        initUtxo: walletUtxo!,
        lovelace: FLOOR - 1n,
      }),
      "below_floor",
    );
    await expectBuilds(
      buildInitDaBondPoolTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        initUtxo: walletUtxo!,
        lovelace: FLOOR,
      }),
    );
    // The pool address must be Script(own policy) with no stake part.
    const staked: AuthenticatedValidator = {
      ...scene.pool,
      spendingScriptAddress: validatorToAddress(
        NETWORK,
        scene.pool.spendingScript,
        { type: "Key", hash: scene.walletKeyHash },
      ),
    };
    await expectRefusal(
      buildInitDaBondPoolTxProgram(scene.lucid, {
        poolValidator: staked,
        parameters,
        initUtxo: walletUtxo!,
        lovelace: FLOOR,
      }),
      "invalid_pool_address",
    );
    await expectRefusal(
      buildTopUpDaBondPoolTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: { utxo: bonded },
        amount: MIN_TOP_UP,
        referenceScripts: { daBondPoolSpending: scene.daParamsUtxo },
      }),
      "reference_script_mismatch",
    );
  });
});
