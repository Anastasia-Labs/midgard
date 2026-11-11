import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  generateEmulatorAccount,
  Lucid,
  toUnit,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  driveProvenCorrectionRemoval,
  excludeReservedFunding,
  resolveProverSigner,
} from "../src/index.js";
import {
  buildProvedDoubleSpendFixture,
  expectStateQueueHeaderOrder,
  type ProvedDoubleSpendFixture,
  submitRemovalForFixture,
} from "./support/submit-init-emulator-fixtures.js";
import { network } from "./support/submit-init-emulator-shared.js";

// A proven fraud-proof record names the queued header it convicts. Whoever
// reads it and the CorrectionLock from L1 can start that removal from an Idle
// lock, or finish it from a lock Locked under it: no admitted decision, no
// journal, not the prover's own wallet.

const lockDatum = async (fixture: ProvedDoubleSpendFixture) => {
  const locks = await fixture.funderLucid.utxosAtWithUnit(
    fixture.contracts.correctionLock.spendingScriptAddress,
    toUnit(
      fixture.contracts.hubOracle.policyId,
      SDK.CORRECTION_LOCK_ASSET_NAME,
    ),
  );
  expect(locks).toHaveLength(1);
  return Data.from(locks[0]!.datum!, SDK.CorrectionLockDatum);
};

/** A removal whose prover stopped after its first peel left the lock Locked. */
const interruptedRemoval = async () => {
  const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });
  let submissions = 0;
  await expect(
    submitRemovalForFixture(fixture, {
      preSubmitBoundary: async () => {
        submissions += 1;
        if (submissions > 1) throw new Error("prover stopped");
      },
    }),
  ).rejects.toThrow("prover stopped");
  await expectStateQueueHeaderOrder({
    lucid: fixture.funderLucid,
    contracts: fixture.contracts,
    expectedHeaderHashes: [fixture.headerHash],
  });
  const locked = await lockDatum(fixture);
  if (locked === "Idle") throw new Error("the first peel left the lock Idle");
  const identity = locked.Locked.correction_identity;
  if (typeof identity !== "object" || !("FraudProof" in identity))
    throw new Error("the first peel locked a non-fraud-proof correction");
  return {
    fixture,
    lock: {
      targetHeaderHash: locked.Locked.target_header_hash,
      fraudProofAssetName: identity.FraudProof.fraud_proof_asset_name,
    },
  };
};

const otherCaller = async (fixture: ProvedDoubleSpendFixture) => {
  const account = generateEmulatorAccount({ lovelace: 0n });
  const signer = resolveProverSigner({
    network,
    walletSeedPhrase: account.seedPhrase,
  });
  const funding = await fixture.funderLucid
    .newTx()
    .pay.ToAddress(signer.address, { lovelace: 2_000_000_000n })
    .pay.ToAddress(signer.address, { lovelace: 1_000_000_000n })
    .complete();
  const fundingTxHash = await (
    await funding.sign.withWallet().complete()
  ).submit();
  await fixture.funderLucid.awaitTx(fundingTxHash);
  const lucid = await Lucid(fixture.emulator, "Custom", {
    slotConfig: fixture.proverLucid.config().slotConfig,
  });
  // A funding reservation of another action holds the larger coin.
  const reserved = `${fundingTxHash}#0`;
  return {
    lucid,
    reserved,
    signer: excludeReservedFunding({
      signer,
      reservedOutRefs: async () => [reserved],
    }),
  };
};

const finish = async (
  fixture: ProvedDoubleSpendFixture,
  correction: Parameters<typeof driveProvenCorrectionRemoval>[0]["correction"],
) => {
  const caller = await otherCaller(fixture);
  const now = BigInt(fixture.emulator.now());
  const result = await driveProvenCorrectionRemoval({
    lucid: caller.lucid,
    blueprint: fixture.realBlueprint,
    deploymentInfo: fixture.deploymentInfo,
    network,
    signer: caller.signer,
    correction,
    validFrom: now - 120_000n,
    validTo: now + 300_000n,
  });
  return { ...caller, result };
};

describe("driving a proven fraud-proof correction from L1", () => {
  it("starts the removal from an Idle lock while the proven header is queued", async () => {
    const fixture = await buildProvedDoubleSpendFixture({ successorCount: 1 });
    expect(await lockDatum(fixture)).toBe("Idle");
    const correction = {
      targetHeaderHash: fixture.headerHash,
      fraudProofAssetName: fixture.step04Result.fraudProofAssetName,
    };

    const { result: started } = await finish(fixture, correction);
    if (started.kind !== "proven") throw new Error(started.kind);
    expect(started.fraudProofOutRef).toBe(outRefLabel(fixture.fraudProofUtxo));
    expect((await started.removal).fraudProver).toBe(
      fixture.proverPaymentKeyHash,
    );

    expect(await lockDatum(fixture)).toBe("Idle");
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [],
    });
    // The record is still on L1; its header is no longer queued.
    expect((await finish(fixture, correction)).result.kind).toBe("finished");
  }, 240_000);

  it("removes the locked target through its proven record and returns the lock to Idle", async () => {
    const { fixture, lock } = await interruptedRemoval();
    expect(lock).toEqual({
      targetHeaderHash: fixture.headerHash,
      fraudProofAssetName: fixture.step04Result.fraudProofAssetName,
    });

    const { lucid, reserved, result: started } = await finish(fixture, lock);
    if (started.kind !== "proven") throw new Error(started.kind);
    expect(started.fraudProofOutRef).toBe(outRefLabel(fixture.fraudProofUtxo));
    const removed = await started.removal;
    expect(removed.fraudProver).toBe(fixture.proverPaymentKeyHash);
    // Paid from the caller's unreserved coin; the reserved one is untouched.
    const [txHash, outputIndex] = reserved.split("#");
    expect(
      await lucid.utxosByOutRef([
        { txHash: txHash!, outputIndex: Number(outputIndex) },
      ]),
    ).toHaveLength(1);

    expect(await lockDatum(fixture)).toBe("Idle");
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [],
    });
    // The finalized view still shows the lock; the live one has moved on.
    expect((await finish(fixture, lock)).result.kind).toBe("finished");
  }, 240_000);

  it("submits nothing for a correction no proven record names", async () => {
    const { fixture, lock } = await interruptedRemoval();
    const before = await lockDatum(fixture);
    // The same family, a header nobody proved fraudulent.
    const unprovenHeader = "00".repeat(lock.targetHeaderHash.length / 2);
    const unproven = {
      targetHeaderHash: unprovenHeader,
      fraudProofAssetName: `${SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleSpend}${unprovenHeader}`,
    };
    expect(lock.fraudProofAssetName).toBe(
      `${SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleSpend}${lock.targetHeaderHash}`,
    );

    expect((await finish(fixture, unproven)).result).toEqual({
      kind: "unproven",
    });
    expect(await lockDatum(fixture)).toEqual(before);
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [fixture.headerHash],
    });
  }, 240_000);
});
