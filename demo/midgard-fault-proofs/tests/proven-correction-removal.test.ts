import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  generateEmulatorAccount,
  Lucid,
  toUnit,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  driveProvenCorrectionRemoval,
  excludeReservedFunding,
  type HoldWalletInputs,
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

/** A signed transaction's inputs and collateral at `address`, and when it
 * can no longer land. */
const walletCoinsOf = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  cbor: string,
  address: string,
) => {
  const body = CML.Transaction.from_cbor_hex(cbor).body();
  const outRefs = (inputs: CML.TransactionInputList | undefined) =>
    Array.from({ length: inputs?.len() ?? 0 }, (_, index) => {
      const input = inputs!.get(index);
      return {
        txHash: input.transaction_id().to_hex(),
        outputIndex: Number(input.index()),
      };
    });
  const spent = await lucid.utxosByOutRef([
    ...outRefs(body.inputs()),
    ...outRefs(body.collateral_inputs()),
  ]);
  const ttl = body.ttl();
  return {
    collateral: outRefs(body.collateral_inputs()).map(outRefLabel),
    expiresAtMs:
      ttl === undefined ? undefined : lucid.slotToUnixTime(Number(ttl)),
    wallet: [
      ...new Set(
        spent.filter((utxo) => utxo.address === address).map(outRefLabel),
      ),
    ].sort(),
  };
};

/** The removal's wallet holds and submissions, in the order they happen;
 * `submitFailure`, when given, fails every submission after recording it. */
const walletEvents = (
  fixture: ProvedDoubleSpendFixture,
  refuse?: (outRefs: readonly string[]) => Error | undefined,
  submitFailure?: Error,
) => {
  const events: (
    | Readonly<{
        kind: "hold";
        outRefs: readonly string[];
        untilMs: number;
        release: () => void;
      }>
    | Readonly<{
        kind: "submit";
        coins: Awaited<ReturnType<typeof walletCoinsOf>>;
      }>
  )[] = [];
  /** Records every submission from now on, with the wallet coins it
   * spends while they are unspent. */
  const watch = (
    lucid: Awaited<ReturnType<typeof Lucid>>,
    walletAddress: string,
  ) => {
    const submit = fixture.emulator.submitTx.bind(fixture.emulator);
    vi.spyOn(fixture.emulator, "submitTx").mockImplementation(async (cbor) => {
      events.push({
        kind: "submit",
        coins: await walletCoinsOf(lucid, cbor, walletAddress),
      });
      if (submitFailure !== undefined) throw submitFailure;
      return await submit(cbor);
    });
  };
  const hold: HoldWalletInputs = ({ outRefs, untilMs }) => {
    expect(untilMs).toBeGreaterThan(Date.now());
    const refused = refuse?.(outRefs);
    if (refused !== undefined) throw refused;
    const release = vi.fn();
    events.push({ kind: "hold", outRefs, untilMs, release });
    return release;
  };
  return { events, hold, watch };
};

const finish = async (
  fixture: ProvedDoubleSpendFixture,
  correction: Parameters<typeof driveProvenCorrectionRemoval>[0]["correction"],
  wallet?: ReturnType<typeof walletEvents>,
) => {
  const caller = await otherCaller(fixture);
  wallet?.watch(caller.lucid, caller.signer.address);
  const now = BigInt(fixture.emulator.now());
  const result = await driveProvenCorrectionRemoval({
    lucid: caller.lucid,
    blueprint: fixture.realBlueprint,
    deploymentInfo: fixture.deploymentInfo,
    network,
    signer: caller.signer,
    correction,
    holdWalletInputs: wallet?.hold ?? (() => () => undefined),
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

    const wallet = walletEvents(fixture);
    const { result: started } = await finish(fixture, correction, wallet);
    if (started.kind !== "proven") throw new Error(started.kind);
    expect(started.fraudProofOutRef).toBe(outRefLabel(fixture.fraudProofUtxo));
    expect((await started.removal).fraudProver).toBe(
      fixture.proverPaymentKeyHash,
    );
    // Each peel held its own wallet coins, collateral included, before it
    // was submitted, until its TTL; the confirmed removal released every
    // hold.
    expect(wallet.events.map(({ kind }) => kind)).toEqual([
      "hold",
      "submit",
      "hold",
      "submit",
    ]);
    for (let index = 0; index < wallet.events.length; index += 2) {
      const [held, submitted] = wallet.events.slice(index, index + 2);
      if (held?.kind !== "hold" || submitted?.kind !== "submit")
        throw new Error("a submission was not held first");
      const { collateral, wallet: coins } = submitted.coins;
      expect(collateral.length).toBeGreaterThan(0);
      expect(coins).toEqual(expect.arrayContaining(collateral));
      expect([...held.outRefs].sort()).toEqual(coins);
      expect(held.untilMs).toBe(submitted.coins.expiresAtMs);
      expect(held.release).toHaveBeenCalledOnce();
    }

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

  it("submits nothing when a wallet coin of the removal is already taken", async () => {
    const { fixture, lock } = await interruptedRemoval();
    const before = await lockDatum(fixture);
    const wallet = walletEvents(fixture, (outRefs) =>
      outRefs.length === 0 ? undefined : new Error("prover coin is reserved"),
    );
    const { result: started } = await finish(fixture, lock, wallet);
    if (started.kind !== "proven") throw new Error(started.kind);
    await expect(started.removal).rejects.toThrow("prover coin is reserved");
    expect(wallet.events).toEqual([]);
    expect(await lockDatum(fixture)).toEqual(before);
    await expectStateQueueHeaderOrder({
      lucid: fixture.funderLucid,
      contracts: fixture.contracts,
      expectedHeaderHashes: [fixture.headerHash],
    });
  }, 240_000);

  it("keeps a failed removal's holds, as its transaction may still land", async () => {
    const { fixture, lock } = await interruptedRemoval();
    const wallet = walletEvents(
      fixture,
      undefined,
      new Error("node lost the removal"),
    );
    const { result: started } = await finish(fixture, lock, wallet);
    if (started.kind !== "proven") throw new Error(started.kind);
    await expect(started.removal).rejects.toThrow("node lost the removal");
    expect(wallet.events.map(({ kind }) => kind)).toEqual(["hold", "submit"]);
    const [held] = wallet.events;
    if (held?.kind !== "hold") throw new Error("the submission was not held");
    expect(held.outRefs.length).toBeGreaterThan(0);
    expect(held.release).not.toHaveBeenCalled();
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
