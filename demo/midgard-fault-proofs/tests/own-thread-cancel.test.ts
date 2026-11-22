/** The header-free cancel of our own computation thread over the real
 * emulator chain: our double-spend thread at step 01, cancelled from its
 * out-ref and the deployment's published scripts alone, with no workflow,
 * no header and no stored record behind it. The cancel spends the thread,
 * so a second one, built before the first lands as after a restart, never
 * lands beside it. */
import { CML, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { outRefLabel } from "../src/runtime.js";
import { buildInit } from "../src/submit-init.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { ownThreadCancellerOf } from "./support/own-thread-canceller.js";
import { walletLovelace } from "./support/own-thread-reclaim-expectations.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  makeFaultProofEmulatorHarness,
  makeHeader,
  network,
  publishRemovalReferenceScripts,
  submitSetupTx,
  withPlaceholderDeposit,
} from "./support/submit-init-emulator-shared.js";
import { buildAndSubmit } from "./support/submit-steps.js";

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

/** Our double-spend thread at step 01 of a header, confirmed. */
const scene = async () => {
  const h = await makeFaultProofEmulatorHarness();
  const { paymentCredential } = getAddressDetails(
    await h.funderLucid.wallet().address(),
  );
  const setup = await submitSetupTx({
    lucid: h.funderLucid,
    contracts: h.contracts,
    nonceUtxo: h.nonceUtxo,
    catalogue: h.catalogue,
    header: withPlaceholderDeposit(
      makeHeader(
        paymentCredential!.hash,
        alignUnixTimeToEmulatorSlotBoundary(
          h.funderLucid,
          h.emulator.now() + 120_000,
        ) - 1,
      ),
    ),
  });
  const removal = await publishRemovalReferenceScripts({
    lucid: h.proverLucid,
    contracts: h.contracts,
  });
  await buildAndSubmit(buildInit, {
    lucid: h.proverLucid,
    witnessReferenceScripts: h.witnessReferenceScripts,
    blueprint: h.realBlueprint,
    deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue, {
      removalReferenceScripts: removal.published,
    }),
    network,
    signer: h.proverSigner,
    fraudCategory: "doubleSpend",
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    awaitConfirmation: true,
  });
  const steps = h.contracts.fraudProofContracts.doubleSpend.steps;
  const policyId = h.contracts.computationThread.policyId;
  const thread = (await h.proverLucid.utxosAt(steps[0]!.spendingScriptAddress))
    .filter((utxo) =>
      Object.keys(utxo.assets).some((unit) => unit.startsWith(policyId)),
    )
    .at(0);
  if (thread === undefined) throw new Error("no step-01 thread was opened");
  return {
    h,
    thread,
    subject: {
      threadOutRef: outRefLabel(thread),
      categoryId: h.catalogue.categories.doubleSpend.categoryId,
      paymentScriptHash: steps[0]!.spendingScriptHash,
    },
  };
};

/** Records the transactions the emulator accepts. */
const recordAccepted = (h: Harness): string[] => {
  const accepted: string[] = [];
  const submit = h.emulator.submitTx.bind(h.emulator);
  h.emulator.submitTx = async (cbor: string) => {
    const txHash = await submit(cbor);
    accepted.push(cbor);
    return txHash;
  };
  return accepted;
};

/**
 * A provider that answers out-ref reads from confirmed blocks only, as a
 * chain indexer does: `thread` still stands until a block spends it, even
 * while a cancel of it is pending.
 */
const confirmedOnlyThread = (h: Harness, thread: UTxO): void => {
  const label = outRefLabel(thread);
  let blocked = false;
  const awaitBlock = h.emulator.awaitBlock.bind(h.emulator);
  h.emulator.awaitBlock = (blocks?: number) => {
    awaitBlock(blocks);
    blocked = true;
  };
  const read = h.emulator.getUtxosByOutRef.bind(h.emulator);
  h.emulator.getUtxosByOutRef = async (outRefs) => {
    const found = await read(outRefs);
    const asked = outRefs.some(
      (outRef) => `${outRef.txHash}#${outRef.outputIndex}` === label,
    );
    return blocked ||
      !asked ||
      found.some((utxo) => outRefLabel(utxo) === label)
      ? found
      : [...found, thread];
  };
};

const due = async () => true;

describe("own-thread cancel with no header in hand", () => {
  it("cancels our step-01 thread to our wallet; a second cancel finds it spent", async () => {
    const s = await scene();
    const accepted = recordAccepted(s.h);
    const { canceller } = ownThreadCancellerOf(s.h);
    expect(canceller.proverKeyHash).toBe(s.h.proverSigner.paymentKeyHash);
    expect(canceller.plan(s.subject)).toMatchObject({
      kind: "cancellable",
      category: "doubleSpend",
      role: "step01",
    });
    const before = await walletLovelace(s.h.proverLucid);

    const outcome = await canceller.cancel({
      subject: s.subject,
      stillDue: due,
    });
    expect(outcome).toMatchObject({
      kind: "submitted",
      reclaimedLovelace: s.thread.assets.lovelace,
    });
    expect(accepted).toHaveLength(1);

    s.h.emulator.awaitBlock();
    const cancel = CML.Transaction.from_cbor_hex(accepted[0]!);
    expect(await walletLovelace(s.h.proverLucid)).toBe(
      before + s.thread.assets.lovelace! - cancel.body().fee(),
    );
    expect(await s.h.proverLucid.utxosByOutRef([s.thread])).toEqual([]);

    expect(
      await canceller.cancel({ subject: s.subject, stillDue: due }),
    ).toEqual({ kind: "spent_elsewhere" });
    expect(accepted).toHaveLength(1);
  }, 900_000);

  it("sends nothing when the thread stops being due before submission", async () => {
    const s = await scene();
    const accepted = recordAccepted(s.h);
    const { canceller } = ownThreadCancellerOf(s.h);
    expect(
      await canceller.cancel({
        subject: s.subject,
        stillDue: async () => false,
      }),
    ).toEqual({ kind: "withdrawn" });
    expect(accepted).toEqual([]);
    expect(await s.h.proverLucid.utxosByOutRef([s.thread])).toHaveLength(1);
  }, 900_000);

  it("a restart between build and landing lands one cancel, never two", async () => {
    const s = await scene();
    // A second fee coin, so the rebuilt cancel is funded from confirmed
    // coins while the first is pending.
    const topUp = await s.h.funderLucid
      .newTx()
      .pay.ToAddress(await s.h.proverLucid.wallet().address(), {
        lovelace: 20_000_000n,
      })
      .complete();
    await (await topUp.sign.withWallet().complete()).submit();
    s.h.emulator.awaitBlock();
    const accepted = recordAccepted(s.h);
    const before = await walletLovelace(s.h.proverLucid);
    const first = await ownThreadCancellerOf(s.h).canceller.cancel({
      subject: s.subject,
      stillDue: due,
    });
    expect(first.kind).toBe("submitted");

    // Restarted with no memory of the first. A provider that shows the
    // pending spend finds the thread gone: nothing to do.
    expect(
      await ownThreadCancellerOf(s.h).canceller.cancel({
        subject: s.subject,
        stillDue: due,
      }),
    ).toEqual({ kind: "spent_elsewhere" });
    // One that reads confirmed blocks only still shows it: the cancel is
    // built again, the ledger refuses it for its spent input, and the
    // attempt is left for a later one.
    confirmedOnlyThread(s.h, s.thread);
    expect(
      await ownThreadCancellerOf(s.h).canceller.cancel({
        subject: s.subject,
        stillDue: due,
      }),
    ).toMatchObject({
      kind: "failed",
      reason: expect.stringContaining("already spent"),
    });
    expect(accepted).toHaveLength(1);

    s.h.emulator.awaitBlock();
    const fee = CML.Transaction.from_cbor_hex(accepted[0]!).body().fee();
    expect(await walletLovelace(s.h.proverLucid)).toBe(
      before + s.thread.assets.lovelace! - fee,
    );
    // Once it has landed, any later attempt finds the thread gone.
    expect(
      await ownThreadCancellerOf(s.h).canceller.cancel({
        subject: s.subject,
        stillDue: due,
      }),
    ).toEqual({ kind: "spent_elsewhere" });
    expect(accepted).toHaveLength(1);
  }, 900_000);
});
