import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type Script,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import {
  availabilityResponderCollateral,
  availabilityResponderOperations,
  selectAvailabilityResponderWallet,
} from "../src/availability/factory.js";
import {
  emulatorFollower,
  noChainIndexCommitteeConfig,
} from "./helpers/emulator-follower.js";

/**
 * The availability responder signs a step whose collateral its wallet
 * received after it started.
 *
 * The responder's wallet is selected by the production startup step, its
 * collateral is read by the production selection for every build, and the
 * step runs through the production operations and the SDK's signing and
 * intent checks, then goes to the Lucid emulator's ledger. A real Publish,
 * Settle or Close needs a deployed protocol and a live challenge; the step
 * here spends a coin at an always-succeeding script instead, completed with
 * the SDK's options for every availability step (exact fee, no coin
 * selection, the collateral as the only wallet input:
 * `midgard-sdk/src/availability-challenge-transactions.complete.ts`). That
 * keeps what matters: the step runs a script, so it needs collateral, and
 * only the collateral asks for the responder's key.
 */

/** `(lam _ (con unit ()))`: a Plutus V3 script that accepts every spend. */
const ALWAYS_SUCCEEDS: Script = { type: "PlutusV3", script: "46450101002499" };
const FEE = 1_000_000n;
const HEADER_HASH = "bb".repeat(28);

const closers: (() => Promise<void> | void)[] = [];
afterEach(async () => {
  for (const close of closers.splice(0).reverse()) await close();
});

const signAndSubmit = async (
  emulator: Emulator,
  lucid: LucidEvolution,
  build: (tx: ReturnType<LucidEvolution["newTx"]>) => typeof tx,
): Promise<string> => {
  const signed = await (await build(lucid.newTx()).complete()).sign
    .withWallet()
    .complete();
  const txHash = await signed.submit();
  emulator.awaitBlock(1);
  return txHash;
};

const vkeyHashes = (signedCbor: string): string[] => {
  const witnesses = CML.Transaction.from_cbor_hex(signedCbor)
    .witness_set()
    .vkeywitnesses();
  return Array.from({ length: witnesses?.len() ?? 0 }, (_, index) =>
    witnesses!.get(index).vkey().hash().to_hex(),
  );
};

const fixture = async () => {
  const responder = generateEmulatorAccount({ lovelace: 50_000_000n });
  const attestation = generateEmulatorAccount({ lovelace: 50_000_000n });
  const funder = generateEmulatorAccount({ lovelace: 100_000_000n });
  const emulator = new Emulator([responder, attestation, funder]);
  // Every Lucid is made before the emulator's clock moves: making one later
  // resets the shared "Custom" slot configuration to start at that moment.
  const lucid = await Lucid(emulator, "Custom");
  const operator = await Lucid(emulator, "Custom");
  operator.selectWallet.fromSeed(responder.seedPhrase);
  const funding = await Lucid(emulator, "Custom");
  funding.selectWallet.fromSeed(funder.seedPhrase);
  const funderAddress = await funding.wallet().address();

  // The protocol input the step spends.
  const scriptAddress = validatorToAddress("Custom", ALWAYS_SUCCEEDS);
  await signAndSubmit(emulator, funding, (tx) =>
    tx.pay.ToContract(
      scriptAddress,
      { kind: "inline", value: Data.void() },
      { lovelace: 10_000_000n },
    ),
  );
  const [protocolInput] = await funding.utxosAt(scriptAddress);
  if (protocolInput === undefined) throw new Error("protocol input missing");

  const config = {
    ...(await noChainIndexCommitteeConfig(responder.seedPhrase)),
    l1SubmitterKeySource: `mnemonic:${attestation.seedPhrase}`,
  };
  const follower = await emulatorFollower(config);
  closers.push(follower.close);
  await follower.forward();
  const journal = openAvailabilityOperationJournal(
    config.availabilityJournalPath!,
  );
  closers.push(() => journal.close());

  // The responder starts: the factory's wallet step.
  const actor = await selectAvailabilityResponderWallet(lucid, {
    ...config,
    availabilitySubmitterKeySource: config.availabilitySubmitterKeySource!,
  });
  const responderAddress = await lucid.wallet().address();
  const startupCoins = await lucid.utxosAt(responderAddress);

  const deploymentIdentity = String(config.contractDeploymentInfo.manifestId);
  const submitted: string[] = [];
  const operations = availabilityResponderOperations({
    lucid,
    reads: follower.reads,
    assertSourceHealthy: async () => {},
    context: {
      deploymentIdentity,
      actor,
      journal,
      stateQueuePolicyId: config.stateQueuePolicyId,
      minimumConfirmationDepth: config.finalityDepth,
      transactionLimits: {
        maxTxSize: 16_384,
        maxTxExMem: 16_500_000n,
        maxTxExSteps: 10_000_000_000n,
        coinsPerUtxoByte: 4_310n,
        feeCeilings: { settle: 2_000_000n },
      },
      // The ledger: the emulator refuses a transaction missing a witness.
      submit: async (signedCbor) => {
        submitted.push(signedCbor);
        return emulator.submitTx(signedCbor);
      },
    },
  });
  const collateralLovelace =
    (FEE * BigInt(lucid.config().protocolParameters!.collateralPercentage) +
      99n) /
    100n;
  /** One availability step: the protocol input back to the funder, less the fee. */
  const step = () =>
    SDK.runDaAvailabilityOperation(operations.context, {
      action: "settle",
      headerHash: HEADER_HASH,
      build: async () => {
        const collateral = await availabilityResponderCollateral(lucid, FEE);
        return lucid
          .newTx()
          .collectFrom([protocolInput], Data.void())
          .attach.SpendingValidator(ALWAYS_SUCCEEDS)
          .pay.ToAddress(funderAddress, {
            lovelace: protocolInput.assets.lovelace - FEE,
          })
          .setMinFee(FEE)
          .validFrom(emulator.now())
          .validTo(emulator.now() + 60_000)
          .complete({
            localUPLCEval: true,
            coinSelection: false,
            presetWalletInputs: [...collateral],
            setCollateral: collateralLovelace,
          });
      },
    });
  /**
   * The wallet's coins move while the responder runs, as a refill after a
   * phase-2 collateral loss does: the startup coins are spent into new ones,
   * which the next step's collateral read selects.
   */
  const refill = () =>
    signAndSubmit(emulator, operator, (tx) =>
      tx.pay.ToAddress(responderAddress, { lovelace: 20_000_000n }),
    );
  return {
    emulator,
    lucid,
    attestation,
    actor,
    deploymentIdentity,
    journal,
    protocolInput,
    startupCoins,
    submitted,
    step,
    refill,
  };
};

describe("the availability responder signs collateral its wallet received after it started", () => {
  it("signs a step whose collateral arrived after startup, and the ledger lands it", async () => {
    const f = await fixture();
    const refillHash = await f.refill();
    const [collateral] = await availabilityResponderCollateral(f.lucid, FEE);
    expect(collateral?.txHash).toBe(refillHash);
    expect(f.startupCoins.map((coin) => coin.txHash)).not.toContain(refillHash);

    const result = await f.step();

    expect(result.status).toBe("submitted");
    expect(f.submitted).toHaveLength(1);
    expect(vkeyHashes(f.submitted[0]!)).toEqual([f.actor]);
    f.emulator.awaitBlock(1);
    expect(
      await f.lucid.utxosByOutRef([{ txHash: result.txHash, outputIndex: 0 }]),
    ).toHaveLength(1);
    expect(await f.lucid.utxosByOutRef([f.protocolInput])).toEqual([]);
  });

  it("refuses a step whose collateral the responder's key does not control, at the reserved-actor check, before anything is persisted or submitted", async () => {
    const f = await fixture();
    // The responder's Lucid holds another key's wallet: the collateral read
    // selects that key's coin, and that key alone signs.
    f.lucid.selectWallet.fromSeed(f.attestation.seedPhrase);

    await expect(f.step()).rejects.toThrow(
      "Availability operation was not signed by its reserved actor",
    );

    expect(f.submitted).toEqual([]);
    expect(f.journal.pending(f.deploymentIdentity, f.actor)).toEqual([]);
  });
});
