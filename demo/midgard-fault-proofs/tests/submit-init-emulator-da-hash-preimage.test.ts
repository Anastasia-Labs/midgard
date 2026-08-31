/**
 * `da-hash-preimage` emulator lifecycle (Goal task `Q44`, §9.1 output 9).
 *
 * Drives the real Aiken step validators through a Lucid emulator with the
 * production submitters: init -> step-01 -> step-02 -> permanent fraud-proof
 * token -> fraudulent block removal, plus the valid-block negative on both
 * planes (off-chain fail-closed and on-chain membership rejection).
 *
 * The committed evidence is a `transactions_root` leaf keyed by a foreign
 * 32-byte key rather than the canonical native-V1 transaction id of its own
 * value — exactly the fault the family adjudicates, and precisely the block no
 * other family can open (every native family requires
 * `native_tx_id_for_version(version, body_cbor) == key`).
 *
 * Kept in its own file so the leaked wasm heap stays far below the ~4 GiB
 * wasm32 ceiling; see tests/support/uplc-heap-guard.ts.
 */

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { computeMidgardNativeTxIdV1, outRefLabel } from "@al-ft/midgard-core";
import {
  DaHashPreimageStep02Datum,
  FraudProofTokenDatum,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  submitDaHashPreimageStep01,
  submitDaHashPreimageStep02,
  type SubmitDaHashPreimageTxInclusion,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import { runEmulatorLifecycleStage } from "./support/emulator/emulator-context.js";
import { submitInit } from "./support/legacy-submit-emulator.js";
import {
  expectStateQueueHeaderOrder,
  setupFraudulentBlockV1 as setupFraudulentBlock,
} from "./support/submit-init-emulator-fixtures.js";
import {
  buildRemovalDeploymentInfo,
  expectSingleUtxoWithUnit,
  l2TransactionSourceCborV1,
  makeFaultProofEmulatorHarnessV1,
  makeNativeTx,
  network,
  publishRemovalReferenceScripts,
  trieRootHex,
} from "./support/submit-init-emulator-shared.js";

/**
 * A key that is not the canonical transaction id of any leaf value in this
 * fixture, used as the block's committed MPF key.
 */
const FOREIGN_COMMITTED_KEY = "99".repeat(32);

type CommittedLeafFixture = {
  readonly transactionsRoot: string;
  readonly l2TransactionCount: bigint;
  /** The key the block actually committed the leaf under. */
  readonly committedTxId: string;
  /** The canonical transaction id the leaf value itself commits to. */
  readonly canonicalTxId: string;
  readonly committedLeafValueCbor: string;
  readonly inclusion: SubmitDaHashPreimageTxInclusion;
};

/**
 * Commits one canonical `Data(L2TransactionSourceV1)` as the single leaf of a
 * block's transactions MPF, under `committedTxId`.
 */
const buildCommittedLeafFixture = async (
  committedKey?: string,
): Promise<CommittedLeafFixture> => {
  const nativeTx = makeNativeTx({
    spendInputCbors: [Buffer.from("61".repeat(32), "hex")],
    fee: 7n,
    referenceByte: "62",
    outputByte: "63",
    witnessByte: "64",
  });
  const committedLeafValue = Buffer.from(
    l2TransactionSourceCborV1(nativeTx),
    "hex",
  );
  const canonicalTxId = computeMidgardNativeTxIdV1(nativeTx).toString("hex");
  const committedTxId = committedKey ?? FOREIGN_COMMITTED_KEY;

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(
    Buffer.from(committedTxId, "hex"),
    Buffer.from(committedLeafValue),
  );
  const proof = await trie.prove(Buffer.from(committedTxId, "hex"));
  const transactionsRoot = trieRootHex(trie);

  return {
    transactionsRoot,
    l2TransactionCount: 1n,
    committedTxId,
    canonicalTxId,
    committedLeafValueCbor: committedLeafValue.toString("hex"),
    inclusion: {
      committedTxId,
      committedLeafValueCbor: committedLeafValue.toString("hex"),
      transactionsPhasRoot: transactionsRoot,
      txMembershipProof: Data.from(proof.toCBOR().toString("hex")) as never,
      txMembershipProofCbor: proof.toCBOR().toString("hex"),
    },
  };
};

const makeEmulatorHarness = async () =>
  await makeFaultProofEmulatorHarnessV1({
    contractOptions: {
      realDaHashPreimage: true,
      alwaysFraudProofCatalogue: true,
    },
  });

describe("da-hash-preimage fault-proof emulator lifecycle", () => {
  it("proves and removes a tail miskeyed-leaf block end to end", async () => {
    const harness = await makeEmulatorHarness();
    const {
      realBlueprint,
      emulator,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
    } = harness;

    // See `publishRemovalReferenceScripts`: removal must source these
    // validators from reference inputs to stay inside the 16,384-byte L1
    // envelope. Published before the header clock is sampled so the whole
    // timeline shifts uniformly.
    const removalReferenceScriptPublications =
      await publishRemovalReferenceScripts({
        lucid: proverLucid,
        contracts,
      });

    const fixture = await buildCommittedLeafFixture();
    expect(fixture.committedTxId).not.toBe(fixture.canonicalTxId);

    const setup = await setupFraudulentBlock({
      funderLucid,
      emulator,
      contracts,
      catalogue,
      fixture,
    });
    const { headerHash } = setup;
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [headerHash],
    });

    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
      removalReferenceScripts: removalReferenceScriptPublications.published,
    });

    // ## init
    const initResult = await runEmulatorLifecycleStage("q44.init", () =>
      submitInit({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "daHashPreimage",
        fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        awaitConfirmation: true,
      }),
    );

    expect(initResult.txHash).toHaveLength(64);
    expect(initResult.fraudulentHeaderHash).toBe(headerHash);
    expect(initResult.fraudCategoryName).toBe("daHashPreimage");
    expect(initResult.fraudCategoryId).toBe(
      catalogue.categories.daHashPreimage.categoryId,
    );
    expect(initResult.computationThreadAssetName).toBe(
      `${catalogue.categories.daHashPreimage.categoryId}${headerHash}`,
    );

    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    const proverPaymentCredential = getAddressDetails(
      await proverLucid.wallet().address(),
    ).paymentCredential;
    expect(proverPaymentCredential?.type).toBe("Key");
    const proverPaymentKeyHash = proverPaymentCredential!.hash;

    // ## step-01: bind the committed leaf to the header
    const step01Result = await runEmulatorLifecycleStage("q44.step-01", () =>
      submitDaHashPreimageStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: fixture.inclusion,
        referenceScriptUtxo:
          harness.faultProofReferenceScripts.fraudProofDaHashPreimage.utxo,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        awaitConfirmation: true,
      }),
    );

    expect(step01Result.txHash).toHaveLength(64);
    expect(step01Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step01Result.committedTxId).toBe(fixture.committedTxId);
    expect(step01Result.verdict).toBe("KeyMismatch");
    expect(step01Result.embeddedTxId).toBe(fixture.canonicalTxId);
    expect(step01Result.derivedTxId).toBeNull();
    expect(step01Result.committedLeafByteCount).toBe(
      fixture.committedLeafValueCbor.length / 2,
    );
    expect(step01Result.committedLeafByteCount).toBeGreaterThan(0);
    await expect(
      proverLucid.utxosAtWithUnit(
        initResult.firstStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const secondStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step01Result.secondStepAddress,
      initResult.computationThreadUnit,
    );
    // The total verdict pinned by L1 matches the off-chain adjudicator.
    expect(Data.from(secondStepUtxo.datum!, DaHashPreimageStep02Datum)).toEqual(
      {
        fraud_prover: proverPaymentKeyHash,
        data: { verdict: "KeyMismatch" },
      },
    );

    // ## step-02: adjudicate and mint the permanent fraud-proof token
    const step02Result = await runEmulatorLifecycleStage("q44.step-02", () =>
      submitDaHashPreimageStep02({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(secondStepUtxo),
        referenceScriptUtxo:
          harness.faultProofReferenceScripts.fraudProofDaHashPreimageStep02
            .utxo,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        awaitConfirmation: true,
      }),
    );

    expect(step02Result.txHash).toHaveLength(64);
    expect(step02Result.fraudulentHeaderHash).toBe(headerHash);
    expect(step02Result.verdict).toBe("KeyMismatch");
    expect(step02Result.fraudProofAssetName).toBe(
      initResult.computationThreadAssetName,
    );
    expect(step02Result.fraudProofUnit).toBe(
      toUnit(
        contracts.fraudProof.policyId,
        initResult.computationThreadAssetName,
      ),
    );
    await expect(
      proverLucid.utxosAtWithUnit(
        step01Result.secondStepAddress,
        initResult.computationThreadUnit,
      ),
    ).resolves.toHaveLength(0);

    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum)).toEqual({
      fraud_prover: proverPaymentKeyHash,
    });

    // ## removal: the proven block leaves the state queue, the token stays
    const removeNow = BigInt(emulator.now());
    const removeResult = await runEmulatorLifecycleStage("q44.removal", () =>
      submitRemoveFraudulentBlock({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        fraudCategory: "daHashPreimage",
        fraudulentHeaderHash: headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    );

    expect(removeResult.fraudCategory).toBe("daHashPreimage");
    expect(removeResult.fraudCategoryId).toBe(
      catalogue.categories.daHashPreimage.categoryId,
    );
    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.removedHeaderHash)).toEqual(
      [headerHash],
    );
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashActiveOperator",
    ]);
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [],
    });
    await expect(
      funderLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
    const retainedFraudProof = await expectSingleUtxoWithUnit(
      proverLucid,
      step02Result.fraudProofAddress,
      step02Result.fraudProofUnit,
    );
    expect(outRefLabel(retainedFraudProof)).toBe(outRefLabel(fraudProofUtxo));
    expect(retainedFraudProof.assets[step02Result.fraudProofUnit]).toBe(1n);
  }, 180_000);

  it("cannot advance a da-hash-preimage thread against a valid block", async () => {
    const harness = await makeEmulatorHarness();
    const {
      realBlueprint,
      emulator,
      funderLucid,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
    } = harness;

    // An honest block: the leaf is committed under its own canonical id.
    const honest = await buildCommittedLeafFixture();
    const honestFixture = await buildCommittedLeafFixture(honest.canonicalTxId);
    expect(honestFixture.committedTxId).toBe(honestFixture.canonicalTxId);

    const setup = await setupFraudulentBlock({
      funderLucid,
      emulator,
      contracts,
      catalogue,
      fixture: honestFixture,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue);

    const initResult = await submitInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: proverSigner,
      fraudCategory: "daHashPreimage",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
      awaitConfirmation: true,
    });
    const firstStepUtxo = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );

    // Plane 1 — off-chain fail-closed: the honest committed leaf derives its
    // own key, so the submitter refuses before building any transaction.
    await expect(
      submitDaHashPreimageStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: honestFixture.inclusion,
        referenceScriptUtxo:
          harness.faultProofReferenceScripts.fraudProofDaHashPreimage.utxo,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        awaitConfirmation: true,
      }),
    ).rejects.toThrow(
      /derives its own key; a valid block cannot be challenged/u,
    );

    // Plane 2 — on-chain: inventing a foreign key for the honest block's leaf
    // passes the local violation predicate, but the L1 membership proof cannot
    // open the committed root under a key the block never committed.
    await expect(
      submitDaHashPreimageStep01({
        lucid: proverLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: proverSigner,
        threadOutRef: outRefLabel(firstStepUtxo),
        stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
        txInclusion: {
          ...honestFixture.inclusion,
          committedTxId: FOREIGN_COMMITTED_KEY,
        },
        referenceScriptUtxo:
          harness.faultProofReferenceScripts.fraudProofDaHashPreimage.utxo,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        awaitConfirmation: true,
      }),
      // The delegated PHAS membership withdrawal is the script that fails:
      // `(FOREIGN_COMMITTED_KEY, value)` is not a leaf of the committed root.
    ).rejects.toThrow(/failed script execution Withdraw/u);

    // The thread is untouched: no step-02 output exists and the valid block is
    // still in the state queue.
    const stillFirstStep = await expectSingleUtxoWithUnit(
      proverLucid,
      initResult.firstStepAddress,
      initResult.computationThreadUnit,
    );
    expect(outRefLabel(stillFirstStep)).toBe(outRefLabel(firstStepUtxo));
    await expectStateQueueHeaderOrder({
      lucid: funderLucid,
      contracts,
      expectedHeaderHashes: [setup.headerHash],
    });
  }, 180_000);
});
