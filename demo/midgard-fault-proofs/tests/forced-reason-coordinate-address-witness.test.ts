import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardForcedTxCanonical,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxProofSource,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { InvalidSignatureContracts } from "../src/invalid-signature/contracts.js";
import { submitInvalidSignatureStep01Forced } from "../src/invalid-signature/submit.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { submitInit } from "../src/submit-init.js";
import { submitInvalidSignatureStep02 } from "../src/submit-invalid-signature-step-02.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import {
  buildInvalidSignatureBlockFixture,
  honestAddressWitness,
  invalidAddressWitness,
  type InvalidSignatureSubject,
  submitRawInvalidSignatureStep02,
} from "./support/invalid-signature-emulator.js";
import { network } from "./support/invalid-signature-wrongful-emulator.network.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

/**
 * A forced AddressWitnessSignatureInvalid reason names a field-7 witness by
 * its position, and invalidSignature reopens exactly that witness. Witness 0
 * signs the transaction; witness 1 carries a signature that does not verify.
 * The verdict is the one the node's classifier writes, so the suite fails if
 * the writer and the proof disagree on how witnesses are counted: one position
 * early names a witness that verifies and convicts; the written position is
 * refused on chain.
 */

const withWitnesses = (addrTxWits: readonly SDK.MidgardAddressWitness[]) =>
  makeNativeTx({
    spendInputCbors: [
      encodeMidgardSpendInputItem({
        txId: Buffer.alloc(32, 0x5b),
        outputIndex: 0,
      }),
    ],
    fee: 13n,
    addrTxWitsPreimageCbor: SDK.encodeAddressWitnessPreimage(addrTxWits),
  });
const signedSubject = await (async (): Promise<InvalidSignatureSubject> => {
  const txId = computeMidgardNativeTxId(withWitnesses([])).toString("hex");
  const addrTxWits = [
    honestAddressWitness({ index: 0, txId }),
    invalidAddressWitness(1),
  ];
  const nativeTx = withWitnesses(addrTxWits);
  const witnessSet = deriveMidgardNativeTxWitnessSetCompact(
    nativeTx.witnessSet,
  );
  return {
    ...(await buildInvalidSignatureBlockFixture(nativeTx)),
    nativeTx,
    addrTxWits,
    witnessSetCompact: {
      addr_tx_wits_hash: witnessSet.addrTxWitsHash.toString("hex"),
      script_tx_wits_hash: witnessSet.scriptTxWitsHash.toString("hex"),
      redeemer_tx_wits_hash: witnessSet.redeemerTxWitsHash.toString("hex"),
    },
    badAddrTxWitIndex: 1n,
  };
})();
const adjudicated = materializeMidgardForcedTxFromCanonical(
  signedSubject.nativeTx,
);
const nativeTxId = computeMidgardNativeTxId(adjudicated).toString("hex");

const writtenWitnessIndex = async (): Promise<bigint> => {
  const verdict = await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(adjudicated),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(adjudicated),
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: {
      reason: { AddressWitnessSignatureInvalid: { witness_index: 1n } },
    },
  });
  return 1n;
};

/**
 * One block whose single forced leaf rejects the transaction with
 * `AddressWitnessSignatureInvalid { witnessIndex }`, a thread bound to that
 * leaf, and the terminal and removal runners over it.
 */
const setupScenario = async (witnessIndex: bigint) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: { realInvalidSignature: true },
  });
  const chain = harness.contracts.fraudProofContracts.invalidSignature;
  const references = [
    harness.faultProofReferenceScripts.fraudProofInvalidSignature!.utxo,
    harness.faultProofReferenceScripts.fraudProofInvalidSignatureStep02!.utxo,
  ] as const;
  const contracts: InvalidSignatureContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle:
        index === 0
          ? "fraud_proofs/invalid_range/step_01.main.spend"
          : "fraud_proofs/invalid_range/step_02.main.spend",
      referenceOutRef: `${references[index].txHash}#${references[index].outputIndex}`,
    })) as unknown as InvalidSignatureContracts["steps"],
    computationThread: harness.contracts.computationThread,
    fraudProof: harness.contracts.fraudProof,
  };
  const catalogue = await buildCatalogueDeploymentInfo(
    harness.contracts.fraudProofs,
  );
  const deploymentInfo = buildRemovalDeploymentInfo(
    harness.contracts,
    catalogue,
  );
  const credential = getAddressDetails(
    await harness.funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential?.type !== "Key") throw new Error("operator key absent");
  const base = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: credential.hash,
    now:
      alignUnixTimeToEmulatorSlotBoundary(
        harness.funderLucid,
        harness.emulator.now() + 120_000,
      ) - 1,
  });
  const source = deriveMidgardForcedTxProofSource(adjudicated);
  const reason = {
    AddressWitnessSignatureInvalid: { witness_index: witnessIndex },
  };
  const leaf = {
    tx_id: nativeTxId,
    submitted_source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: { ForcedTxInvalid: { reason } },
  } as const;
  const key = base.eventKey.ForcedTransactionEventKey.tx_order_id;
  const keyBytes = Buffer.from(Data.to(key, SDK.OutputReference), "hex");
  const valueBytes = Buffer.from(
    Data.to(leaf as never, SDK.ForcedInclusionTxV1Schema as never),
    "hex",
  );
  const root = await buildCountedRoot(SDK.ROOT_DOMAINS.forcedTransactionsV1, [
    { key: keyBytes, value: valueBytes },
  ]);
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  await trie.insert(keyBytes, valueBytes);
  const membership = {
    domain: root.domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    key,
    value: leaf,
    proof: Data.from(
      (await trie.prove(keyBytes)).toCBOR().toString("hex"),
      SDK.Proof,
    ),
  };
  const header = {
    ...base.header,
    blockSlot: 10n,
    forcedTransactionsRoot: root.root,
    forcedTransactionCount: 1n,
  };
  const setup = await submitSetupTx({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    nonceUtxo: harness.nonceUtxo,
    catalogue,
    header,
  });
  const witnessSet = signedSubject.witnessSetCompact;
  const init = await submitInit({
    lucid: harness.proverLucid,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    fraudCategory: "invalidSignature",
    fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
    awaitConfirmation: true,
  });
  const bound = await submitInvalidSignatureStep01Forced({
    lucid: harness.proverLucid,
    contracts,
    categoryId: catalogue.categories.invalidSignature.categoryId,
    signer: harness.proverSigner,
    threadOutRef: `${init.txHash}#${init.firstStepOutputIndex}`,
    evidence: {
      subject: SDK.forcedVerdictSubject({
        transactionId: nativeTxId,
        sourceKey: key,
        rejectionReason: reason,
      }),
      witnessIndex,
      witnessSetHash:
        adjudicated.compact.transactionWitnessSetHash.toString("hex"),
      witnessSet,
      addressWitnesses: signedSubject.addrTxWits,
      nativeTxCompactCbor: leaf.submitted_source.compact_cbor,
    },
    forcedSource: { header, membership, direction: 1n },
    referenceScriptUtxo: references[0],
  });
  const threadOutRef = bound.nextThreadOutRef;
  const finalize = () =>
    submitInvalidSignatureStep02({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo,
      network,
      signer: harness.proverSigner,
      threadOutRef,
      nativeTxCompactCbor: leaf.submitted_source.compact_cbor,
      witnessSetCompact: witnessSet,
      addrTxWitsPreimage: signedSubject.addrTxWits,
      badAddrTxWitIndex: witnessIndex,
      certificatePolicyId: harness.contracts.fieldPreimageCertificate.policyId,
      certificateUtxos: [],
      existingPublicationUtxos: [],
      publishMissingCarriage: false,
      referenceScriptUtxo: references[1],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
  /** The same terminal without the builder's local signature check. */
  const rawFinalize = () =>
    submitRawInvalidSignatureStep02({
      harness,
      deploymentInfo,
      threadOutRef,
      subject: {
        ...signedSubject,
        nativeTxCompactCbor: leaf.submitted_source.compact_cbor,
      },
      referenceScriptUtxo: references[1],
      badAddrTxWitIndex: witnessIndex,
    });
  const remove = async () => {
    const removal = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const now = BigInt(harness.emulator.now());
    return submitRemoveFraudulentBlock({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(harness.contracts, catalogue, {
        removalReferenceScripts: removal.published,
      }),
      network,
      signer: harness.proverSigner,
      fraudCategory: "invalidSignature",
      fraudulentHeaderHash: setup.headerHash,
      awaitConfirmation: true,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => ({
          token: "invalid-signature-coordinate-lease",
          source: "emulator",
          renew: async () => {},
          release: async () => {},
          fail: async () => {},
        }),
      },
      validFrom: now > 120_000n ? now - 120_000n : 0n,
      validTo: now + 300_000n,
    });
  };
  return { finalize, rawFinalize, remove };
};

describe("forced AddressWitnessSignatureInvalid coordinate the node writes", () => {
  it("convicts a coordinate one position early, where the witness verifies", async () => {
    const s = await setupScenario((await writtenWitnessIndex()) - 1n);
    expect((await s.finalize()).fraudProofUnit).toBeTruthy();
    await s.remove();
  }, 600_000);

  it("refuses the written coordinate on chain", async () => {
    const s = await setupScenario(await writtenWitnessIndex());
    // Witness 1's signature does not verify, as the leaf says. Step 02
    // authenticates the thread, the opened field and the exact reason, and
    // the terminal rule, which convicts a forced rejection only when the
    // named witness verifies, returns false. A verbose-traced step 02 is
    // larger than the emulator's reference-script publication target, so
    // reading this trace needs that target lifted for the run.
    await expectOnchainRefusal(
      () => s.rawFinalize(),
      /^Validator returned false$/u,
    );
  }, 600_000);
});
