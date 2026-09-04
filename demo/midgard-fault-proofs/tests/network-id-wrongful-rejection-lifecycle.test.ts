import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  encodeMidgardFieldPreimage,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  ForcedInclusionTxV1Schema,
  forcedVerdictSubject,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  planNetworkIdOutputsOpening,
  type PreparedNetworkIdWrongfulRejection,
  submitNetworkIdCancel,
  submitNetworkIdForcedStep01,
  submitNetworkIdInit,
  submitNetworkIdStep02,
} from "../src/network-id/index.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import {
  makeNetworkIdEmulatorHarness,
  publishNetworkIdReferenceScripts,
} from "./support/network-id-emulator.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;

describe("networkId wrongful-rejection real lifecycle", () => {
  it("runs Init, cancels every step, restarts by out-ref, mints, and removes", async () => {
    const harness = await makeNetworkIdEmulatorHarness();
    const credential = getAddressDetails(
      await harness.funderLucid.wallet().address(),
    ).paymentCredential;
    if (credential?.type !== "Key") throw new Error("funder key absent");
    const base = await buildInvalidForcedTransitionTraceFixture({
      operatorVkey: credential.hash,
      now:
        alignUnixTimeToEmulatorSlotBoundary(
          harness.funderLucid,
          harness.emulator.now() + 120_000,
        ) - 1,
    });
    const input = encodeMidgardSpendInputItem({
      txId: Buffer.from("77".repeat(32), "hex"),
      outputIndex: 0,
    });
    const output = encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x44)]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    });
    const invalid = adjudicateMidgardNativeTxFullValidity(
      makeNativeTx({
        spendInputCbors: [input],
        outputCbors: [output],
        fee: 0n,
      }),
      "TxIsInvalid",
    );
    const transactionId = computeMidgardNativeTxId(invalid).toString("hex");
    const proofSource = deriveMidgardNativeTxProofSource(invalid);
    const forcedTransaction = {
      tx_id: transactionId,
      source: {
        compact_cbor: proofSource.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          proofSource.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          proofSource.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: { ForcedTxInvalid: { reason: "NetworkIdMismatch" } },
    } as const;
    const sourceKey = base.eventKey.ForcedTransactionEventKey.tx_order_id;
    const keyBytes = Buffer.from(Data.to(sourceKey, OutputReference), "hex");
    const valueBytes = Buffer.from(
      Data.to(forcedTransaction as never, ForcedInclusionTxV1Schema as never),
      "hex",
    );
    const root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
      { key: keyBytes, value: valueBytes },
    ]);
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    await trie.insert(keyBytes, valueBytes);
    const membershipProof = await trie.prove(keyBytes);
    const membership = {
      domain: root.domain,
      root: root.root,
      phas_root: root.phasRoot,
      count: root.count,
      key: sourceKey,
      value: forcedTransaction,
      proof: Data.from(membershipProof.toCBOR().toString("hex"), Proof),
    };
    const header = { ...base.header, forcedTransactionsRoot: root.root };
    console.info("[network-id-forced-header]", {
      forcedTransactionCount: header.forcedTransactionCount.toString(),
      rootCount: root.count.toString(),
      root: root.root,
    });
    const setup = await submitSetupTx({
      lucid: harness.funderLucid,
      contracts: harness.contracts,
      nonceUtxo: harness.nonceUtxo,
      catalogue: harness.catalogue,
      header,
    });
    const subject = forcedVerdictSubject({
      transactionId,
      sourceKey,
      rejectionReason: "NetworkIdMismatch",
    });
    const prepared: PreparedNetworkIdWrongfulRejection = {
      headerHash: setup.headerHash,
      expectedNetworkId: 0n,
      badTxId: transactionId,
      nativeTxCompactCbor: proofSource.compactCbor.toString("hex"),
      outputsItemCbors: [output.toString("hex")],
      faultClaim: { kind: "forced-network-mismatch" },
      fault: "ForcedNetworkIdMismatch",
      subject,
      forcedSource: { header, membership, direction: 1n },
      evidence: {
        subject,
        expectedNetworkId: 0n,
        committedNetworkId: 0n,
        outputNetworkIds: [0n],
        outputsItemCbors: [output.toString("hex")],
        outputsPreimageCbor: encodeMidgardFieldPreimage([output]).toString(
          "hex",
        ),
      },
    };
    const [step01Ref, step02Ref] = await publishNetworkIdReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.networkId,
    });
    const captures: Awaited<ReturnType<typeof captureEmulatorSubmission>>[] =
      [];
    const initialize = async () => {
      console.info("[network-id-forced-stage] init");
      const captured = await captureEmulatorSubmission(harness.emulator, () =>
        submitNetworkIdInit({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts: harness.networkId,
          category: harness.category,
          catalogue: {
            policyId: harness.contracts.fraudProofCatalogue.policyId,
            spendingScriptAddress:
              harness.contracts.fraudProofCatalogue.spendingScriptAddress,
            root: harness.catalogue.root,
          },
          signer: harness.proverSigner,
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      captures.push(captured);
      return `${captured.result.txHash}#${captured.result.firstStepOutputIndex.toString()}`;
    };
    const bind = async (threadOutRef: string) => {
      console.info("[network-id-forced-stage] bind");
      const captured = await captureEmulatorSubmission(harness.emulator, () =>
        submitNetworkIdForcedStep01({
          lucid: harness.proverLucid,
          contracts: harness.networkId,
          categoryId: harness.category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          prepared,
          referenceScriptUtxo: step01Ref,
        }),
      );
      captures.push(captured);
      return captured.result.nextThreadOutRef;
    };
    const cancel = async (
      threadOutRef: string,
      step: 0 | 1,
      reference: UTxO,
    ) => {
      console.info(`[network-id-forced-stage] cancel-${step.toString()}`);
      captures.push(
        await captureEmulatorSubmission(harness.emulator, () =>
          submitNetworkIdCancel({
            lucid: harness.proverLucid,
            contracts: harness.networkId,
            categoryId: harness.category.categoryId,
            signer: harness.proverSigner,
            threadOutRef,
            referenceScriptUtxo: reference,
            witnessReferenceScripts: harness.witnessReferenceScripts,
          }),
        ),
      );
      expect(
        (captures.at(-1)!.result as { readonly cancelledStepIndex: number })
          .cancelledStepIndex,
      ).toBe(step);
    };
    await cancel(await initialize(), 0, step01Ref);
    await cancel(await bind(await initialize()), 1, step02Ref);
    const secondOutRef = await bind(await initialize());
    const opening = planNetworkIdOutputsOpening({
      prepared,
      owner: harness.proverSigner.paymentKeyHash,
      publish: true,
    });
    console.info("[network-id-forced-stage] finalize");
    const final = await captureEmulatorSubmission(harness.emulator, () =>
      submitNetworkIdStep02({
        lucid: harness.proverLucid,
        contracts: harness.networkId,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: secondOutRef,
        prepared,
        outputsOpeningPlan: opening,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    captures.push(final);
    const removalRefs = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const deploymentInfo = buildRemovalDeploymentInfo(
      harness.contracts,
      harness.catalogue,
      { removalReferenceScripts: removalRefs.published },
    );
    const now = BigInt(harness.emulator.now());
    captures.push(
      await captureEmulatorSubmission(harness.emulator, () =>
        submitRemoveFraudulentBlock({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo,
          network,
          signer: harness.proverSigner,
          fraudCategory: "networkId",
          fraudulentHeaderHash: setup.headerHash,
          requireReferenceScripts: true,
          validFrom: now > 120_000n ? now - 120_000n : 0n,
          validTo: now + 300_000n,
        }),
      ),
    );
    for (const { measurement } of captures) {
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(16_384);
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(measurement.executionMemory).toBeLessThanOrEqual(16_500_000n);
      expect(measurement.executionSteps).toBeLessThanOrEqual(10_000_000_000n);
    }
    expect(final.result.fraudProofUnit).toBeTruthy();
    console.info(
      `[network-id-forced-lifecycle] ${JSON.stringify(captures.map(({ measurement }) => ({ bytes: measurement.completeSignedBytes, memory: measurement.executionMemory.toString(), cpu: measurement.executionSteps.toString() })))}`,
    );
  }, 600_000);
});
