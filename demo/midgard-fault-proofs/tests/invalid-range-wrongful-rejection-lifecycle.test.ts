import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullValidity,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
} from "@al-ft/midgard-core";
import {
  ForcedInclusionTxSchema,
  forcedVerdictSubject,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, getAddressDetails } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { InvalidRangeContracts } from "../src/invalid-range/contracts-v1.js";
import { prepareInvalidRangeEvidence } from "../src/invalid-range/family-v1.js";
import {
  submitInvalidRangeStep01Forced,
  submitInvalidRangeStep02V1,
} from "../src/invalid-range/submit-v1.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { submitInit } from "../src/submit-init.js";
import { nativeTxFromCoreCompact } from "../src/submit-step-01.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  admitNativeInclusionTwoStepArtifact,
  NATIVE_INCLUSION_TWO_STEP_ARTIFACT,
} from "../src/workflow/production-native-inclusion-two-step-v1.js";
import { submitZeroInputCancel } from "../src/zero-input/submit-cancel-v1.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import { publishRemovalReferenceScripts } from "./support/submit-init-emulator-shared.js";

const network = "Custom" as const;
describe("invalidRange wrongful-rejection real lifecycle", () => {
  it("runs every cancel/restart boundary, forced contradiction, permanent mint, and leased removal", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realInvalidRange: true,
        alwaysFraudProofCatalogue: true,
        alwaysStateQueue: true,
      },
    });
    const chain = harness.contracts.fraudProofContracts.invalidRange;
    const references = [
      harness.faultProofReferenceScripts.fraudProofInvalidRange!.utxo,
      harness.faultProofReferenceScripts.fraudProofInvalidRangeStep02!.utxo,
    ] as const;
    const contracts: InvalidRangeContracts = {
      steps: chain.steps.map((step, index) => ({
        ...step,
        blueprintTitle:
          index === 0
            ? "fraud_proofs/invalid_range/step_01.main.spend"
            : "fraud_proofs/invalid_range/step_02.main.spend",
        referenceOutRef: `${references[index].txHash}#${references[index].outputIndex}`,
      })) as unknown as InvalidRangeContracts["steps"],
      computationThread: harness.contracts.computationThread,
      fraudProof: harness.contracts.fraudProof,
      hubOraclePolicyId: harness.contracts.hubOracle.policyId,
      stateQueuePolicyId: harness.contracts.stateQueue.policyId,
    };
    const catalogue = await buildCatalogueDeploymentInfo(
      harness.contracts.fraudProofs,
    );
    const category = catalogue.categories.invalidRange;
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
    const invalid = adjudicateMidgardNativeTxFullValidity(
      makeNativeTx({
        spendInputCbors: [],
        fee: 0n,
        validityIntervalStart: 5n,
        validityIntervalEnd: 20n,
      }),
      "TxIsInvalid",
    );
    const transactionId = computeMidgardNativeTxId(invalid).toString("hex");
    const source = deriveMidgardNativeTxProofSource(invalid);
    const leaf = {
      tx_id: transactionId,
      source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: {
        ForcedTxInvalid: { reason: "ValidityIntervalExcludesBlockSlot" },
      },
    } as const;
    const key = base.eventKey.ForcedTransactionEventKey.tx_order_id;
    const keyBytes = Buffer.from(Data.to(key, OutputReference), "hex");
    const valueBytes = Buffer.from(
      Data.to(leaf as never, ForcedInclusionTxSchema as never),
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
      key,
      value: leaf,
      proof: Data.from(membershipProof.toCBOR().toString("hex"), Proof),
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
    const evidence = prepareInvalidRangeEvidence({
      subject: forcedVerdictSubject({
        transactionId,
        sourceKey: key,
        rejectionReason: "ValidityIntervalExcludesBlockSlot",
      }),
      blockSlot: header.blockSlot,
      txBody: nativeTxFromCoreCompact(invalid.compact).body,
    });
    const admitted = admitNativeInclusionTwoStepArtifact({
      schemaVersion: NATIVE_INCLUSION_TWO_STEP_ARTIFACT,
      category: "invalidRange",
      headerHash: setup.headerHash,
      detectionId: `invalid-range:forced:0:${transactionId}:ValidityIntervalExcludesBlockSlot`,
      position: 0,
      blockSlot: header.blockSlot.toString(),
      violationReason: "ValidityIntervalExcludesBlockSlot",
      nativeTxId: transactionId,
      nativeTxCompactCbor: leaf.source.compact_cbor,
      l2TransactionSourceCbor: Data.to(
        { tx_id: transactionId, source: leaf.source } as never,
        SDK.L2TransactionSource as never,
      ),
      transactionsPhasRoot: "00".repeat(32),
      txMembershipProofCbor: "",
      sourceKind: "forced",
      subjectCbor: Data.to(
        evidence.subject as never,
        SDK.InvalidRangeVerdictSubjectSchema as never,
      ),
      inputFieldPreimageCbor: "",
      inputFieldCommitment: "00".repeat(32),
      forcedSourceCbor: Data.to(
        { header, membership, direction: 1n } as never,
        SDK.InvalidRangeForcedSourcePayloadSchema as never,
      ),
    });
    expect(admitted.invalidRangeEvidence).toEqual(evidence);
    const captures: Awaited<ReturnType<typeof captureEmulatorSubmission>>[] =
      [];
    const initialize = async () => {
      const c = await captureEmulatorSubmission(harness.emulator, () =>
        submitInit({
          lucid: harness.proverLucid,
          witnessReferenceScripts: harness.witnessReferenceScripts,
          blueprint: harness.realBlueprint,
          deploymentInfo: buildRemovalDeploymentInfo(
            harness.contracts,
            catalogue,
          ),
          network,
          signer: harness.proverSigner,
          fraudCategory: "invalidRange",
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
          awaitConfirmation: true,
        }),
      );
      captures.push(c);
      return `${c.result.txHash}#${c.result.firstStepOutputIndex}`;
    };
    const bind = async (threadOutRef: string) => {
      const c = await captureEmulatorSubmission(harness.emulator, () =>
        submitInvalidRangeStep01Forced({
          lucid: harness.proverLucid,
          contracts,
          categoryId: category.categoryId,
          signer: harness.proverSigner,
          threadOutRef,
          evidence,
          forcedSource: { header, membership, direction: 1n },
          referenceScriptUtxo: references[0],
        }),
      );
      captures.push(c);
      return c.result.nextThreadOutRef;
    };
    const cancel = async (threadOutRef: string, step: 0 | 1) =>
      captures.push(
        await captureEmulatorSubmission(harness.emulator, () =>
          submitZeroInputCancel({
            lucid: harness.proverLucid,
            contracts: contracts as never,
            categoryId: category.categoryId,
            signer: harness.proverSigner,
            threadOutRef,
            referenceScriptUtxo: references[step],
            witnessReferenceScripts: harness.witnessReferenceScripts,
          }),
        ),
      );
    await cancel(await initialize(), 0);
    await cancel(await bind(await initialize()), 1);
    const terminalOutRef = await bind(await initialize());
    const final = await captureEmulatorSubmission(harness.emulator, () =>
      submitInvalidRangeStep02V1({
        lucid: harness.proverLucid,
        contracts,
        categoryId: category.categoryId,
        signer: harness.proverSigner,
        threadOutRef: terminalOutRef,
        evidence,
        referenceScriptUtxo: references[1],
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    captures.push(final);
    expect(final.result.fraudProofUnit).toBeTruthy();
    const removalRefs = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const now = BigInt(harness.emulator.now());
    captures.push(
      await captureEmulatorSubmission(harness.emulator, () =>
        submitRemoveFraudulentBlock({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo: buildRemovalDeploymentInfo(
            harness.contracts,
            catalogue,
            { removalReferenceScripts: removalRefs.published },
          ),
          network,
          signer: harness.proverSigner,
          fraudCategory: "invalidRange",
          fraudulentHeaderHash: setup.headerHash,
          awaitConfirmation: true,
          requireReferenceScripts: true,
          stateQueueMutationLeaseCoordinator: {
            acquire: async () => ({
              token: "invalid-range-lease",
              source: "emulator",
              renew: async () => {},
              release: async () => {},
              fail: async () => {},
            }),
          },
          validFrom: now > 120_000n ? now - 120_000n : 0n,
          validTo: now + 300_000n,
        }),
      ),
    );
    for (const { measurement } of captures) {
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
      expect(measurement.executionMemory).toBeLessThanOrEqual(16_500_000n);
      expect(measurement.executionSteps).toBeLessThanOrEqual(10_000_000_000n);
    }
    console.info(
      `[invalid-range-forced-lifecycle] ${JSON.stringify(captures.map(({ measurement }) => ({ bytes: measurement.completeSignedBytes, memory: measurement.executionMemory.toString(), cpu: measurement.executionSteps.toString() })))}`,
    );
  }, 600_000);
});
