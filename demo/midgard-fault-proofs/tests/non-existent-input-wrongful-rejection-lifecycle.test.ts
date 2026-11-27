import {
  computeHash28,
  computeMidgardNativeTxId,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation/ledger-output-descriptor";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  admitNonExistentInputForcedArtifact,
  nonExistentInputForcedArtifact,
} from "../src/non-existent-input/artifact.js";
import {
  nonExistentInputForcedFieldPlan,
  submitNonExistentInputForcedStep,
} from "../src/non-existent-input/submit.js";
import {
  nonExistentInputForcedSourceMaterial,
  type PreparedNonExistentInputWrongfulRejection,
} from "../src/non-existent-input/wrongful-rejection.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.submit-remove-fraudulent-block.js";
import { buildInit } from "../src/submit-init.submit-init.js";
import {
  buildCountedRoot,
  commitCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { withCommitSlot } from "./support/emulator/header-fixtures.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { publishRemovalReferenceScripts } from "./support/emulator/reference-scripts.publish-fault-proof-witness-reference-scripts.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.publish-operator-lifecycle-reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.build-removal-deployment-info.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { deepProofChunks } from "./support/input-existence-deep-chunks.js";
import { submitFamilyCancel } from "./support/linear-fault-cancel.js";
import { submitRawNonExistentInputForcedStep } from "./support/non-existent-input-raw-step.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init/forced-transitions.js";
import {
  buildAndSubmit,
  certifyFieldCarriage,
  publishFieldCarriage,
} from "./support/submit-steps.js";
import { syntheticDeepMembershipProof } from "./support/synthetic-deep-proof.js";

describe("nonExistentInput wrongful rejection registered lifecycle", () => {
  it.each([
    { count: 1, index: 0n, deep: false },
    { count: 1, index: -1n, deep: false },
    { count: 1, index: 1n, deep: false },
    { count: 819, index: 818n, deep: true },
    { count: 1, index: 0n, deep: false, honest: true },
  ])(
    "proves $count inputs at $index deep=$deep",
    async (scenario) => {
      const { count, index, deep } = scenario;
      const honest = "honest" in scenario;
      const harness = await makeFaultProofEmulatorHarness({
        contractOptions: {
          realNonExistentInput: true,
          alwaysFraudProofCatalogue: true,
        },
      });
      const contracts = {
        hubOraclePolicyId: harness.contracts.hubOracle.policyId,
        steps: harness.contracts.fraudProofContracts.nonExistentInput.steps,
        computationThread: harness.contracts.computationThread,
        fraudProof: harness.contracts.fraudProof,
      };
      const catalogue = await buildCatalogueDeploymentInfo(
        harness.contracts.fraudProofs,
      );
      const categoryId = catalogue.categories.nonExistentInput.categoryId;
      const captures = async <T>(name: string, action: () => Promise<T>) => {
        const captured = await captureEmulatorSubmission(
          harness.emulator,
          action,
        ).catch((error) => {
          throw new Error(`stage ${name}: ${String(error)}`);
        });
        return captured.result;
      };
      const references: UTxO[] = [];
      for (const [stepIndex, step] of contracts.steps.entries())
        references.push(
          (
            await captures(`publish-step-${stepIndex}`, () =>
              publishPlainReferenceScriptUtxo({
                lucid: harness.proverLucid,
                script: step.spendingScript,
                label: `no-input step ${stepIndex}`,
              }),
            )
          ).utxo,
        );
      const referenceScripts = {
        steps: references,
        computationThreadMint:
          harness.witnessReferenceScripts.computationThreadMint!,
        fraudProofMint: harness.witnessReferenceScripts.fraudProofMint!,
      };
      const credential = getAddressDetails(
        await harness.funderLucid.wallet().address(),
      ).paymentCredential;
      if (credential?.type !== "Key") throw new Error("operator absent");
      const base = await buildInvalidForcedTransitionTraceFixture({
        operatorVkey: credential.hash,
        now:
          alignUnixTimeToEmulatorSlotBoundary(
            harness.funderLucid,
            harness.emulator.now() + 120_000,
          ) - 1,
      });
      const inputs = Array.from({ length: count }, (_, i) =>
        encodeMidgardSpendInputItem({
          txId: Buffer.alloc(32, 0x55),
          outputIndex: i,
        }),
      );
      const native = materializeMidgardForcedTxFromCanonical(
        makeNativeTx({ spendInputCbors: inputs, fee: 0n }),
      );
      const source = deriveMidgardForcedTxProofSource(native);
      const leaf = {
        tx_id: computeMidgardNativeTxId(native).toString("hex"),
        submitted_source: {
          compact_cbor: source.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            source.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            source.fieldPreimageLengthsCbor.toString("hex"),
        },
        verdict: {
          ForcedTxInvalid: {
            reason: { InputNotFound: { source_kind: 0n, input_index: index } },
          },
        },
      } as const;
      const eventKey = base.eventKey;
      const forcedKey = eventKey.ForcedTransactionEventKey.tx_order_id;
      const target = inputs[Math.max(0, Math.min(count - 1, Number(index)))]!;
      const descriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: target,
        outputCbor: Buffer.from(
          "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
          "hex",
        ),
      }).descriptorCbor;
      const ledger = await keyValuePhasRootWithCount([
        { key: target, value: descriptor },
      ]);
      const deepLedger = deep
        ? syntheticDeepMembershipProof({
            key: target,
            value: descriptor,
            branchLevels: 64,
          })
        : null;
      const ledgerRoot = honest
        ? SDK.EMPTY_MERKLE_TREE_ROOT
        : (deepLedger?.transactionsPhasRoot ?? ledger.root);
      const membership = async <K, V>(
        domain: SDK.RootDomain,
        key: K,
        value: V,
        keyCbor: string,
        valueCbor: string,
      ): Promise<SDK.RootMembershipProof<K, V>> => {
        const keyBytes = Buffer.from(keyCbor, "hex"),
          valueBytes = Buffer.from(valueCbor, "hex");
        const root = await buildCountedRoot(domain, [
          { key: keyBytes, value: valueBytes },
        ]);
        const ladder = deep
          ? syntheticDeepMembershipProof({
              key: keyBytes,
              value: valueBytes,
              branchLevels: 64,
            })
          : null;
        return {
          domain,
          root:
            ladder === null
              ? root.root
              : await commitCountedRoot({
                  domain,
                  phasRoot: ladder.transactionsPhasRoot,
                  count: 1n,
                }),
          phas_root: ladder?.transactionsPhasRoot ?? root.phasRoot,
          count: 1n,
          key,
          value,
          proof:
            ladder === null
              ? await keyValuePhasProof(
                  { ...root, root: root.phasRoot },
                  keyBytes,
                  valueBytes,
                )
              : Data.from(ladder.proofCbor, SDK.Proof),
        };
      };
      const forcedMembership = await membership(
        SDK.ROOT_DOMAINS.forcedTransactionsV1,
        forcedKey,
        leaf,
        Data.to(forcedKey, SDK.OutputReference),
        Data.to(leaf, SDK.ForcedInclusionTxV1),
      );
      const eventValue = {
        step_index: 0n,
        phase: "ForcedTransaction" as const,
      };
      const eventMembership = await membership(
        SDK.ROOT_DOMAINS.eventToStep,
        eventKey,
        eventValue,
        Data.to(eventKey, SDK.EventKey),
        Data.to(eventValue, SDK.EventToStepValue),
      );
      const transition: SDK.TransitionStep = {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "ForcedTransaction",
        pre_utxos_root: ledgerRoot,
        post_utxos_root: ledgerRoot,
      };
      const transitionMembership = await membership(
        SDK.ROOT_DOMAINS.transitionTrace,
        0n,
        transition,
        Data.to(0n),
        Data.to(transition, SDK.TransitionStep),
      );
      const header = withCommitSlot(harness.contracts.l1SlotConfig, {
        ...base.header,
        forcedTransactionsRoot: forcedMembership.root,
        eventToStepRoot: eventMembership.root,
        transitionTraceRoot: transitionMembership.root,
        utxosRoot: ledgerRoot,
      });
      const forcedSource = {
        header,
        membership: forcedMembership,
      };
      const fullTransactionCbor =
        encodeMidgardForcedTxCanonical(native).toString("hex");
      const material = nonExistentInputForcedSourceMaterial(
        forcedSource,
        fullTransactionCbor,
      );
      const prepared: PreparedNonExistentInputWrongfulRejection = {
        headerHash: computeHash28(SDK.encodeHeaderCbor(header)).toString("hex"),
        forcedSource,
        fullTransactionCbor,
        ...material,
        eventMembership,
        transitionMembership,
        ledgerMembership:
          material.selectedInput === null
            ? null
            : {
                value: descriptor.toString("hex"),
                proof:
                  deepLedger === null
                    ? await keyValuePhasProof(ledger, target, descriptor)
                    : Data.from(deepLedger.proofCbor, SDK.Proof),
              },
      };
      const artifact = nonExistentInputForcedArtifact(prepared);
      if (honest)
        await expect(
          admitNonExistentInputForcedArtifact(artifact),
        ).rejects.toThrow(/root/);
      else
        expect(
          await admitNonExistentInputForcedArtifact(
            JSON.parse(JSON.stringify(artifact)),
          ),
        ).toEqual(prepared);
      for (const changed of [
        { ...artifact, headerHash: "00".repeat(28) },
        { ...artifact, transaction: "00" },
        {
          ...artifact,
          event: Data.to(
            {
              ...eventMembership,
              key: {
                ForcedTransactionEventKey: {
                  tx_order_id: {
                    ...forcedKey,
                    outputIndex: forcedKey.outputIndex + 1n,
                  },
                },
              },
            },
            SDK.EventToStepMembershipProof,
          ),
        },
        {
          ...artifact,
          transition: Data.to(
            {
              ...transitionMembership,
              value: { ...transition, pre_utxos_root: "11".repeat(32) },
            },
            SDK.IndexedTraceProof,
          ),
        },
        {
          ...artifact,
          source: Data.to(
            {
              ...forcedSource,
              membership: {
                ...forcedMembership,
                value: {
                  ...leaf,
                  verdict: {
                    ForcedTxInvalid: {
                      reason: {
                        InputNotFound: { source_kind: 1n, input_index: index },
                      },
                    },
                  },
                },
              },
            } as never,
            SDKFraudProof.NonExistentInputForcedSourcePayloadSchema as never,
          ),
        },
      ])
        await expect(
          admitNonExistentInputForcedArtifact(changed),
        ).rejects.toThrow();
      const setup = await submitSetupTx({
        lucid: harness.funderLucid,
        contracts: harness.contracts,
        nonceUtxo: harness.nonceUtxo,
        catalogue,
        header,
      });
      const plan = nonExistentInputForcedFieldPlan(
        prepared,
        harness.proverSigner.paymentKeyHash,
      );
      if (deep) {
        expect(plan.plan.tier).toBe("Certified");
        expect(native.body.spendInputsPreimageCbor.length).toBe(32763);
      }
      const chunks =
        plan.plan.tier === "Inline"
          ? []
          : await captures("field-publications", () =>
              publishFieldCarriage({
                lucid: harness.proverLucid,
                signer: harness.proverSigner,
                planned: plan,
                publisherAddress: harness.proverSigner.proofMaterialsAddress,
                label: "no-input field",
              }),
            );
      let certificates: typeof references = [];
      if (plan.plan.tier === "Certified") {
        const reference = await publishPlainReferenceScriptUtxo({
          lucid: harness.proverLucid,
          script: harness.contracts.fieldPreimageCertificate.mintingScript,
          label: "no-input certificate",
        });
        const certified = await captures("field-certification", () =>
          certifyFieldCarriage({
            lucid: harness.proverLucid,
            network: "Custom",
            signer: harness.proverSigner,
            planned: plan,
            certificatePolicyId:
              harness.contracts.fieldPreimageCertificate.policyId,
            certificateMintingScript:
              harness.contracts.fieldPreimageCertificate.mintingScript,
            certificateReferenceScriptUtxo: reference.utxo,
            chunkUtxos: chunks,
            compactCbor: leaf.submitted_source.compact_cbor,
            witnessSetCompactCbor:
              leaf.submitted_source.witness_set_compact_cbor,
          }),
        );
        certificates = [certified.certificateUtxo];
      }
      const common = {
        blueprint: harness.realBlueprint,
        network: "Custom" as const,
        witnessReferenceScripts: harness.witnessReferenceScripts,
        lucid: harness.proverLucid,
        contracts,
        categoryId,
        signer: harness.proverSigner,
      };
      const init = async (label: string) => {
        const result = await captures(label, () =>
          buildAndSubmit(buildInit, {
            lucid: harness.proverLucid,
            witnessReferenceScripts: harness.witnessReferenceScripts,
            blueprint: harness.realBlueprint,
            deploymentInfo: buildRemovalDeploymentInfo(
              harness.contracts,
              catalogue,
            ),
            network: "Custom",
            signer: harness.proverSigner,
            fraudCategory: "nonExistentInput",
            fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
            awaitConfirmation: true,
          }),
        );
        return `${result.txHash}#${result.firstStepOutputIndex}`;
      };
      if (honest) {
        let thread = await init("honest-init");
        for (const stepIndex of [0, 1, 2] as const)
          thread = (
            await captures(`honest-step-${stepIndex}`, () =>
              submitRawNonExistentInputForcedStep({
                ...common,
                threadOutRef: thread,
                prepared,
                stepIndex,
                references: referenceScripts,
              }),
            )
          ).nextThreadOutRef;
        await expect(
          submitRawNonExistentInputForcedStep({
            ...common,
            threadOutRef: thread,
            prepared,
            stepIndex: 3,
            references: referenceScripts,
          }),
        ).rejects.toThrow(/script execution|validator|eval/i);
        await captures("honest-cancel", () =>
          submitFamilyCancel("non-existent-input", {
            lucid: harness.proverLucid,
            contracts: contracts as never,
            categoryId,
            signer: harness.proverSigner,
            threadOutRef: thread,
            referenceScriptUtxo: references[3],
            witnessReferenceScripts: harness.witnessReferenceScripts,
          }),
        );
        return;
      }
      const chunked = deep ? await deepProofChunks(harness, prepared) : [];
      const advance = async (
        threadOutRef: string,
        stepIndex: 0 | 1 | 2 | 3,
        label: string,
      ) =>
        captures(label, async () =>
          buildAndSubmit(submitNonExistentInputForcedStep, {
            ...common,
            threadOutRef,
            stepIndex,
            prepared: await admitNonExistentInputForcedArtifact(
              JSON.parse(JSON.stringify(artifact)),
            ),
            referenceScripts,
            fieldCarriageUtxos:
              stepIndex === 1 ? [...chunks, ...certificates] : [],
            certificatePolicyId:
              harness.contracts.fieldPreimageCertificate.policyId,
            publishedProofChunks: chunked[stepIndex] ?? [],
          }),
        );
      if (!deep)
        for (const boundary of [0, 1, 2, 3] as const) {
          let thread = await init(`cancel-${boundary}-init`);
          for (let step = 0; step < boundary; step++)
            thread = (
              await advance(
                thread,
                step as 0 | 1 | 2,
                `cancel-${boundary}-step-${step}`,
              )
            ).nextThreadOutRef!;
          await captures(`cancel-${boundary}`, () =>
            submitFamilyCancel("non-existent-input", {
              lucid: harness.proverLucid,
              contracts: contracts as never,
              categoryId,
              signer: harness.proverSigner,
              threadOutRef: thread,
              referenceScriptUtxo: references[boundary],
              witnessReferenceScripts: harness.witnessReferenceScripts,
            }),
          );
        }
      let thread = await init("init");
      for (const step of [0, 1, 2] as const) {
        if (!deep && index === 0n) {
          const changed =
            step === 0
              ? {
                  ...prepared,
                  forcedSource: {
                    ...forcedSource,
                    membership: {
                      ...forcedMembership,
                      key: {
                        ...forcedKey,
                        outputIndex: forcedKey.outputIndex + 1n,
                      },
                    },
                  },
                }
              : step === 1
                ? {
                    ...prepared,
                    eventMembership: {
                      ...eventMembership,
                      value: { ...eventMembership.value, step_index: 1n },
                    },
                  }
                : {
                    ...prepared,
                    transitionMembership: {
                      ...transitionMembership,
                      value: { ...transition, pre_utxos_root: "22".repeat(32) },
                    },
                  };
          await expect(
            submitRawNonExistentInputForcedStep({
              ...common,
              threadOutRef: thread,
              prepared: changed,
              stepIndex: step,
              references: referenceScripts,
            }),
          ).rejects.toThrow(/script execution|validator|eval/i);
        }
        thread = (await advance(thread, step, `step-${step + 1}`))
          .nextThreadOutRef!;
      }
      if (!deep && index === 0n)
        await expect(
          submitRawNonExistentInputForcedStep({
            ...common,
            threadOutRef: thread,
            prepared: {
              ...prepared,
              ledgerMembership: { value: "00", proof: [] },
            },
            stepIndex: 3,
            references: referenceScripts,
          }),
        ).rejects.toThrow(/script execution|validator|eval/i);
      const terminal = await advance(thread, 3, "step-4");
      expect(terminal.fraudProofUnit).toBeTruthy();
      const removal = await publishRemovalReferenceScripts({
        lucid: harness.proverLucid,
        contracts: harness.contracts,
      });
      const now = BigInt(harness.emulator.now());
      await captures("remove", () =>
        submitRemoveFraudulentBlock({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo: buildRemovalDeploymentInfo(
            harness.contracts,
            catalogue,
            { removalReferenceScripts: removal.published },
          ),
          network: "Custom",
          signer: harness.proverSigner,
          fraudCategory: "nonExistentInput",
          fraudulentHeaderHash: setup.headerHash,
          requireReferenceScripts: true,
          stateQueueMutationLeaseCoordinator: {
            acquire: async () => ({
              token: "no-input-lease",
              source: "emulator",
              renew: async () => {},
              release: async () => {},
              fail: async () => {},
            }),
          },
          validFrom: now > 120000n ? now - 120000n : 0n,
          validTo: now + 300_000n,
        }),
      );
    },
    180_000,
  );
});
