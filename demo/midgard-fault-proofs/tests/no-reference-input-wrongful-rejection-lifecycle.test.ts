import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  adjudicateMidgardNativeTxFullValidity,
  computeHash28,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  encodeMidgardNativeTxCanonical,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerEntryOutputMaterial } from "@al-ft/midgard-validation";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  admitNoReferenceInputForcedArtifact,
  noReferenceInputForcedArtifact,
} from "../src/no-reference-input/artifact.js";
import {
  noReferenceInputForcedFieldPlan,
  submitNoReferenceInputForcedStep,
} from "../src/no-reference-input/submit.js";
import {
  noReferenceInputForcedSourceMaterial,
  type PreparedNoReferenceInputWrongfulRejection,
} from "../src/no-reference-input/wrongful-rejection.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import { submitInit } from "../src/submit-init.js";
import {
  buildCountedRoot,
  commitCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";
import { submitZeroInputCancel } from "../src/zero-input/submit-cancel.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { buildCatalogueDeploymentInfo } from "./support/emulator/catalogue.js";
import { alignUnixTimeToEmulatorSlotBoundary } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";
import { submitSetupTx } from "./support/emulator/setup-tx.js";
import { makeReferenceNativeTx as makeNativeTx } from "./support/no-reference-input-native.js";
import { submitRawNoReferenceInputForcedStep } from "./support/no-reference-input-raw-step.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./support/synthetic-deep-proof.js";

const measurements: VanRossemFitMeasurement[] = [];
afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  expect(
    measurements.some((row) =>
      row.name.startsWith("819/818/true/false/step-4"),
    ),
  ).toBe(true);
  const blueprint = realBlueprintPath;
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/no-reference-input-wrongful-rejection-v1-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger({
      category: "noReferenceInput",
      blueprintSha256: createHash("sha256")
        .update(readFileSync(blueprint))
        .digest("hex"),
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements,
    }),
  );
});

describe("noReferenceInput wrongful rejection registered lifecycle", () => {
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
          realNoReferenceInput: true,
          alwaysFraudProofCatalogue: true,
        },
      });
      const contracts = {
        steps: harness.contracts.fraudProofContracts.noReferenceInput.steps,
        computationThread: harness.contracts.computationThread,
        fraudProof: harness.contracts.fraudProof,
      };
      const catalogue = await buildCatalogueDeploymentInfo(
        harness.contracts.fraudProofs,
      );
      const categoryId = catalogue.categories.noReferenceInput.categoryId;
      const captures = async <T>(
        name: string,
        action: () => Promise<T>,
        kind: "publication" | "lifecycle" = "lifecycle",
      ) => {
        const captured = await captureEmulatorSubmission(
          harness.emulator,
          action,
        ).catch((error) => {
          throw new Error(`stage ${name}: ${String(error)}`);
        });
        captured.measurements.forEach((m, i) =>
          measurements.push({
            name: `${count}/${index}/${deep}/${honest}/${name}/${i}`,
            kind,
            maximumShape: `${count} inputs; ${deep ? 64 : 0} MPF branch levels per source/event/transition/ledger proof`,
            signedBytes: m.completeSignedBytes,
            memoryUnits: m.executionMemory,
            cpuUnits: m.executionSteps,
          }),
        );
        return captured.result;
      };
      const references: UTxO[] = [];
      for (const [stepIndex, step] of contracts.steps.entries())
        references.push(
          (
            await captures(
              `publish-step-${stepIndex}`,
              () =>
                publishPlainReferenceScriptUtxo({
                  lucid: harness.proverLucid,
                  script: step.spendingScript,
                  label: `no-input step ${stepIndex}`,
                }),
              "publication",
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
      const native = adjudicateMidgardNativeTxFullValidity(
        makeNativeTx({
          spendInputCbors: [],
          referenceInputCbors: inputs,
          fee: 0n,
        }),
        "TxIsInvalid",
      );
      const source = deriveMidgardNativeTxProofSource(native);
      const leaf = {
        tx_id: computeMidgardNativeTxId(native).toString("hex"),
        source: {
          compact_cbor: source.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            source.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            source.fieldPreimageLengthsCbor.toString("hex"),
        },
        verdict: {
          ForcedTxInvalid: {
            reason: { InputNotFound: { source_kind: 1n, input_index: index } },
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
      const header = {
        ...base.header,
        blockSlot: 10n,
        forcedTransactionsRoot: forcedMembership.root,
        eventToStepRoot: eventMembership.root,
        transitionTraceRoot: transitionMembership.root,
        utxosRoot: ledgerRoot,
      };
      const forcedSource = {
        header,
        membership: forcedMembership,
        direction: 1n,
      };
      const fullTransactionCbor = encodeMidgardNativeTxCanonical(
        adjudicateMidgardNativeTxFullValidity(native, "TxIsValid"),
      ).toString("hex");
      const material = noReferenceInputForcedSourceMaterial(
        forcedSource,
        fullTransactionCbor,
      );
      const prepared: PreparedNoReferenceInputWrongfulRejection = {
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
      const artifact = noReferenceInputForcedArtifact(prepared);
      if (honest)
        await expect(
          admitNoReferenceInputForcedArtifact(artifact),
        ).rejects.toThrow(/root/);
      else
        expect(
          await admitNoReferenceInputForcedArtifact(
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
                        InputNotFound: { source_kind: 0n, input_index: index },
                      },
                    },
                  },
                },
              },
            } as never,
            SDK.NoReferenceInputForcedSourcePayloadSchema as never,
          ),
        },
      ])
        await expect(
          admitNoReferenceInputForcedArtifact(changed),
        ).rejects.toThrow();
      const setup = await submitSetupTx({
        lucid: harness.funderLucid,
        contracts: harness.contracts,
        nonceUtxo: harness.nonceUtxo,
        catalogue,
        header,
      });
      const plan = noReferenceInputForcedFieldPlan(
        prepared,
        harness.proverSigner.paymentKeyHash,
      );
      if (deep) {
        expect(plan.plan.tier).toBe("Certified");
        expect(native.body.referenceInputsPreimageCbor.length).toBe(32763);
      }
      const chunks =
        plan.plan.tier === "Inline"
          ? []
          : await captures(
              "field-publications",
              () =>
                publishFaultProofFieldCarriage({
                  lucid: harness.proverLucid,
                  signer: harness.proverSigner,
                  planned: plan,
                  publisherAddress: harness.proverSigner.address,
                  label: "reference-input field",
                }),
              "publication",
            );
      let certificates: typeof references = [];
      if (plan.plan.tier === "Certified") {
        const reference = await publishPlainReferenceScriptUtxo({
          lucid: harness.proverLucid,
          script: harness.contracts.fieldPreimageCertificate.mintingScript,
          label: "reference-input certificate",
        });
        const certified = await captures("field-certification", () =>
          certifyFaultProofFieldCarriage({
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
            compactCbor: leaf.source.compact_cbor,
            witnessSetCompactCbor: leaf.source.witness_set_compact_cbor,
          }),
        );
        certificates = [certified.certificateUtxo];
      }
      const init = async (label: string) => {
        const result = await captures(label, () =>
          submitInit({
            lucid: harness.proverLucid,
            witnessReferenceScripts: harness.witnessReferenceScripts,
            blueprint: harness.realBlueprint,
            deploymentInfo: buildRemovalDeploymentInfo(
              harness.contracts,
              catalogue,
            ),
            network: "Custom",
            signer: harness.proverSigner,
            fraudCategory: "noReferenceInput",
            fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
            awaitConfirmation: true,
          }),
        );
        return `${result.txHash}#${result.firstStepOutputIndex}`;
      };
      if (honest) {
        let thread = await init("honest-init");
        for (const stepIndex of [0, 1, 2] as const)
          thread = await captures(`honest-step-${stepIndex}`, () =>
            submitRawNoReferenceInputForcedStep({
              lucid: harness.proverLucid,
              contracts,
              categoryId,
              signer: harness.proverSigner,
              threadOutRef: thread,
              prepared,
              stepIndex,
              references: referenceScripts,
            }),
          );
        await expect(
          submitRawNoReferenceInputForcedStep({
            lucid: harness.proverLucid,
            contracts,
            categoryId,
            signer: harness.proverSigner,
            threadOutRef: thread,
            prepared,
            stepIndex: 3,
            references: referenceScripts,
          }),
        ).rejects.toThrow(/script execution|validator|eval/i);
        await captures("honest-cancel", () =>
          submitZeroInputCancel({
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
      const advance = async (
        threadOutRef: string,
        stepIndex: 0 | 1 | 2 | 3,
        label: string,
      ) =>
        captures(label, async () =>
          submitNoReferenceInputForcedStep({
            lucid: harness.proverLucid,
            contracts,
            categoryId,
            signer: harness.proverSigner,
            threadOutRef,
            stepIndex,
            prepared: await admitNoReferenceInputForcedArtifact(
              JSON.parse(JSON.stringify(artifact)),
            ),
            referenceScripts,
            carriageUtxos: stepIndex === 1 ? [...chunks, ...certificates] : [],
            certificatePolicyId:
              harness.contracts.fieldPreimageCertificate.policyId,
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
            submitZeroInputCancel({
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
            submitRawNoReferenceInputForcedStep({
              lucid: harness.proverLucid,
              contracts,
              categoryId,
              signer: harness.proverSigner,
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
          submitRawNoReferenceInputForcedStep({
            lucid: harness.proverLucid,
            contracts,
            categoryId,
            signer: harness.proverSigner,
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
          fraudCategory: "noReferenceInput",
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
