/** Maximum admitted membership-carriage shapes, with opaque unmaterialized
 * siblings/model count129, and a ledger non-membership at the deepest point an
 * MPF path allows. These are not a complete honest native-source Trie. Each
 * physical consumer executes on the actual registered chain with its
 * field/certificate/verifier overhead included. */
import {
  computeHash28,
  computeHash32,
  encodeCbor,
  encodeMidgardSpendInputItem,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import {
  admitInputExistenceArtifact,
  inputExistenceArtifact,
} from "../src/input-existence/artifact.js";
import { prepareInputExistence } from "../src/input-existence/prepare.js";
import { submitInputExistenceStep } from "../src/input-existence/submit.js";
import {
  type PublishedProofChunk,
  publishProofChunks,
} from "../src/publish-proof-chunks.js";
import { submitInit } from "../src/submit-init.js";
import { commitCountedRoot } from "../src/transition-trace/phas.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import { verifiedMaximumForkPrefixes } from "./support/canonical-decodability-emulator.long-fork-proof.js";
import { registerChunkedVerifyRewardAccount } from "./support/emulator/emulator-context.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import {
  captureEmulatorSubmission,
  measureCompleteSignedTransaction,
} from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import { registerPexcludesExclusionRewardAccount } from "./support/submit-init-emulator-fixtures.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import {
  syntheticDeepInsertProof,
  syntheticDeepMembershipProof,
} from "./support/synthetic-deep-proof.js";

type Shape = "branches" | "forks_branch" | "forks_leaf";
const profiles = (["branches", "forks_branch", "forks_leaf"] as const).flatMap(
  (shape) => [false, true].map((partial) => ({ shape, partial })),
);
const MODEL_COUNT = 129n;
const proofFor = (key: Buffer, value: Buffer, shape: Shape) =>
  shape === "branches"
    ? syntheticDeepMembershipProof({ key, value, branchLevels: 64 })
    : verifiedMaximumForkPrefixes(
        key,
        value,
        shape === "forks_leaf" ? "leaf" : "branch",
      );
const captureSigned = (tag: string, cbor: string) => {
  const m = measureCompleteSignedTransaction(cbor);
  console.info(
    "[t255-max-membership]",
    JSON.stringify({
      tag,
      signedSha256: computeHash32(Buffer.from(cbor, "hex")).toString("hex"),
      bytes: m.completeSignedBytes,
      memory: m.executionMemory.toString(),
      cpu: m.executionSteps.toString(),
      referenceInputs: m.referenceInputCount,
    }),
  );
  expect(m.completeSignedBytes).toBeLessThanOrEqual(16_384);
  expect(m.executionMemory).toBeLessThanOrEqual(13_200_000n);
  expect(m.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
};

describe("registered maximum event/transition counted membership", () => {
  for (const field of [0, 1] as const) {
    it.each(profiles)(
      `runs field ${field} with $shape and partial=$partial`,
      async ({ shape, partial }) => {
        const h = await makeFaultProofEmulatorHarness({
          contractOptions: {
            realNonExistentInput: field === 0,
            realNoReferenceInput: field === 1,
            alwaysFraudProofCatalogue: true,
          },
          registerAdditionalRewardAccounts: async (lucid, bp) => {
            await registerPexcludesExclusionRewardAccount(lucid, bp);
            await registerChunkedVerifyRewardAccount(lucid, bp);
          },
        });
        const category = field === 0 ? "nonExistentInput" : "noReferenceInput";
        const family = h.contracts.fraudProofContracts[category];
        const inputItems = Array.from({ length: 819 }, (_, index) =>
          encodeMidgardSpendInputItem({
            txId: Buffer.from(index.toString(16).padStart(64, "0"), "hex"),
            outputIndex: 0,
          }),
        );
        const native = makeNativeTx({
          spendInputCbors: field === 0 ? inputItems : [],
          fee: 0n,
        });
        // Fields0/1 have the same fixed-width grammar. Materialize the actual
        // canonical source with field1's full maximum when testing that family.
        const tx =
          field === 0
            ? native
            : materializeMidgardNativeTxFromCanonical({
                version: native.version,
                validity: native.validity,
                body: {
                  ...native.body,
                  referenceInputsPreimageCbor: encodeCbor(inputItems),
                },
                witnessSet: native.witnessSet,
              });
        const small = await buildDecodingBlockFixture({
          operatorVkey: await funderPaymentKeyHash(h.funderLucid),
          startTime: BigInt(
            alignUnixTimeToEmulatorSlotBoundary(
              h.funderLucid,
              h.emulator.now() + 120_000,
            ) - 1,
          ),
          priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          subject: { kind: "normal", nativeTx: tx },
        });
        const initial = await canonicalBlockEvidenceFromVerifiedPayload({
          observation: authenticatedHeaderObservation(small),
          payloadEnvelopeCbor: small.payloadEnvelopeCbor,
          daProvenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "test/max-membership-base",
            grade: "security",
          },
        });
        const eventKey: SDK.EventKey = {
          L2TransactionEventKey: { tx_id: small.nativeTxId },
        };
        const base = await prepareInputExistence({
          block: initial,
          eventKey,
          field,
          inputIndex: 0n,
          owner: h.proverSigner.paymentKeyHash,
        });
        if (base.source.kind !== "accepted")
          throw new Error("maximum fixture must bind an accepted source");
        expect(base.fieldPlan.preimage.length).toBe(32763);
        expect(base.fieldPlan.plan.tier).toBe("Certified");
        const sourceProof = proofFor(
          Buffer.from(small.nativeTxId, "hex"),
          Buffer.from(base.source.inclusion.l2TransactionSourceCbor, "hex"),
          shape,
        );
        const eventProof = proofFor(
          Buffer.from(Data.to(eventKey, SDK.EventKey), "hex"),
          Buffer.from(
            Data.to(base.eventMembership.value, SDK.EventToStepValue),
            "hex",
          ),
          shape,
        );
        // The selected input is absent from a ledger whose path to it is
        // 63 forced branch levels and then a neighbouring leaf.
        const absence = syntheticDeepInsertProof({
          key: inputItems[0]!,
          value: computeHash32(Buffer.from("absent selected input")),
        });
        const transition: SDK.TransitionStep = {
          ...base.transitionMembership.value,
          pre_utxos_root: absence.preRoot,
        };
        const traceProof = proofFor(
          Buffer.from(Data.to(0n), "hex"),
          Buffer.from(Data.to(transition, SDK.TransitionStep), "hex"),
          shape,
        );
        const counted = (domain: SDK.RootDomain, phasRoot: string) =>
          commitCountedRoot({ domain, phasRoot, count: MODEL_COUNT });
        const header: SDK.Header = {
          ...small.header,
          l2TransactionCount: MODEL_COUNT,
          totalEventCount: MODEL_COUNT,
          transitionStepCount: MODEL_COUNT,
          transactionsRoot: await counted(
            SDK.ROOT_DOMAINS.transactionsV1,
            sourceProof.transactionsPhasRoot,
          ),
          eventToStepRoot: await counted(
            SDK.ROOT_DOMAINS.eventToStep,
            eventProof.transactionsPhasRoot,
          ),
          transitionTraceRoot: await counted(
            SDK.ROOT_DOMAINS.transitionTrace,
            traceProof.transactionsPhasRoot,
          ),
        };
        const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
          "hex",
        );
        const prepared = admitInputExistenceArtifact(
          inputExistenceArtifact({
            ...base,
            source: {
              ...base.source,
              header,
              headerHash,
              inclusion: {
                ...base.source.inclusion,
                transactionsPhasRoot: sourceProof.transactionsPhasRoot,
                txMembershipProofCbor: sourceProof.proofCbor,
              },
            },
            eventMembership: {
              ...base.eventMembership,
              root: header.eventToStepRoot,
              phas_root: eventProof.transactionsPhasRoot,
              count: MODEL_COUNT,
              proof: Data.from(eventProof.proofCbor, SDK.Proof),
            },
            transitionMembership: {
              ...base.transitionMembership,
              value: transition,
              root: header.transitionTraceRoot,
              phas_root: traceProof.transactionsPhasRoot,
              count: MODEL_COUNT,
              proof: Data.from(traceProof.proofCbor, SDK.Proof),
            },
            ledgerEvidence: {
              Absent: { proof: Data.from(absence.proofCbor, SDK.Proof) },
            },
          }),
          h.proverSigner.paymentKeyHash,
        );
        const setup = await submitSetupTx({
          lucid: h.funderLucid,
          contracts: h.contracts,
          nonceUtxo: h.nonceUtxo,
          catalogue: h.catalogue,
          header,
        });
        expect(setup.headerHash).toBe(headerHash);
        const stepReferences: UTxO[] = [];
        for (const [index, step] of family.steps.entries()) {
          expect(step.spendingScript.script.length / 2).toBeLessThanOrEqual(
            16384,
          );
          stepReferences.push(
            (
              await publishPlainReferenceScriptUtxo({
                lucid: h.funderLucid,
                script: step.spendingScript,
                label: `T255 maximum step${index + 1}`,
              })
            ).utxo,
          );
        }
        const ct = (
          await publishPlainReferenceScriptUtxo({
            lucid: h.funderLucid,
            script: h.contracts.computationThread.mintingScript,
            label: "T255 maximum CT",
          })
        ).utxo;
        const fp = (
          await publishPlainReferenceScriptUtxo({
            lucid: h.funderLucid,
            script: h.contracts.fraudProof.mintingScript,
            label: "T255 maximum FP",
          })
        ).utxo;
        const cert = h.contracts.fieldPreimageCertificate;
        const certRef = (
          await publishPlainReferenceScriptUtxo({
            lucid: h.funderLucid,
            script: cert.mintingScript,
            label: "T255 maximum field certificate",
          })
        ).utxo;
        const tag = `field${field}-${shape}-${partial ? "eight-partial" : "packed"}`;
        const fieldChunks = await publishFaultProofFieldCarriage({
          lucid: h.proverLucid,
          signer: h.proverSigner,
          planned: prepared.fieldPlan,
          publisherAddress: h.proverSigner.address,
          label: tag,
          preSubmitBoundary: ({ signed }) =>
            captureSigned(tag + "-field-publication", signed.toCBOR()),
        });
        const certificate = await certifyFaultProofFieldCarriage({
          lucid: h.proverLucid,
          network,
          signer: h.proverSigner,
          planned: prepared.fieldPlan,
          certificatePolicyId: cert.policyId,
          certificateMintingScript: cert.mintingScript,
          certificateReferenceScriptUtxo: certRef,
          chunkUtxos: fieldChunks,
          compactCbor: prepared.source.compactCbor,
          witnessSetCompactCbor: prepared.source.witnessSetCompactCbor,
          preSubmitBoundary: ({ signed }) =>
            captureSigned(tag + "-field-certification", signed.toCBOR()),
        });
        const allFieldCarriage = [...fieldChunks, certificate.certificateUtxo];
        const publish = async (proof: string, label: string) => {
          const steps = Data.from(proof, SDK.Proof);
          expect(steps).toHaveLength(64);
          const parts = partial
            ? Array.from({ length: 8 }, (_, part) =>
                Data.to(steps.slice(part * 8, part * 8 + 8), SDK.Proof),
              )
            : [proof];
          const chunks: PublishedProofChunk[] = [];
          for (const part of parts)
            chunks.push(
              ...(
                await publishProofChunks({
                  lucid: h.proverLucid,
                  network,
                  signer: h.proverSigner,
                  proofCbor: part,
                  preSubmitBoundary: ({ signed }) =>
                    captureSigned(
                      tag + "-" + label + "-proof-publication",
                      signed.toCBOR(),
                    ),
                })
              ).chunks,
            );
          expect(chunks.length).toBeLessThanOrEqual(8);
          if (partial) expect(chunks).toHaveLength(8);
          return chunks;
        };
        const proofs = [
          sourceProof.proofCbor,
          eventProof.proofCbor,
          traceProof.proofCbor,
          absence.proofCbor,
        ];
        const deploymentInfo = buildRemovalDeploymentInfo(
          h.contracts,
          h.catalogue,
        );
        const init = await submitInit({
          lucid: h.proverLucid,
          blueprint: h.realBlueprint,
          deploymentInfo,
          network,
          signer: h.proverSigner,
          fraudCategory: category,
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
          witnessReferenceScripts: h.witnessReferenceScripts,
          preSubmitBoundary: ({ signed }) =>
            captureSigned(tag + "-init", signed.toCBOR()),
        });
        let threadOutRef = `${init.txHash}#${init.firstStepOutputIndex}`;
        for (const stepIndex of [0, 1, 2, 3] as const) {
          const chunks = await publish(
            proofs[stepIndex]!,
            `step${stepIndex + 1}`,
          );
          const submitted = await captureEmulatorSubmission(h.emulator, () =>
            submitInputExistenceStep({
              lucid: h.proverLucid,
              blueprint: h.realBlueprint,
              network,
              contracts: {
                hubOraclePolicyId: h.contracts.hubOracle.policyId,
                steps: family.steps,
                computationThread: h.contracts.computationThread,
                fraudProof: h.contracts.fraudProof,
              },
              categoryId: h.catalogue.categories[category].categoryId,
              signer: h.proverSigner,
              threadOutRef,
              prepared,
              stepIndex,
              stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
              referenceScripts: {
                steps: stepReferences,
                computationThreadMint: ct,
                fraudProofMint: fp,
              },
              witnessReferenceScripts: h.witnessReferenceScripts,
              fieldCarriageUtxos: allFieldCarriage,
              certificatePolicyId: cert.policyId,
              publishedProofChunks: chunks,
              preSubmitBoundary: ({ signed }) =>
                captureSigned(tag + `-step${stepIndex + 1}`, signed.toCBOR()),
            }),
          );
          threadOutRef = submitted.result.nextThreadOutRef;
          if (stepIndex === 3)
            expect(submitted.result.fraudProofOutRef).toBe(threadOutRef);
        }
      },
      1_200_000,
    );
  }
});
