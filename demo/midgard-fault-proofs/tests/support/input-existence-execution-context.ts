/** Current source/event/ledger fixture shared by retained stage journeys.
 * Published references and economic/removal assertions stay in their callers. */
import type { MidgardNativeTxFull } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { toUnit, type UTxO } from "@lucid-evolution/lucid";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../../src/evidence/canonical-block-evidence.js";
import {
  certifyFaultProofFieldCarriage,
  publishFaultProofFieldCarriage,
} from "../../src/field-opening.js";
import type { InputExistenceDeploymentStepArguments } from "../../src/input-existence/deployment-step.js";
import {
  inputExistenceFieldPlans,
  inputExistenceWithPublishedFields,
} from "../../src/input-existence/field-carriage.js";
import type { PreparedInputExistence } from "../../src/input-existence/prepare.js";
import { prepareInputExistence } from "../../src/input-existence/prepare.js";
import type { submitInputExistenceStep } from "../../src/input-existence/submit.js";
import { checkLedgerAgainstRoot } from "../../src/ledger-at.js";
import { createOwnOutputJournal } from "../../src/workflow/own-outputs.js";
import { authenticatedHeaderObservation } from "../helpers/canonical-block-evidence-fixture.js";
import { inputExistenceChronologyFixture } from "../helpers/input-existence-chronology-fixture.js";
import { chronologySeedDeposits } from "../helpers/input-existence-chronology-seed.js";
import type { makeFaultProofEmulatorHarness } from "./emulator/harness.js";
import { submitSecondHeaderTx } from "./emulator/setup-tx.js";
import { buildDecodingBlockFixture } from "./native-script-decoding-emulator.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  funderPaymentKeyHash,
  network,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
} from "./submit-init-emulator-shared.js";
import { depositEventsRetainedBlock } from "./transition-trace-retained.deposit-events-retained-block.js";

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
export const inputExistenceExecutionContext = async ({
  harness: h,
  nativeTx,
  field,
  inputIndex,
  present = false,
}: {
  readonly harness: Harness;
  readonly nativeTx: MidgardNativeTxFull;
  readonly field: 0 | 1;
  readonly inputIndex: bigint;
  readonly present?: boolean;
}) => {
  const operatorVkey = await funderPaymentKeyHash(h.funderLucid);
  const startTime = BigInt(
    alignUnixTimeToEmulatorSlotBoundary(
      h.funderLucid,
      h.emulator.now() + 120_000,
    ) - 1,
  );
  const material = present
    ? await (async () => {
        const prior = await depositEventsRetainedBlock({
          operatorVkey,
          startTime: startTime - 30_000n,
          endTime: startTime,
          blockSlot: 0n,
          prevHeaderHash: SDK.GENESIS_HEADER_HASH,
          prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          priorLedger: [],
          events: chronologySeedDeposits(),
        });
        const history = await inputExistenceChronologyFixture(
          "normal_creator",
          field,
          {
            operatorVkey,
            startTime,
            predecessorHeaderHash: prior.headerHash,
          },
        );
        const priorSetup = await submitSetupTx({
          lucid: h.funderLucid,
          contracts: h.contracts,
          nonceUtxo: h.nonceUtxo,
          catalogue: h.catalogue,
          header: prior.header,
        });
        const next = await submitSecondHeaderTx({
          lucid: h.funderLucid,
          contracts: h.contracts,
          header: history.evidence.header,
        });
        return {
          evidence: history.evidence,
          eventKey: history.targetEvent,
          predecessor: history.predecessor,
          inputIndex: 0n,
          setup: {
            ...priorSetup,
            header: next.header,
            headerHash: next.headerHash,
            fraudulentBlockOutRef: next.blockOutRef,
            stateQueueBlockUnit: toUnit(
              h.contracts.stateQueue.policyId,
              SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + next.headerHash,
            ),
          },
        };
      })()
    : await (async () => {
        const fixture = await buildDecodingBlockFixture({
          operatorVkey,
          startTime,
          priorLedgerRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          subject: { kind: "normal", nativeTx },
          decoyTransactionCount: 1,
        });
        const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
          observation: authenticatedHeaderObservation(fixture),
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          daProvenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "emulator/input-existence-stages",
            grade: "security",
          },
        });
        return {
          evidence,
          eventKey: { L2TransactionEventKey: { tx_id: fixture.nativeTxId } },
          inputIndex,
          predecessor: await checkLedgerAgainstRoot({
            entries: [],
            utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            label: "retained stage predecessor",
          }),
          setup: await submitSetupTx({
            lucid: h.funderLucid,
            contracts: h.contracts,
            nonceUtxo: h.nonceUtxo,
            catalogue: h.catalogue,
            header: evidence.header,
          }),
        };
      })();
  if (material.setup.headerHash !== material.evidence.headerHash)
    throw new Error(
      "retained input-existence source differs from committed execution header",
    );
  const prepared = await prepareInputExistence({
    block: material.evidence,
    eventKey: material.eventKey,
    field,
    inputIndex: material.inputIndex,
    predecessor: material.predecessor,
    owner: h.proverSigner.paymentKeyHash,
  });
  const category = field === 0 ? "nonExistentInput" : "noReferenceInput";
  const steps: UTxO[] = [];
  for (const [index, step] of h.contracts.fraudProofContracts[
    category
  ].steps.entries())
    steps.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: h.funderLucid,
          script: step.spendingScript,
          label: `${category} retained stage ${index + 1}`,
        })
      ).utxo,
    );
  const computationThreadMint = h.witnessReferenceScripts.computationThreadMint;
  const fraudProofMint = h.witnessReferenceScripts.fraudProofMint;
  if (computationThreadMint === undefined || fraudProofMint === undefined)
    throw new Error(
      "retained input-existence journey is missing mint references",
    );
  const referenceScripts = { steps, computationThreadMint, fraudProofMint };
  const deploymentInfo = buildRemovalDeploymentInfo(h.contracts, h.catalogue);
  const arguments_: Omit<
    InputExistenceDeploymentStepArguments,
    "threadOutRef"
  > = {
    lucid: h.proverLucid,
    blueprint: h.realBlueprint,
    network,
    deploymentInfo,
    signer: h.proverSigner,
    prepared,
    stateQueueBlockOutRef: material.setup.fraudulentBlockOutRef,
    referenceScripts,
    witnessReferenceScripts: h.witnessReferenceScripts,
  };
  return {
    setup: material.setup,
    prepared,
    deploymentInfo,
    referenceScripts,
    stepReferences: steps,
    arguments: arguments_,
  };
};

export const publishInputExistenceSelectionCarriage = async (
  harness: Harness,
  prepared: PreparedInputExistence,
  usePublishedFieldCarriage = false,
) => {
  const ownOutputs = createOwnOutputJournal();
  const selected = usePublishedFieldCarriage
    ? inputExistenceWithPublishedFields(
        prepared,
        harness.proverSigner.paymentKeyHash,
      )
    : prepared;
  const fieldCarriageUtxos: UTxO[] = [];
  for (const planned of inputExistenceFieldPlans(selected)) {
    const chunks = await publishFaultProofFieldCarriage({
      ownOutputs,
      lucid: harness.proverLucid,
      signer: harness.proverSigner,
      planned,
      publisherAddress: harness.proverSigner.address,
      label: "retained input-existence field",
    });
    fieldCarriageUtxos.push(...chunks);
    if (planned.plan.tier === "Certified") {
      const policy = harness.contracts.fieldPreimageCertificate;
      const reference = await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: policy.mintingScript,
        label: "retained input-existence certificate",
      });
      const certificate = await certifyFaultProofFieldCarriage({
        ownOutputs,
        lucid: harness.proverLucid,
        network,
        signer: harness.proverSigner,
        planned,
        certificatePolicyId: policy.policyId,
        certificateMintingScript: policy.mintingScript,
        certificateReferenceScriptUtxo: reference.utxo,
        chunkUtxos: chunks,
        compactCbor: prepared.source.compactCbor,
        witnessSetCompactCbor: prepared.source.witnessSetCompactCbor,
      });
      fieldCarriageUtxos.push(certificate.certificateUtxo);
    }
  }
  return {
    fieldCarriageUtxos,
    usePublishedFieldCarriage,
    certificatePolicyId: harness.contracts.fieldPreimageCertificate.policyId,
  };
};

export const requireInputExistenceTerminalResult = (
  result: Awaited<ReturnType<typeof submitInputExistenceStep>>,
) => {
  const fraudProofOutRef = result.fraudProofOutRef;
  const fraudProofUnit = result.fraudProofUnit;
  if (fraudProofOutRef === undefined || fraudProofUnit === undefined)
    throw new Error(
      "terminal input-existence submission returned no proof output",
    );
  return { ...result, fraudProofOutRef, fraudProofUnit };
};
