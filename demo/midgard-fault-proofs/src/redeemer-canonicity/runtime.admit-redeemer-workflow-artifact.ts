import {
  decodeMidgardFieldPreimage,
  decodeMidgardForcedTxCompact,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { planFaultProofFieldOpening } from "../field-opening.js";
import { type StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { cursorStringField } from "../workflow/cursor-family-runtime.js";
import type { FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { type FamilyAssemblyContext } from "../workflow/family-definition.js";
import type { FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import { type JournalJsonObject } from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import {
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { type RedeemerCanonicityDetection } from "./authenticated-workflow.js";
import {
  REDEEMER_CANONICITY_BLUEPRINT_TITLES,
  type RedeemerCanonicityContracts,
} from "./contracts.js";
import { prepareRedeemerCanonicityEvidence } from "./family.js";
import {
  RedeemerCanonicityStep01SourceSchema,
  RedeemerCanonicityVerdictSubjectSchema,
} from "./schemas.js";
import { submitRedeemerCanonicityStep01Forced } from "./submit-step-01.js";

export const REDEEMER_CANONICITY_CONFIG_KEYS = Object.freeze([
  "manifest",
  "blueprintJson",
  "deploymentInfo",
  "headerHash",
  "lucid",
  "signer",
  "source",
  "decisionDigest",
  "stateQueueMutationLeaseCoordinator",
  "referenceScripts",
] as const);

export type RedeemerCanonicityRemovalReferenceScripts = Readonly<{
  correctionLockSpend: UTxO;
  stateQueueSpend: UTxO;
  stateQueueMint: UTxO;
  stateQueueFraudRemovalWithdraw: UTxO;
  activeOperatorsSpend: UTxO;
  activeOperatorsMint: UTxO;
  retiredOperatorsSpend: UTxO;
  retiredOperatorsMint: UTxO;
  schedulerSpend: UTxO;
}>;

export type RedeemerCanonicityWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
  removal: RedeemerCanonicityRemovalReferenceScripts;
}>;

export type ManifestBoundRedeemerCanonicityWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  referenceScripts: RedeemerCanonicityWorkflowReferenceScripts;
}>;

type Binding = FraudProofWorkflowDeploymentBinding<"redeemerCanonicity">;

export type ManifestBoundRedeemerCanonicityWorkflow = Readonly<{
  binding: Binding;
  l1: FraudProofFamilyL1ObservationPort<"redeemerCanonicity">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: RedeemerCanonicityContracts;
  referenceScripts: RedeemerCanonicityWorkflowReferenceScripts;
  decisionDigest: string;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;

export const STEP_CONTRACT_NAMES = [
  "fraudProofRedeemerCanonicity",
  "fraudProofRedeemerCanonicityStep02",
  "fraudProofRedeemerCanonicityStep03",
] as const;

type BoundContext = FamilyAssemblyContext<
  "redeemerCanonicity",
  (typeof WITNESS_ROLES)[number],
  true,
  3
>;

const bindFamily = (context: BoundContext): RedeemerWorkflowCore => {
  const {
    binding,
    references: { steps, witnesses },
  } = context;
  const chain = binding.resolvedContracts.contracts.redeemerCanonicity;
  const certificate = binding.fieldPreimageCertificate;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 3 ||
    certificate === null ||
    stateQueuePolicyId === undefined
  )
    throw new Error("redeemerCanonicity manifest omitted required contracts");
  const contracts: RedeemerCanonicityContracts = {
    steps: chain.steps.map((step, index) => ({
      ...step,
      blueprintTitle: REDEEMER_CANONICITY_BLUEPRINT_TITLES[index]!,
      referenceOutRef: `${steps[index]!.txHash}#${steps[index]!.outputIndex.toString()}`,
    })) as unknown as RedeemerCanonicityContracts["steps"],
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: binding.resolvedContracts.contracts.fraudProof,
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
    fieldPreimageCertificateMintingScript: certificate.mintingScript,
  };
  return {
    binding,
    l1: context.l1,
    lucid: context.lucid,
    signer: context.signer,
    contracts,
    referenceScripts: {
      ...context.references,
      steps,
      witnesses,
      removal:
        context.auxiliaryReferences as RedeemerCanonicityRemovalReferenceScripts,
    },
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  };
};

export const bound = new WeakMap<BoundContext, RedeemerWorkflowCore>();

export const boundFor = (context: BoundContext) => {
  const existing = bound.get(context);
  if (existing !== undefined) return existing;
  const created = bindFamily(context);
  bound.set(context, created);
  return created;
};

export const selectDetection = (
  detections: readonly RedeemerCanonicityDetection[],
): RedeemerCanonicityDetection => {
  const selected = [...detections].sort((left, right) =>
    left.position === right.position
      ? left.detectionId.localeCompare(right.detectionId)
      : left.position < right.position
        ? -1
        : 1,
  )[0];
  if (selected === undefined)
    throw new Error(
      "redeemerCanonicity retained DA contains no closing detection",
    );
  return selected;
};

export type RedeemerWorkflowCore = Omit<
  ManifestBoundRedeemerCanonicityWorkflow,
  "adapter" | "terminalVerifier" | "releaseFinalityAuthority" | "decisionDigest"
>;

export const admitRedeemerWorkflowArtifact = (artifact: JournalJsonObject) => {
  if (
    artifact.schemaVersion !==
      "midgard-redeemer-canonicity-workflow-artifact-v1" ||
    typeof artifact.headerHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(artifact.headerHash)
  )
    throw new Error("redeemerCanonicity prepared artifact changed");
  const subject = Data.from(
    cursorStringField(artifact, "subjectCbor"),
    RedeemerCanonicityVerdictSubjectSchema as never,
  ) as Parameters<
    typeof prepareRedeemerCanonicityEvidence
  >[0]["finding"]["subject"];
  const evidence = prepareRedeemerCanonicityEvidence({
    finding: { subject, redeemerIndex: Number(artifact.redeemerIndex) },
    fieldPreimage: Buffer.from(
      cursorStringField(artifact, "fieldPreimageHex"),
      "hex",
    ),
    committedFieldHashHex: cursorStringField(artifact, "fieldCommitmentHex"),
  });
  const nativeTxCompactCbor = cursorStringField(
      artifact,
      "nativeTxCompactCbor",
    ),
    witnessSetCompactCbor = cursorStringField(
      artifact,
      "witnessSetCompactCbor",
    );
  const accepted =
    artifact.accepted === null
      ? null
      : parseSubmitStep01TxInclusion({
          ...(artifact.accepted as JournalJsonObject),
          nativeTx: nativeTxFromCoreCompact(
            decodeMidgardNativeTxCompact(
              Buffer.from(nativeTxCompactCbor, "hex"),
            ),
          ),
        });
  const forced =
    artifact.forcedSourceCbor === null
      ? null
      : (
          Data.from(
            cursorStringField(artifact, "forcedSourceCbor"),
            RedeemerCanonicityStep01SourceSchema as never,
          ) as {
            ForcedSource: Parameters<
              typeof submitRedeemerCanonicityStep01Forced
            >[0]["forcedSource"];
          }
        ).ForcedSource;
  if (
    (accepted === null) === (forced === null) ||
    (subject.source_kind === 0n) !== (accepted !== null)
  )
    throw new Error("redeemerCanonicity prepared source changed");
  return {
    artifact,
    evidence,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    accepted,
    forced,
  };
};

export const redeemerField = (
  workflow: RedeemerWorkflowCore,
  artifact: JournalJsonObject,
) => {
  const admitted = admitRedeemerWorkflowArtifact(artifact);
  const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
    Buffer.from(admitted.witnessSetCompactCbor, "hex"),
  );
  return {
    admitted,
    planned: planFaultProofFieldOpening({
      anchorSourceKind: admitted.evidence.subject.source_kind === 1n ? 1n : 0n,
      fieldIndex: 8,
      anchorTxId: admitted.evidence.subject.transaction_id,
      nativeTxCompactCbor: admitted.nativeTxCompactCbor,
      itemCbors: decodeMidgardFieldPreimage(
        Buffer.from(admitted.evidence.fieldPreimageHex, "hex"),
      ),
      owner: workflow.signer.paymentKeyHash,
      witnessSet: {
        addr_tx_wits_hash: Buffer.from(witnessSet.addrTxWitsHash).toString(
          "hex",
        ),
        script_tx_wits_hash: Buffer.from(witnessSet.scriptTxWitsHash).toString(
          "hex",
        ),
        redeemer_tx_wits_hash: Buffer.from(
          witnessSet.redeemerTxWitsHash,
        ).toString("hex"),
      },
      anchorWitnessSetHash: Buffer.from(
        (admitted.evidence.subject.source_kind === 1n
          ? decodeMidgardForcedTxCompact
          : decodeMidgardNativeTxCompact)(
          Buffer.from(admitted.nativeTxCompactCbor, "hex"),
        ).transactionWitnessSetHash,
      ).toString("hex"),
      label: "redeemer-canonicity field opening",
    }),
  };
};
