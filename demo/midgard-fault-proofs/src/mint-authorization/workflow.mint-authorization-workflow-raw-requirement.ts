import {
  deriveMidgardNativeTxProofSource,
  encodeMidgardFieldPreimage,
} from "@al-ft/midgard-core";
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  MintAuthorizationClaimEvidence,
  MintAuthorizationStep04Datum,
  type MintAuthorizationStep04State,
} from "@al-ft/midgard-sdk";
import { MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Data } from "@lucid-evolution/lucid";

import {
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import {
  fetchUtxoByOutRef,
  parseOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { type FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import type { JournalJsonObject } from "../workflow/journal.js";
import type { OwnOutputJournal } from "../workflow/own-outputs.js";
import { createStructuredDataPreimageRequirement } from "../workflow/raw-datum-preimage.js";
import { createRawDatumPreimageRequirement } from "../workflow/raw-datum-preimage-prerequisite.js";
import { admitMintAuthorizationWorkflowArtifact } from "./artifact.js";
import type { MintAuthorizationContracts } from "./contracts.js";
import { mintAuthorizationEvaluationPreimage } from "./evaluate.js";
import { buildMintAuthorizationStep02Evidence } from "./evidence.js";
import { requireMintAuthorizationStepState } from "./submit-common.js";

export type MintAuthorizationWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

export type BoundConfig = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<"mintAuthorization">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: MintAuthorizationContracts;
  references: MintAuthorizationWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

type Prepared = Awaited<
  ReturnType<typeof admitMintAuthorizationWorkflowArtifact>
>;

export const witnessSet = (admitted: Prepared) => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(
    decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(admitted.nativeTxCanonicalCbor, "hex"),
    ).witnessSet,
  );
  return {
    addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
    script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
    redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
  };
};

/**
 * The step-04 action names the input field its thread scans: the refined
 * `scanFieldIndex` copied from the thread's state, field 1 then field 0.
 */
export const MINT_AUTHORIZATION_STEP_04_SCAN_FIELD = "scanFieldIndex";

export const mintAuthorizationStep04ScanField = (
  input: Readonly<Record<string, unknown>>,
) => {
  const value = input[MINT_AUTHORIZATION_STEP_04_SCAN_FIELD];
  if (
    value !== MIDGARD_FIELD_INDEX.referenceInputs &&
    value !== MIDGARD_FIELD_INDEX.spendInputs
  )
    throw new Error("mint authorization: step-04 action omitted its field");
  return value;
};

/** Reads the input field a step-04 thread is scanning from its own state. */
export const readMintAuthorizationStep04ScanField = async ({
  lucid,
  signer,
  threadOutRef,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
}) => {
  const state = requireMintAuthorizationStepState<MintAuthorizationStep04State>(
    {
      threadUtxo: await fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(threadOutRef, "step-04 thread outRef"),
        label: "mint authorization step-04 thread",
      }),
      signer,
      schema: MintAuthorizationStep04Datum,
      stepIndex: 3,
    },
  );
  return mintAuthorizationStep04ScanField({
    [MINT_AUTHORIZATION_STEP_04_SCAN_FIELD]: Number(state.field_index),
  });
};

/**
 * Step-04 scans field 1 and then field 0 at the same step; the refined
 * action carries the field its thread names so carriage is planned for it.
 */
export const mintAuthorizationRefineAction =
  ({
    lucid,
    signer,
  }: {
    readonly lucid: LucidEvolution;
    readonly signer: ResolvedProverSigner;
  }) =>
  async ({
    observed,
  }: {
    readonly observed: Readonly<{
      stage: Readonly<{ kind: string; step?: number; threadOutRef?: string }>;
    }>;
  }): Promise<JournalJsonObject> =>
    observed.stage.kind === "step" &&
    observed.stage.step === 4 &&
    observed.stage.threadOutRef !== undefined
      ? {
          [MINT_AUTHORIZATION_STEP_04_SCAN_FIELD]:
            await readMintAuthorizationStep04ScanField({
              lucid,
              signer,
              threadOutRef: observed.stage.threadOutRef,
            }),
        }
      : {};

export const planMintAuthorizationWorkflowField = (
  admitted: Prepared,
  owner: string,
  input: Readonly<Record<string, unknown>>,
) => {
  const stage = input.stage;
  const fieldIndex =
    stage === "step_02"
      ? MIDGARD_FIELD_INDEX.mint
      : stage === "step_03"
        ? admitted.finding.direction === 0n
          ? MIDGARD_FIELD_INDEX.scriptWitnesses
          : MIDGARD_FIELD_INDEX.addressWitnesses
        : stage === "step_04"
          ? mintAuthorizationStep04ScanField(input)
          : null;
  if (fieldIndex === null) return null;
  const items =
    fieldIndex === MIDGARD_FIELD_INDEX.mint
      ? admitted.mintItemCbors
      : fieldIndex === MIDGARD_FIELD_INDEX.scriptWitnesses
        ? admitted.scriptWitnessItemCbors
        : fieldIndex === MIDGARD_FIELD_INDEX.addressWitnesses
          ? admitted.addrWitnessItemCbors
          : fieldIndex === MIDGARD_FIELD_INDEX.referenceInputs
            ? admitted.referenceInputItemCbors
            : admitted.spendInputItemCbors;
  return planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex,
    anchorTxId: admitted.txInclusion.nativeTxId,
    nativeTxCompactCbor: admitted.nativeTxCompactCbor,
    itemCbors: items.map((item) => Buffer.from(item, "hex")),
    owner,
    ...(fieldIndex >= MIDGARD_FIELD_INDEX.scriptWitnesses
      ? {
          witnessSet: witnessSet(admitted),
          anchorWitnessSetHash: admitted.witnessSet,
        }
      : {}),
    label: `mint authorization field ${fieldIndex}`,
  });
};

export const mintAuthorizationWorkflowFieldRequirement = (
  admitted: Prepared,
  owner: string,
  input: Readonly<Record<string, unknown>>,
  certificate: FieldCarriageRequirement["certificate"],
): FieldCarriageRequirement | null => {
  const planned = planMintAuthorizationWorkflowField(admitted, owner, input);
  if (planned === null) return null;
  return {
    planned,
    compactCbor: admitted.nativeTxCompactCbor,
    witnessSetCompactCbor: deriveMidgardNativeTxProofSource(
      decodeMidgardNativeTxFullFromCanonicalCbor(
        Buffer.from(admitted.nativeTxCanonicalCbor, "hex"),
      ),
    ).witnessSetCompactCbor.toString("hex"),
    certificate,
  };
};

export const mintAuthorizationWorkflowRawRequirement = async (
  admitted: Prepared,
  stage: unknown,
) => {
  if (stage === "step_02") {
    const evidence = await buildMintAuthorizationStep02Evidence({
      reconstruction: admitted.current,
      eventKey: {
        L2TransactionEventKey: { tx_id: admitted.txInclusion.nativeTxId },
      },
    });
    const preimageHex = aikenSerialisedPlutusDataCborPreservingMapOrder(
      Data.to(
        {
          header: admitted.current.header,
          event_to_step_membership: evidence.eventToStepMembership,
          transition_step_membership: evidence.transitionStepMembership,
          policy_index: admitted.finding.policyIndex,
          direction: admitted.finding.direction,
        },
        MintAuthorizationClaimEvidence,
      ),
    );
    return preimageHex.length <= 6000 * 2
      ? null
      : createStructuredDataPreimageRequirement({ preimageHex });
  }
  if (stage !== "step_03" && stage !== "step_06" && stage !== "step_07")
    return null;
  const preimage =
    admitted.finding.direction === 0n
      ? encodeMidgardFieldPreimage(
          admitted.scriptWitnessItemCbors.map((item) =>
            Buffer.from(item, "hex"),
          ),
        )
      : mintAuthorizationEvaluationPreimage(
          Buffer.from(admitted.finding.scriptBytesHex!, "hex"),
          encodeMidgardFieldPreimage(
            admitted.addrWitnessItemCbors.map((item) =>
              Buffer.from(item, "hex"),
            ),
          ),
        );
  if (
    preimage.length === 0 ||
    preimage.length > (admitted.finding.direction === 0n ? 32_768 : 65_536)
  )
    throw new Error(
      "mint authorization preimage exceeds the canonical 32768-byte domain",
    );
  return createRawDatumPreimageRequirement({ preimage });
};

export const resolveField = async ({
  config,
  plan,
  ownOutputs,
}: {
  readonly config: BoundConfig;
  readonly plan: NonNullable<
    ReturnType<typeof planMintAuthorizationWorkflowField>
  >;
  /** Our carriage claims, which the carriage is read by. */
  readonly ownOutputs: OwnOutputJournal;
}) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    ownOutputs,
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("mint-authorization field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    ownOutputs,
    lucid: config.lucid,
    network: config.binding.network,
    planned: plan,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (plan.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("mint-authorization field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};
