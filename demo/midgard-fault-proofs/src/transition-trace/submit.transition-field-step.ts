/**
 * The field-opening half of an accepted-transaction checkpoint: the carriage
 * of the field a phase walks (spend inputs or outputs), published and, at
 * tier 3, certified before the step is built; and, for an output replayed
 * whole, the two output-step verifiers the step must also withdraw from.
 */
import type { TransitionTraceFaultProofContracts } from "@al-ft/midgard-sdk";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  type TxBuilder,
  type UTxO,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";

import {
  certifyFaultProofFieldCarriage,
  faultProofFieldCarriage,
  faultProofFieldOpening,
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { type ContractDeploymentInfo } from "../inspect-contracts.js";
import {
  requireDeploymentReferenceScript,
  type ResolvedProverSigner,
} from "../runtime.js";
import {
  type TransitionTraceField,
  transitionTraceOutputStepIsWhole,
  transitionTracePhaseField,
  type TransitionTracePhaseStep,
} from "./phases.js";
import {
  readTransitionProof,
  transitionL2Replay,
  type TransitionProofInput,
} from "./proof-material.js";
import { TRANSITION_TRACE_YIELD_REFERENCES } from "./yield-references.js";

/** One field's planned opening and the reference inputs that carry it. */
export type TransitionFieldCarriage = {
  readonly planned: FaultProofFieldOpeningPlan;
  readonly references: readonly UTxO[];
};

/**
 * Plan, publish and (at tier 3) certify the carriage of `field` of the
 * accepted transaction `proof` disputes. Existing publications and
 * certificates are reused. With `publish` false nothing is submitted and a
 * missing publication or certificate is an error.
 */
export const prepareTransitionFieldCarriage = async ({
  lucid,
  network,
  signer,
  proof: proofInput,
  field,
  certificatePolicyId,
  certificateMintingScript,
  certificateReferenceScript,
  publish,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly proof: TransitionProofInput;
  readonly field: TransitionTraceField;
  readonly certificatePolicyId: string;
  readonly certificateMintingScript: Script;
  readonly certificateReferenceScript: () => Promise<UTxO>;
  readonly publish: boolean;
}): Promise<TransitionFieldCarriage> => {
  const proof = readTransitionProof(proofInput);
  const replay = transitionL2Replay(proofInput);
  if (
    !("InvalidOneStepTransition" in proof.fault) ||
    !("L2TransactionTransition" in proof.fault.InvalidOneStepTransition.witness)
  )
    throw new Error("transition field carriage requires an L2 transaction");
  if (replay === undefined)
    throw new Error(
      "Transition L2 replay requires its field items and ledger witnesses",
    );
  const membership =
    proof.fault.InvalidOneStepTransition.witness.L2TransactionTransition
      .source_membership;
  const { compact_cbor: compactCbor, witness_set_compact_cbor: witnessSet } =
    Data.from(membership.value, SDK.L2TransactionSource).source;
  const label = `transition-trace field ${field.toString()}`;
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: field,
    anchorTxId: membership.key,
    nativeTxCompactCbor: compactCbor,
    itemCbors: (field === 0 ? replay.spendInputCbors : replay.outputCbors).map(
      (item) => Buffer.from(item, "hex"),
    ),
    owner: signer.paymentKeyHash,
    publish: true,
    label,
  });
  const chunks = publish
    ? await publishFaultProofFieldCarriage({
        lucid,
        signer,
        planned,
        publisherAddress: signer.address,
        label,
      })
    : await resolveFaultProofFieldCarriagePublications({
        lucid,
        publisherAddress: signer.address,
        planned,
      });
  if (chunks === undefined)
    throw new Error(`${label} carriage is not published`);
  signer.selectWallet(lucid);
  if (planned.plan.tier !== "Certified") return { planned, references: chunks };
  let certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid,
    network,
    planned,
    certificatePolicyId,
  });
  if (certificate === undefined) {
    if (!publish) throw new Error(`${label} carriage is not certified`);
    certificate = (
      await certifyFaultProofFieldCarriage({
        lucid,
        network,
        signer,
        planned,
        certificatePolicyId,
        certificateMintingScript,
        certificateReferenceScriptUtxo: await certificateReferenceScript(),
        chunkUtxos: chunks,
        compactCbor,
        witnessSetCompactCbor: witnessSet,
      })
    ).certificateUtxo;
    signer.selectWallet(lucid);
  }
  return { planned, references: [...chunks, certificate] };
};

/** One output-step verifier: its published script and its validator. */
type TransitionOutputVerifier = {
  readonly key: "l2OutputDescriptor" | "l2OutputSummary";
  readonly reference: UTxO;
  readonly validator: SDK.WithdrawalValidator;
};

export type TransitionFieldStep = {
  /** The step's complete reference inputs: the base, field carriage and verifiers. */
  readonly referenceInputs: readonly UTxO[];
  /** The opening callbacks against those reference inputs. */
  readonly openings: {
    readonly fieldOpening: (field: TransitionTraceField) => SDK.FieldOpening;
    readonly fieldCarriage: (field: TransitionTraceField) => SDK.FieldCarriage;
  };
  /** Add the verifier withdrawals a whole output replay names. */
  readonly withdraw: (
    builder: TxBuilder,
    planned: TransitionTracePhaseStep,
  ) => TxBuilder;
  readonly referenceScripts: readonly {
    readonly role: string;
    readonly utxo: UTxO;
    readonly expectedScript: Script;
  }[];
};

/** The field carriage and verifiers the checkpoint at `state` needs. */
export const prepareTransitionFieldStep = async ({
  lucid,
  network,
  signer,
  deploymentInfo,
  contracts,
  proof,
  state,
  outputCbors,
  publish,
  fieldCertificateReferenceScript,
  baseReferences,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly contracts: TransitionTraceFaultProofContracts;
  readonly proof: TransitionProofInput;
  readonly state: SDK.TransitionTraceFinalState;
  readonly outputCbors: readonly string[] | undefined;
  readonly publish: boolean;
  readonly fieldCertificateReferenceScript?: UTxO;
  /** The reference inputs the step reads besides its carriage and verifiers. */
  readonly baseReferences: readonly UTxO[];
}): Promise<TransitionFieldStep> => {
  const field = transitionTracePhaseField(state);
  const certificatePolicyId = contracts.fieldPreimageCertificate.policyId;
  const carriage =
    field === null
      ? undefined
      : await prepareTransitionFieldCarriage({
          lucid,
          network,
          signer,
          proof,
          field,
          certificatePolicyId,
          certificateMintingScript:
            contracts.fieldPreimageCertificate.mintingScript,
          certificateReferenceScript: async () =>
            fieldCertificateReferenceScript ??
            (await requireDeploymentReferenceScript({
              lucid,
              deploymentInfo,
              name: "fieldPreimageCertificateMint",
            })),
          publish,
        });
  const verifiers: readonly TransitionOutputVerifier[] =
    transitionTraceOutputStepIsWhole(state, outputCbors)
      ? await Promise.all(
          (["l2OutputDescriptor", "l2OutputSummary"] as const).map(
            async (key) => ({
              key,
              reference: await requireDeploymentReferenceScript({
                lucid,
                deploymentInfo,
                name: TRANSITION_TRACE_YIELD_REFERENCES[key].entry,
              }),
              validator: contracts.transitionTrace.yields[key],
            }),
          ),
        )
      : [];
  const requireCarriage = (requested: TransitionTraceField) => {
    if (carriage === undefined || requested !== field)
      throw new Error("transition checkpoint opens another field");
    return carriage.planned;
  };
  // Opening indices are positions in the transaction's complete, sorted
  // reference-input set, so they are resolved against all of it.
  const referenceInputs = distinctUtxos([
    ...baseReferences,
    ...(carriage?.references ?? []),
    ...verifiers.map((verifier) => verifier.reference),
  ]);
  return {
    referenceInputs,
    openings: {
      fieldOpening: (requested) =>
        faultProofFieldOpening({
          planned: requireCarriage(requested),
          referenceInputs,
          certificatePolicyId,
          label: "transition-trace field opening",
        }),
      fieldCarriage: (requested) =>
        faultProofFieldCarriage({
          planned: requireCarriage(requested),
          referenceInputs,
          certificatePolicyId,
          label: "transition-trace field carriage",
        }),
    },
    withdraw: (builder, planned) => {
      if ((planned.verifiers === undefined) !== (verifiers.length === 0))
        throw new Error("transition output step disagrees on its verifiers");
      return verifiers.reduce(
        (next, verifier) =>
          next.withdraw(
            validatorToRewardAddress(
              network,
              verifier.validator.withdrawalScript,
            ),
            0n,
            verifier.key === "l2OutputDescriptor"
              ? planned.verifiers!.descriptor
              : planned.verifiers!.summary,
          ),
        builder,
      );
    },
    referenceScripts: verifiers.map((verifier) => ({
      role: TRANSITION_TRACE_YIELD_REFERENCES[verifier.key].role,
      utxo: verifier.reference,
      expectedScript: verifier.validator.withdrawalScript,
    })),
  };
};

/** Each UTxO once, in first-seen order. */
export const distinctUtxos = (utxos: readonly UTxO[]): readonly UTxO[] => {
  const seen = new Set<string>();
  return utxos.filter((utxo) => {
    const key = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
};
