import {
  acceptedVerdictSubjectV1,
  type FieldCarriageV1,
  type ForcedInclusionTxV1,
  forcedVerdictSubjectV1,
  type HeaderV1,
  type NativeTxWitnessSetCompact,
  type OutputReference,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { requireLinearFaultThreadUtxoV1 } from "../linear-fault-family-v1.js";
import { submitMissingNativeScriptTxBindingV1 } from "../missing-native-script-tx/submit-native-binding-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ScriptIntegrityHashMissingContractsV1 } from "./contracts-v1.js";
import type { ScriptIntegrityHashMissingEvidenceV1 } from "./family-v1.js";
import { prepareScriptIntegrityHashMissingEvidenceV1 } from "./family-v1.js";
import {
  ScriptIntegritySpendRedeemersV1,
  ScriptIntegrityStep02DatumV1Schema,
  ScriptIntegrityStep03DatumV1Schema,
  ScriptIntegrityStep04DatumV1Schema,
} from "./schemas-v1.js";
import {
  submitScriptIntegrityHashMissingStep01V1,
  submitScriptIntegrityHashMissingStep02V1,
  submitScriptIntegrityHashMissingStep03V1,
} from "./submitters-v1.js";

type Common = {
  readonly lucid: LucidEvolution;
  readonly contracts: ScriptIntegrityHashMissingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
};

export const submitScriptIntegrityHashMissingStep01AcceptedV1 = async ({
  blueprint,
  network,
  stateQueueBlockOutRef,
  txInclusion,
  witnessReferenceScripts,
  ...common
}: Common & {
  readonly blueprint: unknown;
  readonly network: Network;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: "script-integrity-hash-missing",
    stepIndex: 0,
    threadOutRef: common.threadOutRef,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: common.signer.paymentKeyHash,
      data: {
        BoundAccepted: {
          subject: acceptedVerdictSubjectV1(txInclusion.nativeTxId),
          witness_set_hash: txInclusion.nativeTx.witness_set_hash,
        },
      },
    } as never,
    ScriptIntegrityStep02DatumV1Schema as never,
  );
  return await submitMissingNativeScriptTxBindingV1({
    lucid: common.lucid,
    blueprint,
    network,
    contracts: common.contracts,
    signer: common.signer,
    stepIndex: 0,
    threadUtxo,
    threadToken,
    stateQueueBlockOutRef,
    txInclusion,
    nextDatum,
    spendRedeemerSchema: ScriptIntegritySpendRedeemersV1[0],
    wrapInclusionArgs: (inclusion) => ({
      BindAccepted: { carriage: { RedeemerCarriedInclusion: [inclusion] } },
    }),
    referenceScriptUtxo: common.referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary: common.preSubmitBoundary,
    awaitConfirmation: common.awaitConfirmation ?? true,
  });
};

export const submitScriptIntegrityHashMissingStep01ForcedV1 = async (
  args: Common & { readonly direction: 0n | 1n },
) => {
  const nextDatum = Data.to(
    {
      fraud_prover: args.signer.paymentKeyHash,
      data: {
        PendingForced: { direction: args.direction },
      },
    } as never,
    ScriptIntegrityStep02DatumV1Schema as never,
  );
  return await submitScriptIntegrityHashMissingStep01V1({
    ...args,
    nextDatum,
    buildArgs: ({ input_index, output_index }) => ({
      RecordForced: { direction: args.direction, input_index, output_index },
    }),
  });
};

export const submitScriptIntegrityHashMissingStep02BindingV1 = async ({
  header,
  forcedMembership,
  witnessSetHash,
  ...args
}: Common & {
  readonly header: HeaderV1;
  readonly forcedMembership: RootMembershipProof<
    OutputReference,
    ForcedInclusionTxV1
  > | null;
  readonly witnessSetHash: string;
}) => {
  const subject =
    forcedMembership === null
      ? (() => {
          throw new Error(
            "accepted step-02 requires subject supplied by use of accepted helper",
          );
        })()
      : forcedVerdictSubjectV1({
          transactionId: forcedMembership.value.tx_id,
          sourceKey: forcedMembership.key,
          rejectionReason:
            forcedMembership.value.verdict === "ForcedTxValid"
              ? null
              : forcedMembership.value.verdict.ForcedTxInvalid.reason,
        });
  const nextDatum = Data.to(
    {
      fraud_prover: args.signer.paymentKeyHash,
      data: { subject, witness_set_hash: witnessSetHash },
    } as never,
    ScriptIntegrityStep03DatumV1Schema as never,
  );
  return await submitScriptIntegrityHashMissingStep02V1({
    ...args,
    nextDatum,
    buildArgs: ({ input_index, output_index }) => ({
      input_index,
      output_index,
      header,
      forced_membership: forcedMembership,
    }),
  });
};

export const submitScriptIntegrityHashMissingStep02AcceptedV1 = async ({
  header,
  subject,
  witnessSetHash,
  ...args
}: Common & {
  readonly header: HeaderV1;
  readonly subject: ReturnType<typeof acceptedVerdictSubjectV1>;
  readonly witnessSetHash: string;
}) => {
  const nextDatum = Data.to(
    {
      fraud_prover: args.signer.paymentKeyHash,
      data: { subject, witness_set_hash: witnessSetHash },
    } as never,
    ScriptIntegrityStep03DatumV1Schema as never,
  );
  return await submitScriptIntegrityHashMissingStep02V1({
    ...args,
    nextDatum,
    buildArgs: ({ input_index, output_index }) => ({
      input_index,
      output_index,
      header,
      forced_membership: null,
    }),
  });
};

export const submitScriptIntegrityHashMissingStep03DirectV1 = async ({
  evidence: rawEvidence,
  nativeTxCompactCbor,
  witnessSet,
  ...args
}: Common & {
  readonly evidence: ScriptIntegrityHashMissingEvidenceV1;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
}) => {
  const evidence = prepareScriptIntegrityHashMissingEvidenceV1(rawEvidence);
  const nextDatum = Data.to(
    {
      fraud_prover: args.signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        script_integrity_hash: evidence.scriptIntegrityHash,
        contains_non_native_script: evidence.scriptLanguages.some(
          (language) => language !== 0,
        ),
        has_redeemers: evidence.redeemerCount > 0,
      },
    } as never,
    ScriptIntegrityStep04DatumV1Schema as never,
  );
  const scriptCarriage: FieldCarriageV1 = {
    Inline: { preimage: evidence.scriptWitnessesPreimageCbor },
  };
  const redeemerCarriage: FieldCarriageV1 = {
    Inline: { preimage: evidence.redeemersPreimageCbor },
  };
  return await submitScriptIntegrityHashMissingStep03V1({
    ...args,
    nextDatum,
    buildArgs: ({ input_index, output_index }) => ({
      Direct: {
        input_index,
        output_index,
        native_tx_compact_cbor: nativeTxCompactCbor,
        witness_set: witnessSet,
        script_witnesses: scriptCarriage,
        redeemers: redeemerCarriage,
      },
    }),
  });
};
