import {
  acceptedVerdictSubject,
  type FieldCarriage,
  type ForcedInclusionTx,
  forcedVerdictSubject,
  type Header,
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

import { requireLinearFaultThreadUtxo } from "../linear-fault-family-v1.js";
import { submitMissingNativeScriptTxBinding } from "../missing-native-script-tx/submit-native-binding-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { ScriptIntegrityHashMissingContracts } from "./contracts-v1.js";
import type { ScriptIntegrityHashMissingEvidence } from "./family-v1.js";
import { prepareScriptIntegrityHashMissingEvidence } from "./family-v1.js";
import {
  ScriptIntegritySpendRedeemers,
  ScriptIntegrityStep02DatumSchema,
  ScriptIntegrityStep03DatumSchema,
  ScriptIntegrityStep04DatumSchema,
} from "./schemas-v1.js";
import {
  submitScriptIntegrityHashMissingStep01,
  submitScriptIntegrityHashMissingStep02,
  submitScriptIntegrityHashMissingStep03,
} from "./submitters-v1.js";

type Common = {
  readonly lucid: LucidEvolution;
  readonly contracts: ScriptIntegrityHashMissingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
};

export const submitScriptIntegrityHashMissingStep01Accepted = async ({
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
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
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
          subject: acceptedVerdictSubject(txInclusion.nativeTxId),
          witness_set_hash: txInclusion.nativeTx.witness_set_hash,
        },
      },
    } as never,
    ScriptIntegrityStep02DatumSchema as never,
  );
  return await submitMissingNativeScriptTxBinding({
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
    spendRedeemerSchema: ScriptIntegritySpendRedeemers[0],
    wrapInclusionArgs: (inclusion) => ({
      BindAccepted: { carriage: { RedeemerCarriedInclusion: [inclusion] } },
    }),
    referenceScriptUtxo: common.referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary: common.preSubmitBoundary,
    awaitConfirmation: common.awaitConfirmation ?? true,
  });
};

export const submitScriptIntegrityHashMissingStep01Forced = async (
  args: Common & { readonly direction: 0n | 1n },
) => {
  const nextDatum = Data.to(
    {
      fraud_prover: args.signer.paymentKeyHash,
      data: {
        PendingForced: { direction: args.direction },
      },
    } as never,
    ScriptIntegrityStep02DatumSchema as never,
  );
  return await submitScriptIntegrityHashMissingStep01({
    ...args,
    nextDatum,
    buildArgs: ({ input_index, output_index }) => ({
      RecordForced: { direction: args.direction, input_index, output_index },
    }),
  });
};

export const submitScriptIntegrityHashMissingStep02Binding = async ({
  header,
  forcedMembership,
  witnessSetHash,
  ...args
}: Common & {
  readonly header: Header;
  readonly forcedMembership: RootMembershipProof<
    OutputReference,
    ForcedInclusionTx
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
      : forcedVerdictSubject({
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
    ScriptIntegrityStep03DatumSchema as never,
  );
  return await submitScriptIntegrityHashMissingStep02({
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

export const submitScriptIntegrityHashMissingStep02Accepted = async ({
  header,
  subject,
  witnessSetHash,
  ...args
}: Common & {
  readonly header: Header;
  readonly subject: ReturnType<typeof acceptedVerdictSubject>;
  readonly witnessSetHash: string;
}) => {
  const nextDatum = Data.to(
    {
      fraud_prover: args.signer.paymentKeyHash,
      data: { subject, witness_set_hash: witnessSetHash },
    } as never,
    ScriptIntegrityStep03DatumSchema as never,
  );
  return await submitScriptIntegrityHashMissingStep02({
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

export const submitScriptIntegrityHashMissingStep03Direct = async ({
  evidence: rawEvidence,
  nativeTxCompactCbor,
  witnessSet,
  ...args
}: Common & {
  readonly evidence: ScriptIntegrityHashMissingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSet: NativeTxWitnessSetCompact;
}) => {
  const evidence = prepareScriptIntegrityHashMissingEvidence(rawEvidence);
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
    ScriptIntegrityStep04DatumSchema as never,
  );
  const scriptCarriage: FieldCarriage = {
    Inline: { preimage: evidence.scriptWitnessesPreimageCbor },
  };
  const redeemerCarriage: FieldCarriage = {
    Inline: { preimage: evidence.redeemersPreimageCbor },
  };
  return await submitScriptIntegrityHashMissingStep03({
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
