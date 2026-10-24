/** Synchronous per-step event/transition/ledger-carriage planning.
 * The caller keeps accepted-source awaits and its missing-reference refusal. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { LinearFaultContracts } from "../linear-fault-family.js";
import {
  builtChunkReferenceIndices,
  chunkedNonMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  type PublishedProofChunk,
} from "../proof-chunk-carriage.js";
import {
  encodeRawPexcludesProofRedeemer,
  getCompiledScript,
  phasMembershipRewardAddress,
} from "../runtime.js";
import { PEXCLUDES_EXCLUSION_WITHDRAW_TITLE } from "../step-support.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../witness-reference-scripts.js";
import { prepareCountedMembershipCarriage } from "./count-carriage.js";
import type { PreparedInputExistence } from "./prepare.js";

export type InputExistenceContracts = LinearFaultContracts & {
  hubOraclePolicyId: string;
  computationThread: { policyId: string; mintingScript: Script };
  fraudProof: {
    policyId: string;
    mintingScript: Script;
    spendingScriptAddress: string;
  };
};

export const inputExistenceStates = (prepared: PreparedInputExistence) =>
  [
    {
      subject: prepared.source.subject,
      header: prepared.source.header,
      compact: prepared.compact,
    },
    {
      subject: prepared.source.subject,
      header: prepared.source.header,
      compact: prepared.compact,
      source_kind: BigInt(prepared.field),
      input_index: prepared.inputIndex,
      selected_input: prepared.selectedInput,
      claimed_input: prepared.claimedInput,
      step_index: prepared.eventMembership.value.step_index,
    },
    {
      subject: prepared.source.subject,
      compact: prepared.compact,
      network_order_context: SDK.networkOrderContext(
        prepared.compact,
        prepared.source.header,
      ),
      source_kind: BigInt(prepared.field),
      input_index: prepared.inputIndex,
      selected_input: prepared.selectedInput,
      claimed_input: prepared.claimedInput,
      pre_utxos_root: prepared.transitionMembership.value.pre_utxos_root,
    },
  ] as const;

export const prepareInputExistenceStepCarriage = ({
  stepIndex,
  prepared,
  blueprint,
  network,
  stepReference,
  fieldCarriageUtxos,
  publishedProofChunks,
  witnessReferenceScripts,
  family,
  referenceInputs,
  candidates,
}: {
  readonly stepIndex: 0 | 1 | 2 | 3;
  readonly prepared: PreparedInputExistence;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly stepReference: UTxO;
  readonly fieldCarriageUtxos: readonly UTxO[];
  readonly publishedProofChunks: readonly PublishedProofChunk[];
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly family: string;
  readonly referenceInputs: readonly UTxO[];
  readonly candidates: {
    role: string;
    utxo: UTxO | undefined;
    expectedScript: Script;
  }[];
}) => {
  let attach: (
    tx: ReturnType<LucidEvolution["newTx"]>,
  ) => ReturnType<LucidEvolution["newTx"]> = (tx) => tx;
  let eventCarriage:
    | ReturnType<
        typeof prepareCountedMembershipCarriage<
          SDK.EventKey,
          SDK.EventToStepValue
        >
      >
    | undefined;
  let transitionCarriage:
    | ReturnType<
        typeof prepareCountedMembershipCarriage<bigint, SDK.TransitionStep>
      >
    | undefined;
  let absenceAddress: string | undefined;
  let absencePublished = false;
  if (stepIndex === 1) {
    eventCarriage = prepareCountedMembershipCarriage({
      witness: prepared.eventMembership,
      keyBytes: Data.to(prepared.eventMembership.key, SDK.EventKey),
      valueBytes: Data.to(prepared.eventMembership.value, SDK.EventToStepValue),
      blueprint,
      network,
      baseReferenceInputs: [...fieldCarriageUtxos, stepReference],
      publishedProofChunks,
      witnessReferenceScripts,
      label: `${family} event`,
    });
    referenceInputs = eventCarriage.referenceInputs;
    attach = eventCarriage.attachWithdrawal;
    candidates.push(...eventCarriage.referenceScriptCandidates);
  } else if (stepIndex === 2) {
    transitionCarriage = prepareCountedMembershipCarriage({
      witness: prepared.transitionMembership,
      keyBytes: Data.to(prepared.transitionMembership.key),
      valueBytes: Data.to(
        prepared.transitionMembership.value,
        SDK.TransitionStep,
      ),
      blueprint,
      network,
      baseReferenceInputs: [stepReference],
      publishedProofChunks,
      witnessReferenceScripts,
      label: `${family} transition`,
    });
    referenceInputs = transitionCarriage.referenceInputs;
    attach = transitionCarriage.attachWithdrawal;
    candidates.push(...transitionCarriage.referenceScriptCandidates);
  } else if (
    stepIndex === 3 &&
    typeof prepared.ledgerEvidence === "object" &&
    "Absent" in prepared.ledgerEvidence
  ) {
    const selected = prepared.selectedInput;
    if (selected === null)
      throw new Error(`${family}: absent ledger key missing`);
    absencePublished = publishedProofChunks.length > 0;
    const script = absencePublished
      ? chunkedVerifyWithdrawalScript(blueprint)
      : {
          type: "PlutusV3" as const,
          script: getCompiledScript(
            blueprint,
            PEXCLUDES_EXCLUSION_WITHDRAW_TITLE,
          ),
        };
    const reference = absencePublished
      ? witnessReferenceScripts?.chunkedVerifyWithdraw
      : witnessReferenceScripts?.pexcludesWithdraw;
    const validator = witnessWithdrawalValidatorCarriage({
      script,
      referenceUtxo: reference,
      label: `${family} ledger exclusion`,
    });
    absenceAddress = phasMembershipRewardAddress(network, script);
    referenceInputs = [
      ...referenceInputs,
      ...publishedProofChunks.map(({ utxo }) => utxo),
      ...validator.referenceInputs,
    ];
    const keyBytes =
      SDK.encodeMidgardTxInputCanonical(selected).toString("hex");
    const proofCbor = Data.to(prepared.ledgerEvidence.Absent.proof, SDK.Proof);
    const address = absenceAddress;
    candidates.push({
      role: `${family} ledger exclusion`,
      utxo: reference,
      expectedScript: script,
    });
    attach = (tx) =>
      validator.attach(
        absencePublished
          ? tx.withdraw(address, 0n, ((ctx) =>
              chunkedNonMembershipClaimRedeemer({
                merkleRoot: prepared.transitionMembership.value.pre_utxos_root,
                keyBytes,
                orderedChunkReferenceInputIndices: builtChunkReferenceIndices({
                  ctx,
                  chunks: publishedProofChunks,
                  label: family,
                }),
              })) satisfies BuildTxWithRedeemer)
          : tx.withdraw(
              address,
              0n,
              encodeRawPexcludesProofRedeemer({
                root: prepared.transitionMembership.value.pre_utxos_root,
                keyBytes,
                nonMembershipProofCbor: proofCbor,
              }),
            ),
      );
  } else if (publishedProofChunks.length > 0) {
    throw new Error(`${family}: this step has no delegated proof claim`);
  }
  return {
    referenceInputs,
    attach,
    eventCarriage,
    transitionCarriage,
    absenceAddress,
    absencePublished,
  };
};

export type InputExistenceStepCarriagePlan = ReturnType<
  typeof prepareInputExistenceStepCarriage
>;
