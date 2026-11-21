import type { VerdictSubject } from "@al-ft/midgard-sdk/fraud-proof";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { buildLinearFaultFinalize } from "../linear-fault-finalize.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { MintItemNonCanonicalContracts } from "./contracts.js";
import {
  MINT_ITEM_OUTCOME_UNREACHABLE,
  mintItemClaimIsUnreachable,
  type MintItemEvidence,
  mintItemEvidenceCloses,
} from "./mint-item-non-canonical.js";
import {
  MintItemStep04DatumSchema,
  MintItemStep04RedeemerSchema,
} from "./schemas.js";

type FinalizeInput = Omit<
  Parameters<typeof buildLinearFaultFinalize>[0],
  | "family"
  | "stepIndex"
  | "step"
  | "computationThread"
  | "fraudProof"
  | "spendRedeemerSchema"
  | "buildFamilyArgs"
> & { readonly contracts: MintItemNonCanonicalContracts };

const finalize = async ({ contracts, ...input }: FinalizeInput) =>
  await buildLinearFaultFinalize({
    ...input,
    family: "mint-item-non-canonical",
    stepIndex: 3,
    step: contracts.steps[3],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    spendRedeemerSchema: MintItemStep04RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
  });

export const buildMintItemNonCanonicalStep04 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintItemNonCanonicalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: MintItemEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  if (!mintItemEvidenceCloses(evidence)) {
    throw new Error(
      "mint-item-non-canonical: terminal scan does not contradict verdict",
    );
  }
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "mint-item-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    item_index: bigint;
    item_length: bigint;
    item_hash: string;
    chunk_hashes: readonly string[];
    outcome: bigint;
  }>({
    threadUtxo,
    signer,
    schema: MintItemStep04DatumSchema as never,
    family: "mint-item-non-canonical",
    stepIndex,
  });
  const expectedOutcome = evidence.canonical ? 1n : 2n;
  if (
    state.item_index !== BigInt(evidence.itemIndex) ||
    state.item_length !== BigInt(evidence.itemLength) ||
    state.item_hash !== evidence.itemHash ||
    state.chunk_hashes.join(":") !== evidence.chunkHashes.join(":") ||
    state.outcome !== expectedOutcome
  ) {
    throw new Error(
      "mint-item-non-canonical: terminal state differs from prepared evidence",
    );
  }
  return await finalize({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadToken,
    referenceScriptUtxo,
    witnessReferenceScripts,
  });
};

/**
 * Finalizes the terminal state step 01 wrote for a forced `MintItemMalformed`
 * rejection: no machine step emits that reason, so the rejection is wrongful.
 */
export const buildMintItemNonCanonicalStep04UnreachableClaim = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintItemNonCanonicalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "mint-item-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: VerdictSubject;
    outcome: bigint;
  }>({
    threadUtxo,
    signer,
    schema: MintItemStep04DatumSchema as never,
    family: "mint-item-non-canonical",
    stepIndex,
  });
  if (
    state.outcome !== MINT_ITEM_OUTCOME_UNREACHABLE ||
    !mintItemClaimIsUnreachable(state.subject)
  )
    throw new Error(
      "mint-item-non-canonical: terminal state is not an unreachable claim",
    );
  return await finalize({
    lucid,
    contracts,
    signer,
    threadUtxo,
    threadToken,
    referenceScriptUtxo,
    witnessReferenceScripts,
  });
};
