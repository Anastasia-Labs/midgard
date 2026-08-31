/**
 * Atomic claim-registry composition for computation-thread transactions.
 *
 * Init opens `(category, header)`, cancellation deletes only a live claim, and
 * successful finalization permanently changes that live claim to `Closed`.
 * The successor root is derived from the same proof consumed on-chain; callers
 * never supply or override either registry root.
 */
import {
  buildClaimRegistryMutationTransition,
  buildClaimRegistrySpendingValidator,
  claimIdFromCategoryAndHeader,
  ClaimRegistryDatum,
  type ClaimRegistryDatum as ClaimRegistryDatumData,
  type ClaimRegistryMutationKind,
  ClaimRegistryRedeemer,
  claimRegistryUnit,
  fetchClaimRegistryUTxOProgram,
  parseFaultProofBlueprint,
  Proof,
  type Proof as ProofData,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { parseContractDeploymentInfo } from "./inspect-contracts.js";
import {
  derivedChunkReferenceIndices,
  type PublishedProofChunkV1,
  requireBuiltChunkReferenceIndices,
  splitProofIntoChunkDatums,
} from "./proof-chunk-carriage.js";
import {
  requireDeploymentReferenceScript,
  requireDeploymentScriptHash,
} from "./runtime.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import { witnessSpendingValidatorCarriageV1 } from "./witness-reference-scripts-v1.js";

export type ClaimRegistryMutationEvidenceV1 = {
  /** Exact proof steps used to derive the predecessor and successor roots. */
  readonly proof: ProofData;
  /** Non-empty only when the exact proof is carried in published chunk UTxOs. */
  readonly publishedProofChunks?: readonly PublishedProofChunkV1[];
};

export type PreparedClaimRegistryMutationV1 = {
  readonly claimId: string;
  readonly kind: ClaimRegistryMutationKind;
  /** Authenticated predecessor identity; used to reject stale workflow proofs. */
  readonly predecessorOutRef: string;
  readonly predecessorDatum: ClaimRegistryDatumData;
  readonly registryUtxo: UTxO;
  readonly registryScript: Script;
  readonly referenceInputs: readonly UTxO[];
  readonly referenceScriptUtxo: UTxO;
  readonly outputDatum: string;
  readonly apply: (tx: TxBuilder) => TxBuilder;
};

/** Fail closed before a family composes an atomic Open/Cancel/Close. */
export const requirePreparedClaimRegistryMutationV1 = ({
  mutation,
  kind,
  claimId,
  label,
}: {
  readonly mutation: PreparedClaimRegistryMutationV1 | undefined;
  readonly kind: ClaimRegistryMutationKind;
  readonly claimId: string;
  readonly label: string;
}): PreparedClaimRegistryMutationV1 => {
  if (mutation === undefined) {
    throw new Error(`${label}: claim-registry ${kind} mutation is required`);
  }
  if (mutation.kind !== kind || mutation.claimId !== claimId) {
    throw new Error(
      `${label}: claim-registry mutation changed its ${kind}/${claimId} identity`,
    );
  }
  return mutation;
};

const canonicalProofCbor = (proof: ProofData): string => Data.to(proof, Proof);

const requirePublishedChunksMatchProof = ({
  proof,
  chunks,
}: {
  readonly proof: ProofData;
  readonly chunks: readonly PublishedProofChunkV1[];
}): void => {
  const expectedDatums = splitProofIntoChunkDatums(canonicalProofCbor(proof));
  if (expectedDatums.length !== chunks.length || chunks.length === 0) {
    throw new Error(
      "Claim-registry published carriage must contain every chunk of one non-empty proof.",
    );
  }
  for (const [position, expected] of expectedDatums.entries()) {
    const chunk = chunks[position]!;
    if (chunk.datumCbor !== expected) {
      throw new Error(
        `Claim-registry proof chunk ${chunk.outRef} at position ${position.toString()} does not match the mutation proof.`,
      );
    }
  }
};

/**
 * Fetches and authenticates the unique singleton and returns a composable
 * transaction fragment. Published proof indices are derived twice: first from
 * the complete proposed reference set and then from Lucid's built context.
 */
export const prepareClaimRegistryMutationV1 = async ({
  lucid,
  claimRegistryAddress,
  claimRegistryScript,
  claimRegistryReferenceUtxo,
  hubOraclePolicyId,
  computationThreadPolicyId,
  categoryId,
  headerHash,
  claimId: suppliedClaimId,
  kind,
  evidence = { proof: [] },
}: {
  readonly lucid: LucidEvolution;
  readonly claimRegistryAddress: string;
  readonly claimRegistryScript: Script;
  readonly claimRegistryReferenceUtxo: UTxO | undefined;
  readonly hubOraclePolicyId: string;
  readonly computationThreadPolicyId: string;
  readonly categoryId?: string;
  readonly headerHash?: string;
  /** Exact computation-thread asset name; equivalent to categoryId+headerHash. */
  readonly claimId?: string;
  readonly kind: ClaimRegistryMutationKind;
  /** Omission is the canonical empty-registry proof; it fails after any claim. */
  readonly evidence?: ClaimRegistryMutationEvidenceV1;
}): Promise<PreparedClaimRegistryMutationV1> => {
  const authentic = await Effect.runPromise(
    fetchClaimRegistryUTxOProgram(lucid, {
      claimRegistryAddress,
      hubOraclePolicyId,
    }),
  );
  if (
    authentic.datum.computation_thread_policy_id !== computationThreadPolicyId
  ) {
    throw new Error(
      "Claim-registry datum is bound to a different computation-thread policy.",
    );
  }

  const chunks = evidence.publishedProofChunks ?? [];
  const carriedByChunks = chunks.length > 0;
  if (carriedByChunks) {
    requirePublishedChunksMatchProof({ proof: evidence.proof, chunks });
  }
  const carriage = carriedByChunks
    ? ({
        kind: "published-chunks",
        orderedChunkReferenceInputIndices: [],
      } as const)
    : ({ kind: "redeemer-carried" } as const);
  const claimId =
    suppliedClaimId ??
    (categoryId !== undefined && headerHash !== undefined
      ? claimIdFromCategoryAndHeader(categoryId, headerHash)
      : undefined);
  if (claimId === undefined || !/^[0-9a-f]{64}$/u.test(claimId)) {
    throw new Error(
      "Claim-registry mutation requires one exact 32-byte computation-thread asset name.",
    );
  }
  const rootTransition = buildClaimRegistryMutationTransition({
    currentDatum: authentic.datum,
    kind,
    claimId,
    proof: evidence.proof,
    carriage,
  });
  const outputDatum = Data.to(rootTransition.datum, ClaimRegistryDatum);
  const outputMatches = computationThreadOutputPredicate({
    address: claimRegistryAddress,
    datum: outputDatum,
    unit: claimRegistryUnit(hubOraclePolicyId),
  });
  const registryCarriage = witnessSpendingValidatorCarriageV1({
    script: claimRegistryScript,
    referenceUtxo: claimRegistryReferenceUtxo,
    label: `claim-registry ${kind}`,
  });
  const referenceInputs = [
    ...chunks.map(({ utxo }) => utxo),
    ...registryCarriage.referenceInputs,
  ];
  const derivedChunkIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks,
    label: `claim-registry ${kind}`,
  });
  const redeemer = ((ctx) => {
    requireBuiltChunkReferenceIndices({
      ctx,
      chunks,
      derived: derivedChunkIndices,
      label: `claim-registry ${kind}`,
    });
    const matchingOutputs = ctx.outputs.filter(outputMatches);
    if (matchingOutputs.length !== 1) {
      throw new Error(
        `Claim-registry ${kind} must create exactly one authentic successor output.`,
      );
    }
    const transition = buildClaimRegistryMutationTransition({
      currentDatum: authentic.datum,
      kind,
      claimId,
      proof: evidence.proof,
      carriage: carriedByChunks
        ? {
            kind: "published-chunks",
            orderedChunkReferenceInputIndices: derivedChunkIndices,
          }
        : { kind: "redeemer-carried" },
    });
    return Data.to(transition.redeemer, ClaimRegistryRedeemer);
  }) satisfies BuildTxWithRedeemer;

  const referenceScriptUtxo = registryCarriage.referenceInputs[0]!;
  return {
    claimId,
    kind,
    predecessorOutRef: `${authentic.utxo.txHash}#${authentic.utxo.outputIndex.toString()}`,
    predecessorDatum: authentic.datum,
    registryUtxo: authentic.utxo,
    registryScript: claimRegistryScript,
    referenceInputs,
    referenceScriptUtxo,
    outputDatum,
    apply: (tx) => {
      return registryCarriage
        .attach(
          tx.readFrom(referenceInputs).collectFrom([authentic.utxo], redeemer),
        )
        .pay.ToContract(
          claimRegistryAddress,
          { kind: "inline", value: outputDatum },
          authentic.utxo.assets,
        );
    },
  };
};

/**
 * Deployment-bound counterpart used by submitters: derives the validator from
 * the current blueprint, checks the manifest hash, and fetches the mandatory
 * published reference script before touching the singleton.
 */
export const prepareDeploymentClaimRegistryMutationV1 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  computationThreadPolicyId,
  claimRegistryReferenceUtxo,
  categoryId,
  headerHash,
  claimId,
  kind,
  evidence,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  /**
   * Published `claim_registry.spend` reference UTxO from the caller's witness
   * roster. Deployments that name the validator by hash alone carry its
   * reference script on that roster instead of in the manifest; the supplied
   * UTxO is still hash-checked against the validator derived here, so this is a
   * carriage route rather than a trust shortcut. Omit it to source the
   * reference script from the manifest's own `claimRegistrySpend` entry.
   */
  readonly claimRegistryReferenceUtxo?: UTxO;
  readonly categoryId?: string;
  readonly headerHash?: string;
  readonly claimId?: string;
  readonly kind: ClaimRegistryMutationKind;
  readonly evidence?: ClaimRegistryMutationEvidenceV1;
}): Promise<PreparedClaimRegistryMutationV1> => {
  const parsedDeployment = parseContractDeploymentInfo(deploymentInfo);
  const hubOraclePolicyId = requireDeploymentScriptHash(
    parsedDeployment,
    "hubOracleMint",
  );
  const claimRegistry = await Effect.runPromise(
    buildClaimRegistrySpendingValidator({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId,
    }),
  );
  const deployedHash = requireDeploymentScriptHash(
    parsedDeployment,
    "claimRegistrySpend",
  );
  if (claimRegistry.spendingScriptHash !== deployedHash) {
    throw new Error(
      `Claim-registry script hash mismatch: deployment=${deployedHash}, derived=${claimRegistry.spendingScriptHash}.`,
    );
  }
  const referenceUtxo =
    claimRegistryReferenceUtxo ??
    (await requireDeploymentReferenceScript({
      lucid,
      deploymentInfo: parsedDeployment,
      name: "claimRegistrySpend",
    }));
  return await prepareClaimRegistryMutationV1({
    lucid,
    claimRegistryAddress: claimRegistry.spendingScriptAddress,
    claimRegistryScript: claimRegistry.spendingScript,
    claimRegistryReferenceUtxo: referenceUtxo,
    hubOraclePolicyId,
    computationThreadPolicyId,
    ...(categoryId === undefined ? {} : { categoryId }),
    ...(headerHash === undefined ? {} : { headerHash }),
    ...(claimId === undefined ? {} : { claimId }),
    kind,
    ...(evidence === undefined ? {} : { evidence }),
  });
};

/**
 * The applied `claim_registry.spend` validator a family contracts record
 * carries. Every family submitter has to open, close or cancel its claim in
 * the same transaction as the computation-thread mint, so the validator is
 * resolved once when the contracts record is built rather than re-derived from
 * the blueprint at each of the ~50 call sites — most of which are handed a
 * contracts record and nothing else.
 */
export type FaultProofClaimRegistryContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * Derives the applied `claim_registry.spend` validator a family contracts
 * record carries. When a deployment manifest is supplied the derived hash is
 * cross-checked against the deployed `claimRegistrySpend` entry, so a contracts
 * record can never carry a validator the deployment does not publish.
 */
export const resolveFaultProofClaimRegistryContractV1 = async ({
  blueprint,
  network,
  hubOraclePolicyId,
  deploymentInfo,
}: {
  readonly blueprint: unknown;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly deploymentInfo?: unknown;
}): Promise<FaultProofClaimRegistryContractV1> => {
  const claimRegistry = await Effect.runPromise(
    buildClaimRegistrySpendingValidator({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId,
    }),
  );
  if (deploymentInfo !== undefined) {
    const deployedHash = requireDeploymentScriptHash(
      parseContractDeploymentInfo(deploymentInfo),
      "claimRegistrySpend",
    );
    if (claimRegistry.spendingScriptHash !== deployedHash) {
      throw new Error(
        `Claim-registry script hash mismatch: deployment=${deployedHash}, derived=${claimRegistry.spendingScriptHash}.`,
      );
    }
  }
  return claimRegistry;
};

/**
 * Contracts-bound preparation: the family submitters' door. It takes the
 * applied validator the contracts record already carries, so it needs neither
 * a blueprint nor a deployment manifest.
 */
export const prepareFamilyClaimRegistryMutationV1 = async ({
  lucid,
  claimRegistry,
  claimRegistryReferenceUtxo,
  hubOraclePolicyId,
  computationThreadPolicyId,
  categoryId,
  headerHash,
  claimId,
  kind,
  evidence,
}: {
  readonly lucid: LucidEvolution;
  readonly claimRegistry: FaultProofClaimRegistryContractV1;
  readonly claimRegistryReferenceUtxo: UTxO | undefined;
  readonly hubOraclePolicyId: string;
  readonly computationThreadPolicyId: string;
  readonly categoryId?: string;
  readonly headerHash?: string;
  readonly claimId?: string;
  readonly kind: ClaimRegistryMutationKind;
  readonly evidence?: ClaimRegistryMutationEvidenceV1;
}): Promise<PreparedClaimRegistryMutationV1> =>
  await prepareClaimRegistryMutationV1({
    lucid,
    claimRegistryAddress: claimRegistry.spendingScriptAddress,
    claimRegistryScript: claimRegistry.spendingScript,
    claimRegistryReferenceUtxo,
    hubOraclePolicyId,
    computationThreadPolicyId,
    ...(categoryId === undefined ? {} : { categoryId }),
    ...(headerHash === undefined ? {} : { headerHash }),
    ...(claimId === undefined ? {} : { claimId }),
    kind,
    ...(evidence === undefined ? {} : { evidence }),
  });

/**
 * Blueprint-bound counterpart for the call sites that hold a blueprint but no
 * applied contracts record: derives `claim_registry.spend` from the blueprint
 * and the hub-oracle policy id the computation-thread policy was parameterized
 * with, then delegates to the contracts-bound door above.
 */
export const prepareBlueprintClaimRegistryMutationV1 = async ({
  lucid,
  blueprint,
  network,
  hubOraclePolicyId,
  claimRegistryReferenceUtxo,
  computationThreadPolicyId,
  categoryId,
  headerHash,
  claimId,
  kind,
  evidence,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly claimRegistryReferenceUtxo: UTxO | undefined;
  readonly computationThreadPolicyId: string;
  readonly categoryId?: string;
  readonly headerHash?: string;
  readonly claimId?: string;
  readonly kind: ClaimRegistryMutationKind;
  readonly evidence?: ClaimRegistryMutationEvidenceV1;
}): Promise<PreparedClaimRegistryMutationV1> => {
  const claimRegistry = await Effect.runPromise(
    buildClaimRegistrySpendingValidator({
      blueprint: parseFaultProofBlueprint(blueprint),
      network,
      hubOraclePolicyId,
    }),
  );
  return await prepareFamilyClaimRegistryMutationV1({
    lucid,
    claimRegistry,
    claimRegistryReferenceUtxo,
    hubOraclePolicyId,
    computationThreadPolicyId,
    ...(categoryId === undefined ? {} : { categoryId }),
    ...(headerHash === undefined ? {} : { headerHash }),
    ...(claimId === undefined ? {} : { claimId }),
    kind,
    ...(evidence === undefined ? {} : { evidence }),
  });
};
