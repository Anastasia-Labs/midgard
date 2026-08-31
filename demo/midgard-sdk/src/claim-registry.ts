import {
  buildMidgardMpfProofFoldTraceV1,
  type MidgardMpfProofStepV1,
} from "@al-ft/midgard-core/mpf-proof-fold-v1";
import {
  type Address,
  Data,
  fromText,
  type LucidEvolution,
  type PolicyId,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import {
  type GenericErrorFields,
  H32Schema,
  LucidError,
  type Proof,
  ProofSchema,
  ScriptHashSchema,
} from "@/common.js";
import {
  authenticateUTxOs,
  type AuthenticUTxO,
  fetchSingleAuthenticUTxOProgram,
} from "@/internals.js";
import { EMPTY_MERKLE_TREE_ROOT } from "@/ledger-constants.js";

export const CLAIM_REGISTRY_ASSET_NAME = fromText("MIDGARD_CLAIM_REGISTRY");
export const CLAIM_REGISTRY_LIVE_VALUE = fromText("MIDGARD_CLAIM_LIVE_V1");
export const CLAIM_REGISTRY_CLOSED_VALUE = fromText("MIDGARD_CLAIM_CLOSED_V1");

const NULL_MPF_ROOT = "00".repeat(32);

export const ClaimRegistryDatumSchema = Data.Object({
  claims_root: H32Schema,
  computation_thread_policy_id: ScriptHashSchema,
});
export type ClaimRegistryDatum = Data.Static<typeof ClaimRegistryDatumSchema>;
export const ClaimRegistryDatum =
  ClaimRegistryDatumSchema as unknown as ClaimRegistryDatum;

export const ClaimRegistryMutationProofSchema = Data.Enum([
  Data.Object({ RedeemerCarried: Data.Tuple([ProofSchema]) }),
  Data.Object({
    PublishedChunks: Data.Object({
      ordered_chunk_reference_input_indices: Data.Array(Data.Integer()),
    }),
  }),
]);
export type ClaimRegistryMutationProof = Data.Static<
  typeof ClaimRegistryMutationProofSchema
>;
export const ClaimRegistryMutationProof =
  ClaimRegistryMutationProofSchema as unknown as ClaimRegistryMutationProof;

export const ClaimRegistryMutationSchema = Data.Enum([
  Data.Object({
    OpenClaim: Data.Object({
      claim_id: H32Schema,
      proof: ClaimRegistryMutationProofSchema,
    }),
  }),
  Data.Object({
    CancelClaim: Data.Object({
      claim_id: H32Schema,
      proof: ClaimRegistryMutationProofSchema,
    }),
  }),
  Data.Object({
    CloseClaim: Data.Object({
      claim_id: H32Schema,
      proof: ClaimRegistryMutationProofSchema,
    }),
  }),
]);
export type ClaimRegistryMutation = Data.Static<
  typeof ClaimRegistryMutationSchema
>;
export const ClaimRegistryMutation =
  ClaimRegistryMutationSchema as unknown as ClaimRegistryMutation;

export const ClaimRegistryRedeemerSchema = Data.Enum([
  Data.Object({
    Mutate: Data.Object({ mutation: ClaimRegistryMutationSchema }),
  }),
  Data.Object({
    Deinit: Data.Object({ hub_oracle_input_index: Data.Integer() }),
  }),
]);
export type ClaimRegistryRedeemer = Data.Static<
  typeof ClaimRegistryRedeemerSchema
>;
export const ClaimRegistryRedeemer =
  ClaimRegistryRedeemerSchema as unknown as ClaimRegistryRedeemer;

export type ClaimRegistryConfig = {
  readonly claimRegistryAddress: Address;
  readonly hubOraclePolicyId: PolicyId;
};

export type ClaimRegistryUTxO = AuthenticUTxO<ClaimRegistryDatum>;

export type ClaimRegistryMutationCarriage =
  | { readonly kind: "redeemer-carried" }
  | {
      readonly kind: "published-chunks";
      readonly orderedChunkReferenceInputIndices: readonly bigint[];
    };

export type ClaimRegistryMutationKind = "open" | "cancel" | "close";

export type ClaimRegistryMutationTransition = {
  readonly datum: ClaimRegistryDatum;
  readonly redeemer: ClaimRegistryRedeemer;
};

export const claimRegistryUnit = (hubOraclePolicyId: PolicyId): string =>
  toUnit(hubOraclePolicyId, CLAIM_REGISTRY_ASSET_NAME);

export const claimIdFromCategoryAndHeader = (
  categoryId: string,
  headerHash: string,
): string => {
  if (!/^[0-9a-f]{8}$/u.test(categoryId)) {
    throw new Error(
      "Claim category id must be exactly four lowercase hex bytes",
    );
  }
  if (!/^[0-9a-f]{56}$/u.test(headerHash)) {
    throw new Error("Claim header hash must be exactly 28 lowercase hex bytes");
  }
  return `${categoryId}${headerHash}`;
};

const proofToCoreSteps = (proof: Proof): readonly MidgardMpfProofStepV1[] =>
  proof.map((step) => {
    if ("Branch" in step) {
      return {
        kind: "branch" as const,
        skip: Number(step.Branch.skip),
        neighbors: Buffer.from(step.Branch.neighbors, "hex"),
      };
    }
    if ("Fork" in step) {
      return {
        kind: "fork" as const,
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: Buffer.from(step.Fork.neighbor.prefix, "hex"),
          root: Buffer.from(step.Fork.neighbor.root, "hex"),
        },
      };
    }
    return {
      kind: "leaf" as const,
      skip: Number(step.Leaf.skip),
      key: Buffer.from(step.Leaf.key, "hex"),
      value: Buffer.from(step.Leaf.value, "hex"),
    };
  });

const foldRoot = (
  claimId: string,
  value: string,
  proof: Proof,
): { readonly including: string; readonly excluding: string } => {
  if (!/^[0-9a-f]{64}$/u.test(claimId)) {
    throw new Error("Claim id must be exactly 32 lowercase hex bytes");
  }
  const terminal = buildMidgardMpfProofFoldTraceV1({
    key: Buffer.from(claimId, "hex"),
    value: Buffer.from(value, "hex"),
    steps: proofToCoreSteps(proof),
  }).terminal;
  return {
    including: Buffer.from(terminal.includingRoot).toString("hex"),
    excluding: Buffer.from(terminal.excludingRoot).toString("hex"),
  };
};

const mutationProofData = (
  proof: Proof,
  carriage: ClaimRegistryMutationCarriage,
): ClaimRegistryMutationProof =>
  carriage.kind === "redeemer-carried"
    ? { RedeemerCarried: [proof] }
    : {
        PublishedChunks: {
          ordered_chunk_reference_input_indices: [
            ...carriage.orderedChunkReferenceInputIndices,
          ],
        },
      };

/**
 * Derives the exact successor datum from the same authenticated MPF proof that
 * the on-chain singleton consumes. The next root is never caller supplied.
 */
export const buildClaimRegistryMutationTransition = ({
  currentDatum,
  kind,
  claimId,
  proof,
  carriage,
}: {
  readonly currentDatum: ClaimRegistryDatum;
  readonly kind: ClaimRegistryMutationKind;
  readonly claimId: string;
  readonly proof: Proof;
  readonly carriage: ClaimRegistryMutationCarriage;
}): ClaimRegistryMutationTransition => {
  const live = foldRoot(claimId, CLAIM_REGISTRY_LIVE_VALUE, proof);
  const proofData = mutationProofData(proof, carriage);
  let nextRoot: string;
  let mutation: ClaimRegistryMutation;

  if (kind === "open") {
    const predecessorMatches =
      currentDatum.claims_root === live.excluding ||
      (currentDatum.claims_root === EMPTY_MERKLE_TREE_ROOT &&
        live.excluding === NULL_MPF_ROOT);
    if (!predecessorMatches) {
      throw new Error("Claim-registry open proof does not match current root");
    }
    nextRoot = live.including;
    mutation = { OpenClaim: { claim_id: claimId, proof: proofData } };
  } else if (kind === "cancel") {
    if (currentDatum.claims_root !== live.including) {
      throw new Error("Claim-registry cancel proof does not match live root");
    }
    nextRoot =
      live.excluding === NULL_MPF_ROOT
        ? EMPTY_MERKLE_TREE_ROOT
        : live.excluding;
    mutation = { CancelClaim: { claim_id: claimId, proof: proofData } };
  } else {
    if (currentDatum.claims_root !== live.including) {
      throw new Error("Claim-registry close proof does not match live root");
    }
    nextRoot = foldRoot(claimId, CLAIM_REGISTRY_CLOSED_VALUE, proof).including;
    mutation = { CloseClaim: { claim_id: claimId, proof: proofData } };
  }

  return {
    datum: {
      claims_root: nextRoot,
      computation_thread_policy_id: currentDatum.computation_thread_policy_id,
    },
    redeemer: { Mutate: { mutation } },
  };
};

export const utxosToClaimRegistryUTxOs = (
  utxos: UTxO[],
  hubOraclePolicyId: PolicyId,
): Effect.Effect<ClaimRegistryUTxO[], LucidError> =>
  authenticateUTxOs<ClaimRegistryDatum>(
    utxos,
    hubOraclePolicyId,
    ClaimRegistryDatum,
  ).pipe(
    Effect.map((authentic) =>
      authentic.filter(
        ({ assetName }) => assetName === CLAIM_REGISTRY_ASSET_NAME,
      ),
    ),
  );

export class ClaimRegistryError extends EffectData.TaggedError(
  "ClaimRegistryError",
)<GenericErrorFields> {}

export const fetchClaimRegistryUTxOProgram = (
  lucid: LucidEvolution,
  config: ClaimRegistryConfig,
): Effect.Effect<ClaimRegistryUTxO, ClaimRegistryError | LucidError> =>
  fetchSingleAuthenticUTxOProgram<
    ClaimRegistryUTxO,
    LucidError,
    ClaimRegistryError
  >(lucid, {
    address: config.claimRegistryAddress,
    policyId: config.hubOraclePolicyId,
    utxoLabel: "claim registry",
    conversionFunction: utxosToClaimRegistryUTxOs,
    onUnexpectedAuthenticUTxOCount: () =>
      new ClaimRegistryError({
        message: "Failed to fetch the claim-registry UTxO",
        cause:
          "Exactly one authentic claim-registry UTxO was expected, but none or more were found",
      }),
  });
