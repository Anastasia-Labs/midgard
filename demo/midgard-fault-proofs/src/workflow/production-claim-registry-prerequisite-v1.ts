import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import {
  buildClaimRegistryMutationTransition,
  CLAIM_REGISTRY_CLOSED_VALUE,
  CLAIM_REGISTRY_LIVE_VALUE,
  claimIdFromCategoryAndHeader,
  ClaimRegistryDatum,
  type ClaimRegistryDatum as ClaimRegistryDatumData,
  type ClaimRegistryMutationKind,
  claimRegistryUnit,
  type FraudProofCatalogueCategoryName,
  Proof,
  type Proof as ProofData,
} from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  Data,
  type LucidEvolution,
  type Network,
} from "@lucid-evolution/lucid";

import {
  type PreparedClaimRegistryMutationV1,
  prepareDeploymentClaimRegistryMutationV1,
} from "../claim-registry-transaction-v1.js";
import {
  resolvePublishedProofChunksV1,
  splitProofIntoChunkDatums,
} from "../publish-proof-chunks.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  type KeyValuePhasEntry,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../transition-trace/phas.js";
import type { JournalJsonObjectV1 } from "./journal-v1.js";
import type { FraudProofWorkflowActionV1 } from "./orchestrator-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePortV1,
  type ProductionProofChunkPrerequisitePortV1,
} from "./production-proof-chunk-prerequisite-v1.js";
import type { FraudProofAuthenticatedPublicationObserverV1 } from "./raw-l1-publication-observation-v1.js";
import {
  admitFraudProofRawL1SnapshotV1,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY_V1,
  type FraudProofRawL1SnapshotAuthorityV1,
  type FraudProofRawL1SnapshotV1,
} from "./raw-l1-snapshot-v1.js";
import {
  validateVerifiedFraudProofReleaseFinalityPolicyV1,
  type VerifiedFraudProofReleaseFinalityPolicyV1,
} from "./release-finality-policy-v1.js";

export const PRODUCTION_CLAIM_REGISTRY_PREREQUISITE_V1 =
  "midgard-production-claim-registry-prerequisite-v1" as const;
export const PRODUCTION_CLAIM_REGISTRY_PUBLIC_PROOF_DERIVER_V1 =
  "midgard-production-claim-registry-public-proof-deriver-v1" as const;
export const PRODUCTION_CLAIM_REGISTRY_PUBLIC_PROOF_V1 =
  "midgard-production-claim-registry-public-proof-v1" as const;

const OUT_REF = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u;
const CLAIM_ID = /^[0-9a-f]{64}$/u;

export type ProductionClaimRegistryActionV1 = Readonly<{
  kind: ClaimRegistryMutationKind;
}>;

/**
 * Source-neutral proof derivation over already admitted public L1 history.
 * Its result remains untrusted: the prerequisite canonicalizes the proof and
 * independently folds it to the freshly authenticated singleton root.
 */
export interface ProductionClaimRegistryPublicProofDeriverV1 {
  readonly deriverVersion: typeof PRODUCTION_CLAIM_REGISTRY_PUBLIC_PROOF_DERIVER_V1;
  derive(input: {
    readonly snapshot: FraudProofRawL1SnapshotV1;
    readonly claimId: string;
    readonly kind: ClaimRegistryMutationKind;
    readonly predecessorOutRef: string;
    readonly predecessorDatum: ClaimRegistryDatumData;
  }): Promise<unknown>;
}

const outputAssets = (outputCbor: string): Readonly<Record<string, bigint>> =>
  coreToTxOutput(CML.TransactionOutput.from_cbor_hex(outputCbor)).assets;

const outputCarriesExactRegistryUnit = (
  outputCbor: string,
  unit: string,
): boolean => {
  const assets = outputAssets(outputCbor);
  return (
    (assets[unit] ?? 0n) === 1n &&
    Object.entries(assets).every(
      ([candidate, quantity]) =>
        candidate === "lovelace" || candidate === unit || quantity === 0n,
    )
  );
};

const bodyOutputs = (
  txHash: string,
  body: CML.TransactionBody,
): readonly FraudProofRawL1SnapshotV1["scopes"][number]["utxos"][number][] => {
  const outputs = body.outputs();
  return Object.freeze(
    Array.from({ length: outputs.len() }, (_, outputIndex) => {
      const output = outputs.get(outputIndex);
      return Object.freeze({
        outRef: `${txHash}#${outputIndex.toString()}`,
        outputCbor: output.to_canonical_cbor_hex(),
        datumCbor: output.datum()?.as_datum()?.to_canonical_cbor_hex() ?? null,
        referenceScriptCbor:
          output.script_ref()?.to_canonical_cbor_hex() ?? null,
      });
    }),
  );
};

const decodeRegistryDatum = (
  datumCbor: string | null,
  computationThreadPolicyId: string,
  label: string,
): ClaimRegistryDatumData => {
  if (datumCbor === null) throw new Error(`${label} omitted its inline datum`);
  let datum: ClaimRegistryDatumData;
  try {
    datum = Data.from(datumCbor, ClaimRegistryDatum);
  } catch {
    throw new Error(`${label} datum is malformed`);
  }
  if (
    canonicalPlutusDataCbor(Data.to(datum, ClaimRegistryDatum)) !== datumCbor ||
    datum.computation_thread_policy_id !== computationThreadPolicyId
  ) {
    throw new Error(`${label} datum is non-canonical or policy-mismatched`);
  }
  return datum;
};

const policyMint = (
  body: CML.TransactionBody,
  policyId: string,
): readonly Readonly<{ assetName: string; quantity: bigint }>[] => {
  const mint = body.mint();
  if (mint === undefined) return Object.freeze([]);
  const policy = CML.ScriptHash.from_hex(policyId);
  const assets = mint.get_assets(policy);
  if (assets === undefined) return Object.freeze([]);
  const names = assets.keys();
  return Object.freeze(
    Array.from({ length: names.len() }, (_, index) => {
      const name = names.get(index);
      const quantity = assets.get(name);
      if (quantity === undefined || quantity === 0n) {
        throw new Error("claim-registry history contains a zero mint quantity");
      }
      return Object.freeze({ assetName: name.to_hex(), quantity });
    }),
  );
};

const entriesFromClaims = (
  claims: ReadonlyMap<string, string>,
): readonly KeyValuePhasEntry[] =>
  [...claims.entries()].map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));

const rootFromClaims = async (
  claims: ReadonlyMap<string, string>,
): Promise<string> =>
  (await keyValuePhasRootWithCount(entriesFromClaims(claims))).root;

/**
 * Reconstructs the singleton's entire claim set from the admitted public
 * registry-token lineage. Kupo coverage starts at genesis, Ogmios supplies
 * every exact transaction/input/output byte, and the lineage itself gives a
 * total order even when several mutations share one Cardano block.
 */
export const createAuthenticatedPublicL1ClaimRegistryProofDeriverV1 =
  (): ProductionClaimRegistryPublicProofDeriverV1 => ({
    deriverVersion: PRODUCTION_CLAIM_REGISTRY_PUBLIC_PROOF_DERIVER_V1,
    derive: async ({
      snapshot,
      claimId,
      kind,
      predecessorOutRef,
      predecessorDatum,
    }) => {
      if (!CLAIM_ID.test(claimId) || !OUT_REF.test(predecessorOutRef)) {
        throw new Error("claim-registry proof request identity is malformed");
      }
      const scope = snapshot.scopes.find(
        (candidate) => candidate.role === "claim_registry",
      );
      if (scope === undefined || scope.utxos.length !== 1) {
        throw new Error(
          "claim-registry public history omitted the singleton scope",
        );
      }
      const current = scope.utxos[0]!;
      if (current.outRef !== predecessorOutRef) {
        throw new Error(
          "claim-registry public history changed the requested predecessor",
        );
      }
      const history = snapshot.history[0];
      if (
        snapshot.history.length !== 1 ||
        history === undefined ||
        history.unit !== snapshot.historyUnits[0] ||
        history.fromGenesis !== true ||
        history.completeThroughPointId !== snapshot.cursor.point.pointId
      ) {
        throw new Error(
          "claim-registry proof derivation requires one complete from-genesis unit history",
        );
      }
      const registryUnit = history.unit;
      const txByHash = new Map(
        snapshot.transactions.map(
          (transaction) => [transaction.txHash, transaction] as const,
        ),
      );
      const creationByOutRef = new Map<
        string,
        {
          readonly transaction: (typeof snapshot.transactions)[number];
          readonly output: (typeof scope.utxos)[number];
        }
      >();
      for (const transaction of snapshot.transactions) {
        const body = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor);
        for (const output of bodyOutputs(transaction.txHash, body)) {
          if (outputCarriesExactRegistryUnit(output.outputCbor, registryUnit)) {
            if (creationByOutRef.has(output.outRef)) {
              throw new Error(
                "claim-registry public history duplicated an output identity",
              );
            }
            creationByOutRef.set(output.outRef, { transaction, output });
          }
        }
      }

      const reverse: readonly {
        readonly transaction: (typeof snapshot.transactions)[number];
        readonly output: (typeof scope.utxos)[number];
      }[] = (() => {
        const result: {
          transaction: (typeof snapshot.transactions)[number];
          output: (typeof scope.utxos)[number];
        }[] = [];
        const seen = new Set<string>();
        let cursor = predecessorOutRef;
        while (true) {
          if (seen.has(cursor)) {
            throw new Error(
              "claim-registry public history contains a lineage cycle",
            );
          }
          seen.add(cursor);
          const created = creationByOutRef.get(cursor);
          if (created === undefined) {
            throw new Error(
              `claim-registry public history omitted creation of ${cursor}`,
            );
          }
          result.push(created);
          const registryInputs = created.transaction.resolvedInputs.filter(
            (input) =>
              outputCarriesExactRegistryUnit(input.outputCbor, registryUnit),
          );
          if (registryInputs.length === 0) break;
          if (registryInputs.length !== 1) {
            throw new Error(
              "claim-registry mutation did not consume exactly one predecessor",
            );
          }
          cursor = registryInputs[0]!.outRef;
        }
        return Object.freeze(result);
      })();
      const ordered = [...reverse].reverse();
      if (
        ordered.length !== history.transactionHashes.length ||
        ordered.some(
          ({ transaction }) =>
            !history.transactionHashes.includes(transaction.txHash),
        ) ||
        history.transactionHashes.some((txHash) => !txByHash.has(txHash))
      ) {
        throw new Error(
          "claim-registry public unit history contains an omitted, extra, or forked transition",
        );
      }

      const claims = new Map<string, string>();
      for (const [index, { transaction, output }] of ordered.entries()) {
        const body = CML.TransactionBody.from_cbor_hex(transaction.bodyCbor);
        const datum = decodeRegistryDatum(
          output.datumCbor,
          predecessorDatum.computation_thread_policy_id,
          `claim-registry lineage output ${output.outRef}`,
        );
        const registryMint = policyMint(body, registryUnit.slice(0, 56));
        const registryAssetName = registryUnit.slice(56);
        const exactRegistryMint = registryMint.filter(
          (entry) => entry.assetName === registryAssetName,
        );
        if (index === 0) {
          if (
            ordered.length === 0 ||
            exactRegistryMint.length !== 1 ||
            exactRegistryMint[0]!.quantity !== 1n ||
            datum.claims_root !== (await rootFromClaims(claims))
          ) {
            throw new Error(
              "claim-registry genesis did not mint the singleton at the empty root",
            );
          }
          continue;
        }
        if (exactRegistryMint.length !== 0) {
          throw new Error(
            "claim-registry mutation illegally minted or burned its singleton token",
          );
        }
        const threadMint = policyMint(
          body,
          predecessorDatum.computation_thread_policy_id,
        );
        if (
          threadMint.length !== 1 ||
          !CLAIM_ID.test(threadMint[0]!.assetName) ||
          (threadMint[0]!.quantity !== 1n && threadMint[0]!.quantity !== -1n)
        ) {
          throw new Error(
            "claim-registry mutation is not coupled to one exact computation-thread token",
          );
        }
        const mutation = threadMint[0]!;
        if (mutation.quantity === 1n) {
          if (claims.has(mutation.assetName)) {
            throw new Error("claim-registry history contains a duplicate open");
          }
          claims.set(mutation.assetName, CLAIM_REGISTRY_LIVE_VALUE);
          if (datum.claims_root !== (await rootFromClaims(claims))) {
            throw new Error("claim-registry open successor root is forged");
          }
          continue;
        }
        if (claims.get(mutation.assetName) !== CLAIM_REGISTRY_LIVE_VALUE) {
          throw new Error(
            "claim-registry history burned a non-live computation thread",
          );
        }
        const cancelled = new Map(claims);
        cancelled.delete(mutation.assetName);
        const closed = new Map(claims);
        closed.set(mutation.assetName, CLAIM_REGISTRY_CLOSED_VALUE);
        const [cancelRoot, closeRoot] = await Promise.all([
          rootFromClaims(cancelled),
          rootFromClaims(closed),
        ]);
        const isCancel = datum.claims_root === cancelRoot;
        const isClose = datum.claims_root === closeRoot;
        if (isCancel === isClose) {
          throw new Error(
            "claim-registry burn successor is neither exactly cancel nor exactly close",
          );
        }
        claims.clear();
        for (const [key, value] of isCancel ? cancelled : closed) {
          claims.set(key, value);
        }
      }
      const rebuiltRoot = await rootFromClaims(claims);
      if (
        rebuiltRoot !== predecessorDatum.claims_root ||
        decodeRegistryDatum(
          current.datumCbor,
          predecessorDatum.computation_thread_policy_id,
          "current claim-registry singleton",
        ).claims_root !== rebuiltRoot
      ) {
        throw new Error(
          "claim-registry public history does not reproduce the authenticated current root",
        );
      }
      const entries = await keyValuePhasRootWithCount(
        entriesFromClaims(claims),
      );
      const key = Buffer.from(claimId, "hex");
      const proof =
        kind === "open"
          ? claims.has(claimId)
            ? (() => {
                throw new Error("claim-registry open target already exists");
              })()
            : entries.entries.length === 0
              ? []
              : await keyValuePhasNonMembershipProof(entries, key)
          : claims.get(claimId) !== CLAIM_REGISTRY_LIVE_VALUE
            ? (() => {
                throw new Error(
                  "claim-registry cancel/close target is not live",
                );
              })()
            : await keyValuePhasProof(
                entries,
                key,
                Buffer.from(CLAIM_REGISTRY_LIVE_VALUE, "hex"),
              );
      return Object.freeze({
        schemaVersion: PRODUCTION_CLAIM_REGISTRY_PUBLIC_PROOF_V1,
        claimId,
        kind,
        predecessorOutRef,
        predecessorRoot: predecessorDatum.claims_root,
        proofCbor: canonicalPlutusDataCbor(Data.to(proof, Proof)),
      });
    },
  });

export type ProductionClaimRegistryPublicProofV1 = Readonly<{
  schemaVersion: typeof PRODUCTION_CLAIM_REGISTRY_PUBLIC_PROOF_V1;
  claimId: string;
  kind: ClaimRegistryMutationKind;
  predecessorOutRef: string;
  predecessorRoot: string;
  proofCbor: string;
}>;

export interface ProductionClaimRegistryPrerequisiteV1<
  Category extends FraudProofCatalogueCategoryName,
> {
  readonly prerequisiteVersion: typeof PRODUCTION_CLAIM_REGISTRY_PREREQUISITE_V1;
  readonly category: Category;
  /** Journal-visible publication prerequisite for every non-empty live proof. */
  readonly proofChunks: ProductionProofChunkPrerequisitePortV1<Category>;
  /**
   * Resolves the opaque atomic mutation immediately before the family builder.
   * A changed singleton out-ref/root fails closed and must be observed again.
   */
  resolveMutation(input: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowActionV1;
    readonly artifact: JournalJsonObjectV1;
  }): Promise<PreparedClaimRegistryMutationV1>;
}

type VerifiedLiveProofV1 = Readonly<{
  proof: ProofData;
  proofCbor: string;
  mutation: PreparedClaimRegistryMutationV1;
}>;

const exactRecord = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  const parsed = value as Readonly<Record<string, unknown>>;
  const actual = Object.keys(parsed).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
};

const parsePublicProof = ({
  value,
  claimId,
  kind,
  predecessorOutRef,
  predecessorRoot,
}: {
  readonly value: unknown;
  readonly claimId: string;
  readonly kind: ClaimRegistryMutationKind;
  readonly predecessorOutRef: string;
  readonly predecessorRoot: string;
}): ProductionClaimRegistryPublicProofV1 => {
  const parsed = exactRecord(
    value,
    [
      "schemaVersion",
      "claimId",
      "kind",
      "predecessorOutRef",
      "predecessorRoot",
      "proofCbor",
    ],
    "claim-registry public proof",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_CLAIM_REGISTRY_PUBLIC_PROOF_V1 ||
    parsed.claimId !== claimId ||
    parsed.kind !== kind ||
    parsed.predecessorOutRef !== predecessorOutRef ||
    parsed.predecessorRoot !== predecessorRoot ||
    typeof parsed.proofCbor !== "string"
  ) {
    throw new Error(
      "claim-registry public proof changed its live action/root identity",
    );
  }
  let proof: ProofData;
  let proofCbor: string;
  try {
    proof = Data.from(parsed.proofCbor, Proof);
    proofCbor = canonicalPlutusDataCbor(Data.to(proof, Proof));
  } catch {
    throw new Error("claim-registry public proof is not canonical Proof CBOR");
  }
  if (proofCbor !== parsed.proofCbor) {
    throw new Error("claim-registry public proof is not canonical Proof CBOR");
  }
  return Object.freeze({
    schemaVersion: PRODUCTION_CLAIM_REGISTRY_PUBLIC_PROOF_V1,
    claimId,
    kind,
    predecessorOutRef,
    predecessorRoot,
    proofCbor,
  });
};

/**
 * Pure local admission boundary for an untrusted public proof. The returned
 * proof has already been folded against the exact authenticated predecessor;
 * transaction construction repeats the same check against Lucid's singleton.
 */
export const verifyProductionClaimRegistryPublicProofV1 = ({
  value,
  claimId,
  kind,
  predecessorOutRef,
  predecessorDatum,
}: {
  readonly value: unknown;
  readonly claimId: string;
  readonly kind: ClaimRegistryMutationKind;
  readonly predecessorOutRef: string;
  readonly predecessorDatum: ClaimRegistryDatumData;
}): Readonly<{ proof: ProofData; proofCbor: string }> => {
  const admitted = parsePublicProof({
    value,
    claimId,
    kind,
    predecessorOutRef,
    predecessorRoot: predecessorDatum.claims_root,
  });
  const proof = Data.from(admitted.proofCbor, Proof);
  buildClaimRegistryMutationTransition({
    currentDatum: predecessorDatum,
    kind,
    claimId,
    proof,
    carriage: { kind: "redeemer-carried" },
  });
  return Object.freeze({ proof, proofCbor: admitted.proofCbor });
};

const decodeAuthenticSingleton = ({
  snapshot,
  address,
  unit,
  computationThreadPolicyId,
}: {
  readonly snapshot: FraudProofRawL1SnapshotV1;
  readonly address: string;
  readonly unit: string;
  readonly computationThreadPolicyId: string;
}): Readonly<{
  outRef: string;
  datum: ClaimRegistryDatumData;
}> => {
  const scope = snapshot.scopes.find(
    (candidate) => candidate.role === "claim_registry",
  );
  if (
    scope === undefined ||
    scope.address !== address ||
    scope.utxos.length !== 1
  ) {
    throw new Error(
      "authenticated raw L1 did not expose exactly one claim-registry singleton",
    );
  }
  const raw = scope.utxos[0]!;
  if (!OUT_REF.test(raw.outRef) || raw.datumCbor === null) {
    throw new Error("claim-registry singleton omitted its exact inline datum");
  }
  const output = CML.TransactionOutput.from_cbor_hex(raw.outputCbor);
  const assets = coreToTxOutput(output).assets;
  if (
    (assets[unit] ?? 0n) !== 1n ||
    Object.entries(assets).some(
      ([candidate, quantity]) =>
        candidate !== "lovelace" && candidate !== unit && quantity !== 0n,
    )
  ) {
    throw new Error(
      "claim-registry singleton does not carry the exact sole registry token",
    );
  }
  let datum: ClaimRegistryDatumData;
  try {
    datum = Data.from(raw.datumCbor, ClaimRegistryDatum);
  } catch {
    throw new Error("claim-registry singleton datum is malformed");
  }
  if (
    canonicalPlutusDataCbor(Data.to(datum, ClaimRegistryDatum)) !==
      raw.datumCbor ||
    datum.computation_thread_policy_id !== computationThreadPolicyId
  ) {
    throw new Error(
      "claim-registry singleton datum is non-canonical or policy-mismatched",
    );
  }
  return Object.freeze({ outRef: raw.outRef, datum });
};

/**
 * Builds the shared claim prerequisite. No artifact may supply a root/proof:
 * the action mapping supplies only Open/Cancel/Close, the claim id is derived
 * from the deployment category and workflow header, and the proof deriver sees
 * only the admitted complete public registry-token history.
 */
export const createProductionClaimRegistryPrerequisiteV1 = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  categoryId,
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  computationThreadPolicyId,
  claimRegistryAddress,
  hubOraclePolicyId,
  rawL1,
  releaseFinality: releaseFinalityInput,
  publications,
  proofs,
  mutationForAction,
  transactionConfirmed,
}: {
  readonly category: Category;
  readonly categoryId: string;
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly computationThreadPolicyId: string;
  readonly claimRegistryAddress: string;
  readonly hubOraclePolicyId: string;
  readonly rawL1: FraudProofRawL1SnapshotAuthorityV1;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicyV1;
  readonly publications: FraudProofAuthenticatedPublicationObserverV1;
  readonly proofs: ProductionClaimRegistryPublicProofDeriverV1;
  readonly mutationForAction: (input: {
    readonly action: FraudProofWorkflowActionV1;
    readonly artifact: JournalJsonObjectV1;
  }) => ProductionClaimRegistryActionV1 | null;
  readonly transactionConfirmed: (input: {
    readonly headerHash: string;
    readonly txHash: string;
  }) => Promise<boolean>;
}): ProductionClaimRegistryPrerequisiteV1<Category> => {
  if (!/^[0-9a-f]{8}$/u.test(categoryId)) {
    throw new Error(
      "claim-registry category id must be four lowercase hex bytes",
    );
  }
  if (!/^[0-9a-f]{56}$/u.test(computationThreadPolicyId)) {
    throw new Error(
      "claim-registry computation-thread policy must be 28-byte hex",
    );
  }
  if (!/^[0-9a-f]{56}$/u.test(hubOraclePolicyId)) {
    throw new Error("claim-registry hub policy must be 28-byte hex");
  }
  if (rawL1.authorityVersion !== FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY_V1) {
    throw new Error(
      "claim-registry prerequisite requires raw public L1 history",
    );
  }
  if (
    proofs.deriverVersion !== PRODUCTION_CLAIM_REGISTRY_PUBLIC_PROOF_DERIVER_V1
  ) {
    throw new Error(
      "claim-registry prerequisite requires the public proof deriver V1",
    );
  }
  const releaseFinality =
    validateVerifiedFraudProofReleaseFinalityPolicyV1(releaseFinalityInput);
  const unit = claimRegistryUnit(hubOraclePolicyId);

  const liveProof = async ({
    headerHash,
    action,
    artifact,
  }: {
    readonly headerHash: string;
    readonly action: FraudProofWorkflowActionV1;
    readonly artifact: JournalJsonObjectV1;
  }): Promise<VerifiedLiveProofV1 | null> => {
    const requested = mutationForAction({ action, artifact });
    if (requested === null) return null;
    const claimId = claimIdFromCategoryAndHeader(categoryId, headerHash);
    const request = {
      deploymentIdentityDigest: releaseFinality.deploymentIdentityDigest,
      releaseIdentityDigest: releaseFinality.releaseIdentityDigest,
      finalityPolicyDigest: releaseFinality.policyDigest,
      headerHash,
      scopes: [
        { role: "claim_registry" as const, address: claimRegistryAddress },
      ],
      historyUnits: [unit],
    };
    const snapshot = admitFraudProofRawL1SnapshotV1({
      value: await rawL1.capture(request),
      request,
      releaseFinality,
    });
    const predecessor = decodeAuthenticSingleton({
      snapshot,
      address: claimRegistryAddress,
      unit,
      computationThreadPolicyId,
    });
    const verified = verifyProductionClaimRegistryPublicProofV1({
      value: await proofs.derive({
        snapshot,
        claimId,
        kind: requested.kind,
        predecessorOutRef: predecessor.outRef,
        predecessorDatum: predecessor.datum,
      }),
      claimId,
      kind: requested.kind,
      predecessorOutRef: predecessor.outRef,
      predecessorDatum: predecessor.datum,
    });
    const mutation = await prepareDeploymentClaimRegistryMutationV1({
      lucid,
      blueprint,
      deploymentInfo,
      network,
      computationThreadPolicyId,
      claimId,
      kind: requested.kind,
      evidence: { proof: verified.proof },
    });
    if (
      mutation.predecessorOutRef !== predecessor.outRef ||
      mutation.predecessorDatum.claims_root !== predecessor.datum.claims_root ||
      mutation.predecessorDatum.computation_thread_policy_id !==
        predecessor.datum.computation_thread_policy_id ||
      mutation.registryUtxo.address !== claimRegistryAddress
    ) {
      throw new Error(
        "claim-registry singleton changed after public proof derivation; refetch required",
      );
    }
    return Object.freeze({
      proof: verified.proof,
      proofCbor: verified.proofCbor,
      mutation,
    });
  };

  const proofChunks = createAuthenticatedProofChunkPrerequisitePortV1({
    category,
    lucid,
    network,
    signer,
    publications,
    proofCborForAction: async ({ headerHash, action, artifact }) =>
      (await liveProof({ headerHash, action, artifact }))?.proofCbor ?? null,
    transactionConfirmed,
  });

  return Object.freeze({
    prerequisiteVersion: PRODUCTION_CLAIM_REGISTRY_PREREQUISITE_V1,
    category,
    proofChunks,
    resolveMutation: async ({
      headerHash,
      action,
      artifact,
    }: {
      readonly headerHash: string;
      readonly action: FraudProofWorkflowActionV1;
      readonly artifact: JournalJsonObjectV1;
    }) => {
      const resolved = await liveProof({ headerHash, action, artifact });
      if (resolved === null) {
        throw new Error("workflow action is not a claim-registry boundary");
      }
      if (splitProofIntoChunkDatums(resolved.proofCbor).length === 0) {
        return resolved.mutation;
      }
      const chunks = await resolvePublishedProofChunksV1({
        lucid,
        address: signer.address,
        proofCbor: resolved.proofCbor,
      });
      if (chunks === undefined) {
        throw new Error(
          "claim-registry mutation cannot bypass its journaled proof publication",
        );
      }
      const mutation = await prepareDeploymentClaimRegistryMutationV1({
        lucid,
        blueprint,
        deploymentInfo,
        network,
        computationThreadPolicyId,
        claimId: resolved.mutation.claimId,
        kind: mutationForAction({ action, artifact })!.kind,
        evidence: { proof: resolved.proof, publishedProofChunks: chunks },
      });
      if (
        mutation.predecessorOutRef !== resolved.mutation.predecessorOutRef ||
        mutation.predecessorDatum.claims_root !==
          resolved.mutation.predecessorDatum.claims_root
      ) {
        throw new Error(
          "claim-registry singleton changed before atomic mutation; refetch required",
        );
      }
      return mutation;
    },
  });
};
