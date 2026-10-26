/** Plans a whole `scriptIntegrityHashMismatch` proof from the bound source
 * and predicts its conviction before any transaction is spent. An honest leaf
 * is refused.
 *
 * The expected hash is computed from the language set of every execution the
 * transaction makes and from the committed redeemer witness hash. An accepted
 * leaf, normal or forced, convicts on a mismatch. A rejection recording this
 * reason convicts when there is no mismatch; a rejection recording another
 * reason convicts when there is a mismatch and that reason ranks after the
 * script-integrity phase. */
import {
  decodeMidgardAddressBytes,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardMintPolicyItem,
  decodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import {
  directPurposeBindTransitions,
  type DirectPurposeSourceEvidence,
  directPurposeSourceTransitions,
} from "../direct-purpose-family/source-walk.js";
import {
  buildDirectSourceIndexEvidence,
  directOutputAddress,
  type DirectScriptMaterial,
} from "../direct-script-derivation/index.js";
import {
  SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL,
  scriptIntegrityHashMismatchContractRoles,
  type ScriptIntegrityHashMismatchContracts,
} from "./contracts.js";
import {
  scriptIntegrityHashMismatchFieldPlans,
  type ScriptIntegrityHashMismatchTransition,
  sharedSourceTransition,
} from "./custody.js";
import {
  type ScriptIntegrityHashMismatchWalk,
  type ScriptIntegrityHashMismatchWalkBatch,
  scriptIntegrityHashMismatchWalkTransitions,
} from "./walk.js";

export class ScriptIntegrityHashMismatchHonestLeaf extends Error {
  constructor(message: string) {
    super(`${SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL}: ${message}`);
    this.name = "ScriptIntegrityHashMismatchHonestLeaf";
  }
}

/** The transaction's committed script integrity hash and the one its
 * executions require, from the committed redeemer witness hash of the witness
 * set the bound compact commits to. */
export const scriptIntegrityHashMismatchVerdict = (
  material: DirectScriptMaterial,
  languageBitmap: bigint,
) => {
  const committed = Buffer.from(
    material.material.compact.transactionBody.scriptIntegrityHash,
  ).toString("hex");
  const expected = SDK.allPurposesExpectedScriptIntegrityHash(
    decodeMidgardNativeTxWitnessSetCompact(
      material.material.proofSource.witnessSetCompactCbor,
    ).redeemerTxWitsHash,
    languageBitmap,
  );
  const mismatch = committed !== expected;
  return {
    committed,
    expected,
    mismatch,
    convicts: SDK.scriptIntegrityHashMismatchConvicts(
      material.bound.subject,
      material.bound.network_order_context,
      mismatch,
    ),
  };
};

/** The script hash of every purpose the walk resolves, in walk order: spends
 * with a script credential, mint policies, required observers, then
 * protected-script receive outputs. `null` when a spent input has no
 * predecessor descriptor, which the walk cannot show. */
const purposeScriptHashes = (
  material: DirectScriptMaterial,
): readonly string[] | null => {
  const spends: string[] = [];
  for (const item of material.fields[0]!) {
    const entry = material.ledger.root.entries.find((candidate) =>
      candidate.key.equals(item),
    );
    if (entry === undefined) return null;
    const credential = decodeMidgardAddressBytes(
      decodeMidgardLedgerOutputCommitment(entry.value).address,
    ).paymentCredential;
    if (credential.kind === "Script")
      spends.push(credential.hash.toString("hex"));
  }
  return [
    ...spends,
    ...material.fields[5]!.map((item) =>
      Buffer.from(decodeMidgardMintPolicyItem(item).policyId).toString("hex"),
    ),
    ...material.fields[3]!.map((item) => item.toString("hex")),
    ...material.fields[2]!.flatMap((item) => {
      const address = directOutputAddress(item);
      return address.protected && address.paymentCredential.kind === "Script"
        ? [address.paymentCredential.hash.toString("hex")]
        : [];
    }),
  ];
};

/** The walk's language bitmap from the selected sources; `null` when a
 * purpose's script has no source, so no walk completes. */
const walkedLanguageBitmap = (
  material: DirectScriptMaterial,
  evidence: DirectPurposeSourceEvidence,
): bigint | null => {
  const hashes = purposeScriptHashes(material);
  if (hashes === null) return null;
  const executions: SDK.DirectAllPurposesExecution[] = [];
  for (const scriptHash of hashes) {
    const selected = evidence.selected.get(scriptHash);
    if (selected === undefined) return null;
    executions.push({
      scriptHash,
      languageTag: selected.descriptor.language_tag,
    });
  }
  return SDK.allPurposesFacts(executions).language_bitmap;
};

export type ScriptIntegrityHashMismatchProofPlan = {
  readonly evidence: DirectPurposeSourceEvidence;
  readonly plan: ReturnType<typeof scriptIntegrityHashMismatchFieldPlans>;
  /** Every custody transition from `bindEvent` to the terminal's input. */
  readonly transitions: readonly ScriptIntegrityHashMismatchTransition[];
  /** The completed walk the terminal reads. */
  readonly final: SDK.DirectAllPurposesState;
  /** The all-purposes walk, paged from any thread datum it holds. */
  readonly walk: ScriptIntegrityHashMismatchWalk;
  readonly committed: string;
  readonly expected: string;
  readonly mismatch: boolean;
  readonly convicts: boolean;
};

export const prepareScriptIntegrityHashMismatchProof = async ({
  material,
  contracts,
  prover,
  batch,
}: {
  readonly material: DirectScriptMaterial;
  readonly contracts: ScriptIntegrityHashMismatchContracts;
  readonly prover: string;
  readonly batch?: ScriptIntegrityHashMismatchWalkBatch;
}): Promise<ScriptIntegrityHashMismatchProofPlan> => {
  const roles = scriptIntegrityHashMismatchContractRoles(contracts);
  const evidence = await buildDirectSourceIndexEvidence(material);
  const plan = scriptIntegrityHashMismatchFieldPlans(material, prover);
  const source = [
    ...directPurposeBindTransitions(material, prover),
    ...directPurposeSourceTransitions({
      material,
      evidence,
      prover,
      plan,
      resolvedScriptHash: roles.sourceResolved.spendingScriptHash,
      witnessesScriptHash: roles.sourceWitnesses.spendingScriptHash,
    }),
  ].map(sharedSourceTransition);
  const walk = await scriptIntegrityHashMismatchWalkTransitions({
    material,
    evidence,
    prover,
    plan,
    spendScriptHash: roles.allPurposesSpend.spendingScriptHash,
    ...(batch === undefined ? {} : { batch }),
  });
  return {
    evidence,
    plan,
    transitions: [...source, ...walk.transitions],
    final: walk.final,
    walk,
    ...scriptIntegrityHashMismatchVerdict(
      material,
      walk.final.facts.language_bitmap,
    ),
  };
};

/** The plan, refused before spending when the completed walk cannot
 * convict. */
export const requireConvictingScriptIntegrityHashMismatchProof = async (
  params: Parameters<typeof prepareScriptIntegrityHashMismatchProof>[0],
): Promise<ScriptIntegrityHashMismatchProofPlan> => {
  const prepared = await prepareScriptIntegrityHashMismatchProof(params);
  if (!prepared.convicts)
    throw new ScriptIntegrityHashMismatchHonestLeaf(
      prepared.mismatch
        ? "the leaf records an earlier reason for the mismatch; nothing to prove"
        : "the committed script integrity hash is the expected one; nothing to prove",
    );
  return prepared;
};

/** Whether the bound leaf convicts, without planning custody; `null` when it
 * does not, or when a purpose's script or spent descriptor is absent and no
 * walk completes. */
export const scriptIntegrityHashMismatchDetection = async (
  material: DirectScriptMaterial,
): Promise<{ readonly mismatch: boolean } | null> => {
  const evidence = await buildDirectSourceIndexEvidence(material);
  const bitmap = walkedLanguageBitmap(material, evidence);
  if (bitmap === null) return null;
  const verdict = scriptIntegrityHashMismatchVerdict(material, bitmap);
  return verdict.convicts ? { mismatch: verdict.mismatch } : null;
};
