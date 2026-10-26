/** Plans a whole `unusedScriptWitness` proof from the bound source and
 * predicts its conviction before any transaction is spent. An honest leaf is
 * refused.
 *
 * A field-6 witness is used when it is the source selected for its script
 * hash and some purpose of the transaction executes that hash. An accepted
 * leaf, normal or forced, convicts on an unused witness. A rejection
 * recording this reason convicts when its witness is used or absent; a
 * rejection recording another reason convicts when a witness is unused and
 * that reason ranks after the witness's phase-8 audit position. */
import {
  decodeMidgardAddressBytes,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardMintPolicyItem,
  decodeMidgardVersionedScriptEnvelope,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import type { DirectPurposeTransition } from "../direct-purpose-family/custody.js";
import { directPurposeInputDescriptorsPresent } from "../direct-purpose-family/script-purposes.js";
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
  UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL,
  unusedScriptWitnessContractRoles,
  type UnusedScriptWitnessContracts,
} from "./contracts.js";
import {
  unusedScriptWitnessFieldPlans,
  unusedScriptWitnessSharedTransition,
  type UnusedScriptWitnessTransition,
} from "./custody.js";
import {
  type UnusedScriptWitnessWalk,
  type UnusedScriptWitnessWalkBatch,
  unusedScriptWitnessWalkTransitions,
} from "./walk.js";
import {
  unusedScriptWitnessFieldWalk,
  unusedScriptWitnessHashAt,
} from "./witness-walk.js";

export class UnusedScriptWitnessHonestLeaf extends Error {
  constructor(message: string) {
    super(`${UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL}: ${message}`);
    this.name = "UnusedScriptWitnessHonestLeaf";
  }
}

/** The script hash of every purpose the walk resolves: spends with a script
 * credential, mint policies, required observers, then protected-script
 * receive outputs. `null` when a spent input has no predecessor descriptor,
 * which the walk cannot show. */
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

/** What the terminal reads about the witness at `scriptIndex`, from the
 * selected sources and the purposes the walk reaches. */
const witnessFacts = (
  material: DirectScriptMaterial,
  evidence: DirectPurposeSourceEvidence,
  reached: ReadonlySet<string>,
  scriptIndex: bigint,
): SDK.UnusedScriptWitnessFacts => {
  const hash = unusedScriptWitnessHashAt(material.fields[6]!, scriptIndex);
  if (hash === null) return null;
  const selected = evidence.selected.get(hash)?.descriptor;
  return {
    selected:
      selected !== undefined &&
      selected.field_index === 6n &&
      selected.item_index === scriptIndex,
    reached: reached.has(hash),
  };
};

/** The leaf's recorded `UnusedScriptWitness` index, if it records one. */
const recordedScriptIndex = (material: DirectScriptMaterial): bigint | null => {
  const reason = material.bound.subject.rejection_reason;
  return reason !== null &&
    typeof reason === "object" &&
    "UnusedScriptWitness" in reason
    ? reason.UnusedScriptWitness.script_index
    : null;
};

export type UnusedScriptWitnessChoice = Readonly<{
  scriptIndex: bigint;
  unused: boolean;
}>;

/** The first witness index, in ascending order over every field-6 witness
 * and the leaf's recorded index, whose terminal convicts; `null` when none
 * does, or when a purpose's script has no source and no walk completes. */
const chooseWitness = (
  material: DirectScriptMaterial,
  evidence: DirectPurposeSourceEvidence,
): UnusedScriptWitnessChoice | null => {
  const hashes = purposeScriptHashes(material);
  if (hashes === null || hashes.some((hash) => !evidence.selected.has(hash)))
    return null;
  const reached = new Set(hashes);
  const recorded = recordedScriptIndex(material);
  const count = BigInt(material.fields[6]!.length);
  const candidates = [
    ...new Set([
      ...(recorded === null ? [] : [recorded]),
      ...Array.from({ length: Number(count) }, (_, index) => BigInt(index)),
    ]),
  ].sort((left, right) => (left < right ? -1 : left > right ? 1 : 0));
  for (const scriptIndex of candidates) {
    const unused = SDK.unusedScriptWitnessIsUnused(
      witnessFacts(material, evidence, reached, scriptIndex),
    );
    if (
      SDK.unusedScriptWitnessConvicts(
        material.bound.subject,
        material.bound.network_order_context,
        scriptIndex,
        unused,
      )
    )
      return { scriptIndex, unused };
  }
  return null;
};

/** Whether every field-6 item carries a versioned script envelope; the
 * source walk refuses a binding whose witness field does not. */
const witnessesDecode = (material: DirectScriptMaterial): boolean =>
  material.fields[6]!.every((item) => {
    try {
      decodeMidgardVersionedScriptEnvelope(item);
      return true;
    } catch {
      return false;
    }
  });

export type UnusedScriptWitnessProofPlan = {
  readonly evidence: DirectPurposeSourceEvidence;
  readonly plan: ReturnType<typeof unusedScriptWitnessFieldPlans>;
  readonly scriptIndex: bigint;
  /** The bind and source custody as the shared kit plans it, for paging. */
  readonly source: readonly DirectPurposeTransition[];
  /** Every custody transition from `bindEvent` to the terminal's input. */
  readonly transitions: readonly UnusedScriptWitnessTransition[];
  /** The all-purposes walk, paged from any thread datum it holds. */
  readonly walk: UnusedScriptWitnessWalk;
  /** The completed witness walk the terminal reads. */
  readonly final: SDK.UnusedScriptWitnessWalkState;
  /** The source selected for the witness's script hash, with its proof;
   * `null` when field 6 ends before the witness. */
  readonly selection: SDK.UnusedScriptWitnessSelection | null;
  readonly unused: boolean;
  readonly convicts: boolean;
};

export const prepareUnusedScriptWitnessProof = async ({
  material,
  contracts,
  prover,
  scriptIndex,
  batch,
}: {
  readonly material: DirectScriptMaterial;
  readonly contracts: UnusedScriptWitnessContracts;
  readonly prover: string;
  /** The witness to walk; the first convicting one otherwise. */
  readonly scriptIndex?: bigint;
  readonly batch?: UnusedScriptWitnessWalkBatch;
}): Promise<UnusedScriptWitnessProofPlan> => {
  const roles = unusedScriptWitnessContractRoles(contracts);
  const evidence = await buildDirectSourceIndexEvidence(material);
  const plan = unusedScriptWitnessFieldPlans(material, prover);
  const chosen =
    scriptIndex ??
    chooseWitness(material, evidence)?.scriptIndex ??
    // An honest leaf recording this reason still plans that witness's walk
    // so the caller can show the terminal refuse it.
    recordedScriptIndex(material);
  if (chosen === null)
    throw new UnusedScriptWitnessHonestLeaf(
      "no witness is unused before the leaf's reason",
    );
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
  ];
  const hash = unusedScriptWitnessHashAt(material.fields[6]!, chosen);
  const walk = await unusedScriptWitnessWalkTransitions({
    material,
    evidence,
    prover,
    plan,
    spendScriptHash: roles.allPurposesSpend.spendingScriptHash,
    probe: hash ?? "",
    ...(batch === undefined ? {} : { batch }),
  });
  const field = unusedScriptWitnessFieldWalk({
    scan: walk.final,
    scriptIndex: chosen,
    prover,
    plan,
  });
  const lookup = hash === null ? null : await evidence.lookup(hash);
  if (lookup !== null && lookup.selected === null)
    throw new Error(
      `${UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL}: witness ${chosen.toString()} is not in the source map`,
    );
  const selection =
    lookup === null ? null : { source: lookup.selected!, proof: lookup.proof };
  const unused = SDK.unusedScriptWitnessIsUnused(
    selection === null
      ? null
      : {
          selected:
            selection.source.field_index === 6n &&
            selection.source.item_index === chosen,
          reached: walk.final.facts.probe_reached,
        },
  );
  return {
    evidence,
    plan,
    scriptIndex: chosen,
    source,
    transitions: [
      ...source.map(unusedScriptWitnessSharedTransition),
      ...walk.transitions,
      ...field.transitions,
    ],
    walk,
    final: field.final,
    selection,
    unused,
    convicts: SDK.unusedScriptWitnessConvicts(
      material.bound.subject,
      material.bound.network_order_context,
      chosen,
      unused,
    ),
  };
};

/** The plan, refused before spending when the completed walk cannot
 * convict. */
export const requireConvictingUnusedScriptWitnessProof = async (
  params: Parameters<typeof prepareUnusedScriptWitnessProof>[0],
): Promise<UnusedScriptWitnessProofPlan> => {
  const prepared = await prepareUnusedScriptWitnessProof(params);
  if (!prepared.convicts)
    throw new UnusedScriptWitnessHonestLeaf(
      prepared.unused
        ? "the leaf records an earlier reason for the unused witness; nothing to prove"
        : "the witness is used; nothing to prove",
    );
  return prepared;
};

/** The convicting witness, without planning custody; `null` when none
 * convicts, when a spent or referenced input has no predecessor descriptor,
 * when a witness is not a versioned script, or when a purpose's script has
 * no source, so no walk completes. */
export const unusedScriptWitnessDetection = async (
  material: DirectScriptMaterial,
): Promise<UnusedScriptWitnessChoice | null> => {
  if (
    !directPurposeInputDescriptorsPresent(material) ||
    !witnessesDecode(material)
  )
    return null;
  return chooseWitness(
    material,
    await buildDirectSourceIndexEvidence(material),
  );
};
