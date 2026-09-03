/**
 * `value-not-preserved` evidence builders (offchain plan §1.5/§4).
 *
 * Everything here produces the exact wire values the on-chain steps verify:
 *
 * - the 38-byte ledger out-ref key (`encode_midgard_tx_input`'s §5.3
 *   spelling) and the MPF membership proof of a spent input's descriptor
 *   under the challenged header's `prev_utxos_root` — the single
 *   ledger-membership witness path, shared with `apply_l2_spends`;
 * - the full authenticated asset-leaf walk of a descriptor for a token
 *   claim: every leaf `0..asset_count-1` with its frontier siblings, which
 *   is what makes a zero contribution an established absence on-chain;
 * - the §8.8 tier-1 (`Inline`) field openings for the spend-inputs, outputs
 *   and mint preimages.
 *
 * The builders fail closed on any disagreement between the descriptor bytes
 * and the material they are asked to open — a witness this module refuses
 * would have aborted on-chain after the thread had already paid for the
 * step.
 */
import {
  buildMidgardLedgerOutputAssetFrontier,
  decodeMidgardLedgerOutputCommitment,
  type MidgardLedgerOutputAsset,
} from "@al-ft/midgard-core";
import { buildMidgardValidationMerkleMembership } from "@al-ft/midgard-core";
import type { MidgardValue } from "@al-ft/midgard-core/codec";
import {
  encodeMidgardTxInputCanonical,
  type FieldCarriage,
  type FieldOpening,
  fieldOpeningForField,
  MIDGARD_FIELD_INDEX,
  type MidgardTxInput,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { VALUE_NOT_PRESERVED_CATEGORY_LABEL } from "./contracts-v1.js";
import type {
  AssetLeafOpening,
  ClaimedAsset,
  FrontierPeak,
  SpentInputValueWitness,
} from "./schemas-v1.js";

const evidenceError = (message: string): Error =>
  new Error(`${VALUE_NOT_PRESERVED_CATEGORY_LABEL} evidence: ${message}`);

// ---------------------------------------------------------------------------
// Units and values
// ---------------------------------------------------------------------------

/**
 * Flattens a `MidgardValue`'s assets in the canonical §5.3 order (policy
 * bytewise, then asset name length-first bytewise) — the exact leaf order of
 * the descriptor's asset frontier and of a canonical mint field.
 */
export const flattenMidgardValueAssets = (
  value: MidgardValue,
): readonly MidgardLedgerOutputAsset[] => {
  const flattened: MidgardLedgerOutputAsset[] = [];
  const policies = [...value.assets.keys()].sort((left, right) =>
    Buffer.compare(Buffer.from(left, "hex"), Buffer.from(right, "hex")),
  );
  for (const policyHex of policies) {
    const names = [...(value.assets.get(policyHex) ?? new Map()).keys()].sort(
      (left, right) => {
        const leftBytes = Buffer.from(left, "hex");
        const rightBytes = Buffer.from(right, "hex");
        return (
          leftBytes.length - rightBytes.length ||
          Buffer.compare(leftBytes, rightBytes)
        );
      },
    );
    for (const nameHex of names) {
      const quantity = value.assets.get(policyHex)?.get(nameHex);
      if (quantity === undefined || quantity === 0n) continue;
      flattened.push({
        policyId: Buffer.from(policyHex, "hex"),
        assetName: Buffer.from(nameHex, "hex"),
        quantity,
      });
    }
  }
  return flattened;
};

/** Whether one flattened asset is the claimed unit. */
export const assetMatchesClaim = (
  claim: ClaimedAsset,
  asset: MidgardLedgerOutputAsset,
): boolean =>
  claim !== "AdaAsset" &&
  asset.policyId.toString("hex") === claim.TokenAsset.policy_id &&
  asset.assetName.toString("hex") === claim.TokenAsset.asset_name;

/**
 * The claimed asset's quantity in one `MidgardValue` — the machine's
 * per-value contribution restricted to the claimed unit.
 */
export const claimedQuantityOfValue = (
  claim: ClaimedAsset,
  value: MidgardValue,
): bigint => {
  if (claim === "AdaAsset") return value.lovelace;
  return flattenMidgardValueAssets(value).reduce(
    (total, asset) =>
      assetMatchesClaim(claim, asset) ? total + asset.quantity : total,
    0n,
  );
};

// ---------------------------------------------------------------------------
// The ledger out-ref key and membership proof
// ---------------------------------------------------------------------------

/**
 * Twin of `encode_midgard_tx_input` — the §5.3 fixed 38-byte spend-input
 * item IS the pre-state ledger trie's key for the out-ref.
 */
export const valueNotPreservedOutpointKey = (input: MidgardTxInput): Buffer =>
  encodeMidgardTxInputCanonical(input);

/**
 * A handle over the pre-state ledger MPF — the same minimal shape the
 * decoding family's evidence takes, so a fixture trie serves both.
 */
export type ValueNotPreservedLedgerTrieHandle = {
  readonly rootHex: string;
  readonly prove: (key: Buffer) => Promise<Buffer>;
};

/**
 * The `ledger_membership_proof` a `FoldInput` witness carries: the MPF proof
 * of the spent input's descriptor under the thread's `prev_utxos_root`.
 * Refuses a trie whose root is not that commitment — a proof from any other
 * tree would abort on-chain after the thread's unrepeatable bind.
 */
export const buildValueNotPreservedLedgerMembership = async ({
  trie,
  outpointKey,
  prevUtxosRootHex,
}: {
  readonly trie: ValueNotPreservedLedgerTrieHandle;
  readonly outpointKey: Buffer;
  readonly prevUtxosRootHex: string;
}): Promise<Proof> => {
  const trieRoot = trie.rootHex.toLowerCase();
  const committedRoot = prevUtxosRootHex.toLowerCase();
  if (trieRoot !== committedRoot) {
    throw evidenceError(
      `ledger trie root ${trieRoot} is not the thread's prev_utxos_root ${committedRoot}`,
    );
  }
  const proofCbor = await trie.prove(outpointKey);
  return Data.from(Buffer.from(proofCbor).toString("hex"), Proof);
};

// ---------------------------------------------------------------------------
// The spent-input value witness
// ---------------------------------------------------------------------------

/**
 * Builds one `SpentInputValueWitness`.
 *
 * For an ADA claim the walk is empty by requirement — the value is the
 * descriptor's own `lovelace` scalar. For a token claim the witness opens
 * EVERY leaf of the descriptor's asset frontier in index order, each with
 * its membership siblings; the builder re-derives the frontier from the
 * supplied assets and refuses if its commitment or count disagree with the
 * descriptor bytes, which is exactly the mismatch the on-chain walk would
 * refuse.
 */
export const buildSpentInputValueWitness = async ({
  claim,
  descriptorCbor,
  spentValue,
  trie,
  input,
  prevUtxosRootHex,
}: {
  readonly claim: ClaimedAsset;
  /** The exact committed `LedgerOutputCommitmentV1` bytes, hex. */
  readonly descriptorCbor: string;
  /** The spent output's value — the source of the asset-leaf walk. */
  readonly spentValue: MidgardValue;
  readonly trie: ValueNotPreservedLedgerTrieHandle;
  readonly input: MidgardTxInput;
  readonly prevUtxosRootHex: string;
}): Promise<SpentInputValueWitness> => {
  const descriptor = decodeMidgardLedgerOutputCommitment(
    Buffer.from(descriptorCbor, "hex"),
  );
  const ledgerMembershipProof = await buildValueNotPreservedLedgerMembership({
    trie,
    outpointKey: valueNotPreservedOutpointKey(input),
    prevUtxosRootHex,
  });
  if (claim === "AdaAsset") {
    return {
      descriptor_cbor: descriptorCbor,
      ledger_membership_proof: ledgerMembershipProof,
      asset_peaks: [],
      asset_openings: [],
    };
  }
  const assets = flattenMidgardValueAssets(spentValue);
  const frontier = buildMidgardLedgerOutputAssetFrontier(assets);
  if (frontier.count !== descriptor.assetCount) {
    throw evidenceError(
      `spent value flattens to ${frontier.count.toString()} asset leaves, but the descriptor commits ${descriptor.assetCount.toString()}`,
    );
  }
  if (
    Buffer.from(frontier.commitment).toString("hex") !==
    Buffer.from(descriptor.assetFrontierCommitment).toString("hex")
  ) {
    throw evidenceError(
      "spent value's asset frontier does not re-derive the descriptor's asset_frontier_commitment",
    );
  }
  const peaks: FrontierPeak[] = frontier.frontier.peaks.map((peak) => ({
    height: BigInt(peak.height),
    hash: Buffer.from(peak.hash).toString("hex"),
  }));
  const openings: AssetLeafOpening[] = assets.map((asset, index) => {
    const membership = buildMidgardValidationMerkleMembership(
      frontier.leaves,
      index,
    );
    return {
      policy_id: asset.policyId.toString("hex"),
      asset_name: asset.assetName.toString("hex"),
      quantity: asset.quantity,
      siblings: membership.siblings.map((sibling) =>
        Buffer.from(sibling).toString("hex"),
      ),
    };
  });
  return {
    descriptor_cbor: descriptorCbor,
    ledger_membership_proof: ledgerMembershipProof,
    asset_peaks: peaks,
    asset_openings: openings,
  };
};

/**
 * The claimed quantity a witness attests — the same fold the on-chain
 * `spent_input_claimed_quantity_v1` performs, used by the step-02 submitter
 * to pre-compute the advanced thread state.
 */
export const witnessClaimedQuantity = ({
  claim,
  witness,
}: {
  readonly claim: ClaimedAsset;
  readonly witness: SpentInputValueWitness;
}): bigint => {
  if (claim === "AdaAsset") {
    const descriptor = decodeMidgardLedgerOutputCommitment(
      Buffer.from(witness.descriptor_cbor, "hex"),
    );
    return descriptor.lovelace;
  }
  return witness.asset_openings.reduce(
    (total, opening) =>
      opening.policy_id === claim.TokenAsset.policy_id &&
      opening.asset_name === claim.TokenAsset.asset_name
        ? total + opening.quantity
        : total,
    0n,
  );
};

// ---------------------------------------------------------------------------
// §8.8 tier-1 field openings
// ---------------------------------------------------------------------------

/** The v1 carriage: the step's own redeemer carries the preimage inline. */
export const inlineFieldCarriage = (preimage: Buffer): FieldCarriage => ({
  Inline: { preimage: preimage.toString("hex") },
});

/** The field-0 (spend inputs) opening both step-02 arms carry. */
export const spendInputsOpening = ({
  nativeTxCompactCbor,
  spendInputsPreimageCbor,
}: {
  readonly nativeTxCompactCbor: string;
  readonly spendInputsPreimageCbor: Buffer;
}): FieldOpening =>
  fieldOpeningForField({
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    nativeTxCompactCbor,
    carriage: inlineFieldCarriage(spendInputsPreimageCbor),
  });
