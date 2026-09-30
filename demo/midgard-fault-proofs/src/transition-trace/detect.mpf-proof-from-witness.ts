import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { midgardMpfTerminalBranchKeepsTwoChildren } from "@al-ft/midgard-core";
import {
  decodeMidgardSpendInputItem,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";

import {
  detection,
  type TransitionTraceDetection,
} from "./detect.detect-count-faults.js";
import { acceptedTerminalPostRoot } from "./detect.detect-source-membership-mismatches.js";
import {
  TransitionTraceChallengerError,
  transitionTraceError,
} from "./errors.js";
import { type TransitionTraceReconstruction } from "./reconstruct.js";
import {
  type AcceptedTransactionTransitionMismatchEvidence,
  buildAcceptedTransactionTransitionMismatchFault,
} from "./witnesses.js";

export const detectAcceptedTransactionTransitionMismatches = (
  reconstruction: TransitionTraceReconstruction,
  evidence: readonly AcceptedTransactionTransitionMismatchEvidence[],
): readonly TransitionTraceDetection[] => {
  const detections: TransitionTraceDetection[] = [];
  for (const item of evidence) {
    if (item.claim.descriptor_membership.value.verdict !== "Accepted") {
      throw transitionTraceError(
        "missingWitnessData",
        "Accepted transition mismatch evidence must reference an accepted descriptor.",
      );
    }
    const committedPostRoot =
      item.claim.transition_step_membership.value.post_utxos_root;
    const validatedPostRoot = acceptedTerminalPostRoot(
      item.terminalAcceptanceWitnessCbor,
    );
    if (committedPostRoot !== validatedPostRoot) {
      detections.push(
        detection({
          reconstruction,
          kind: "acceptedTransactionTransitionMismatch",
          invariant: "accepted_transaction_uses_validated_ledger_root",
          diagnostic: `Accepted transaction transition commits ${committedPostRoot}, but its authenticated terminal validation witness commits ${validatedPostRoot}.`,
          fault: buildAcceptedTransactionTransitionMismatchFault(item),
        }),
      );
    }
  }
  return detections;
};

export const exactHexBytes = (value: string, label: string): Buffer => {
  if (
    value.length % 2 !== 0 ||
    (value.length > 0 && !/^[0-9a-f]+$/u.test(value))
  ) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} must be canonical lowercase hexadecimal bytes.`,
    );
  }
  return Buffer.from(value, "hex");
};

const exactProofInteger = (
  value: bigint,
  label: string,
  maximum: number,
): number => {
  if (value < 0n || value > BigInt(maximum)) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} is outside the supported MPF proof range.`,
    );
  }
  return Number(value);
};

export const mpfProofFromWitness = ({
  key,
  value,
  proof,
  label,
}: {
  readonly key: Buffer;
  readonly value: Buffer | undefined;
  readonly proof: SDK.Proof;
  readonly label: string;
}): MpfProof => {
  try {
    return MpfProof.fromJSON(
      key,
      value,
      proof.map((step, index) => {
        const stepLabel = `${label}[${index.toString()}]`;
        if ("Branch" in step) {
          const neighbors = exactHexBytes(
            step.Branch.neighbors,
            `${stepLabel}.neighbors`,
          );
          if (neighbors.length !== 4 * 32) {
            throw transitionTraceError(
              "missingWitnessData",
              `${stepLabel}.neighbors must contain exactly four MPF hashes.`,
            );
          }
          return {
            type: "branch",
            skip: exactProofInteger(step.Branch.skip, `${stepLabel}.skip`, 64),
            neighbors: step.Branch.neighbors,
          };
        }
        if ("Fork" in step) {
          const prefix = exactHexBytes(
            step.Fork.neighbor.prefix,
            `${stepLabel}.neighbor.prefix`,
          );
          const root = exactHexBytes(
            step.Fork.neighbor.root,
            `${stepLabel}.neighbor.root`,
          );
          if (
            prefix.length > 64 ||
            prefix.some((nibble) => nibble > 0x0f) ||
            root.length !== 32
          ) {
            throw transitionTraceError(
              "missingWitnessData",
              `${stepLabel}.neighbor is not a canonical MPF fork.`,
            );
          }
          return {
            type: "fork",
            skip: exactProofInteger(step.Fork.skip, `${stepLabel}.skip`, 64),
            neighbor: {
              nibble: exactProofInteger(
                step.Fork.neighbor.nibble,
                `${stepLabel}.neighbor.nibble`,
                15,
              ),
              prefix: step.Fork.neighbor.prefix,
              root: step.Fork.neighbor.root,
            },
          };
        }
        const neighborKey = exactHexBytes(step.Leaf.key, `${stepLabel}.key`);
        const neighborValue = exactHexBytes(
          step.Leaf.value,
          `${stepLabel}.value`,
        );
        if (neighborKey.length !== 32 || neighborValue.length !== 32) {
          throw transitionTraceError(
            "missingWitnessData",
            `${stepLabel} is not a canonical MPF leaf.`,
          );
        }
        return {
          type: "leaf",
          skip: exactProofInteger(step.Leaf.skip, `${stepLabel}.skip`, 64),
          neighbor: {
            key: step.Leaf.key,
            value: step.Leaf.value,
          },
        };
      }),
    );
  } catch (cause) {
    if (cause instanceof TransitionTraceChallengerError) {
      throw cause;
    }
    throw transitionTraceError(
      "missingWitnessData",
      `${label} is not a well-formed MPF proof.`,
      cause,
    );
  }
};

/** A delete witness whose proof ends in a Branch must keep two other children
 * in that branch, shown by its neighbour groups or by the group `opening`
 * (`terminal_branch_keeps_two_children`); otherwise the post-delete root it
 * derives is one the honest trie never holds, and the chain refuses it. */
export const requireDeletionKeepsTwoChildren = ({
  proof,
  opening,
  label,
}: {
  readonly proof: SDK.Proof;
  readonly opening: string;
  readonly label: string;
}): void => {
  const terminal = proof.at(-1);
  if (terminal === undefined || !("Branch" in terminal)) return;
  if (
    !midgardMpfTerminalBranchKeepsTwoChildren(
      exactHexBytes(terminal.Branch.neighbors, `${label}.neighbors`),
      exactHexBytes(opening, `${label}.opening`),
    )
  ) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} ends in a Branch that does not keep two other children.`,
    );
  }
};

export const normalizedMpfRoot = (
  root: Buffer | null,
  label: string,
): string => {
  if (root === null) {
    return SDK.EMPTY_MERKLE_TREE_ROOT;
  }
  if (root.length !== 32) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} did not derive a 32-byte MPF root.`,
    );
  }
  return root.toString("hex");
};

export const verifyProofRoot = ({
  proof,
  includingItem,
  expectedRoot,
  label,
}: {
  readonly proof: MpfProof;
  readonly includingItem: boolean;
  readonly expectedRoot: string;
  readonly label: string;
}): void => {
  let actualRoot: string;
  try {
    actualRoot = normalizedMpfRoot(proof.verify(includingItem), label);
  } catch (cause) {
    if (cause instanceof TransitionTraceChallengerError) {
      throw cause;
    }
    throw transitionTraceError(
      "missingWitnessData",
      `${label} cannot be replayed as an MPF proof.`,
      cause,
    );
  }
  if (actualRoot !== expectedRoot) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} does not open the current authenticated ledger root.`,
    );
  }
};

/**
 * The one valid byte form of a spend input here is the §5.3 field-0/1 item —
 * `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes — which is what
 * on-chain `ledger_outref_key` / `encode_midgard_tx_input` derives and what
 * `decode_midgard_tx_input_cbor` accepts. `encodeCbor([txId, index])` would
 * spell indices 0–23 minimally and reject every key the trie actually holds.
 */
export const canonicalNativeSpendInput = (
  bytes: Buffer,
  label: string,
): Buffer => {
  try {
    const input = decodeMidgardSpendInputItem(bytes);
    const canonical = encodeMidgardSpendInputItem(input);
    if (!canonical.equals(bytes)) {
      throw new Error("input CBOR is not the exact canonical encoding");
    }
    return canonical;
  } catch (cause) {
    throw transitionTraceError(
      "missingWitnessData",
      `${label} is not a canonical V1 MidgardTxInput.`,
      cause,
    );
  }
};

export type Tag4OutputAsset = {
  readonly policyId: Buffer;
  readonly assetName: Buffer;
  readonly quantity: bigint;
};

export type Tag4VersionedScript = {
  readonly language: 0n | 3n | 128n;
  readonly bytes: Buffer;
};

export type Tag4Output = {
  readonly address: Buffer;
  readonly lovelace: bigint;
  readonly assets: readonly Tag4OutputAsset[];
  readonly datumCbor?: Buffer;
  readonly scriptRef?: Tag4VersionedScript;
};

export const tag4ByteAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): number => {
  const value = bytes[offset];
  if (value === undefined) {
    throw new Error(`${label} exceeds the output byte length`);
  }
  return value;
};

export const tag4UintAt = (
  bytes: Buffer,
  offset: number,
  label: string,
): { readonly value: bigint; readonly nextOffset: number } => {
  const tag = tag4ByteAt(bytes, offset, label);
  if (tag <= 23) {
    return { value: BigInt(tag), nextOffset: offset + 1 };
  }
  const byteCount =
    tag === 24 ? 1 : tag === 25 ? 2 : tag === 26 ? 4 : tag === 27 ? 8 : 0;
  if (byteCount === 0 || offset + 1 + byteCount > bytes.length) {
    throw new Error(`${label} is not an Aiken V1 unsigned integer`);
  }
  let value = 0n;
  for (let index = 0; index < byteCount; index += 1) {
    value = value * 256n + BigInt(tag4ByteAt(bytes, offset + 1 + index, label));
  }
  return { value, nextOffset: offset + 1 + byteCount };
};
