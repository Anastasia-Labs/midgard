import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { MpfError } from "../../../mpf/index.js";
import {
  branchRootFromNeighbors,
  computeBranchHash,
  computeLeafHash,
  digest,
  merkleRoot16,
  MPF_NULL_ROOT,
  nibbles,
  NON_MEMBERSHIP_DUMMY_VALUE,
  normalizeVerifiedPhasRoot,
  parseProofInteger,
  type PhasProofTraversalMode,
  proofBytes,
} from "./phas.key-value-phas-non-membership-proof.js";

const traversePhasProof = (
  mode: PhasProofTraversalMode,
  path: string,
  cursor: number,
  proof: readonly SDK.ProofStep[],
  index: number,
): Buffer => {
  const step = proof[index];
  if (step === undefined) {
    return mode.kind === "including"
      ? computeLeafHash(path.slice(cursor), mode.valueDigest)
      : MPF_NULL_ROOT;
  }
  if ("Branch" in step) {
    const skip = parseProofInteger(step.Branch.skip, "branch skip");
    const nextCursor = cursor + 1 + skip;
    const root = traversePhasProof(mode, path, nextCursor, proof, index + 1);
    const thisNibble = Number.parseInt(path[nextCursor - 1]!, 16);
    const prefix = path.slice(cursor, nextCursor - 1);
    return computeBranchHash(
      prefix,
      branchRootFromNeighbors(
        thisNibble,
        root,
        proofBytes(step.Branch.neighbors, "branch neighbors"),
      ),
    );
  }
  if ("Fork" in step) {
    const skip = parseProofInteger(step.Fork.skip, "fork skip");
    const neighbor = step.Fork.neighbor;
    if (mode.kind === "excluding" && proof[index + 1] === undefined) {
      const prefixParts =
        skip === 0
          ? [
              Buffer.from([
                parseProofInteger(neighbor.nibble, "fork neighbor nibble"),
              ]),
              proofBytes(neighbor.prefix, "fork neighbor prefix"),
            ]
          : [
              nibbles(path.slice(cursor, cursor + skip)),
              Buffer.from([
                parseProofInteger(neighbor.nibble, "fork neighbor nibble"),
              ]),
              proofBytes(neighbor.prefix, "fork neighbor prefix"),
            ];
      return digest(
        Buffer.concat([
          ...prefixParts,
          proofBytes(neighbor.root, "fork neighbor root"),
        ]),
      );
    }
    const nextCursor = cursor + 1 + skip;
    const root = traversePhasProof(mode, path, nextCursor, proof, index + 1);
    const thisNibble = Number.parseInt(path[nextCursor - 1]!, 16);
    const neighborNibble = parseProofInteger(
      neighbor.nibble,
      mode.kind === "including" ? "fork nibble" : "fork neighbor nibble",
    );
    if (neighborNibble === thisNibble) {
      throw new Error("Fork proof neighbor uses the proven path nibble");
    }
    const nodes: Record<number, Buffer> = {
      [thisNibble]: root,
      [neighborNibble]: digest(
        Buffer.concat([
          proofBytes(neighbor.prefix, "fork neighbor prefix"),
          proofBytes(neighbor.root, "fork neighbor root"),
        ]),
      ),
    };
    return computeBranchHash(
      path.slice(cursor, nextCursor - 1),
      merkleRoot16(nodes),
    );
  }
  if ("Leaf" in step) {
    const neighborPath = proofBytes(
      step.Leaf.key,
      "leaf neighbor key",
    ).toString("hex");
    if (mode.kind === "excluding" && proof[index + 1] === undefined) {
      return computeLeafHash(
        neighborPath.slice(cursor),
        proofBytes(step.Leaf.value, "leaf neighbor value"),
      );
    }
    const skip = parseProofInteger(step.Leaf.skip, "leaf skip");
    const nextCursor = cursor + 1 + skip;
    const root = traversePhasProof(mode, path, nextCursor, proof, index + 1);
    const thisNibble = Number.parseInt(path[nextCursor - 1]!, 16);
    if (
      mode.kind === "including" &&
      neighborPath.slice(0, cursor) !== path.slice(0, cursor)
    ) {
      throw new Error("Leaf proof neighbor is not under the expected prefix");
    }
    const neighborNibble = Number.parseInt(neighborPath[nextCursor - 1]!, 16);
    if (neighborNibble === thisNibble) {
      throw new Error("Leaf proof neighbor uses the proven path nibble");
    }
    const nodes: Record<number, Buffer> = {
      [thisNibble]: root,
      [neighborNibble]: computeLeafHash(
        neighborPath.slice(nextCursor),
        proofBytes(step.Leaf.value, "leaf neighbor value"),
      ),
    };
    return computeBranchHash(
      path.slice(cursor, nextCursor - 1),
      merkleRoot16(nodes),
    );
  }
  throw new Error("Unknown PHAS proof step");
};

export const rootFromPhasProof = ({
  key,
  value,
  proof,
  includingItem,
}: {
  readonly key: Buffer;
  readonly value?: Buffer;
  readonly proof: SDK.Proof;
  readonly includingItem: boolean;
}): Effect.Effect<string, MpfError, never> =>
  Effect.try({
    try: () => {
      if (includingItem && value === undefined) {
        throw new Error("PHAS membership proof verification requires a value");
      }
      const path = digest(key).toString("hex");
      const root = includingItem
        ? traversePhasProof(
            { kind: "including", valueDigest: digest(value!) },
            path,
            0,
            proof,
            0,
          )
        : traversePhasProof({ kind: "excluding" }, path, 0, proof, 0);
      return normalizeVerifiedPhasRoot(root).toString("hex");
    },
    catch: (e) => MpfError.phasRoot(e),
  });

export const verifyKeyValuePhasMembershipProof = ({
  root,
  key,
  value,
  proof,
}: {
  readonly root: string;
  readonly key: Buffer;
  readonly value: Buffer;
  readonly proof: SDK.Proof;
}): Effect.Effect<void, MpfError, never> =>
  Effect.gen(function* () {
    const verifiedRoot = yield* rootFromPhasProof({
      key,
      value,
      proof,
      includingItem: true,
    });
    if (verifiedRoot !== root) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `PHAS membership proof root mismatch: expected=${root},actual=${verifiedRoot}`,
          ),
        ),
      );
    }
  });

export const verifyKeyValuePhasNonMembershipProof = ({
  root,
  key,
  proof,
}: {
  readonly root: string;
  readonly key: Buffer;
  readonly proof: SDK.Proof;
}): Effect.Effect<void, MpfError, never> =>
  Effect.gen(function* () {
    const verifiedRoot = yield* rootFromPhasProof({
      key,
      proof,
      includingItem: false,
    });
    yield* rootFromPhasProof({
      key,
      value: NON_MEMBERSHIP_DUMMY_VALUE,
      proof,
      includingItem: true,
    });
    if (verifiedRoot !== root) {
      return yield* Effect.fail(
        MpfError.phasRoot(
          new Error(
            `PHAS non-membership proof root mismatch: expected=${root},actual=${verifiedRoot}`,
          ),
        ),
      );
    }
  });
