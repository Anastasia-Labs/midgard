import type { Level } from "level";

import { assertNativeMpfHashHex } from "./protocol.js";
import { assertStoredNode } from "./service.encode-stored-node.js";
import {
  type DecodedPromotionRecord,
  EMPTY_ROOT_HEX,
  type StoredValue,
} from "./service.normalize-owner-options.js";
import { keyNibbles } from "./service.parse-promotion-records.js";

/** Refuses a promotion whose records are not exactly the nodes reachable from
 * `candidateRoot` that `db` does not hold, each linked at its own path. */
export const validatePromotionClosure = async (
  db: Level<string, StoredValue>,
  candidateRoot: string,
  baseRoot: string,
  records: readonly DecodedPromotionRecord[],
): Promise<void> => {
  assertNativeMpfHashHex(candidateRoot, "candidateRoot");
  const byHash = new Map(records.map((record) => [record.hashHex, record]));
  if (candidateRoot === baseRoot || candidateRoot === EMPTY_ROOT_HEX) {
    if (records.length !== 0) {
      throw new Error(
        "Empty or no-op native MPF promotion returned generated records",
      );
    }
    return;
  }
  if (!byHash.has(candidateRoot)) {
    throw new Error("Native MPF promotion closure is missing candidate root");
  }
  const visited = new Set<string>();
  const visit = async (hash: string, path: string): Promise<void> => {
    const record = byHash.get(hash);
    if (record === undefined) {
      try {
        assertStoredNode(await db.get(hash), hash);
        return;
      } catch (error) {
        throw new Error(
          `Native MPF promotion references missing durable child ${hash}: ${String(error)}`,
        );
      }
    }
    if (visited.has(hash)) return;
    visited.add(hash);
    const nodePath = path + record.stored.prefix;
    if (record.stored.__kind === "Leaf") {
      if (keyNibbles(record.stored.key) !== nodePath) {
        throw new Error(
          `Native MPF promoted leaf ${hash} is linked at the wrong path`,
        );
      }
      return;
    }
    await Promise.all(
      record.stored.children.map((child, branch) =>
        child === null
          ? undefined
          : visit(child, nodePath + branch.toString(16)),
      ),
    );
  };
  await visit(candidateRoot, "");
  if (visited.size !== records.length) {
    throw new Error(
      `Native MPF promotion contains unreachable generated records: reachable=${visited.size.toString()},records=${records.length.toString()}`,
    );
  }
};
