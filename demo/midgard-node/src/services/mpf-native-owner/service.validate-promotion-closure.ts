import { assertNativeMpfHashHex } from "./protocol.js";
import {
  type DecodedPromotionRecord,
  EMPTY_ROOT_HEX,
} from "./service.normalize-owner-options.js";
import { keyNibbles } from "./service.parse-promotion-records.js";

/** Refuses a promotion whose records are not exactly the new nodes reachable
 * from `candidateRoot`, each linked at its own path. A child hash with no
 * record names a node the store must already hold; the store's `adopt`
 * refuses one it does not, before it writes anything. */
export const validatePromotionClosure = (
  candidateRoot: string,
  baseRoot: string,
  records: readonly DecodedPromotionRecord[],
): void => {
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
  const pending: { hash: string; path: string }[] = [
    { hash: candidateRoot, path: "" },
  ];
  for (let next = pending.pop(); next !== undefined; next = pending.pop()) {
    const { hash, path } = next;
    const record = byHash.get(hash);
    if (record === undefined || visited.has(hash)) continue;
    visited.add(hash);
    const nodePath = path + record.stored.prefix;
    if (record.stored.__kind === "Leaf") {
      if (keyNibbles(record.stored.key) !== nodePath) {
        throw new Error(
          `Native MPF promoted leaf ${hash} is linked at the wrong path`,
        );
      }
      continue;
    }
    record.stored.children.forEach((child, branch) => {
      if (child !== null)
        pending.push({ hash: child, path: nodePath + branch.toString(16) });
    });
  }
  if (visited.size !== records.length) {
    throw new Error(
      `Native MPF promotion contains unreachable generated records: reachable=${visited.size.toString()},records=${records.length.toString()}`,
    );
  }
};
