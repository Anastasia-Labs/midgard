import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  makeAuthorityRecord,
  recordName,
  sha256,
} from "../../src/runtime/trusted-head-authority.exact-record.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import {
  directory,
  head,
  policy,
  recordAuthenticationKey,
} from "./trusted-head-authority.policy.js";
export const legacyScene = async (count: number) => {
  const path = await directory(),
    finalityPolicy = policy();
  let prior: string | null = null;
  for (let i = 0; i < count; i++) {
    const record = makeAuthorityRecord({
      head: head(finalityPolicy, i, "77"),
      priorRecordSha256: prior,
      recordAuthenticationKey,
    });
    const raw = watcherCanonicalJson(record);
    await writeFile(join(path, recordName(BigInt(i))), raw);
    prior = sha256(raw);
  }
  return { path, policy: finalityPolicy, recordAuthenticationKey };
};
